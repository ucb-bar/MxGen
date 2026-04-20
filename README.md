# MxGen

Configurable low-precision floating-point hardware generators for microscaled (MX) datapaths, written in Chisel.

## Supported Formats

| Format | Exponent | Significand | Bit Width | Elements per input |
|--------|----------|-------------|-----------|------------------|
| FP4 (E2M1) | 2 | 2 | 4 | 2 (dual) |
| FP6 E2M3 | 2 | 4 | 6 | 1 (single) |
| FP6 E3M2 | 3 | 3 | 6 | 2 (dual) |
| FP8 E4M3 | 4 | 4 | 8 | 1 (single) |
| FP8 E5M2 | 5 | 3 | 8 | 2 (dual) |

Dual-element formats (significand width < 4) pack two values per output slot and produce 2 or 4 outputs. Single-element formats produce 1 output.

## PE Multiplication Modes

The PE contains a 2x2 MACU (Multiply-Accumulate Unit) grid. Each format combination maps to a mode based on the activation and weight significand widths:

| Mode | Act Sig | Wei Sig | Act Inputs | Wei Inputs | Outputs | Format Combinations |
|------|---------|---------|------------|------------|---------|---------------------|
| 0 | 2 | 2 | 2 | 2 | 4 | FP4 x FP4 |
| 1 | 2 | 3 | 2 | 2 | 4 | FP4 x E3M2, FP4 x E5M2 |
| 2 | 2 | 4 | 2 | 1 | 2 | FP4 x E2M3, FP4 x E4M3 |
| 3 | 3 | 2 | 2 | 2 | 4 | E3M2 x FP4, E5M2 x FP4 |
| 4 | 3 | 3 | 2 | 2 | 4 | E3M2 x E3M2, E3M2 x E5M2, E5M2 x E3M2, E5M2 x E5M2 |
| 5 | 3 | 4 | 2 | 1 | 2 | E3M2 x E2M3, E3M2 x E4M3, E5M2 x E2M3, E5M2 x E4M3 |
| 6 | 4 | 2 | 1 | 2 | 2 | E2M3 x FP4, E4M3 x FP4 |
| 7 | 4 | 3 | 1 | 2 | 2 | E2M3 x E3M2, E2M3 x E5M2, E4M3 x E3M2, E4M3 x E5M2 |
| 8 | 4 | 4 | 1 | 1 | 1 | E2M3 x E2M3, E2M3 x E4M3, E4M3 x E2M3, E4M3 x E4M3 |

## Hardware Configurations

`MxConfig` controls which formats and modes are elaborated. Only the hardware needed for the selected formats is generated.

| Config | Formats | Modes | Cross-format |
|--------|---------|-------|--------------|
| `MxConfig.fp4Only` | FP4 | 0 | N/A |
| `MxConfig.fp6` | E2M3, E3M2 | 4, 5, 7, 8 | Yes |
| `MxConfig.fp8` | E4M3, E5M2 | 4, 5, 7, 8 | Yes |
| `MxConfig.mxGemmini` | FP4, E3M2, E4M3 | 0, 4, 8 | No (same-format only) |
| `MxConfig.all` | All 5 formats | 0-8 | Yes |

Key `MxConfig` parameters:

- `actFormats` / `weiFormats` — which MX formats to support
- `expAdderWidths` — per-lane exponent adder widths (must be wide enough for the widest exponent; use `Seq(5,5,5,5)` when E5M2 is included)
- `productFormat` / `accFormat` — intermediate product and accumulator precision
- `modesOverride` — explicitly restrict which PE modes are instantiated
- `laneOutputWidths` — optional per-lane output bit widths

## Building and Testing

Requires [Mill](https://mill-build.com/).

```bash
# Compile
./mill test.compile

# Run a single test
./mill test.testOnly mxgen.MxFpMul_FP4Only_BF16Out_Spec

# Run all tests with summary
./run_tests.sh
```

Tests are listed in `test/tests.yml`. To add a new test, append an entry:

```yaml
- spec: mxgen.YourNewTest_Spec
  label: Your Test Label
```
