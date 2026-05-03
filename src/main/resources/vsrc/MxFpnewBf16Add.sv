// MxFpnewBf16Add — multi-lane BF16 adder using cvfpu (fpnew) FMA in ADD mode.
//
// Per lane: result = a + b, where a/b are IEEE BF16 (1+8+7 = 16 bits).
// Internally each lane instantiates fpnew_fma with FpFormat=FP16ALT (BF16),
// op=ADD which sets the multiplicand to +1.0 so the unit computes b+c.
//
// Pipeline depth = NumPipeRegs (PipeConfig=DISTRIBUTED). Inputs are assumed
// always-valid; we tie in_valid_i=1 / out_ready_i=1 for streaming use, which
// matches how MxFpMul drives its adders.

module MxFpnewBf16Add #(
  parameter int unsigned NumLanes    = 1,
  parameter int unsigned NumPipeRegs = 0
) (
  input  logic                          clock,
  input  logic                          reset,
  input  logic [NumLanes-1:0][15:0]     a_i,
  input  logic [NumLanes-1:0][15:0]     b_i,
  output logic [NumLanes-1:0][15:0]     out_o
);
  // Active-low reset for fpnew_fma's `FF macros.
  logic rst_n;
  assign rst_n = ~reset;

  for (genvar i = 0; i < NumLanes; i++) begin : g_lane
    logic [2:0][15:0] operands;
    // op=ADD: fpnew_fma overrides operand_a to +1.0 internally and computes
    // operand_b + operand_c. Operand[0] is don't-care; route a→[1], b→[2].
    assign operands[0] = '0;
    assign operands[1] = a_i[i];
    assign operands[2] = b_i[i];

    logic [15:0] result;
    fpnew_pkg::status_t status_unused;
    logic                ext_bit_unused;
    logic                in_ready_unused;
    logic                out_valid_unused;
    logic                busy_unused;

    fpnew_fma #(
      .FpFormat    ( fpnew_pkg::FP16ALT       ),
      .NumPipeRegs ( NumPipeRegs              ),
      .PipeConfig  ( fpnew_pkg::DISTRIBUTED   ),
      .TagType     ( logic                    ),
      .AuxType     ( logic                    )
    ) i_fma (
      .clk_i           ( clock                  ),
      .rst_ni          ( rst_n                  ),
      .operands_i      ( operands               ),
      .is_boxed_i      ( 3'b111                 ),
      .rnd_mode_i      ( fpnew_pkg::RNE         ),
      .op_i            ( fpnew_pkg::ADD         ),
      .op_mod_i        ( 1'b0                   ),
      .tag_i           ( 1'b0                   ),
      .aux_i           ( 1'b0                   ),
      .in_valid_i      ( 1'b1                   ),
      .in_ready_o      ( in_ready_unused        ),
      .flush_i         ( 1'b0                   ),
      .result_o        ( result                 ),
      .status_o        ( status_unused          ),
      .extension_bit_o ( ext_bit_unused         ),
      .tag_o           (                        ),
      .aux_o           (                        ),
      .out_valid_o     ( out_valid_unused       ),
      .out_ready_i     ( 1'b1                   ),
      .busy_o          ( busy_unused            )
    );

    assign out_o[i] = result;
  end
endmodule
