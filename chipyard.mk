# Chipyard ingests this when mxgen is registered as a generator.
# Mirrors radiance's cvfpu setup: prepend cvfpu packages, expose include path.

mxgen_vsrc := $(base_dir)/generators/mxgen/src/main/resources/vsrc

EXT_INCDIRS += $(mxgen_vsrc)/cvfpu/src/common_cells/include

VCS_NONCC_OPTS := \
    $(mxgen_vsrc)/cvfpu/src/common_cells/src/cf_math_pkg.sv \
    $(mxgen_vsrc)/cvfpu/src/fpnew_pkg.sv \
    $(VCS_NONCC_OPTS)

ifeq ($(sim_name),verilator)
EXTRA_SIM_PREPROC_DEFINES += \
    $(mxgen_vsrc)/cvfpu/src/common_cells/src/cf_math_pkg.sv \
    $(mxgen_vsrc)/cvfpu/src/fpnew_pkg.sv

EXT_INCDIRS += \
    $(mxgen_vsrc)/cvfpu/src/common_cells \
    $(GEN_COLLATERAL_DIR)
endif
