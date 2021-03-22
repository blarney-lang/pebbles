PEBBLES_ROOT ?= $(realpath ..)
QPF = DE10_Pro.qpf

.PHONY: all
all: checkenv qsys
	quartus_dse $(QPF)            \
    --num-seeds 15              \
    --launcher local            \
    --num-concurrent 4
	quartus_dse $(QPF) --report utilization
	quartus_dse $(QPF) --report fmax_summary

.PHONY: qsys
qsys: checkenv
	make -C $(PEBBLES_ROOT)/src
	make -C $(PEBBLES_ROOT)/boot
	qsys-generate -syn DE10_Pro_QSYS.qsys --quartus-project=$(QPF)

.PHONY: one
one: checkenv qsys
	quartus_sh --flow compile $(QPF)

.PHONY: report
report: checkenv
	quartus_dse $(QPF) --report utilization
	quartus_dse $(QPF) --report fmax_summary

.PHONY: update-mif
update-mif: checkenv
	quartus_cdb --update_mif DE10_Pro.qpf
	quartus_asm DE10_Pro.qpf

.PHONY: download-sof
download-sof: checkenv
	quartus_pgm -m jtag -o "p;output_files/DE10_Pro.sof"

.PHONY: clean
clean:
	rm -rf tmp-clearbox synth_dumps qdb output_files
	rm -rf DE10_Pro_QSYS reset_release
	rm -f DE10_Pro.qws *.rpt *.csv
	ls ip/DE10_Pro_QSYS/ | grep -v -E '\.ip$$' \
    | xargs -i rm -rf ip/DE10_Pro_QSYS/{}

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))
