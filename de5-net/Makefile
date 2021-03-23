PEBBLES_ROOT ?= $(realpath ..)

.PHONY: one
one: checkenv
	make -C $(PEBBLES_ROOT)/src
	make -C $(PEBBLES_ROOT)/boot
	qsys-generate -syn S5_DDR3_QSYS.qsys
	quartus_sh --flow compile Golden_top.qpf

.PHONY: many
many: checkenv
	make -C $(PEBBLES_ROOT)/src
	make -C $(PEBBLES_ROOT)/boot
	qsys-generate -syn S5_DDR3_QSYS.qsys
	quartus_dse Golden_top.qpf    \
    --num-seeds 7               \
    --launcher local            \
    --num-concurrent 4
	quartus_dse Golden_top.qpf --report utilization
	quartus_dse Golden_top.qpf --report fmax_summary

.PHONY: report
report: checkenv
	quartus_dse Golden_top.qpf --report utilization
	quartus_dse Golden_top.qpf --report fmax_summary

.PHONY: download-sof
download-sof: checkenv
	quartus_pgm -m jtag -o "p;Golden_top.sof"

.PHONY: update-mif
update-mif: checkenv
	quartus_cdb --update_mif Golden_top.qpf
	quartus_asm Golden_top.qpf

.PHONY: clean
clean: clean-mif
	rm -f *.rpt *.msg *.summary *.sld *.sopcinfo *.jdi
	rm -f *.pin *.done *.qws *.sof *.csv *.qws *.smsg *.mif
	rm -rf dse* db incremental_db .qsys_edit reconfig_mif
	rm -rf S5_DDR3_QSYS

.PHONY: clean-mif
clean-mif:
	rm -f *.mif

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))
