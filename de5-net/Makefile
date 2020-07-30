PEBBLES_ROOT ?= $(realpath ..)

.PHONY: one
one: checkenv
	make -C $(PEBBLES_ROOT)/src
	make -C $(PEBBLES_ROOT)/boot
	qsys-generate -syn SoC.qsys
	quartus_sh --flow compile DE5.qpf

.PHONY: many
many: checkenv
	make -C $(PEBBLES_ROOT)/src
	make -C $(PEBBLES_ROOT)/boot
	qsys-generate -syn SoC.qsys
	quartus_dse DE5.qpf    \
    --num-seeds 7               \
    --launcher local            \
    --num-concurrent 4
	quartus_dse DE5.qpf --report utilization
	quartus_dse DE5.qpf --report fmax_summary

.PHONY: report
report: checkenv
	quartus_dse DE5.qpf --report utilization
	quartus_dse DE5.qpf --report fmax_summary

.PHONY: download-sof
download-sof: checkenv
	quartus_pgm -m jtag -o "p;DE5.sof"

.PHONY: update-mif
update-mif: checkenv
	quartus_cdb --update_mif DE5.qpf
	quartus_asm DE5.qpf

.PHONY: clean
clean: clean-mif
	rm -f *.rpt *.msg *.summary *.sld *.sopcinfo *.jdi
	rm -f *.pin *.done *.qws *.sof *.csv *.qws *.smsg
	rm -rf dse* db incremental_db SoC .qsys_edit reconfig_mif SoC .qsys-edit

.PHONY: clean-mif
clean-mif:
	rm -f *.mif

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))
