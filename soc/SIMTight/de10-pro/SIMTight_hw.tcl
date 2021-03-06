# TCL File Generated by Component Editor 16.1
# Tue Feb 16 11:35:54 GMT 2021
# DO NOT MODIFY


# 
# SIMTight "SIMTight" v1.0
#  2021.02.16.11:35:54
# 
# 

# 
# request TCL package from ACDS 16.1
# 
package require -exact qsys 16.1


# 
# module SIMTight
# 
set_module_property DESCRIPTION ""
set_module_property NAME SIMTight
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property AUTHOR ""
set_module_property DISPLAY_NAME SIMTight
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false


# 
# file sets
# 
add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL SIMTight
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file SIMTight.v VERILOG PATH ../src/SIMTight.v TOP_LEVEL_FILE


# 
# parameters
# 


# 
# display items
# 


# 
# connection point reset
# 
add_interface reset reset end
set_interface_property reset associatedClock clock_sink
set_interface_property reset synchronousEdges DEASSERT
set_interface_property reset ENABLED true
set_interface_property reset EXPORT_OF ""
set_interface_property reset PORT_NAME_MAP ""
set_interface_property reset CMSIS_SVD_VARIABLES ""
set_interface_property reset SVD_ADDRESS_GROUP ""

add_interface_port reset reset reset Input 1


# 
# connection point dram_master
# 
add_interface dram_master avalon start
set_interface_property dram_master addressUnits WORDS
set_interface_property dram_master associatedClock clock_sink
set_interface_property dram_master associatedReset reset
set_interface_property dram_master bitsPerSymbol 8
set_interface_property dram_master burstOnBurstBoundariesOnly false
set_interface_property dram_master burstcountUnits WORDS
set_interface_property dram_master doStreamReads false
set_interface_property dram_master doStreamWrites false
set_interface_property dram_master holdTime 0
set_interface_property dram_master linewrapBursts false
set_interface_property dram_master maximumPendingReadTransactions 0
set_interface_property dram_master maximumPendingWriteTransactions 0
set_interface_property dram_master readLatency 0
set_interface_property dram_master readWaitTime 1
set_interface_property dram_master setupTime 0
set_interface_property dram_master timingUnits Cycles
set_interface_property dram_master writeWaitTime 0
set_interface_property dram_master ENABLED true
set_interface_property dram_master EXPORT_OF ""
set_interface_property dram_master PORT_NAME_MAP ""
set_interface_property dram_master CMSIS_SVD_VARIABLES ""
set_interface_property dram_master SVD_ADDRESS_GROUP ""

add_interface_port dram_master in0_socDRAMIns_avl_dram_readdata readdata Input 512
add_interface_port dram_master in0_socDRAMIns_avl_dram_readdatavalid readdatavalid Input 1
add_interface_port dram_master in0_socDRAMIns_avl_dram_waitrequest waitrequest Input 1
add_interface_port dram_master out_socDRAMOuts_avl_dram_address address Output 26
add_interface_port dram_master out_socDRAMOuts_avl_dram_burstcount burstcount Output 4
add_interface_port dram_master out_socDRAMOuts_avl_dram_byteen byteenable Output 64
add_interface_port dram_master out_socDRAMOuts_avl_dram_read read Output 1
add_interface_port dram_master out_socDRAMOuts_avl_dram_write write Output 1
add_interface_port dram_master out_socDRAMOuts_avl_dram_writedata writedata Output 512


# 
# connection point uart_master
# 
add_interface uart_master avalon start
set_interface_property uart_master addressUnits SYMBOLS
set_interface_property uart_master associatedClock clock_sink
set_interface_property uart_master associatedReset reset
set_interface_property uart_master bitsPerSymbol 8
set_interface_property uart_master burstOnBurstBoundariesOnly false
set_interface_property uart_master burstcountUnits WORDS
set_interface_property uart_master doStreamReads false
set_interface_property uart_master doStreamWrites false
set_interface_property uart_master holdTime 0
set_interface_property uart_master linewrapBursts false
set_interface_property uart_master maximumPendingReadTransactions 0
set_interface_property uart_master maximumPendingWriteTransactions 0
set_interface_property uart_master readLatency 0
set_interface_property uart_master readWaitTime 1
set_interface_property uart_master setupTime 0
set_interface_property uart_master timingUnits Cycles
set_interface_property uart_master writeWaitTime 0
set_interface_property uart_master ENABLED true
set_interface_property uart_master EXPORT_OF ""
set_interface_property uart_master PORT_NAME_MAP ""
set_interface_property uart_master CMSIS_SVD_VARIABLES ""
set_interface_property uart_master SVD_ADDRESS_GROUP ""

add_interface_port uart_master in0_socUARTIns_avl_jtaguart_readdata readdata Input 32
add_interface_port uart_master in0_socUARTIns_avl_jtaguart_waitrequest waitrequest Input 1
add_interface_port uart_master out_socUARTOuts_avl_jtaguart_address address Output 3
add_interface_port uart_master out_socUARTOuts_avl_jtaguart_read read Output 1
add_interface_port uart_master out_socUARTOuts_avl_jtaguart_write write Output 1
add_interface_port uart_master out_socUARTOuts_avl_jtaguart_writedata writedata Output 32


# 
# connection point clock_sink
# 
add_interface clock_sink clock end
set_interface_property clock_sink clockRate 0
set_interface_property clock_sink ENABLED true
set_interface_property clock_sink EXPORT_OF ""
set_interface_property clock_sink PORT_NAME_MAP ""
set_interface_property clock_sink CMSIS_SVD_VARIABLES ""
set_interface_property clock_sink SVD_ADDRESS_GROUP ""

add_interface_port clock_sink clock clk Input 1

