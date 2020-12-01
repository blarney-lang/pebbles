#define NOTE(string)

NOTE("SoC config for Terasic's Stratix-V-based DE5-Net board")

NOTE("DRAM configuration")
NOTE("==================")

NOTE("Size of data field in DRAM request/response")
#define DRAMBeatBits 256
#define DRAMBeatBytes 32
#define DRAMBeatHalfs 16
#define DRAMBeatWords 8
#define DRAMBeatLogBytes 5

NOTE("Max burst size = 2^(DRAMBurstWidth-1)")
#define DRAMBurstWidth 4

NOTE("Size of DRAM in beats")
#define DRAMAddrWidth 23

NOTE("Maximum number of inflight requests supported by DRAM wrapper")
#define DRAMLogMaxInFlight 5
