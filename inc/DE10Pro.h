#define NOTE(string)

NOTE("SoC config for Terasic's Stratix-10-based DE10-Pro board")

NOTE("DRAM configuration")
NOTE("==================")

NOTE("Size of data field in DRAM request/response")
#define DRAMBeatBits 512
#define DRAMBeatBytes 64
#define DRAMBeatHalfs 32
#define DRAMBeatWords 16
#define DRAMBeatLogBytes 6

NOTE("Max burst size = 2^(DRAMBurstWidth-1)")
#define DRAMBurstWidth 4

NOTE("Size of DRAM in beats")
#define DRAMAddrWidth 26

NOTE("Maximum number of inflight requests supported by DRAM wrapper")
#define DRAMLogMaxInFlight 5

NOTE("SIMT configuration")
NOTE("==================")

NOTE("Number of SIMT lanes")
#define SIMTLanes 32
#define SIMTLogLanes 5

NOTE("Host CPU simple blocking data cache configuration")
NOTE("=================================================")

NOTE("Number of cache lines (each line is of DRAM beat size)")
#define SBDCacheLogLines 7
