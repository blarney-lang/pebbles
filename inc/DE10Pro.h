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

NOTE("Number of lanes per SIMT core")
#define SIMTLanes 32
#define SIMTLogLanes 5

NOTE("Number of warps per SIMT core")
#define SIMTWarps 64
#define SIMTLogWarps 6

NOTE("Number of bits used to track function call depth")
#define SIMTLogMaxCallDepth 5

NOTE("Stack size (in bytes) for each SIMT thread")
#define SIMTLogBytesPerStack 19

NOTE("Size of each SRAM bank (in words)")
#define SIMTLogWordsPerSRAMBank 9

NOTE("CPU configuration")
NOTE("=================")

NOTE("Size of tightly coupled instruction memory")
#define CPUInstrMemLogWords 13

NOTE("Number of cache lines (line size == DRAM beat size)")
#define SBDCacheLogLines 7

NOTE("Memory map")
NOTE("==========")

NOTE("Space reserved for boot loader")
#define MaxBootImageBytes 512
