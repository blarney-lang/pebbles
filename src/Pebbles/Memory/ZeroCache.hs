module Pebbles.Memory.ZeroCache
  ( makeStreamCache
  , StreamCacheReqId
  , StreamCacheReq(..)
  , StreamCacheResp(..)
  , StreamCacheConfig(..)
  ) where

-- SoC config
#include <Config.h>

-- Tag cache parameters
#define StreamCacheLogItemsPerBeat    (DRAMBeatLogBytes+3)
#define StreamCacheLogBeatsPerLine    ZeroCacheLogBeatsPerLine
#define StreamCacheLogNumWays         ZeroCacheLogNumWays
#define StreamCacheLogSets            ZeroCacheLogSets
#define StreamCacheLogMaxInflight     ZeroCacheLogMaxInflight
#define StreamCachePendingReqsPerWay  ZeroCachePendingReqsPerWay
#define StreamCacheAddrWidth          (DRAMAddrWidth-DRAMBeatLogBytes-3)

-- Cache code
#include <Pebbles/Memory/StreamCache.hs>
