module Pebbles.Memory.TagCache
  ( makeStreamCache
  , StreamCacheReqId
  , StreamCacheReq(..)
  , StreamCacheResp(..)
  , StreamCacheConfig(..)
  ) where

-- SoC config
#include <Config.h>

-- Tag cache parameters
#define StreamCacheLogItemsPerBeat    (DRAMBeatLogBytes-1)
#define StreamCacheLogBeatsPerLine    TagCacheLogBeatsPerLine
#define StreamCacheLogNumWays         TagCacheLogNumWays
#define StreamCacheLogSets            TagCacheLogSets
#define StreamCacheLogMaxInflight     TagCacheLogMaxInflight
#define StreamCachePendingReqsPerWay  TagCachePendingReqsPerWay
#define StreamCacheAddrWidth          (DRAMAddrWidth-5)

-- Cache code
#include <Pebbles/Memory/StreamCache.hs>
