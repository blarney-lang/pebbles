module CHERI.CapLibBase where

import Blarney
import Blarney.Core.BV

type CapPipeWidth = 91
type CapPipe = Bit CapPipeWidth

type CapPipeMetaWidth = 59
type CapPipeMeta = Bit CapPipeMetaWidth

type CapMemWidth = 65
type CapMem = Bit CapMemWidth

type CapMemMetaWidth = 33
type CapMemMeta = Bit CapMemMetaWidth

type CapAddrWidth = 32
type CapAddr = Bit CapAddrWidth

data Exact t =
  Exact {
    exact :: Bit 1
  , value :: t
  } deriving (Generic, Interface, Bits)

data HardPerms =
  HardPerms {
    permitSetCID :: Bit 1
  , accessSysRegs :: Bit 1
  , permitUnseal :: Bit 1
  , permitCCall :: Bit 1
  , permitSeal :: Bit 1
  , permitStoreLocalCap :: Bit 1
  , permitStoreCap :: Bit 1
  , permitLoadCap :: Bit 1
  , permitStore :: Bit 1
  , permitLoad :: Bit 1
  , permitExecute :: Bit 1
  , global :: Bit 1
  } deriving (Generic, Interface, Bits)

isValidCap :: Bit 91 -> Bit 1
isValidCap cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isValidCap"
    [("wrap64_isValidCap_cap", 91)]
    [("wrap64_isValidCap", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_isValidCap"]

setValidCap :: Bit 91 -> Bit 1 -> Bit 91
setValidCap cap valid = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setValidCap"
    [("wrap64_setValidCap_cap", 91), ("wrap64_setValidCap_valid", 1)]
    [("wrap64_setValidCap", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack valid]
      [Just "wrap64_setValidCap"]

getFlags :: Bit 91 -> Bit 1
getFlags cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getFlags"
    [("wrap64_getFlags_cap", 91)]
    [("wrap64_getFlags", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getFlags"]

setFlags :: Bit 91 -> Bit 1 -> Bit 91
setFlags cap flags = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setFlags"
    [("wrap64_setFlags_cap", 91), ("wrap64_setFlags_flags", 1)]
    [("wrap64_setFlags", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack flags]
      [Just "wrap64_setFlags"]

getHardPerms :: Bit 91 -> HardPerms
getHardPerms cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getHardPerms"
    [("wrap64_getHardPerms_cap", 91)]
    [("wrap64_getHardPerms", 12)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getHardPerms"]

setHardPerms :: Bit 91 -> HardPerms -> Bit 91
setHardPerms cap hardperms = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setHardPerms"
    [("wrap64_setHardPerms_cap", 91), ("wrap64_setHardPerms_hardperms", 12)]
    [("wrap64_setHardPerms", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack hardperms]
      [Just "wrap64_setHardPerms"]

getSoftPerms :: Bit 91 -> Bit 16
getSoftPerms cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getSoftPerms"
    [("wrap64_getSoftPerms_cap", 91)]
    [("wrap64_getSoftPerms", 16)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getSoftPerms"]

setSoftPerms :: Bit 91 -> Bit 16 -> Bit 91
setSoftPerms cap softperms = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setSoftPerms"
    [("wrap64_setSoftPerms_cap", 91), ("wrap64_setSoftPerms_softperms", 16)]
    [("wrap64_setSoftPerms", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack softperms]
      [Just "wrap64_setSoftPerms"]

getPerms :: Bit 91 -> Bit 31
getPerms cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getPerms"
    [("wrap64_getPerms_cap", 91)]
    [("wrap64_getPerms", 31)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getPerms"]

setPerms :: Bit 91 -> Bit 31 -> Bit 91
setPerms cap perms = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setPerms"
    [("wrap64_setPerms_cap", 91), ("wrap64_setPerms_perms", 31)]
    [("wrap64_setPerms", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack perms]
      [Just "wrap64_setPerms"]

getKind :: Bit 91 -> Bit 7
getKind cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getKind"
    [("wrap64_getKind_cap", 91)]
    [("wrap64_getKind", 7)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getKind"]

setKind :: Bit 91 -> Bit 7 -> Bit 91
setKind cap kind = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setKind"
    [("wrap64_setKind_cap", 91), ("wrap64_setKind_kind", 7)]
    [("wrap64_setKind", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack kind]
      [Just "wrap64_setKind"]

setType :: Bit 91 -> Bit 4 -> Bit 91
setType cap t = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setType"
    [("wrap64_setType_cap", 91), ("wrap64_setType_t", 4)]
    [("wrap64_setType", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack t]
      [Just "wrap64_setType"]

getType :: Bit 91 -> Bit 4
getType cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getType"
    [("wrap64_getType_cap", 91)]
    [("wrap64_getType", 4)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getType"]

isSealed :: Bit 91 -> Bit 1
isSealed cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isSealed"
    [("wrap64_isSealed_cap", 91)]
    [("wrap64_isSealed", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_isSealed"]

isSentry :: Bit 91 -> Bit 1
isSentry cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isSentry"
    [("wrap64_isSentry_cap", 91)]
    [("wrap64_isSentry", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_isSentry"]

isSealedWithType :: Bit 91 -> Bit 1
isSealedWithType cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isSealedWithType"
    [("wrap64_isSealedWithType_cap", 91)]
    [("wrap64_isSealedWithType", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_isSealedWithType"]

getAddr :: Bit 91 -> Bit 32
getAddr cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getAddr"
    [("wrap64_getAddr_cap", 91)]
    [("wrap64_getAddr", 32)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getAddr"]

setAddr :: Bit 91 -> Bit 32 -> Exact (Bit 91)
setAddr cap addr = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setAddr"
    [("wrap64_setAddr_cap", 91), ("wrap64_setAddr_addr", 32)]
    [("wrap64_setAddr", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack addr]
      [Just "wrap64_setAddr"]

getOffset :: Bit 91 -> Bit 32
getOffset cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getOffset"
    [("wrap64_getOffset_cap", 91)]
    [("wrap64_getOffset", 32)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getOffset"]

modifyOffset :: Bit 91 -> Bit 32 -> Bit 1 -> Exact (Bit 91)
modifyOffset cap offset doInc = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_modifyOffset"
    [("wrap64_modifyOffset_cap", 91), ("wrap64_modifyOffset_offset", 32), ("wrap64_modifyOffset_doInc", 1)]
    [("wrap64_modifyOffset", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack offset, toBV $ pack doInc]
      [Just "wrap64_modifyOffset"]

setOffset :: Bit 91 -> Bit 32 -> Exact (Bit 91)
setOffset cap offset = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setOffset"
    [("wrap64_setOffset_cap", 91), ("wrap64_setOffset_offset", 32)]
    [("wrap64_setOffset", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack offset]
      [Just "wrap64_setOffset"]

incOffset :: Bit 91 -> Bit 32 -> Exact (Bit 91)
incOffset cap inc = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_incOffset"
    [("wrap64_incOffset_cap", 91), ("wrap64_incOffset_inc", 32)]
    [("wrap64_incOffset", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack inc]
      [Just "wrap64_incOffset"]

getBase :: Bit 91 -> Bit 32
getBase cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getBase"
    [("wrap64_getBase_cap", 91)]
    [("wrap64_getBase", 32)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getBase"]

getTop :: Bit 91 -> Bit 33
getTop cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getTop"
    [("wrap64_getTop_cap", 91)]
    [("wrap64_getTop", 33)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getTop"]

getLength :: Bit 91 -> Bit 33
getLength cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getLength"
    [("wrap64_getLength_cap", 91)]
    [("wrap64_getLength", 33)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getLength"]

isInBounds :: Bit 91 -> Bit 1 -> Bit 1
isInBounds cap isTopIncluded = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isInBounds"
    [("wrap64_isInBounds_cap", 91), ("wrap64_isInBounds_isTopIncluded", 1)]
    [("wrap64_isInBounds", 1)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack isTopIncluded]
      [Just "wrap64_isInBounds"]

setBounds :: Bit 91 -> Bit 32 -> Exact (Bit 91)
setBounds cap length = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setBounds"
    [("wrap64_setBounds_cap", 91), ("wrap64_setBounds_length", 32)]
    [("wrap64_setBounds", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack length]
      [Just "wrap64_setBounds"]

nullWithAddr :: Bit 32 -> Bit 91
nullWithAddr addr = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_nullWithAddr"
    [("wrap64_nullWithAddr_addr", 32)]
    [("wrap64_nullWithAddr", 91)]
    [] False False Nothing) 
      [toBV $ pack addr]
      [Just "wrap64_nullWithAddr"]

almightyCapPipe :: Bit 91
almightyCapPipe  = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_almightyCapPipe"
    []
    [("wrap64_almightyCapPipe", 91)]
    [] False False Nothing) 
      []
      [Just "wrap64_almightyCapPipe"]

nullCapPipe :: Bit 91
nullCapPipe  = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_nullCapPipe"
    []
    [("wrap64_nullCapPipe", 91)]
    [] False False Nothing) 
      []
      [Just "wrap64_nullCapPipe"]

validAsType :: Bit 91 -> Bit 32 -> Bit 1
validAsType dummy checkType = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_validAsType"
    [("wrap64_validAsType_dummy", 91), ("wrap64_validAsType_checkType", 32)]
    [("wrap64_validAsType", 1)]
    [] False False Nothing) 
      [toBV $ pack dummy, toBV $ pack checkType]
      [Just "wrap64_validAsType"]

fromMem :: (Bit 1, Bit 64) -> Bit 91
fromMem mem_cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_fromMem"
    [("wrap64_fromMem_mem_cap", 65)]
    [("wrap64_fromMem", 91)]
    [] False False Nothing) 
      [toBV $ pack mem_cap]
      [Just "wrap64_fromMem"]

toMem :: Bit 91 -> (Bit 1, Bit 64)
toMem cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_toMem"
    [("wrap64_toMem_cap", 91)]
    [("wrap64_toMem", 65)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_toMem"]

getMeta :: Bit 91 -> Bit 32
getMeta cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getMeta"
    [("wrap64_getMeta_cap", 91)]
    [("wrap64_getMeta", 32)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getMeta"]

almightyCapMem :: Bit 65
almightyCapMem  = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_almightyCapMem"
    []
    [("wrap64_almightyCapMem", 65)]
    [] False False Nothing) 
      []
      [Just "wrap64_almightyCapMem"]

nullCapMem :: Bit 65
nullCapMem  = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_nullCapMem"
    []
    [("wrap64_nullCapMem", 65)]
    [] False False Nothing) 
      []
      [Just "wrap64_nullCapMem"]

nullCapMemInteger :: Integer = 0x00007C30200000000
almightyCapMemInteger :: Integer = 0x1FFF0000000000000
nullCapPipeInteger :: Integer = 0x00000000000001F690003F0
almightyCapPipeInteger :: Integer = 0x40000000003FFDF690003F0
