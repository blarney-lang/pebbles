module CHERI.CapLibBase where

import Blarney
import Blarney.Core.BV

type InternalCapWidth = 91
type InternalCap = Bit InternalCapWidth

type InternalCapMetaDataWidth = 59
type InternalCapMetaData = Bit InternalCapMetaDataWidth

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

isValidCap :: InternalCap -> Bit 1
isValidCap cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isValidCap"
    [("wrap64_isValidCap_cap", 91)]
    [("wrap64_isValidCap", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_isValidCap"]

setValidCap :: InternalCap -> Bit 1 -> InternalCap
setValidCap cap valid = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setValidCap"
    [("wrap64_setValidCap_cap", 91), ("wrap64_setValidCap_valid", 1)]
    [("wrap64_setValidCap", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack valid]
      [Just "wrap64_setValidCap"]

getFlags :: InternalCap -> Bit 1
getFlags cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getFlags"
    [("wrap64_getFlags_cap", 91)]
    [("wrap64_getFlags", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getFlags"]

setFlags :: InternalCap -> Bit 1 -> InternalCap
setFlags cap flags = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setFlags"
    [("wrap64_setFlags_cap", 91), ("wrap64_setFlags_flags", 1)]
    [("wrap64_setFlags", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack flags]
      [Just "wrap64_setFlags"]

getHardPerms :: InternalCap -> HardPerms
getHardPerms cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getHardPerms"
    [("wrap64_getHardPerms_cap", 91)]
    [("wrap64_getHardPerms", 12)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getHardPerms"]

setHardPerms :: InternalCap -> HardPerms -> InternalCap
setHardPerms cap hardperms = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setHardPerms"
    [("wrap64_setHardPerms_cap", 91), ("wrap64_setHardPerms_hardperms", 12)]
    [("wrap64_setHardPerms", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack hardperms]
      [Just "wrap64_setHardPerms"]

getSoftPerms :: InternalCap -> Bit 16
getSoftPerms cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getSoftPerms"
    [("wrap64_getSoftPerms_cap", 91)]
    [("wrap64_getSoftPerms", 16)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getSoftPerms"]

setSoftPerms :: InternalCap -> Bit 16 -> InternalCap
setSoftPerms cap softperms = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setSoftPerms"
    [("wrap64_setSoftPerms_cap", 91), ("wrap64_setSoftPerms_softperms", 16)]
    [("wrap64_setSoftPerms", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack softperms]
      [Just "wrap64_setSoftPerms"]

getPerms :: InternalCap -> Bit 31
getPerms cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getPerms"
    [("wrap64_getPerms_cap", 91)]
    [("wrap64_getPerms", 31)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getPerms"]

setPerms :: InternalCap -> Bit 31 -> InternalCap
setPerms cap perms = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setPerms"
    [("wrap64_setPerms_cap", 91), ("wrap64_setPerms_perms", 31)]
    [("wrap64_setPerms", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack perms]
      [Just "wrap64_setPerms"]

getKind :: InternalCap -> Bit 7
getKind cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getKind"
    [("wrap64_getKind_cap", 91)]
    [("wrap64_getKind", 7)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getKind"]

setKind :: InternalCap -> Bit 7 -> InternalCap
setKind cap kind = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setKind"
    [("wrap64_setKind_cap", 91), ("wrap64_setKind_kind", 7)]
    [("wrap64_setKind", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack kind]
      [Just "wrap64_setKind"]

setType :: InternalCap -> Bit 4 -> InternalCap
setType cap t = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setType"
    [("wrap64_setType_cap", 91), ("wrap64_setType_t", 4)]
    [("wrap64_setType", 91)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack t]
      [Just "wrap64_setType"]

getType :: InternalCap -> Bit 4
getType cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getType"
    [("wrap64_getType_cap", 91)]
    [("wrap64_getType", 4)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getType"]

isSealed :: InternalCap -> Bit 1
isSealed cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isSealed"
    [("wrap64_isSealed_cap", 91)]
    [("wrap64_isSealed", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_isSealed"]

isSentry :: InternalCap -> Bit 1
isSentry cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isSentry"
    [("wrap64_isSentry_cap", 91)]
    [("wrap64_isSentry", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_isSentry"]

isSealedWithType :: InternalCap -> Bit 1
isSealedWithType cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isSealedWithType"
    [("wrap64_isSealedWithType_cap", 91)]
    [("wrap64_isSealedWithType", 1)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_isSealedWithType"]

getAddr :: InternalCap -> Bit 32
getAddr cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getAddr"
    [("wrap64_getAddr_cap", 91)]
    [("wrap64_getAddr", 32)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getAddr"]

setAddr :: InternalCap -> Bit 32 -> Exact InternalCap
setAddr cap addr = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setAddr"
    [("wrap64_setAddr_cap", 91), ("wrap64_setAddr_addr", 32)]
    [("wrap64_setAddr", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack addr]
      [Just "wrap64_setAddr"]

getOffset :: InternalCap -> Bit 32
getOffset cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getOffset"
    [("wrap64_getOffset_cap", 91)]
    [("wrap64_getOffset", 32)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getOffset"]

modifyOffset :: InternalCap -> Bit 32 -> Bit 1 -> Exact InternalCap
modifyOffset cap offset doInc = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_modifyOffset"
    [("wrap64_modifyOffset_cap", 91), ("wrap64_modifyOffset_offset", 32), ("wrap64_modifyOffset_doInc", 1)]
    [("wrap64_modifyOffset", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack offset, toBV $ pack doInc]
      [Just "wrap64_modifyOffset"]

setOffset :: InternalCap -> Bit 32 -> Exact InternalCap
setOffset cap offset = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setOffset"
    [("wrap64_setOffset_cap", 91), ("wrap64_setOffset_offset", 32)]
    [("wrap64_setOffset", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack offset]
      [Just "wrap64_setOffset"]

incOffset :: InternalCap -> Bit 32 -> Exact InternalCap
incOffset cap inc = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_incOffset"
    [("wrap64_incOffset_cap", 91), ("wrap64_incOffset_inc", 32)]
    [("wrap64_incOffset", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack inc]
      [Just "wrap64_incOffset"]

getBase :: InternalCap -> Bit 32
getBase cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getBase"
    [("wrap64_getBase_cap", 91)]
    [("wrap64_getBase", 32)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getBase"]

getTop :: InternalCap -> Bit 33
getTop cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getTop"
    [("wrap64_getTop_cap", 91)]
    [("wrap64_getTop", 33)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getTop"]

getLength :: InternalCap -> Bit 33
getLength cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_getLength"
    [("wrap64_getLength_cap", 91)]
    [("wrap64_getLength", 33)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_getLength"]

isInBounds :: InternalCap -> Bit 1 -> Bit 1
isInBounds cap isTopIncluded = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_isInBounds"
    [("wrap64_isInBounds_cap", 91), ("wrap64_isInBounds_isTopIncluded", 1)]
    [("wrap64_isInBounds", 1)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack isTopIncluded]
      [Just "wrap64_isInBounds"]

setBounds :: InternalCap -> Bit 32 -> Exact InternalCap
setBounds cap length = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_setBounds"
    [("wrap64_setBounds_cap", 91), ("wrap64_setBounds_length", 32)]
    [("wrap64_setBounds", 92)]
    [] False False Nothing) 
      [toBV $ pack cap, toBV $ pack length]
      [Just "wrap64_setBounds"]

nullWithAddr :: Bit 32 -> InternalCap
nullWithAddr addr = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_nullWithAddr"
    [("wrap64_nullWithAddr_addr", 32)]
    [("wrap64_nullWithAddr", 91)]
    [] False False Nothing) 
      [toBV $ pack addr]
      [Just "wrap64_nullWithAddr"]

almightyCap :: InternalCap
almightyCap  = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_almightyCap"
    []
    [("wrap64_almightyCap", 91)]
    [] False False Nothing) 
      []
      [Just "wrap64_almightyCap"]

nullCap :: InternalCap
nullCap  = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_nullCap"
    []
    [("wrap64_nullCap", 91)]
    [] False False Nothing) 
      []
      [Just "wrap64_nullCap"]

validAsType :: InternalCap -> Bit 32 -> Bit 1
validAsType dummy checkType = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_validAsType"
    [("wrap64_validAsType_dummy", 91), ("wrap64_validAsType_checkType", 32)]
    [("wrap64_validAsType", 1)]
    [] False False Nothing) 
      [toBV $ pack dummy, toBV $ pack checkType]
      [Just "wrap64_validAsType"]

fromMem :: (Bit 1, Bit 64) -> InternalCap
fromMem mem_cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_fromMem"
    [("wrap64_fromMem_mem_cap", 65)]
    [("wrap64_fromMem", 91)]
    [] False False Nothing) 
      [toBV $ pack mem_cap]
      [Just "wrap64_fromMem"]

toMem :: InternalCap -> (Bit 1, Bit 64)
toMem cap = 
  unpack $ FromBV $ head $ makePrim (Custom
    "module_wrap64_toMem"
    [("wrap64_toMem_cap", 91)]
    [("wrap64_toMem", 65)]
    [] False False Nothing) 
      [toBV $ pack cap]
      [Just "wrap64_toMem"]

nullCapInteger :: Integer = 0x00000000000001F690003F0
almightyCapInteger :: Integer = 0x40000000003FFDF690003F0
