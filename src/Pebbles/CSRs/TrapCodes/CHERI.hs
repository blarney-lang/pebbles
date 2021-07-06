module Pebbles.CSRs.TrapCodes.CHERI where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.CSRs.TrapCodes.Interface

cheri_exc_lengthViolation               = excCapCode 0x01
cheri_exc_tagViolation                  = excCapCode 0x02
cheri_exc_sealViolation                 = excCapCode 0x03
cheri_exc_typeViolation                 = excCapCode 0x04
cheri_exc_callTrap                      = excCapCode 0x05
cheri_exc_returnTrap                    = excCapCode 0x06
cheri_exc_sysStackUnderflow             = excCapCode 0x07
cheri_exc_softDefPermViolation          = excCapCode 0x08
cheri_exc_mmuProhibitsStoreViolation    = excCapCode 0x09
cheri_exc_representabilityViolation     = excCapCode 0x0a
cheri_exc_unalignedBaseViolation        = excCapCode 0x0b
cheri_exc_globalViolation               = excCapCode 0x10
cheri_exc_permitExecuteViolation        = excCapCode 0x11
cheri_exc_permitLoadViolation           = excCapCode 0x12
cheri_exc_permitStoreViolation          = excCapCode 0x13
cheri_exc_permitLoadCapViolation        = excCapCode 0x14
cheri_exc_permitStoreCapViolation       = excCapCode 0x15
cheri_exc_permitStoreLocalCapViolation  = excCapCode 0x16
cheri_exc_permitSealViolation           = excCapCode 0x17
cheri_exc_accessSysRegsViolation        = excCapCode 0x18
cheri_exc_permitCCallViolation          = excCapCode 0x19
cheri_exc_permitCCallIDCViolation       = excCapCode 0x1a
cheri_exc_unsealViolation               = excCapCode 0x1b
cheri_exc_setCIDViolation               = excCapCode 0x1c
