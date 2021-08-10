module Pebbles.CSRs.TrapCodes.Standard where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.CSRs.TrapCodes.Interface

exc_instrAddrMisaligned     = excCode 0
exc_instrAccessFault        = excCode 1
exc_illegalInstr            = excCode 2
exc_breakpoint              = excCode 3
exc_loadAddrMisaligned      = excCode 4
exc_loadAccessFault         = excCode 5
exc_storeAMOAddrMisaligned  = excCode 6
exc_storeAMOAccessFault     = excCode 7
exc_eCallFromU              = excCode 8
exc_eCallFromS              = excCode 9
exc_res10                   = excCode 10
exc_eCallFromM              = excCode 11
exc_instrPageFault          = excCode 12
exc_loadPageFault           = excCode 13
exc_res14                   = excCode 14
exc_storeAMOPageFault       = excCode 15

int_res0                    = intCode 0
int_softIrqS                = intCode 1
int_res2                    = intCode 2
int_softIrqM                = intCode 3
int_res4                    = intCode 4
int_timerIrqS               = intCode 5
int_res6                    = intCode 6
int_timerIrqM               = intCode 7
int_res8                    = intCode 8
int_externalIrqS            = intCode 9
int_res10                   = intCode 10
int_externalIrqM            = intCode 11
