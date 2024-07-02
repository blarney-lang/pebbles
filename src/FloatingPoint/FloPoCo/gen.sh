#!/bin/bash

F=500

flopoco name=FFromIEEE outputFile="FFromIEEE.vhd" target=StratixV frequency=$F useHardMult=0 useTargetOpt=0 InputIEEE wEIn=8 wFIn=23 wEOut=8 wFOut=23
flopoco name=FToIEEE outputFile="FToIEEE.vhd" target=StratixV frequency=$F useHardMult=0 useTargetOpt=0 OutputIEEE wEIn=8 wFIn=23 wEOut=8 wFOut=23
flopoco name=FFPMult outputFile="FFPMult.vhd" target=StratixV frequency=$F useHardMult=0 useTargetOpt=0 FPMult wE=8 wF=23
flopoco name=FFPDiv outputFile="FFPDiv.vhd" target=StratixV frequency=$F useHardMult=0 useTargetOpt=0 FPDiv wE=8 wF=23
flopoco name=FFPSqrt outputFile="FFPSqrt.vhd" target=StratixV frequency=$F useHardMult=0 useTargetOpt=0 FPSqrt wE=8 wF=23
