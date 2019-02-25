module Main where

import Data.Word
import Numeric

import Hapstone.Capstone
import Hapstone.Internal.Capstone as HIC

asm_buffer = [0x55, 0x48, 0x8b, 0x05, 0xb8, 0x13, 0x00, 0x00] :: [Word8]

action' :: HIC.Csh -> HIC.CsInsn -> IO()
action' handle instruction = putStrLn ("0x" ++ a ++ "\t" ++ m ++ "\t" ++ o)
  where
    m = mnemonic instruction
    o = opStr instruction
    a = (showHex $ address instruction) ""

disasm = Disassembler {
  arch = HIC.CsArchX86
  , modes = [HIC.CsMode64]
  , buffer = asm_buffer
  , addr = 0x1000
  , num = 0
  , Hapstone.Capstone.detail = True
  , skip = Just (defaultSkipdataStruct)
  , action = action'
  }

main = disasmIO disasm
