module Lib( 
    Code_of_operation,
    Parameter(..),
--    Command(..),

--    parse_block,
--    parse_file,
      some_func
    ) where

import Text.ParserCombinators.Parsec
import Data.Char
import Data.Word
import qualified Data.List as DL
import Numeric
import Foreign
import Control.Monad.State

data Code_of_operation = Invalid_code
                       | AAA
                       | AAD
                       | AAM
                       | AAS
                       | ADC
                       | ADD
                       | ADDPD
                       | ADDPS
                       | ADDSD
                       | ADDSS
                       | ADDSUBPD
                       | ADDUBPS
                       | AND
                       | ANDNPD
                       | ANDNPS
                       | ANDPD
                       | ANDPS
                       | ARPL
                       | BOUND
                       | BSF
                       | BSR
                       | BT
                       | BTC
                       | BTR
                       | BTS
                       | CALL
                       | CALLF
                       | CBW
                       | CDQ
                       | CDQE
                       | CLC
                       | CLD
                       | CLFLUSH
                       | CLI
                       | CLTS
                       | CMC
                       | CMOVA
                       | CMOVB
                       | CMOVBE
                       | CMOVE
                       | CMOVG
                       | CMOVGE
                       | CMOVL
                       | CMOVLE
                       | CMOVNB
                       | CMOVNE
                       | CMOVNO
                       | CMOVNP
                       | CMOVNS
                       | CMOVO
                       | CMOVP
                       | CMOVS
                       | CMP
                       | CMPS
                       | CMPXCHG
                       | CMPXCHG16B
                       | CMPXCHG8B
                       | COMISD
                       | COMISS
                       | CPUID
                       | CWD
                       | CWDE
                       | DAA
                       | DAS
                       | DEC
                       | DIV
                       | DIVPD
                       | DIVPS
                       | DIVSD
                       | DIVSS
                       | EMMS
                       | ENTER
                       | FABS
                       | FADD
                       | FADDP
                       | FBLD
                       | FBSTP
                       | FCHS
                       | FCLEX
                       | FCMOVB
                       | FCMOVBE
                       | FCMOVE
                       | FCMOVNB
                       | FCMOVNBE
                       | FCMOVNE
                       | FCMOVNU
                       | FCMOVU
                       | FCOM
                       | FCOMI
                       | FCOMIP
                       | FCOMP
                       | FCOMPP
                       | FDIV
                       | FDIVP
                       | FDIVR
                       | FDIVRP
                       | FFREE
                       | FIADD
                       | FICOM
                       | FICOMP
                       | FIDIV
                       | FIDIVR
                       | FILD
                       | FIMUL
                       | FINIT
                       | FIST
                       | FISTP
                       | FISTPP
                       | FISTTP
                       | FISUB
                       | FISUBR
                       | FLD
                       | FLD1
                       | FLDCW
                       | FLDENV
                       | FLDL2E
                       | FLDL2T
                       | FLDLG2
                       | FLDLN2
                       | FLDPI
                       | FLDZ
                       | FMUL
                       | FMULP
                       | FNOP
                       | FRSTOR
                       | FSAVE
                       | FST
                       | FSTCW
                       | FSTENV
                       | FSTP
                       | FSTSW
                       | FSUB
                       | FSUBP
                       | FSUBR
                       | FSUBRP
                       | FTST
                       | FUCOM
                       | FUCOMI
                       | FUCOMIP
                       | FUCOMP
                       | FUCOMPP
                       | FXAM
                       | FXCH
                       | FXRSTOR
                       | FXSAVE
                       | HADDPD
                       | HADDPS
                       | HLT
                       | HSUBPD
                       | HSUBPS
                       | IDIV
                       | IMUL
                       | BSWAP
                       | IN
                       | INC
                       | INS
                       | INT
                       | INT3
                       | INTO
                       | INVD
                       | INVLPG
                       | IRET
                       | JA
                       | JB
                       | JBE
                       | JCXZ
                       | JE
                       | JG
                       | JGE
                       | JL
                       | JLE
                       | JMP
                       | JMPF
                       | JMPN
                       | JNB
                       | JNE
                       | JNO
                       | JNP
                       | JNS
                       | JO
                       | JP
                       | JS
                       | LAHF
                       | LAR
                       | LDDQU
                       | LDMXCSR
                       | LDS
                       | LEA
                       | LEAVE
                       | LES
                       | LFENCE
                       | LFS
                       | LGDT
                       | LGS
                       | LIDT
                       | LLDT
                       | LMSW
                       | LODS
                       | LOOP
                       | LOOPE
                       | LOOPNE
                       | LSL
                       | LSS
                       | LTR
                       | MASKMOVQ
                       | MAXPD
                       | MAXPS
                       | MAXSD
                       | MAXSS
                       | MFENCE
                       | MINPD
                       | MINPS
                       | MINSD
                       | MINSS
                       | MONITOR
                       | MOV 
                       | MOVAPD
                       | MOVAPS
                       | MOVDDUP
                       | MOVHPD
                       | MOVHPS
                       | MOVLHPS
                       | MOVLPD
                       | MOVLPS
                       | MOVLSDUP
                       | MOVMSKPD
                       | MOVMSKPS
                       | MOVNTDQ
                       | MOVNTPD
                       | MOVNTPS
                       | MOVNTQ
                       | MOVQ
                       | MOVS
                       | MOVSD
                       | MOVSLDUP
                       | MOVSS
                       | MOVSXB
                       | MOVSXD
                       | MOVSXW
                       | MOVUPD
                       | MOVUPS
                       | MOVZXB
                       | MOVZXW
                       | MUL
                       | MULPD
                       | MULPS
                       | MULSD
                       | MULSS
                       | MWAIT
                       | NEG
                       | NOP
                       | NOT
                       | OR
                       | ORPD
                       | ORPS
                       | OUT
                       | OUTS
                       | PADDB
                       | PADDD
                       | PADDQ
                       | PADDSB
                       | PADDSW
                       | PADDUSB
                       | PADDUSW
                       | PADDW
                       | PAND
                       | PANDN
                       | PAUSE
                       | PAVGB
                       | PAVGW
                       | PMADDWD
                       | PMAXSW
                       | PMAXUB
                       | PMINSW
                       | PMINUB
                       | PMOVMSKB
                       | PMULHUW
                       | PMULHW
                       | PMULLW
                       | PMULUDQ
                       | POP
                       | POPA
                       | POPAD
                       | POPF
                       | POPFD
                       | POPFQ
                       | POR
                       | PREFETCHNTA
                       | PREFETCHT0
                       | PREFETCHT1
                       | PREFETCHT2
                       | PSADBW
                       | PSLLD
                       | PSLLDQ
                       | PSLLQ
                       | PSLLW
                       | PSRAD
                       | PSRAW
                       | PSRLD
                       | PSRLDQ
                       | PSRLQ
                       | PSRLW
                       | PSUBB
                       | PSUBD
                       | PSUBQ
                       | PSUBSB
                       | PSUBSQ
                       | PSUBUSB
                       | PSUBUSW
                       | PSUBW
                       | PUSH
                       | PUSHA
                       | PUSHAD
                       | PUSHF
                       | PUSHFD
                       | PUSHFQ
                       | PXOR
                       | RCL
                       | RCPPS
                       | RCPSS
                       | RCR
                       | RDMSR
                       | RDPMC
                       | RDTSC
                       | RET
                       | RETF
                       | ROL
                       | ROR
                       | RSM
                       | RSQRTPS
                       | RSQRTSS
                       | SAHF
                       | SAR
                       | SBB
                       | SCAS
                       | SETA
                       | SETB
                       | SETBE
                       | SETE
                       | SETG
                       | SETGE
                       | SETL
                       | SETLE
                       | SETNB
                       | SETNE
                       | SETNO
                       | SETNP
                       | SETNS
                       | SETO
                       | SETP
                       | SETS
                       | SFENCE
                       | SGDT
                       | SHL
                       | SHLD
                       | SHR
                       | SHRD
                       | SIDT
                       | SLDT
                       | SMSW
                       | SQRTPD
                       | SQRTPS
                       | SQRTSD
                       | SQRTSS
                       | STC
                       | STD
                       | STI
                       | STMXCSR
                       | STOS
                       | STR
                       | SUB
                       | SUBPD
                       | SUBPS
                       | SUBSD
                       | SUBSS
                       | SWAPGS
                       | SYSCALL
                       | SYSENTER
                       | SYSEXIT
                       | TEST
                       | UCOMISD
                       | UCOMISS
                       | UD2
                       | UNPCKHPD
                       | UNPCKHPS
                       | UNPCKLPD
                       | UNPCKLPS
                       | VERR
                       | VERW
                       | VMCALL
                       | VMCLEAR
                       | VMLAUNCH
                       | VMPTRLD
                       | VMPTRST
                       | VMREAD
                       | VMRESUME
                       | VMWRITE
                       | VMXOFF
                       | VMXON
                       | WAIT
                       | WBINVD
                       | WRMSR
                       | XADD
                       | XCHG
                       | XLAT
                       | XOR
                       | XORPD
                       | XORPS
  deriving (Show, Eq)

show_code_of_operation :: Code_of_operation -> String
show_code_of_operation = (map toLower) . show

data Parameter = P_immediate_value Word32
               | P_absolute_address Word32 Parameter_size
               | P_register String Int
               | P_fp_register Int
               | P_indirect_register String Parameter_size
               | P_indirect_register_with_disp String Int Parameter_size
               | P_base_plus_index String String Int Parameter_size
               | P_scaled_index_with_disp String Int Int Parameter_size
               | P_base_plus_scaled_index_with_disp String String Int Int Parameter_size
  deriving(Eq)

show_immediate_parameter :: Word32 -> String
show_immediate_parameter i =
  let h = showHex i "H"
      (f:_) = h
  in (if isDigit f then "" else "0") ++ h

show_address i =
  let w :: Word32
      w = fromIntegral i
      h = showHex w "H"
      (f:_) = h
  in (if isDigit f then "" else "0") ++ h

data Parameter_size = PS_NONE
                    | PS_8
                    | PS_16
                    | PS_32
                    | PS_64
                    | PS_128
                    | PS_F_32
                    | PS_F_64
                    | PS_F_80
  deriving(Show, Eq)

show_parameter_size PS_NONE = ""
show_parameter_size PS_8 = "byte_ptr"
show_parameter_size PS_16 = "word_ptr"
show_parameter_size PS_32 = "dword_ptr"
show_parameter_size PS_64 = "qword_ptr"
show_parameter_size PS_128 = "dqword_ptr"
show_parameter_size PS_F_32 = "dword_ptr"
show_parameter_size PS_F_64 = "qword_ptr"
show_parameter_size PS_F_80 = "tbyte_ptr"


show_parameter parameter_size (P_immediate_value w) = show_immediate_parameter w
show_parameter parameter_size (P_absolute_address w sz) = show_parameter_size sz ++ "[" ++ show_address w ++ "]"
show_parameter parameter_size (P_register s num) = s
show_parameter parameter_size (P_fp_register 0) = "st"
show_parameter parameter_size (P_fp_register i) = "st(" ++ show i ++ ")"
show_parameter parameter_size (P_indirect_register s sz) = show_parameter_size sz ++ "[" ++ s ++ "]"
show_parameter parameter_size (P_indirect_register_with_disp s disp sz) = show_parameter_size sz ++ "[" ++ s ++ (if disp<0 then "" else "+") ++ show disp ++ "]"
show_parameter parameter_size (P_base_plus_index b i s sz) = show_parameter_size sz ++ "[" ++ b ++ "+" ++ i ++ "*" ++ show s ++"]" 
show_parameter parameter_size (P_scaled_index_with_disp i s disp sz) = show_parameter_size sz ++ "[" ++ i ++ "*" ++ show s ++ (if disp < 0 then "" else "+") ++ show disp ++"]" 
show_parameter parameter_size (P_base_plus_scaled_index_with_disp b i s disp sz) = show_parameter_size sz ++ "[" ++ b ++ "+" ++ i ++ "*" ++ show s ++ (if disp < 0 then "" else "+") ++ show disp ++"]" 

data Operation = Bad_operation Word8 String Int [Word8]
               | Not_operation Int String
               | Operation { op_code :: Code_of_operation,
                             op_size :: Parameter_size,
                             parameters :: [Parameter],
                             address :: Int,
                             bytes_array :: [Word8]
                           }
  deriving (Eq)

show_hex32 :: Int -> String
show_hex32 i =
  let w :: Word32
      w = fromIntegral i
      s = showHex w ""
  in take (8 - length s) (repeat '0') ++ s

show_hex8 :: Word8 -> String
show_hex8 i =
  let s = showHex i ""
  in take (2 - length s) ['0','0'] ++ s

expand s i = s ++ take (i - length s) (repeat ' ')

show_position pos bytes = show_hex32 pos ++ " " ++ expand (concat (DL.intersperse " " (map show_hex8 bytes))) 30

show_operation :: Operation -> [Char]
show_operation (Bad_operation b desc pos bytes) = show_position pos bytes ++ "(" ++ desc ++ ", byte=" ++ show b ++ ")"
show_operation (Not_operation pos s) = show_hex32 pos ++ "                       " ++ s
show_operation (Operation code parameter_size [] pos bytes) = show_position pos bytes ++ show_code_of_operation code
show_operation (Operation code parameter_size parameters pos bytes) = show_position pos bytes ++ expand (show_code_of_operation code) 6 ++ " " ++ concat (DL.intersperse "," (map (show_parameter parameter_size) parameters))

instance Show Operation where 
  show = show_operation

data Operat = BadOperat Word8 String
            | Operat Code_of_operation Parameter_size [Parameter]

type Byte_parser a = GenParser Word8 Parser_state a

any_byte :: Byte_parser Word8
any_byte = do
  tokenPrim show_byte next_pos test_byte
  where
    show_byte b = show b
    next_pos pos x xs = incSourceColumn pos 1
    test_byte b = Just b

any_word = do
  byte_0 <- any_byte
  byte_1 <- any_byte
  let word_0, word_1 :: Word16
      word_0 = fromIntegral byte_0
      word_1 = fromIntegral byte_1
  return $ word_0 .|. shiftL word_1 8

any_dword = do
  word_0 <- any_word
  word_1 <- any_word
  let dword_0, dword_1 :: Word32
      dword_0 = fromIntegral word_0
      dword_1 = fromIntegral word_1
  return $ dword_0 .|. shiftL dword_1 16


any_parameter :: Byte_parser Word32
any_parameter = do
  st <- getState
  case parameter_size st of
    PS_16 -> do
      w <- any_word
      let w' :: Word32
          w'= fromIntegral w
      return w'
    PS_32 -> any_dword
      
data Parser_state = Parser_state { parameter_size :: Parameter_size,
                                   address_size :: Parameter_size,
                                   x64 :: Bool,
                                   prefix :: [Word8]
                                 }

switch_parameter_size PS_16 = PS_32
switch_parameter_size PS_32 = PS_16
switch_address_size PS_16 = PS_32
switch_address_size PS_32 = PS_16

byte b = do
  tokenPrim show_byte next_pos test_byte
  where
  show_byte byte = show byte
  next_pos pos x xs = incSourceColumn pos 1
  test_byte byte = if elem byte b then Just byte else Nothing

byte' byte b res = do
  if elem byte b then Just res else Nothing

add_prefix b = do
  st <- getState
  setState st{prefix = b : prefix st}

parse_prefix = do
  (byte [0xf0, 0xf2, 0xf3, 0x2e, 0x36, 0x3e, 0x26, 0x64, 0x65, 0x2e,0x3e] >>= add_prefix)
    <|>
    do byte [0x66]
       st <- getState
       setState st{parameter_size = switch_parameter_size (parameter_size st)}
       add_prefix 0x66
    <|>
    do byte [0x67]
       st <- getState
       setState st{address_size = switch_address_size (address_size st)}
       add_prefix 0x67
    <|>
    do st <- getState
       if x64 st 
          then (byte [0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f] >>= add_prefix)
          else pzero

registers_names_8 = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]
registers_names_16 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
registers_names_32 = ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi"]
registers_names_64 = ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"]

register_name r = do
    st <- getState
    if x64 st
      then return $ "r" ++ show (r + 8)
      else case parameter_size st of
             PS_16 -> return $ registers_names_16 !! r
             PS_32 -> return $ registers_names_32 !! r

only_x64 p b = do
  st <- getState
  if x64 st
    then p b
    else return $ Bad_operation b "only in x64 mode"

only_not_x64 p b = do
  st <- getState
  if x64 st
    then return $ Bad_operation b "only in x64 mode"
    else p b

operation_parameters_size = do
  st <- getState
  if x64 st
    then return $ PS_64
    else return $ parameter_size st 

parse_address :: Parameter_size -> Byte_parser (Parameter, Parameter, Word8, Word8, Word8)
parse_address s = do
  b <- any_byte
  parse_address' s b

parse_address_modifier' = do
  return (shiftR b 6, (shiftR b 3 .&.& 7), (b .&. 7))

parse_address' :: Parameter_size -> Word8 -> Byte_parser (Parameter, Parameter, Word8, Word8, Word8)
parse_address' parameter_size modifier = do
  b <- any_byte
  (mode, register, rm) <- parse_address_modifier' modifier
  st <- getState
  case mod of
    0 -> case rm of ...


parse_arithmetic_operation :: Code_of_operation -> Word8 -> Byte_parser Operat
parse_arithmetic_operation op b = do
  parameter_size <- operation_parameters_size
  case b .&. 0x07 of
    0 -> do (op1, op2, mod, reg, rm) <- parse_address parameter_size
            return $ Operat op PS_8 [op1, (P_register (registers_names_8 !! fromIntegral reg)) (fromIntegral reg)]
    1 -> do (op1, op2, mod, reg, rm) <- parse_address parameter_size
            return $ Operat op parameter_size [op1, op2]
    2 -> do (op1, op2, mod, reg, rm) <- parse_address parameter_size
            return $ Operat op PS_8 [(P_register (registers_names_8 !! fromIntegral reg)) (fromIntegral reg), op1]
    3 -> do (op1, op2, mod, reg, rm) <- parse_address parameter_size
            return $ Operat op parameter_size [op2, op1]
    4 -> do b <- any_byte
            return $ Operat op PS_8 [(P_register "al" 0), (P_immediate_value (fromIntegral b))]
    5 -> do b <- any_parameter
            rn <- register_name 0
            return $ Operat op parameter_size [(P_register rn 0), (P_immediate_value b)]
    _ -> return $ Bad_operation b "no arithmetic instruction code (error)"

parse_push_segment_register_operation :: String -> Word8 -> Byte_parser Operat
parse_push_segment_register_operation r _ = do
  return $ Operat PUSH PS_16 [(P_register r 0)]

parse_pop_segment_register_operation :: String -> Word8 -> Byte_parser Operat
parse_pop_segment_register_operation r _ = do
  return $ Operat POP PS_16 [(P_register r 0)]

two_bytes_instruction_code_handler :: Word8 -> Byte_parser Operat
two_bytes_instruction_code_handler b1 = do
  b <- any_byte
  return $ Bad_operation b "not implemented"

one_byte_instruction_code_handler b = do
  (byte' b [0x00 .. 0x05] (parse_arithmetic_operation ADD))
  <|>
  (byte' b [0x08 .. 0x0d] (parse_arithmetic_operation OR))
  <|>
  (byte' b [0x10 .. 0x15] (parse_arithmetic_operation ADC))
  <|>
  (byte' b [0x18 .. 0x1d] (parse_arithmetic_operation SBB))
  <|>
  (byte' b [0x20 .. 0x25] (parse_arithmetic_operation AND))
  <|>
  (byte' b [0x28 .. 0x2d] (parse_arithmetic_operation SUB))
  <|>
  (byte' b [0x30 .. 0x35] (parse_arithmetic_operation XOR))
  <|>
  (byte' b [0x38 .. 0x3d] (parse_arithmetic_operation CMP))
  <|>
  (byte' b [0x06] (only_not_x64 (parse_push_segment_register_operation "es")))
  <|>
  (byte' b [0x07] (only_not_x64 (parse_pop_segment_register_operation "es")))
  <|>
  (byte' b [0x0e] (only_not_x64 (parse_push_segment_register_operation "cs")))
  <|>
  (byte' b [0x16] (only_not_x64 (parse_push_segment_register_operation "ss")))
  <|>
  (byte' b [0x17] (only_not_x64 (parse_pop_segment_register_operation "ss")))
  <|>
  (byte' b [0x1e] (only_not_x64 (parse_push_segment_register_operation "ds")))
  <|>
  (byte' b [0x1f] (only_not_x64 (parse_pop_segment_register_operation "ds")))
  <|>
  (byte' b [0x0f] (two_bytes_instruction_code_handler))
  <|>
  (byte' b [0x26, 0x2e, 0x36, 0x3e] (parse_undefined_prefix))
    
operation = do
  start_pos' <- getPosition
  let start_pos = sourceColumn start_pos' -1
  input <- getInput
  many parse_prefix
  b <- any_byte
  st <- getState
  setState st{parameter_size = parameter_size st, address_size = parameter_size st, prefix = []}
  case (one_byte_instruction_code_handler b) of
    Just p -> do i <- p b
                 end_pos' <- getPosition
                 let end_pos = sourceColumn end_pos' -1
                 case i of
                   Operat oc parameter_size parameters -> do
                     return $ Operation oc parameter_size parameters start_pos (take (end_pos - start_pos) input)
                   BadOperat b desc ->
                     return $ Bad_operation b desc start_pos (take (end_pos - start_pos) input)
    Nothing -> do BadOperat b desc <- BadOperat b "invalid operation"
                  end_pos' <- getPosition
                  let end_pos = sourceColumn end_pos' -1
                  return $ BadOperat b desc start_pos (take (end_pos - start_pos) input)

operations_list = many operation

parse_operations st l =
  return (runParser operations_list st "memory block" l)

parser_state = Parser_state { parameter_size = PS_32, address_size = PS_32, x64 = False, prefix = []}

parse_block :: Ptr Word8 -> Int -> IO (Either ParseError [Operation])
parse_block ptr len = do
  l <- to_list ptr len 0 []
  parse_operations parser_state (reverse l)
  where
    to_list :: (Ptr Word8) -> Int -> Int -> [Word8] -> IO [Word8]
    to_list ptr len index accumulator | index < len = do
                                  p <- peekByteOff ptr index
                                  to_list ptr len (index +1) (p : accumulator)
    to_list ptr len index accumulator | index >= len = return accumulator

some_func :: IO ()
some_func = putStrLn "some_func"
