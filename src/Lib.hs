module Lib( 
    Code_of_operation,
    Parameter(..),
--    Command(..),

--    parse_block,
--    parse_file,
      some_func
    ) where

import Data.Char
import Data.Word
import qualified Data.List as DL
import Numeric

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

data Parameter = P_Immediate_value Word32
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


show_parameter parameter_size (P_Immediate_value w) = show_immediate_parameter w
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


some_func :: IO ()
some_func = putStrLn "some_func"
