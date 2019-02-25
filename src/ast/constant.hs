module osreh.Ast.Constant where

import osreh.Ast.Type

data Constant =
  Int { bits::Word32, value :: Integer}
  | Float { value :: osreh.Ast.Type.Any_float}
  | Array { member_type :: osreh.Asy.Type.Type, value :: [Constant]}
  | Add_integer {
      op0 :: Constant,
      op1 :: Constant
      }
  | Add_float {
      op0 :: Constant,
      op1 :: Constant
      }
  | Sub_integer {
      op0 :: Constant,
      op1 :: Constant
      }
  | Sub_float {
      op0 :: Constant,
      op1 :: Constant
      }
  | Mul_integer {
      op0 :: Constant,
      op1 :: Constant
      }
  | Mul_float {
      op0 :: Constant,
      op1 :: Constant
      }
  | Div_integer {
      op0 :: Constant,
      op1 :: Constant
      }
  | Div_float {
      op0 :: Constant,
      op1 :: Constant
      }
  | Rem_integer {
      op0 :: Constant,
      op1 :: Constant
      }
  | Rem_float {
      op0 :: Constant,
      op1 :: Constant
      }    
  | Shl {
      op0 :: Constant,
      op1 :: Constant
      }
  | LShr {
      op0 :: Constant,
      op1 :: Constant
      }
  | RShr {
      op0 :: Constant,
      op1 :: Constant
      }
  | And {
      op0 :: Constant,
      op1 :: Constant
      }
  | Or {
      op0 :: Constant,
      op1 :: Constant
      }
  | Xor {
      op0 :: Constant,
      op1 :: Constant
      }
  | Cmp_integer {
      op0 :: Constant,
      op1 :: Constant
      }
  | Cmp_float {
      op0 :: Constant,
      op1 :: Constant
      }
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
