module osreh.Ast.Type where

data Address = Address Word32
  deriving (Eq, Ord, Real, Show, Typable, Data, Generic)

data Any_float =
  FP16 Word16
  | FP32 Float
  | FP64 Double
  | FP128 Word64 Word64
  | FP80 Word16 Word64

data Floating_point_type =
  FP16
  | FP32
  | FP64
  | FP128
  | FP80
  deriving (Eq, Ord, Real, Show, Typable, Data, Generic)

data Type =
  Void
  | Integer_type { bits :: Word32 }
  | Pointer_type { pointed_type :: Type, address :: Address}
  | Floating_point_type { float "" Floating_point_type}
  deriving (Eq, Ord, Real, Show, Typable, Data, Generic)

