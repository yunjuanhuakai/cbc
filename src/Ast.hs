{-# LANGUAGE DeriveGeneric #-}

module Ast where

import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )
import           Text.PrettyPrint.GenericPretty ( Out )

---------------------------- fc ------------------------------

data FC
  = FC
    { fn :: String
    , start :: (Int, Int)
    , end :: (Int, Int)
    }
  | NoFC
  deriving (Show, Eq, Ord, Generic)

fcIn :: FC -> FC -> Bool
fcIn (FC fn (s_l, s_c) (e_l, e_c)) (FC fn' (s_l', s_c') (e_l', e_c')) =
  fn
    == fn'
    && (s_l == s_l' && s_c > s_c' || s_l > s_l')
    && (e_l == e_l' && e_c < e_c' || e_l < e_l')
fcIn _ _ = False

fcFname :: FC -> String
fcFname (FC f _ _) = f
fcFname NoFC       = "(no file)"

fcStart :: FC -> (Int, Int)
fcStart (FC _ start _) = start
fcStart NoFC           = (0, 0)

fcEnd :: FC -> (Int, Int)
fcEnd (FC _ _ end) = end
fcEnd NoFC         = (0, 0)

instance Monoid FC where
  mempty = NoFC
  mappend (FC f start end) (FC f' start' end')
    | f == f' = FC f (min start start') (max end end')
    | otherwise = NoFC
  mappend _ _ = NoFC

--------------------------------- decl ------------------------------------

data Declaration
  = Variable
    { declFC :: FC
    , declType :: Type
    , declName :: String
    , declInit :: Maybe Expr
    }
  | Function
    { declFC :: FC
    , returnType :: Type
    , declName :: String
    , declParams :: [Param]
    , functionBody :: Stmt
    }
  | UndefineFunction
    { declFC :: FC
    , returnType :: Type
    , declName :: String
    , declParams :: [Param]
    }
  | Struct
    { declFC :: FC
    , declName :: String
    , declParams :: [Param]
    }
  | Union
    { declFC :: FC
    , declName :: String
    , declParams :: [Param]
    }
  | Typedef
    { declFC :: FC
    , declType :: Type
    , newName :: String
    }
  deriving (Eq, Show, Generic)

declToType :: Declaration -> Type
declToType (Variable _ t _ _       ) = t
declToType (Function _ t _ params _) = CbFunction t $ map paramType params
declToType (UndefineFunction _ t _ params) =
  CbFunction t $ map paramType params
declToType (Struct  _ name params) = CbStruct name params
declToType (Union   _ name params) = CbUnion name params
declToType (Typedef _ t    _     ) = t

--------------------------------- stmt ------------------------------------

data Stmt
  = If FC Expr Stmt (Maybe Stmt)
  | Switch FC Expr [Stmt]
  | Case FC Expr Stmt
  | Default FC Stmt
  | For FC Stmt (Maybe Expr) Stmt Stmt
  | While FC Expr Stmt
  | DoWhile FC Stmt Expr
  | Block FC [Declaration] [Stmt]
  | Return FC (Maybe Expr)
  | Label FC String
  | Goto FC String
  | Continue FC
  | Break FC
  | Expression FC Expr
  deriving (Eq, Show, Generic)

----------------------------- expr ------------------------------

data Expr
  = Funcall FC [Expr] Expr
  | SizeofType FC Type
  | SizeofExpr FC Expr
  | Assign FC Expr Expr
  | OpAssign FC String Expr Expr
  | Binary FC String Expr Expr
  | Unary FC String Expr
  | Prefix FC String Expr
  | Suffix FC String Expr
  | Cast FC Type Expr
  | Cond FC Expr Expr Expr
  | Address FC Expr
  | Dereference FC Expr
  | Member FC String Expr
  | PtrMember FC String Expr
  | Arrayref FC Expr Expr
  | Varable FC String
  | IntLiteral FC Int
  | FloatLiteral FC Double
  | StringLiteral FC String
  | CharLiteral FC Char
  | BoolLiteral FC Bool
  deriving (Eq, Show, Ord, Generic)

----------------------------- unit -----------------------------

data Unit =
  Unit
  { imports :: [Import],
    declarations :: [Declaration]
  } deriving (Eq, Show, Generic)

data Import = Import
  { importFc :: FC,
    importPath :: FilePath
  }
  deriving (Eq, Show, Generic)

--------------------------------- type ------------------------------------

data Param
  = Param
    { paramType :: Type
    , paramName :: String
    , paramInit :: Maybe Expr
    }
  deriving (Show, Eq, Ord, Generic)

data Type
  = CbInt | CbUInt
  | CbChar | CbUChar
  | CbBool
  | CbVoid
  | CbLong | CbULong
  | CbFloat | CbUFloat
  | CbDouble | CbUDouble
  | CbArray Type Int
  | CbPtr Type
  | CbStruct String [Param]
  | CbUnion String [Param]
  | CbFunction Type [Type]
  | CbConst Type
  | CbUnknown String
  deriving (Eq, Show, Ord, Generic)

unknown :: Type -> Bool
unknown (CbUnknown _) = True
unknown _             = False

instance Out FC
instance Out Param
instance Out Type
instance Out Expr
instance Out Stmt
instance Out Declaration
instance Out Unit
instance Out Import
