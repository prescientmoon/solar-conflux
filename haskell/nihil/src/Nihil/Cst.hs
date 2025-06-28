module Nihil.Cst () where

import Text.Megaparsec qualified as M

---------- Utility types (spans, comments, tokens, names, etc)
data Span = Span
  { from ∷ M.SourcePos
  , to ∷ M.SourcePos
  }

-- TODO: make this contain all the data it requires
data Comment = Comment
  { content ∷ Text
  , span ∷ Span
  }

data Token a
  = Token
  { comments ∷ [Comment]
  , span ∷ Span
  , value ∷ a
  }

type CName = Token Text

data CDelimited a = CDelimited
  { tOpen ∷ Token ()
  , inner ∷ a
  , tClose ∷ Token ()
  }

---------- Nihil CST
data CModule
  = CModule
  { tModule ∷ Maybe (Token ())
  , name ∷ Maybe CName
  , exports ∷ Maybe (CDelimited CName)
  , tWhere ∷ Maybe (Token ())
  , decls ∷ [CDecl]
  , eof ∷ Token ()
  }

data CDecl
  = CDeclIndLike CIndLike
  | CDeclTypeAlias CTypeAlias
  | CDeclForeignType CForeignType
  | CDeclForeignValue CForeignValue
  | CDeclValue CValue

data CIndLikeKind = Inductive | Coinductive | Trait
data CIndLike
  = CIndLike
  { kind ∷ Token CIndLikeKind
  , name ∷ Maybe CName
  , args ∷ [CName]
  , tWhere ∷ Maybe (Token ())
  , fields ∷ [CField]
  }

data CField
  = CField
  { name ∷ Maybe CName
  , tColon ∷ Maybe (Token ())
  , ty ∷ Maybe CType
  }

data CTypeAlias
  = CTypeAlias
  { tType ∷ Token ()
  , name ∷ Maybe CName
  , args ∷ [CName]
  , tEq ∷ Maybe (Token ())
  , body ∷ Maybe (CType)
  }

data CForeignType
  = CForeignType
  { tForeign ∷ Token ()
  , tType ∷ Token ()
  , name ∷ Maybe CName
  , args ∷ [CName]
  }

data CForeignValue
  = CForeignValue
  { tForeign ∷ Token ()
  , name ∷ Maybe CName
  , tColon ∷ Maybe (Token ())
  , ty ∷ Maybe CType
  }

data CValue
  = CValue
  { name ∷ Maybe CName
  , tColon ∷ Maybe (Token ())
  , ty ∷ Maybe CType
  , branches ∷ [CValueEquation]
  }

data CValueEquation = CValueEquation
  { name ∷ CName
  , args ∷ [CName]
  , tEq ∷ Maybe (Token ())
  , expr ∷ Maybe CExpr
  }

data CType
  = CTyForall CTForall
  | CTyArrow CTArrow
  | CTyApp CTApp
  | CTyRef CTRef
  | CTyVar CTVar
  | CTyParens (CDelimited CType)

data CTForall = CTForall
  { tForall ∷ Token ()
  , names ∷ [CName]
  , dot ∷ Maybe (Token ())
  , ty ∷ Maybe CType
  }

data CTArrowKind = CNormal | CTrait | CSelf
data CTArrow = CTArrow
  { from ∷ Maybe CType
  , kind ∷ CTArrowKind
  , to ∷ Maybe CType
  }

data CTApp = CTApp
  { f ∷ CType
  , a ∷ CType
  }

data CTRef = CTRef
  { ampersand ∷ Token ()
  , ty ∷ Maybe CType
  }

data CTVar = CTVar
  { name ∷ CName
  }

data CExpr
