module Nihil.Cst.Expr
  ( Expr (..)
  , Var (..)
  , Lambda (..)
  , App (..)
  , Match (..)
  , MatchKind (..)
  , MatchBranch (..)
  , Pattern (..)
  , PatternApp (..)
  , With (..)
  , If (..)
  , Block (..)
  , Do (..)
  , Make (..)
  , Statement (..)
  , LocalDeclaration (..)
  ) where

import Relude

import Nihil.Cst.Base qualified as Base
import Nihil.Cst.Type qualified as Type
import Prettyprinter qualified as PP

data Expr
  = EVar Var
  | ELambda Lambda
  | EApp App
  | EMatch Match
  | EWith With
  | EIf If
  | EBlock Block
  | EMake Make
  | EParens (Base.Delimited Expr)
  deriving (Generic, Show)

instance PP.Pretty Expr where
  pretty _ = "<Expr>"

data Var = Var
  { name ∷ Base.Name
  }
  deriving (Generic, Show)

data Lambda = Lambda
  { lam ∷ Base.Token ()
  , args ∷ [Base.Name]
  , arrow ∷ Maybe (Base.Token ())
  , body ∷ Maybe Expr
  }
  deriving (Generic, Show)

data App = App
  { f ∷ Expr
  , a ∷ Expr
  }
  deriving (Generic, Show)

data Match = Match
  { kind ∷ Base.Token MatchKind
  , exprs ∷ Base.Separated (Base.Token ()) Expr
  , where' ∷ Base.Token ()
  , branches ∷ [MatchBranch]
  }
  deriving (Generic, Show)

data MatchKind = Inductive | Coinductive
  deriving (Generic, Show)

data MatchBranch = MatchBranch
  { patterns ∷ Base.Separated (Base.Token ()) Expr
  , arrow ∷ Maybe (Base.Token ())
  , body ∷ Maybe Expr
  }
  deriving (Generic, Show)

data Pattern
  = PName Base.Name -- var → ...
  | PWildcard (Base.Token ()) -- _ → ...
  | PApp PatternApp
  | PParens (Base.Delimited Pattern)
  deriving (Generic, Show)

instance PP.Pretty Pattern where
  pretty _ = "<Pattern>"

data PatternApp = PatternApp
  { dot ∷ Base.Token ()
  , head ∷ Maybe Base.Name
  , args ∷ [Pattern]
  }
  deriving (Generic, Show)

data With = With
  { with ∷ Base.Token ()
  , expr ∷ Maybe Expr
  , block ∷ Maybe Block
  }
  deriving (Generic, Show)

data If = If
  { if' ∷ Base.Token ()
  , expr ∷ Maybe Expr
  , block ∷ Maybe Block
  }
  deriving (Generic, Show)

data Block
  = BDo Do -- do ...
  | BRemaining (Base.Token ()) -- ⤵
  deriving (Generic, Show)

data Do = Do
  { do' ∷ Base.Token ()
  , statements ∷ [Statement]
  }
  deriving (Generic, Show)

data Make = Make
  { make ∷ Base.Token ()
  , statements ∷ [Statement]
  }
  deriving (Generic, Show)

data Statement
  = SDecl LocalDeclaration
  | SExpr Expr
  deriving (Generic, Show)

data LocalDeclaration = LocalDeclaration
  { name ∷ Base.Name
  , colon ∷ Maybe (Base.Token ())
  , ty ∷ Maybe Type.Type'
  , eq ∷ Maybe (Base.Token ())
  , value ∷ Maybe Expr
  }
  deriving (Generic, Show)
