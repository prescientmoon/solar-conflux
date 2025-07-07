module Nihil.Cst.Expr
  ( Expr (..)
  , Var (..)
  , Lambda (..)
  , App (..)
  , Match (..)
  , MatchKind (..)
  , MatchBranch (..)
  , Pattern (..)
  , PatternProj (..)
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
import Optics qualified as O
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
  { patterns ∷ Base.Separated Base.Token' Pattern
  , arrow ∷ Maybe Base.Token'
  , body ∷ Maybe Expr
  }
  deriving (Generic, Show)

data Pattern
  = PName Base.Name -- var → ...
  | PWildcard Base.Token' -- _ → ...
  | PProj PatternProj -- .Thing a → ...
  | PParens (Base.Delimited (Maybe Pattern))
  deriving (Generic, Show)

instance PP.Pretty Pattern where
  pretty (PName x) = PP.pretty x
  pretty (PWildcard x) = PP.pretty x
  pretty (PProj x) = PP.pretty x
  pretty (PParens x) = PP.pretty x

instance Base.HasSpan Pattern where
  spanOf (PName x) = Base.spanOf x
  spanOf (PWildcard x) = Base.spanOf x
  spanOf (PProj x) = Base.spanOf x
  spanOf (PParens (Base.Delimited{..})) =
    Base.mergeSpans
      (Base.spanOf open)
      [ Base.spanOf <$> inner
      , Base.spanOf <$> close
      ]

instance Base.HasTrivia Pattern where
  attachTrivia t (PName x) = PName <$> Base.attachTrivia t x
  attachTrivia t (PWildcard x) = PWildcard <$> Base.attachTrivia t x
  attachTrivia t (PProj x) = PProj <$> Base.attachTrivia t x
  attachTrivia t (PParens x) = PParens <$> Base.attachTrivia t x

data PatternProj = PatternProj
  { dot ∷ Base.Token'
  , head ∷ Maybe Base.Name
  , args ∷ Seq Pattern
  }
  deriving (Generic, Show)

instance PP.Pretty PatternProj where
  pretty app =
    Base.prettyTree "PatternProj" $
      catMaybes
        [ Just . PP.pretty $ O.view #dot app
        , PP.pretty <$> O.view #head app
        ]
        <> (toList $ PP.pretty <$> O.view #args app)

instance Base.HasSpan PatternProj where
  spanOf app =
    Base.mergeSpans (Base.spanOf $ O.view #dot app) $
      (Base.spanOf <$> O.view #head app)
        : (toList $ Just . Base.spanOf <$> O.view #args app)

instance Base.HasTrivia PatternProj where
  attachTrivia t = Just . O.over #dot (Base.tokAttachTrivia t)

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
