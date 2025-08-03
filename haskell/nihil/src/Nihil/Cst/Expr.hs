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
import Text.Megaparsec qualified as M

data Expr
  = EVar Var
  | ELambda Lambda
  | EApp App
  | EMatch Match
  | EWith With -- unimplemented
  | EIf If -- unimplemented
  | EBlock Block -- unimplemented
  | EMake Make -- unimplemented
  | EParens (Base.Delimited (Maybe Expr))
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Expr where
  pretty (EVar x) = PP.pretty x
  pretty (ELambda x) = PP.pretty x
  pretty (EApp x) = PP.pretty x
  pretty (EMatch x) = PP.pretty x
  pretty (EWith x) = PP.pretty x
  pretty (EIf x) = PP.pretty x
  pretty (EBlock x) = PP.pretty x
  pretty (EMake x) = PP.pretty x
  pretty (EParens x) = PP.pretty x

data Var = Var
  { name ∷ Base.Name
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Var where
  pretty (Var{..}) = PP.pretty name

data Lambda = Lambda
  { lam ∷ Base.Token'
  , patterns ∷ Seq Pattern
  , arrow ∷ Maybe Base.Token'
  , body ∷ Maybe Expr
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Lambda where
  pretty (Lambda{..}) =
    Base.prettyTree "Lambda" . catMaybes $
      [ Just $ PP.pretty lam
      , guard (not $ null patterns) $> Base.prettyTree "patterns" do
          toList $ PP.pretty <$> patterns
      , PP.pretty <$> arrow
      , PP.pretty <$> body
      ]

data App = App
  { f ∷ Expr
  , a ∷ Expr
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty App where
  pretty (App{..}) =
    Base.prettyTree "App" $
      [ PP.pretty f
      , PP.pretty a
      ]

data Match = Match
  { kind ∷ Base.Token MatchKind
  , exprs ∷ Base.Separated Base.Token' Expr
  , where' ∷ Maybe Base.Token'
  , branches ∷ Seq MatchBranch
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Match where
  pretty (Match{..}) =
    Base.prettyTree "Match" . catMaybes $
      [ Just $ PP.pretty kind
      , Just $ PP.pretty exprs
      , PP.pretty <$> where'
      , guard (not $ null branches) $> Base.prettyTree "branches" do
          toList $ PP.pretty <$> branches
      ]

data MatchKind = Inductive | Coinductive
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty MatchKind where
  pretty Inductive = "inductive"
  pretty Coinductive = "coinductive"

data MatchBranch = MatchBranch
  { start ∷ M.SourcePos
  , patterns ∷ Base.Separated Base.Token' Pattern
  , arrow ∷ Maybe Base.Token'
  , body ∷ Maybe Expr
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty MatchBranch where
  pretty (MatchBranch{..}) =
    Base.prettyTree "MatchBranch" . catMaybes $
      [ Just $ PP.pretty patterns
      , PP.pretty <$> arrow
      , PP.pretty <$> body
      ]

data Pattern
  = PName Base.Name -- var → ...
  | PWildcard Base.Token' -- _ → ...
  | PProj PatternProj -- .Thing a → ...
  | PParens (Base.Delimited (Maybe Pattern))
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Pattern where
  pretty (PName x) = PP.pretty x
  pretty (PWildcard x) = PP.pretty x
  pretty (PProj x) = PP.pretty x
  pretty (PParens x) = PP.pretty x

data PatternProj = PatternProj
  { dot ∷ Base.Token'
  , head ∷ Maybe Base.Name
  , args ∷ Seq Pattern
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty PatternProj where
  pretty app =
    Base.prettyTree "Projection pattern" $
      catMaybes
        [ Just . PP.pretty $ O.view #dot app
        , PP.pretty <$> O.view #head app
        ]
        <> (toList $ PP.pretty <$> O.view #args app)

data With = With
  { with ∷ Base.Token'
  , expr ∷ Maybe Expr
  , block ∷ Maybe Block
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty With where
  pretty (With{..}) =
    Base.prettyTree "With" . catMaybes $
      [ Just $ PP.pretty with
      , PP.pretty <$> expr
      , PP.pretty <$> block
      ]

data If = If
  { if' ∷ Base.Token'
  , expr ∷ Maybe Expr
  , block ∷ Maybe Block
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty If where
  pretty (If{..}) =
    Base.prettyTree "If" . catMaybes $
      [ Just $ PP.pretty if'
      , PP.pretty <$> expr
      , PP.pretty <$> block
      ]

data Block
  = BDo Do -- do ...
  | BRemaining (Base.Token ()) -- ⤵
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Block where
  pretty (BDo x) = PP.pretty x
  pretty (BRemaining x) = PP.pretty x

data Do = Do
  { do' ∷ Base.Token'
  , statements ∷ Seq Statement
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Do where
  pretty (Do{..}) =
    Base.prettyTree "Do" . catMaybes $
      [ Just $ PP.pretty do'
      , guard (not $ null statements) $> Base.prettyTree "statements" do
          toList $ PP.pretty <$> statements
      ]

data Make = Make
  { make ∷ Base.Token'
  , statements ∷ Seq Statement
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Make where
  pretty (Make{..}) =
    Base.prettyTree "Make" . catMaybes $
      [ Just $ PP.pretty make
      , guard (not $ null statements) $> Base.prettyTree "statements" do
          toList $ PP.pretty <$> statements
      ]

data Statement
  = SDecl LocalDeclaration
  | SExpr Expr
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty Statement where
  pretty (SDecl x) = PP.pretty x
  pretty (SExpr x) = PP.pretty x

data LocalDeclaration = LocalDeclaration
  { name ∷ Base.Name
  , colon ∷ Maybe Base.Token'
  , ty ∷ Maybe Type.Type'
  , eq ∷ Maybe Base.Token'
  , value ∷ Maybe Expr
  }
  deriving (Generic, Show, Base.HasTrivia)

instance PP.Pretty LocalDeclaration where
  pretty (LocalDeclaration{..}) =
    Base.prettyTree "LocalDeclaration" . catMaybes $
      [ Just $ PP.pretty name
      , PP.pretty <$> colon
      , PP.pretty <$> ty
      , PP.pretty <$> eq
      , PP.pretty <$> value
      ]
