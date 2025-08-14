module Nihil.Compiler.Term where

import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import Nihil.Compiler.Ast qualified as Ast
import Nihil.Utils (Icit (..))
import Prettyprinter qualified as PP
import Relude

-- {{{ Syntax building blocks

-- | De Bruijn index.
newtype Ix = Ix Int
  deriving (Eq, Show)
  deriving newtype (Num)

-- | De Bruijn level.
newtype Lvl = Lvl Int
  deriving (Eq, Show)
  deriving newtype (Num)

newtype MetaVar = MetaVar Int
  deriving (Generic, Show)
  deriving newtype (Eq, Hashable, Num)

newtype CheckVar = CheckVar Int
  deriving (Generic, Show)
  deriving newtype (Eq, Hashable, Num)

type Pruning = Seq (Maybe Icit)

-- }}}
-- {{{ Terms

type Type' = Term
data Term
  = TPi Icit (Maybe Ast.Binder) Type' Type'
  | TLambda Icit (Maybe Ast.Binder) Type' Term
  | TVar Ix
  | TApp Icit Term Term
  | TAppPruning Term Pruning
  | TUnknown
  | TU
  | TMeta MetaVar
  | TCheck CheckVar
  | TLet Ast.Binder Term Type' Term
  deriving (Generic, Show)

-- }}}
-- {{{ Values
type Env = Seq Value
type Spine = Seq (Value, Icit)
data Closure = Closure Env Term
  deriving (Show, Generic)

type VType = Value
data Value
  = VFlex MetaVar Spine
  | VRigid Lvl Spine
  | VUnknown Spine
  | VLambda Icit (Maybe Ast.Binder) VType Closure
  | VPi Icit (Maybe Ast.Binder) VType Closure
  | VU
  deriving (Show, Generic)

-- }}}
-- {{{ Pretty printing
instance PP.Pretty Term where
  pretty = \case
    TU → "*"
    TVar (Ix ix) → show ix
    TUnknown → "_"
    TMeta (MetaVar i) → "?" <> show i
    TCheck (CheckVar i) → "??" <> show i
    TLambda icit binder ty term →
      fold
        [ "λ"
        , lambdaArg icit binder ty
        , ". "
        , PP.pretty term
        ]
    TPi icit binder ty codomain →
      fold
        [ "∏"
        , lambdaArg icit binder ty
        , ". "
        , PP.pretty codomain
        ]
    TLet binder term ty in' →
      PP.align $
        PP.sep
          [ PP.hsep
              [ "let"
              , PP.pretty binder
              , "="
              , PP.pretty term
              , "::"
              , PP.pretty ty
              ]
          , PP.hsep
              [ "in"
              , PP.pretty in'
              ]
          ]
    TApp icit func arg →
      PP.align $
        PP.sep
          [ wrapIf
              (appHeadNeedsParens func)
              PP.parens
              (PP.pretty func)
          , wrapIf
              (icit /= Explicit || appArgNeedsParens arg)
              (icitDelim icit)
              (PP.pretty arg)
          ]
    TAppPruning f' pruning → PP.brackets $ PP.pretty $ go 0 f' pruning
     where
      go _ f Seq.Empty = f
      go ix f (rest :|> Nothing) = go (ix + 1) f rest
      go ix f (rest :|> Just icit) =
        TApp icit (go (ix + 1) f rest) (TVar ix)
   where
    lambdaArg icit binder ty =
      icitDelim icit $ case binder of
        Just (Ast.Resolved _ _ name) →
          PP.hsep
            [ PP.pretty name
            , "::"
            , PP.pretty ty
            ]
        _ →
          PP.hsep
            [ "_"
            , "::"
            , PP.pretty ty
            ]

    icitDelim Explicit = PP.parens
    icitDelim Implicit = PP.braces
    icitDelim Auto = PP.brackets

    appHeadNeedsParens (TApp Explicit _ (TLambda{})) = True
    appHeadNeedsParens (TApp Explicit _ (TPi{})) = True
    appHeadNeedsParens (TApp Explicit _ (TLet{})) = True
    appHeadNeedsParens (TLambda{}) = True
    appHeadNeedsParens (TPi{}) = True
    appHeadNeedsParens (TLet{}) = True
    appHeadNeedsParens _ = False

    appArgNeedsParens (TApp _ _ _) = True
    appArgNeedsParens _ = False

    wrapIf True f d = f d
    wrapIf False _ d = d

-- }}}
