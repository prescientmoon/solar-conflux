module Nihil.Ast.Term
  ( MetaVar (..)
  , CheckVar (..)
  , Term (..)
  , Pruning
  , Spine
  , eval
  , infer
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence (Seq ((:<|), (:|>)), (|>))
import Data.Sequence qualified as Seq
import Nihil.Ast.State qualified as Ast
import Nihil.Utils (Icit (..))
import Optics ((%))
import Optics qualified as O
import Relude

-- The `MonadElab` constraint
-- {{{ State & contexts types
data ElabState = ElabState
  { checkVars ∷ Seq CheckEntry
  , metaVars ∷ Seq MetaEntry
  }
  deriving (Show, Generic)

data CheckEntry
  = Checked Term
  | -- | We got stuck solving Γ ⊢ t : A, thus elaborated to a meta m for now.
    Unchecked Context Term VType MetaVar
  deriving (Show, Generic)

data MetaEntry
  = Solved Value VType
  | -- | An unsolved meta that blocks a numb of checking problems.
    Unsolved (HashSet CheckVar) VType
  deriving (Show, Generic)

data Context = Context
  { env ∷ Env
  , srcNames ∷ HashMap (Ast.ScopeId, Text) (Lvl, VType)
  -- ^ Holds the types for the various names in context.
  , rigid ∷ Pruning
  -- ^ Which variables in scope represent rigid variables
  -- (i.e. things not bound by a `let-in` and the like).
  , variables ∷ Seq VarEntry
  }
  deriving (Show, Generic)

type MonadElab m =
  ( MonadState ElabState m
  , MonadReader Context m
  , Ast.MonadCompile m
  )

data VarEntry
  = Bind (Maybe Ast.Name) Type'
  | Define Ast.Name Term Type'
  deriving (Show, Generic)

-- }}}
-- {{{ Context manipulation
getEnv ∷ ∀ m. (MonadElab m) ⇒ m Env
getEnv = asks $ O.view #env

-- | Override the current env. This is a dangerous operation!
withEnv ∷ ∀ m a. (MonadElab m) ⇒ Env → m a → m a
withEnv e = local (O.set #env e)

-- | Add a new value to the environment. This doesn't expand the rest of the
-- context properly. Prefer `@bindCtx@ instead.
expandEnv ∷ ∀ m a. (MonadElab m) ⇒ Value → m a → m a
expandEnv v = local (O.over #env (|> v))

-- | Add a new rigid variable to the current scope.
--
-- This is the "proper" version of `@expandEnv@.
bindCtx ∷ ∀ m a. (MonadElab m) ⇒ Maybe Ast.Name → VType → m a → m a
bindCtx (Just (Ast.Unresolved _ _)) _ _ = error "Impossible: unresonved name."
bindCtx (Just name@(Ast.Resolved _ sid tName)) vty c = do
  l ← Lvl <$> contextSize
  ty ← quote vty
  c & local \ctx →
    Context
      { env = ctx.env |> VRigid l mempty
      , srcNames = HashMap.insert (sid, tName) (l, vty) ctx.srcNames
      , rigid = ctx.rigid |> Just Explicit
      , variables = ctx.variables |> Bind (Just name) ty
      }
bindCtx Nothing vty c = do
  l ← Lvl <$> contextSize
  ty ← quote vty
  c & local \ctx →
    Context
      { env = ctx.env |> VRigid l mempty
      , rigid = ctx.rigid |> Just Explicit
      , variables = ctx.variables |> Bind Nothing ty
      , srcNames = ctx.srcNames
      }

contextSize ∷ ∀ m. (MonadElab m) ⇒ m Int
contextSize = do
  c ← ask
  pure $ Seq.length c.env

-- }}}
-- {{{ Metas & closing over terms
freshMetaRaw ∷ ∀ m. (MonadElab m) ⇒ VType → m MetaVar
freshMetaRaw ty = do
  entries ← gets @ElabState $ O.view #metaVars
  O.modifying @_ @ElabState #metaVars (|> Unsolved mempty ty)
  pure $ MetaVar $ Seq.length entries

-- | Generates a meta that is applied to every rigid variable in scope.
--
-- Intuitively, metas are "placeholder" type variables that live in the global
-- scope. In order to give them access to local rigid variables, we apply
-- everything to them as if they were a function, causing them to later
-- be solved to a lambda that makes use of said parameters.
freshMeta ∷ ∀ m. (MonadElab m) ⇒ VType → m Term
freshMeta ty = do
  ty' ← closeVTy ty
  m ← freshMetaRaw ty'
  pruning ← asks $ O.view #rigid
  pure $ TAppPruning (TMeta m) pruning

-- | Wrap a type such that it no longer references anything in the context.
closeTy ∷ ∀ m. (MonadElab m) ⇒ Type' → m Type'
closeTy ty' = do
  locals ← asks $ O.view #variables
  go locals ty'
 where
  go Seq.Empty ty = pure ty
  go (rest :|> Define name a t) ty = do
    go rest $ TLet name a t ty
  go (rest :|> Bind name t) ty = do
    go rest $ TPi Explicit name t ty

-- | Wrap a type such that it no longer references anything in the context.
closeVTy ∷ ∀ m. (MonadElab m) ⇒ VType → m VType
closeVTy vt = do
  t ← quote vt
  closed ← closeTy t
  withEnv mempty $ eval closed

-- }}}

-- {{{ Syntax building blocks

-- | De Bruijn index.
newtype Ix = Ix Int
  deriving (Eq, Show)
  deriving newtype (Num)

-- | De Bruijn level.
newtype Lvl = Lvl Int
  deriving (Eq, Show)
  deriving newtype (Num)

levelToIndex ∷ ∀ m. (MonadElab m) ⇒ Lvl → m Ix
levelToIndex (Lvl x) = do
  size ← contextSize
  pure $ Ix (size - x - 1)

newtype MetaVar = MetaVar Int
  deriving (Generic, Show)
  deriving newtype (Eq, Hashable)

newtype CheckVar = CheckVar Int
  deriving (Generic, Show)
  deriving newtype (Eq, Hashable)

type Pruning = Seq (Maybe Icit)

-- }}}
-- {{{ Terms

type Type' = Term
data Term
  = TPi Icit (Maybe Ast.Binder) Type' Type'
  | TVar Ix
  | TApp Icit Term Term
  | TAppPruning Term Pruning
  | TLambda Icit Ast.Binder Term
  | TUnknown
  | TU
  | TMeta MetaVar
  | TCheck CheckVar
  | TLet Ast.Binder Term Type' Term
  deriving (Generic, Show)

-- }}}

-- Evaluation
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
  | VLambda Icit Ast.Binder Closure
  | VPi Icit (Maybe Ast.Binder) VType Closure
  | VU
  deriving (Show, Generic)

-- }}}
-- {{{ Evaluation
eval ∷ ∀ m. (MonadElab m) ⇒ Term → m Value
eval = \case
  TU → pure VU
  TApp icit f a → do
    vf ← eval f
    va ← eval a
    evalApp icit vf va
  TAppPruning t pruning → do
    vt ← eval t
    evalAppPruning vt pruning
  TVar ix → do
    e ← getEnv
    pure $ Seq.index e $ coerce ix
  TLambda icit name body → do
    e ← getEnv
    pure $ VLambda icit name $ Closure e body
  TPi icit name ty body → do
    vty ← eval ty
    e ← getEnv
    pure $ VPi icit name vty $ Closure e body
  -- TODO: check whether they got resolved in the meantime
  TMeta m → forceMeta $ VFlex m mempty
  TUnknown → pure $ VUnknown mempty
  TLet _ thing _ in' → do
    vthing ← eval thing
    expandEnv vthing $ eval in'
  TCheck _ → pure $ error "unimplemented check evaluation"

evalApp ∷ ∀ m. (MonadElab m) ⇒ Icit → Value → Value → m Value
evalApp icit (VFlex m sp) a = pure $ VFlex m (sp |> (a, icit))
evalApp icit (VRigid r sp) a = pure $ VRigid r (sp |> (a, icit))
evalApp icit (VUnknown sp) a = pure $ VUnknown (sp |> (a, icit))
evalApp _ (VLambda _ _ cl) a = evalClosure cl a
evalApp _ _ _ = error "The impossible happened: cannot apply term."

evalAppSpine ∷ ∀ m. (MonadElab m) ⇒ Value → Spine → m Value
evalAppSpine v Seq.Empty = pure v
evalAppSpine v ((a, icit) :<| rest) = do
  res ← evalApp icit v a
  evalAppSpine res rest

evalAppPruning ∷ ∀ m. (MonadElab m) ⇒ Value → Pruning → m Value
evalAppPruning v' pruning = getEnv >>= go v' pruning
 where
  go ∷ Value → Pruning → Env → m Value
  go v Seq.Empty Seq.Empty = pure v
  go v (Just icit :<| pr') (t :<| e') = do
    res ← evalApp icit v t
    go res pr' e'
  go v (Nothing :<| pr') (_ :<| e') = do
    go v pr' e'
  go _ _ _ = error "Pruning has different size from context."

evalClosure ∷ ∀ m. (MonadElab m) ⇒ Closure → Value → m Value
evalClosure (Closure e inner) a = do
  withEnv e $ expandEnv a $ eval inner

forceMeta ∷ ∀ m. (MonadElab m) ⇒ Value → m Value
forceMeta e@(VFlex meta spine) = do
  entry ← O.preuse @_ @ElabState (#metaVars % O.ix (coerce meta))
  case entry of
    Just (Solved val _) → do
      res ← evalAppSpine val spine
      forceMeta res
    _ → pure e
forceMeta e = pure e

-- }}}
-- {{{ Quoting

-- | Turn a value back into a term.
quote ∷ ∀ m. (MonadElab m) ⇒ Value → m Term
quote =
  forceMeta >=> \case
    VUnknown spine → quoteSpine TUnknown spine
    VFlex meta spine → quoteSpine (TMeta meta) spine
    VRigid level spine → do
      ix ← levelToIndex level
      quoteSpine (TVar ix) spine
    VLambda icit binder closure → do
      size ← contextSize
      let vv = VRigid (Lvl size) mempty
      vbody ← evalClosure closure vv
      body ← expandEnv vv $ quote vbody
      pure $ TLambda icit binder body
    VPi icit binder vdomain closure → do
      size ← contextSize
      let vv = VRigid (Lvl size) mempty
      vbody ← evalClosure closure vv
      body ← expandEnv vv $ quote vbody
      domain ← quote vdomain
      pure $ TPi icit binder domain body
    VU → pure TU

-- | Turn a spine applied to a value back into a term.
quoteSpine ∷ ∀ m. (MonadElab m) ⇒ Term → Spine → m Term
quoteSpine t Seq.Empty = pure t
quoteSpine t ((value, icit) :<| sp) = do
  res ← quote value
  quoteSpine (TApp icit t res) sp

-- }}}

-- Elaboration
-- {{{ Inference
infer ∷ ∀ m. (MonadElab m) ⇒ Ast.Expr → m (Term, VType)
infer (Ast.EVar _ (Ast.Unresolved _ _)) = error "Impossible: unresolved name."
infer (Ast.EVar _ (Ast.Resolved _ sid name)) = do
  names ← asks \x → x.srcNames
  case HashMap.lookup (sid, name) names of
    Nothing → error "Impossible: variable not in scope."
    Just (level, type') → do
      ix ← levelToIndex level
      pure (TVar ix, type')
infer (Ast.EApp _ func arg) = do
  let icit = Explicit -- We don't support more right now

  -- Insert implicit arguments
  (icit', func', tfunc) ← case icit of
    Auto → error "Auto calls are not supported right now!"
    Implicit → do
      (func', tfunc) ← infer func
      pure (Implicit, func', tfunc)
    Explicit → do
      -- Since this is an explicit application, we fill
      -- every implicit argument with a meta.
      (func', tfunc) ← insertApps =<< infer func
      pure (Explicit, func', tfunc)

  -- ensure that `func` is a ∏-type
  (domain, codomain) ←
    forceMeta tfunc >>= \case
      -- If the domain is already a ∏ type, we are done.
      VPi icit'' _ domain codomain → do
        when (icit' /= icit'') do
          -- TODO: proper error handling
          error "Implicitness missmatch"
        pure (domain, codomain)

      -- Otherwise, we create metas for the domain and codomain,
      -- and unify the "artificial" ∏-type with the actual type.
      _other → do
        domain ← freshMeta VU
        vdomain ← eval domain
        codomain ← bindCtx Nothing vdomain $ freshMeta VU
        e ← getEnv
        let vcodomain = Closure e codomain

        -- unifyCatch cxt (VPi "x" i a b) tty ExpectedInferred
        pure (vdomain, vcodomain)

  arg' ← check arg domain
  varg ← eval arg'
  targ ← evalClosure codomain varg

  pure (TApp icit' func' arg', targ)
infer (Ast.EMatch _ Seq.Empty ((Seq.Empty, _, body) :<| Seq.Empty)) = infer body
infer (Ast.EMatch _ Seq.Empty (((Ast.PName _ binder :<| Seq.Empty), _, body) :<| Seq.Empty)) = do
  -- Generate a new meta for the domain of the function
  meta ← freshMeta VU
  vMeta ← eval meta

  let icit = Explicit -- We don't support more right now
  (body', tbody) ← bindCtx (Just binder) vMeta do
    (body', vtbody) ← insertAppsNeutral =<< infer body
    tbody ← quote vtbody
    pure (body', tbody)

  e ← getEnv
  pure
    ( TLambda icit binder body'
    , VPi icit (Just binder) vMeta $ Closure e tbody
    )
infer (Ast.EPi _ _ icit binder domain codomain) = do
  domain' ← check domain VU
  va ← eval domain'
  codomain' ← bindCtx (Just binder) va $ check codomain VU
  pure (TPi icit (Just binder) domain' codomain', VU)
infer (Ast.Eannotation _ e ty) = do
  ty' ← check ty VU
  vty ← eval ty'
  e' ← check e vty
  pure (e', vty)
infer (Ast.EHole _) = do
  ty ← freshMeta VU
  vty ← eval ty
  e ← freshMeta vty
  pure (e, vty)
infer (Ast.EUnknown i) = infer $ Ast.EHole i
infer (Ast.EMatch _ _ _) = error "Unimplemented"

-- | Insert fresh implicit applications.
insertApps ∷ ∀ m. (MonadElab m) ⇒ (Term, VType) → m (Term, VType)
insertApps (t, va) =
  forceMeta va >>= \case
    VPi Implicit _ a b → do
      meta ← freshMeta a
      vmeta ← eval meta
      let app = TApp Implicit t meta
      vapp ← evalClosure b vmeta
      insertApps (app, vapp)
    VPi Auto _ _ _ → error "Auto calls are not supported right now!"
    other → pure (t, other)

-- | Insert fresh implicit applications to a term which is not
--   an implicit lambda (i.e. neutral).
insertAppsNeutral ∷ ∀ m. (MonadElab m) ⇒ (Term, VType) → m (Term, VType)
insertAppsNeutral = \case
  (t@(TLambda Implicit _ _), va) → pure (t, va)
  (t, va) → insertApps (t, va)

-- }}}
-- {{{ Checking
check ∷ ∀ m. (MonadElab m) ⇒ Ast.Expr → VType → m Term
check = error "Unimplemented"

-- }}}
-- {{{ Unification
-- }}}

-- {{{ Ramblings about syntax
{-

domain -> codomain
∀a, codomain
∀(a, b, c: domain), codomain
(a, b, c, d, e, f : other) => codomain

∏(a, b: domain), codomain
∀(a, b: domain), codomain

∏domain, codomain
∀domain, codomain

{a: B} -> something
{a: B} => something

∀a. b -- same as ∏{a:_}, b
∀(a:A). b -- same as ∏{a:A}, b
∀{a:A}. b -- same as ∏{auto a:A}, b
∀(auto a:A). b -- same as ∏{auto a:A}, b
∀{auto a:A}. b -- disallowed

∏a. b -- same as ∏(a:_), b
∏(a:A). b -- same as ∏(a:A), b
∏(auto a:A). b -- disallowed
∏{a:A}. b -- same as ∏{a:A}, b
∏{auto a:A}. b -- same as ∏{auto a:A}, b

a -> b -- same as ∏(_:a), b
a => b -- same as ∏{auto _:a}, b

A -> B

A -> ...
A ~> ...
A => ...

∀a -> ...
∀a ~> ...
∀a => ...

forall a -> ...
forall a ~> ...
forall a => ...

{a: A} -> ...
{a: A} ~> ...
{a: A} => ...

Text.mk_text_lit : ∀a b, TextLiteral a => Review b a -> TextLiteral b
Text.mk_text_lit .from_text r t = review (TextLiteral.from_text t)

Text.mk_text_lit : ∀a b, TextLiteral a => Review b a -> TextLiteral b
Text.mk_text_lit .from_text r t = review (TextLiteral.from_text t)

A -> B
A ~> B
A => B

∏a, B(a)
∀a, B(a)
◇a, B(a)

∏{a: A}, B(a)
∀{a: A}, B(a)
◇{a: A}, B(a)

-- Flexible syntax
∏a b c, B(a)
∏{a: A} {b: A} {c: A}, B(a)
∏{a, b, c: A}, B(a)
-}
-- }}}
