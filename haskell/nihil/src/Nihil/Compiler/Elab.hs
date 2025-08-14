module Nihil.Compiler.Elab
  ( infer
  , check
  , eval
  , quote
  , normalForm
  , checkEverything
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Sequence (Seq ((:<|), (:|>)), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Traversable (for)
import Error.Diagnose qualified as DG
import Nihil.Compiler.Ast (NodeId)
import Nihil.Compiler.Ast qualified as Ast
import Nihil.Compiler.Monad
  ( CheckEntry (..)
  , CompilerContext (..)
  , CompilerState (..)
  , MetaEntry (..)
  , MonadCompile
  , VarEntry (Bind, Define)
  , getSpan
  , reportError
  )
import Nihil.Compiler.Term
  ( CheckVar (..)
  , Closure (Closure)
  , Env
  , Ix (Ix)
  , Lvl (Lvl)
  , MetaVar (..)
  , Pruning
  , Spine
  , Term (..)
  , Type'
  , VType
  , Value (..)
  )
import Nihil.Error qualified as Error
import Nihil.Utils (Icit (..))
import Nihil.Utils qualified as Utils
import Optics ((%))
import Optics qualified as O
import Prettyprinter qualified as PP
import Relude

-- {{{ Env juggling
getEnv ∷ ∀ m. (MonadCompile m) ⇒ m Env
getEnv = asks $ O.view #env

-- | Override the current env. This is a dangerous operation!
withEnv ∷ ∀ m a. (MonadCompile m) ⇒ Env → m a → m a
withEnv e = local (O.set #env e)

-- | Add a new value to the environment. This doesn't expand the rest of the
-- context properly. Prefer `@bindCtx@ instead.
expandEnv ∷ ∀ m a. (MonadCompile m) ⇒ Value → m a → m a
expandEnv v = local (O.over #env (|> v))

-- }}}
-- {{{ Safer context juggling

-- | Add a new rigid variable to the current scope.
--
-- This is the "proper" version of `@expandEnv@.
bindCtx ∷ ∀ m a. (MonadCompile m) ⇒ Maybe Ast.Name → VType → m a → m a
bindCtx (Just (Ast.Unresolved _ _)) _ _ = error "Impossible: unresonved name."
bindCtx (Just name@(Ast.Resolved _ sid tName)) vty c = do
  l ← Lvl <$> contextSize
  ty ← quote vty
  c & local \ctx →
    CompilerContext
      { env = ctx.env |> VRigid l mempty
      , srcNames = HashMap.insert (sid, tName) (l, vty) ctx.srcNames
      , rigid = ctx.rigid |> Just Explicit
      , variables = ctx.variables |> Bind (Just name) ty
      }
bindCtx Nothing vty c = do
  l ← Lvl <$> contextSize
  ty ← quote vty
  c & local \ctx →
    CompilerContext
      { env = ctx.env |> VRigid l mempty
      , rigid = ctx.rigid |> Just Explicit
      , variables = ctx.variables |> Bind Nothing ty
      , srcNames = ctx.srcNames
      }

-- bindRigid  ∷ ∀ m a. (MonadCompile m) ⇒ Maybe Ast.Name → VType → m a → m a

contextSize ∷ ∀ m. (MonadCompile m) ⇒ m Int
contextSize = do
  c ← ask
  pure $ Seq.length c.env

-- | Creates a rigid variable referencing the next variable that's going to be
-- added to the scope.
createRigid ∷ ∀ m. (MonadCompile m) ⇒ m Value
createRigid = do
  size ← contextSize
  pure $ VRigid (Lvl size) mempty

-- | Evaluates a closure with a mere rigid variable as the argument.
insideClosure ∷ ∀ m. (MonadCompile m) ⇒ Closure → m Value
insideClosure closure = createRigid >>= evalClosure closure

levelToIndex ∷ ∀ m. (MonadCompile m) ⇒ Lvl → m Ix
levelToIndex (Lvl x) = do
  size ← contextSize
  pure $ Ix (size - x - 1)

-- }}}
-- {{{ Metas & closing over terms
freshMetaRaw ∷ ∀ m. (MonadCompile m) ⇒ VType → m MetaVar
freshMetaRaw ty = do
  entries ← gets $ O.view #metaVars
  O.modifying #metaVars (|> Unsolved mempty ty)
  pure $ MetaVar $ Seq.length entries

freshCheckRaw ∷ ∀ m. (MonadCompile m) ⇒ Ast.Expr → VType → MetaVar → m CheckVar
freshCheckRaw term ty placeholder = do
  ctx ← ask
  entries ← gets $ O.view #checkVars
  O.modifying #checkVars (|> Unchecked ctx term ty placeholder)
  pure $ CheckVar $ Seq.length entries

markBlocking ∷ ∀ m. (MonadCompile m) ⇒ MetaVar → CheckVar → m ()
markBlocking mv cv = do
  O.modifying
    (#metaVars % O.ix (coerce mv) % #_Unsolved % O._1)
    $ HashSet.insert cv

-- | Generates a meta that is applied to every rigid variable in scope.
--
-- Intuitively, metas are "placeholder" type variables that live in the global
-- scope. In order to give them access to local rigid variables, we apply
-- everything to them as if they were a function, causing them to later
-- be solved to a lambda that makes use of said parameters.
freshMeta ∷ ∀ m. (MonadCompile m) ⇒ VType → m Term
freshMeta ty = do
  ty' ← closeVType ty
  m ← freshMetaRaw ty'
  pruning ← asks $ O.view #rigid
  pure $ TAppPruning (TMeta m) pruning

-- | Wrap a type such that it no longer references anything in the context.
closeType ∷ ∀ m. (MonadCompile m) ⇒ Type' → m Type'
closeType ty' = do
  locals ← asks $ O.view #variables
  go locals ty'
 where
  go Seq.Empty ty = pure ty
  go (rest :|> Define name a t) ty = do
    go rest $ TLet name a t ty
  go (rest :|> Bind name t) ty = do
    go rest $ TPi Explicit name t ty

-- | Wrap a term such that it no longer references anything in the context.
closeTerm ∷ ∀ m. (MonadCompile m) ⇒ Term → m Term
closeTerm term' = do
  locals ← asks $ O.view #variables
  go locals term'
 where
  go Seq.Empty ty = pure ty
  go (rest :|> Define name a t) ty = do
    go rest $ TLet name a t ty
  go (rest :|> Bind name t) ty = do
    go rest $ TLambda Explicit name t ty

-- | Wrap a type such that it no longer references anything in the context.
closeVType ∷ ∀ m. (MonadCompile m) ⇒ VType → m VType
closeVType vt = do
  t ← quote vt
  closed ← closeType t
  withEnv mempty $ eval closed

-- }}}

-- Evaluation
-- {{{ Evaluation
eval ∷ ∀ m. (MonadCompile m) ⇒ Term → m Value
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
    pure $ Seq.index e $ Seq.length e - 1 - coerce ix
  TLambda icit name domain body → do
    vdomain ← eval domain
    e ← getEnv
    pure $ VLambda icit name vdomain $ Closure e body
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
  TCheck cv →
    lookupCheck cv >>= \case
      -- We know from the saved context which variables the placeholder
      -- abstracts over.
      Unchecked ctx _ _ meta → local (const ctx) do
        meta' ← forceMeta $ VFlex meta mempty
        rigid ← asks \x → x.rigid
        evalAppPruning meta' rigid
      Checked t → eval t

evalApp ∷ ∀ m. (MonadCompile m) ⇒ Icit → Value → Value → m Value
evalApp icit (VFlex m sp) a = pure $ VFlex m (sp |> (a, icit))
evalApp icit (VRigid r sp) a = pure $ VRigid r (sp |> (a, icit))
evalApp icit (VUnknown sp) a = pure $ VUnknown (sp |> (a, icit))
evalApp _ (VLambda _ _ _ cl) a = evalClosure cl a
evalApp _ _ _ = error "The impossible happened: cannot apply term."

evalAppSpine ∷ ∀ m. (MonadCompile m) ⇒ Value → Spine → m Value
evalAppSpine v Seq.Empty = pure v
evalAppSpine v ((a, icit) :<| rest) = do
  res ← evalApp icit v a
  evalAppSpine res rest

evalAppPruning ∷ ∀ m. (MonadCompile m) ⇒ Value → Pruning → m Value
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

evalClosure ∷ ∀ m. (MonadCompile m) ⇒ Closure → Value → m Value
evalClosure (Closure e inner) a = do
  withEnv e $ expandEnv a $ eval inner

lookupMeta ∷ ∀ m. (MonadCompile m) ⇒ MetaVar → m MetaEntry
lookupMeta meta = do
  entry ← O.preuse (#metaVars % O.ix (coerce meta))
  case entry of
    Just entry' → pure entry'
    Nothing → error "Impossible: tried to lookup invalid meta var."

solveMeta ∷ ∀ m. (MonadCompile m) ⇒ MetaVar → Value → VType → m ()
solveMeta meta val ty = do
  O.assign (#metaVars % O.ix (coerce meta)) $ Solved val ty

forceMeta ∷ ∀ m. (MonadCompile m) ⇒ Value → m Value
forceMeta e@(VFlex meta spine) = do
  entry ← lookupMeta meta
  case entry of
    Solved val _ → do
      res ← evalAppSpine val spine
      forceMeta res
    _ → pure e
forceMeta e = pure e

lookupCheck ∷ ∀ m. (MonadCompile m) ⇒ CheckVar → m CheckEntry
lookupCheck cv = do
  entry ← O.preuse (#checkVars % O.ix (coerce cv))
  case entry of
    Just entry' → pure entry'
    Nothing → error "Impossible: tried to lookup invalid check var."

solveCheck ∷ ∀ m. (MonadCompile m) ⇒ CheckVar → Term → m ()
solveCheck cv term = do
  O.assign (#checkVars % O.ix (coerce cv)) $ Checked term

-- }}}
-- {{{ Quoting

-- | Turn a value back into a term.
quote ∷ ∀ m. (MonadCompile m) ⇒ Value → m Term
quote =
  forceMeta >=> \case
    VUnknown spine → quoteSpine TUnknown spine
    VFlex meta spine → quoteSpine (TMeta meta) spine
    VRigid level spine → do
      ix ← levelToIndex level
      quoteSpine (TVar ix) spine
    VLambda icit binder vdomain closure → do
      domain ← quote vdomain
      vv ← createRigid
      vbody ← insideClosure closure
      body ← expandEnv vv $ quote vbody
      pure $ TLambda icit binder domain body
    VPi icit binder vdomain closure → do
      vv ← createRigid
      vbody ← insideClosure closure
      body ← expandEnv vv $ quote vbody
      domain ← quote vdomain
      pure $ TPi icit binder domain body
    VU → pure TU

-- | Turn a spine applied to a value back into a term.
quoteSpine ∷ ∀ m. (MonadCompile m) ⇒ Term → Spine → m Term
quoteSpine t Seq.Empty = pure t
quoteSpine t ((value, icit) :<| sp) = do
  res ← quote value
  quoteSpine (TApp icit t res) sp

normalForm ∷ ∀ m. (MonadCompile m) ⇒ Term → m Term
normalForm = quote <=< eval

-- }}}

-- Elaboration
-- {{{ Inference
debugLogs ∷ Bool
debugLogs = True

infer ∷ ∀ m. (MonadCompile m) ⇒ Ast.Expr → m (Term, VType)
infer e = do
  (tm, vty) ← infer' e
  ty ← quote vty
  when debugLogs do
    tm' ← normalForm tm
    traceM . Text.unpack . Utils.textPretty' . PP.vsep $
      [ "Inferred the type of"
      , PP.indent 2 $ PP.pretty e
      , "to"
      , PP.indent 2 $ PP.pretty ty
      , "elaborating to"
      , PP.indent 2 $ PP.pretty tm
      , PP.indent 2 $ PP.pretty tm'
      , "-----------"
      ]

  pure (tm, vty)

infer' ∷ ∀ m. (MonadCompile m) ⇒ Ast.Expr → m (Term, VType)
infer' (Ast.EVar _ (Ast.Unresolved _ _)) = error "Impossible: unresolved name."
infer' (Ast.EVar _ (Ast.Resolved _ sid name)) = do
  names ← asks \x → x.srcNames
  case HashMap.lookup (sid, name) names of
    Nothing → error "Impossible: variable not in scope."
    Just (level, type') → do
      ix ← levelToIndex level
      pure (TVar ix, type')
infer' (Ast.EApp i func arg) = do
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
          getSpan i >>= traverse_ \s →
            reportError
              "ImplicitnessMissmatch"
              "The implicitness of the function being called does not match that of the function call."
              [
                ( s
                , DG.This $
                    "The function being applied has implicitness "
                      <> show icit''
                      <> ", while the call has implicitness "
                      <> show icit'
                      <> "."
                )
              ]
              []
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
-- Zero argument lambda
infer' (Ast.EMatch _ Seq.Empty ((Seq.Empty, _, body) :<| Seq.Empty)) = infer body
-- One argument pattern-less lambda
infer' (Ast.EMatch _ Seq.Empty (((Ast.PName _ binder :<| Seq.Empty), _, body) :<| Seq.Empty)) = do
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
    ( TLambda icit (Just binder) meta body'
    , VPi icit (Just binder) vMeta $ Closure e tbody
    )
infer' (Ast.EPi _ _ icit binder domain codomain) = do
  domain' ← check domain VU
  va ← eval domain'
  codomain' ← bindCtx (Just binder) va $ check codomain VU
  pure (TPi icit (Just binder) domain' codomain', VU)
infer' (Ast.EAnnotation _ e ty) = do
  ty' ← check ty VU
  vty ← eval ty'
  e' ← check e vty
  pure (e', vty)
infer' (Ast.EHole _) = do
  ty ← freshMeta VU
  vty ← eval ty
  e ← freshMeta vty
  pure (e, vty)
infer' (Ast.EUnknown i) = infer $ Ast.EHole i
infer' (Ast.EMatch _ _ _) = error "Unimplemented"

-- | Insert fresh implicit applications.
insertApps ∷ ∀ m. (MonadCompile m) ⇒ (Term, VType) → m (Term, VType)
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
insertAppsNeutral ∷ ∀ m. (MonadCompile m) ⇒ (Term, VType) → m (Term, VType)
insertAppsNeutral = \case
  (t@(TLambda Implicit _ _ _), va) → pure (t, va)
  (t, va) → insertApps (t, va)

-- }}}
-- {{{ Checking
check ∷ ∀ m. (MonadCompile m) ⇒ Ast.Expr → VType → m Term
check expr vty = do
  term ← check' expr vty
  ty ← quote vty
  term' ← normalForm term
  when debugLogs . traceM . Text.unpack . Utils.textPretty' . PP.vsep $
    [ "Checked whether"
    , PP.indent 2 $ PP.pretty expr
    , "has type"
    , PP.indent 2 $ PP.pretty ty
    , "elaborating to"
    , PP.indent 2 $ PP.pretty term
    , PP.indent 2 $ PP.pretty term'
    , "-----------"
    ]
  pure term

check' ∷ ∀ m. (MonadCompile m) ⇒ Ast.Expr → VType → m Term
check' expr =
  forceMeta >=> \expected → case (expr, expected) of
    (Ast.EHole _, _) → freshMeta expected
    -- Zero argument lambda
    (Ast.EMatch _ Seq.Empty ((Seq.Empty, _, body) :<| Seq.Empty), _) → do
      check body expected
    -- One argument pattern-less lambda
    ( Ast.EMatch _ Seq.Empty (((Ast.PName _ binder :<| Seq.Empty), _, body) :<| Seq.Empty)
      , VPi icit _ vdomain vcodomain
      ) | icit == Explicit {- We currently only support this -} → do
        res ← insideClosure vcodomain
        body' ← bindCtx (Just binder) vdomain $ check body res
        domain ← quote vdomain
        pure $ TLambda icit (Just binder) domain body'
    -- From elaboration-zoo:
    -- "If we're checking a variable with unknown type, with an implicit function,
    -- we immediately unify types. This is a modest but useful approximation of
    -- polymorphic argument inference."
    (Ast.EVar _ (Ast.Resolved _ sid name), VPi Implicit binder domain codomain) → do
      names ← asks \x → x.srcNames
      case HashMap.lookup (sid, name) names of
        Nothing → error "Impossible: variable not in scope."
        Just (level, type') →
          forceMeta type' >>= \type'' → case type'' of
            VFlex _ _ → do
              unifyExpected expected type''
              ix ← levelToIndex level
              pure $ TVar ix
            _ → insertImplicitLambda binder domain codomain
    -- If the ∏ is implicit and the expression is neither an
    -- implicit lambda nor a variable, simply insert an implicit lambda
    (_, VPi Implicit binder domain codomain) → do
      insertImplicitLambda binder domain codomain
    -- Postpone checking if the expected type is a meta
    (_, VFlex _ _) → do
      cexpected ← closeVType expected
      placeholder ← freshMetaRaw cexpected
      cv ← freshCheckRaw expr expected placeholder
      markBlocking placeholder cv
      pure $ TCheck cv
    _ → do
      -- TODO: carry "reasons" for the expected type to be that.
      (expr', inferred) ← insertApps =<< infer expr
      unifyExpected expected inferred
      pure expr'
 where
  insertImplicitLambda binder vdomain vcodomain = do
    res ← insideClosure vcodomain
    expr' ← bindCtx Nothing vdomain $ check expr res
    domain ← quote vdomain
    pure $ TLambda Implicit binder domain expr'

  unifyExpected ∷ VType → VType → m ()
  unifyExpected expected other = do
    errors ← unify expected other
    unless (null errors) do
      getSpan expr >>= traverse_ \s →
        reportError
          "ExpectedType"
          "Expression does not have the expected type."
          [
            ( s
            , DG.This $
                "This expression was expected to have type "
                  <> show expected
                  <> ", but has type "
                  <> show other
                  <> " instead."
            )
          ]
          $ toList (DG.Note <$> errors)

-- }}}
-- {{{ Unification
unify ∷ ∀ m. (MonadCompile m) ⇒ Value → Value → m (Seq Error.Doc)
unify vl' vr' = do
  vl ← forceMeta vl'
  vr ← forceMeta vr'
  when debugLogs do
    l ← quote vl
    r ← quote vr
    traceM . Text.unpack . Utils.textPretty' . PP.vsep $
      [ "Unifying"
      , PP.indent 2 $ PP.pretty l
      , "and"
      , PP.indent 2 $ PP.pretty r
      , "-----------"
      ]

  case (vl, vr) of
    (VU, VU) → pure mempty
    (VPi icit _ dom codom, VPi icit' _ dom' codom')
      | icit == icit' → do
          res ← unify dom dom'

          vcodom ← insideClosure codom
          vcodom' ← insideClosure codom'
          res' ← bindCtx Nothing dom $ unify vcodom vcodom'

          pure $ res <> res'
      | otherwise → do
          -- TODO: error
          pure . pure . fold $
            [ "Missmatched ∏-type implicitnesses"
            , show icit
            , " and "
            , show icit'
            , "."
            ]
    (VLambda _ _ dom body, VLambda _ _ dom' body') → do
      res ← unify dom dom'
      vbody ← insideClosure body
      vbody' ← insideClosure body'
      res' ← bindCtx Nothing dom $ unify vbody vbody'
      pure $ res <> res'
    (expr, VLambda icit _ domain body) → do
      vv ← createRigid
      res ← evalApp icit expr vv
      vbody ← insideClosure body
      bindCtx Nothing domain $ unify res vbody
    (VLambda icit _ domain body, expr) → do
      vv ← createRigid
      vbody ← insideClosure body
      res ← evalApp icit expr vv
      bindCtx Nothing domain $ unify vbody res
    (VRigid v spine, VRigid v' spine')
      | v == v' → unifySpine spine spine'
      | otherwise → do
          pure . pure . fold $
            [ "Rigid variable missmatch between `"
            , show v
            , "` and `"
            , show v'
            , "`."
            ]
    -- (VFlex m sp , VFlex m' sp'   ) | m == m' -> intersect l m sp sp'
    -- (VFlex m sp , VFlex m' sp'   )           -> flexFlex l m sp m' sp'
    -- (VFlex m sp , t'             ) -> solve l m sp t'
    -- (t          , VFlex m' sp'   ) -> solve l m' sp' t
    _ → do
      l ← quote vl
      r ← quote vr
      pure . pure . PP.vsep $
        [ "Cannot unify types "
        , PP.indent 2 $ PP.pretty l
        , " and "
        , PP.indent 2 $ PP.pretty r
        , "."
        ]

unifySpine ∷ ∀ m. (MonadCompile m) ⇒ Spine → Spine → m (Seq Error.Doc)
unifySpine Seq.Empty Seq.Empty = pure mempty
-- Note: we don't have to compare explicitness here, since we know from the
-- recursive call that the two spines have the same type.
unifySpine ((f, _) :<| rest) ((f', _) :<| rest') = do
  res ← unify f f'
  res' ← unifySpine rest rest'
  pure $ res <> res'
unifySpine l r = do
  pure . pure . fold $
    [ "Spines have different lengths: "
    , show l
    , " and "
    , show r
    , "respectively."
    ]

-- }}}

-- Bookkeeping
-- {{{ Postponed checking

-- | Unify the result of a postponed checking with its placeholder metavariable.
unifyCheckPlaceholder
  ∷ ∀ m
   . (MonadCompile m)
  ⇒ Term
  → MetaVar
  → m (Seq Error.Doc)
unifyCheckPlaceholder term meta =
  lookupMeta meta >>= \case
    -- If the placeholder meta is unsolved, we can solve it efficiently here,
    -- without any possibility of failure.
    Unsolved blocks ty → do
      -- we can simply close the checked term, to get the solution
      closed ← closeTerm term
      solution ← withEnv empty $ eval closed
      solveMeta meta solution ty
      errs ← for (toList blocks) retryCheck
      pure $ fold errs

    -- otherwise we have to do full unification
    Solved solution _ → do
      vterm ← eval term
      rigid ← asks \x → x.rigid
      vsolution ← evalAppPruning solution rigid
      unify vterm vsolution

-- | Try to perform a delayed checking.
retryCheck ∷ ∀ m. (MonadCompile m) ⇒ CheckVar → m (Seq Error.Doc)
retryCheck cv =
  lookupCheck cv >>= \case
    Checked _ → pure mempty
    -- still blocked by another meta
    Unchecked ctx ast vty meta →
      forceMeta vty >>= \case
        VFlex meta' _ → do
          markBlocking meta' cv
          pure mempty
        other → local (const ctx) do
          ast' ← check ast other
          errs ← unifyCheckPlaceholder ast' meta
          solveCheck cv ast'
          pure errs

-- | Unblock and perform all remaining checking problems, assuming each time
--   that no implicit insertion should occur.
checkEverything ∷ ∀ m. (MonadCompile m) ⇒ NodeId → m ()
checkEverything i = do
  when debugLogs . traceM . Text.unpack . Utils.textPretty' . PP.vsep $
    [ "Checking postponned problems"
    , "-----------"
    ]
  errors ← go 0
  getSpan i >>= traverse_ \s → do
    for_ errors \err → do
      reportError
        "DelayedCheckFailed"
        "An error occurred while solving posponed checking problems."
        [(s, DG.This err)]
        []
 where
  go ∷ CheckVar → m (Seq Error.Doc)
  go cv = do
    entries ← Seq.length <$> gets (O.view #checkVars)
    if
      | (coerce cv < entries) → do
          errs ←
            lookupCheck cv >>= \case
              Checked _ → pure mempty
              Unchecked ctx ast vty meta → local (const ctx) do
                (ast', tyast) ← insertAppsNeutral =<< infer ast
                solveCheck cv ast'
                errs ← unify vty tyast
                errs' ← unifyCheckPlaceholder ast' meta
                pure $ errs <> errs'
          errs' ← go (cv + 1)
          pure $ errs <> errs'
      | otherwise → pure mempty

-- }}}
