module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Reader (ask, local, runReader)
import Control.Plus (empty)
import Data.Array (reverse)
import Data.Array as Array
import Data.Array.NonEmpty (some)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (foldl, foldlDefault)
import Data.Foldable as Foldable
import Data.Functor.Mu (Mu(..))
import Data.HashMap as HashMap
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.String (joinWith)
import Data.Traversable (class Foldable, class Traversable, foldrDefault, traverseDefault)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, launchAff_, makeAff)
import Effect.Class.Console (logShow)
import Effect.Ref as Ref
import Matryoshka (class Corecursive, cata, embed, project)
import Node.Encoding (Encoding(..))
import Node.Process (stdin)
import Node.Stream (Readable, onDataString, onEnd, onError, pause)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, LanguageDef, alphaNum, letter, makeTokenParser)

readText :: forall w. Readable w -> Aff (Maybe String)
readText r = makeAff $ \res -> do
  dataRef <- Ref.new ""
  onDataString r UTF8 \chunk ->
    Ref.modify_ (_ <> chunk) dataRef
  onEnd r do
    allData <- Ref.read dataRef
    res $ Right (Just allData)
  onError r $ Left >>> res
  pure $ effectCanceler (pause r)

main :: Effect Unit
main = launchAff_ do
  text <- fromMaybe " " <$> readText stdin
  case parseLambdaCalculus text of
    Left err -> logShow err
    Right ast -> logShow $ project $ reduce $ introduceDeBrujinIndices ast


---------- Types
data AstF v f
  = Call f f
  | Lambda String f
  | Var v

type Ast v = Mu (AstF v)

data DeBrujin 
  = Bound Int String
  | Free String

derive instance functorAst :: Functor (AstF v)

instance foldableAst :: Foldable (AstF v) where
  foldMap f = case _ of
    Call function argument -> f function <> f argument
    Var _ -> mempty
    Lambda _ body -> f body

  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableAst :: Traversable (AstF v) where
  sequence = case _ of
    Call f a -> Call <$> f <*> a
    Lambda name body -> Lambda name <$> body
    Var v -> pure $ Var v

  traverse = traverseDefault

instance showAst :: Show (AstF DeBrujin (Ast DeBrujin)) where
  show :: AstF DeBrujin (Ast DeBrujin) -> String
  show = case _ of
    Var (Free name) -> name
    Var (Bound _ name) -> name
    Call function argument 
      -> withParensIf (project function) needsParensLeft 
      <> " "
      <> withParensIf (project argument) needsParensRight
    Lambda name body -> showLambdas (name : Nil) (project body)
    where
    showLambdas names (Lambda name body) = showLambdas (name:names) (project body)
    showLambdas Nil body = show body
    showLambdas names body 
      = "\\" 
      <> joinWith " " (reverse $ Array.fromFoldable names) 
      <> ". " 
      <> show body 

    withParensIf term predicate = if predicate term
      then "(" <> show term <> ")" 
      else show term

    needsParensLeft = case _ of
      Lambda _ _ -> true
      _ -> false

    needsParensRight = case _ of
      Call _ _ -> true
      Lambda _ _ -> true
      _ -> false

data Error
  = NotInScope Int String

---------- Constructors
call :: forall t v. Corecursive t (AstF v) => t -> t -> t
call f a = embed (Call f a)

var :: forall t v. Corecursive t (AstF v) => v -> t
var = Var >>> embed

lambda :: forall t v. Corecursive t (AstF v) => String -> t -> t
lambda l t = embed (Lambda l t)

---------- Reduction
introduceDeBrujinIndices :: Ast String -> Ast DeBrujin
introduceDeBrujinIndices i = cata go i # flip runReader HashMap.empty 
  where
  go = case _ of
    Var name -> ado
      indices <- ask
      in var $ maybe (Free name) (\index -> Bound index name) $ HashMap.lookup name indices
    Call func arg -> call <$> func <*> arg
    Lambda name body -> lambda name <$> local updateContext body
      where
      updateContext m = ((+) one <$> m) `HashMap.union` HashMap.singleton name zero

betaReduction :: Ast DeBrujin -> Ast DeBrujin
betaReduction = cata case _ of
  Var v -> var v
  Call function argument -> do
    case project function of
      Lambda _ body -> betaReduction $ unshift $ substitute 0 (shift argument) body
      _ -> call function argument
  Lambda name body -> lambda name body

etaReduction :: Ast DeBrujin -> Ast DeBrujin 
etaReduction = cata case _ of
  Var v -> var v
  Call f a -> call f a 
  Lambda _ (In (Call term (In (Var (Bound 0 _))))) | not (occurs 0 term) -> unshift term
  Lambda name l -> lambda name l

substitute :: Int -> Ast DeBrujin -> Ast DeBrujin -> Ast DeBrujin
substitute at with within = cata go within # flip runReader (at /\ with) 
  where
  go = case _ of
    Call f a -> call <$> f <*> a
    Var (Bound index name) -> ado
      at' /\ with'  <- ask
      in if index == at' then with'  else var (Bound index name)
    Var v -> pure $ var v
    Lambda name body -> lambda name <$> local updateContext body 
      where updateContext (at' /\ with') = (at' + 1) /\ shift with' 

shiftBy :: Int -> Ast DeBrujin -> Ast DeBrujin
shiftBy amount term = cata go term 0 
  where
  go = case _ of
    Var (Bound index name) -> \over -> var $ flip Bound name if index < over then index else index + amount 
    Var v -> const $ var v
    Call f a -> \over -> call (f over) (a over)
    Lambda name body -> \over -> lambda name $ body (over + 1)
  

shift :: Ast DeBrujin -> Ast DeBrujin
shift = shiftBy 1

unshift :: Ast DeBrujin -> Ast DeBrujin
unshift = shiftBy (-1)

occurs :: Int -> Ast DeBrujin -> Boolean
occurs = flip (cata go)
  where
  go = case _ of
    Var (Bound index _) -> (==) index
    Call f a -> f || a
    Lambda _ body -> (+) 1 >>> body
    _ -> const false

reduce :: Ast DeBrujin -> Ast DeBrujin
reduce = betaReduction >>> etaReduction

---------- Parser
-- | Punctuation to start the declaration of a lambda expression.
lambdaStarts :: Array String
lambdaStarts = [ "\\", "λ" ]

-- | Punctuation to start the declaration of the body of a lambda expression.
lambdaBodyStarts :: Array String
lambdaBodyStarts = [ "->", "." ]

-- | Declaration for lambda calculus (with comments).
-- | This is used to generate the lexer
language :: LanguageDef
language =
  LanguageDef
    { commentStart: "{-"
    , commentEnd: "-}"
    , commentLine: "--"
    , nestedComments: true
    , opStart: empty
    , opLetter: empty
    , caseSensitive: true
    , reservedOpNames: lambdaStarts <> lambdaBodyStarts
    , reservedNames: []
    , identStart: letter
    , identLetter: alphaNum <|> oneOf [ '_', '\'' ]
    }

-- | The lexer
tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser language

-- | Parsers which run in the identity monad and parse strings
type LambdaParser r
  = Parser String r

-- | Parser for individual lambda calculus expressions.
-- | This references itself so we use it within a fixpoint operator
expression' :: LambdaParser (Ast String) -> LambdaParser (Ast String)
expression' expr = do
  -- expression'' <- atom
  (NonEmpty expression'' args) <- NonEmptyArray.toNonEmpty <$> some atom
  pure $ foldl call expression'' args
  where
  { parens, identifier, reservedOp } = tokenParser

  atom :: LambdaParser (Ast String)
  atom = wrapped <|> lambdaExpr <|> variable

  wrapped :: LambdaParser (Ast String)
  wrapped = parens expr

  variable :: LambdaParser (Ast String)
  variable = var <$> identifier

  lambdaExpr :: LambdaParser (Ast String)
  lambdaExpr = do
    Foldable.oneOf $ reservedOp <$> lambdaStarts
    (NonEmpty arg args) <- NonEmptyArray.toNonEmpty <$> NonEmptyArray.reverse <$> some identifier
    Foldable.oneOf $ reservedOp <$> lambdaBodyStarts
    body <- expr
    let
      baseAst = lambda arg body
    pure $ foldl (flip lambda) baseAst args

-- | Parser for lambda calculus.
expression :: LambdaParser (Ast String)
expression = fix expression'

-- | Try parsing a string into a lambda calculus ast.
parseLambdaCalculus :: String -> Either ParseError (Ast String)
parseLambdaCalculus = flip runParser expression

