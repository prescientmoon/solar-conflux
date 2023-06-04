module Term where

import Num (class Add, class Compare, class NAreEqual, class Sub, NProxy, Succ, Zero, five, four, one, three, two, zero, kind Num)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Ordering (GT, kind Ordering)
import Type.Data.Boolean (class And, class Or, BProxy(..))

foreign import kind Term
foreign import data Var :: Num -> Term
foreign import data Abstraction :: Term -> Term
foreign import data Application :: Term -> Term -> Term 

-- Reduce
class Reduce (input :: Term) (output :: Term) | input -> output

instance reduceGeneral :: 
    ( BetaReduce a b 
    , EtaReduce b c 
    ) => Reduce a c

-- Checking if an expression references a variable
class HasReference (term :: Term) (index :: Num) (output :: Boolean) | term index -> output

instance hasReferenceVar :: 
    NAreEqual index var result 
    => HasReference (Var var) index result
else instance hasReferenceCall :: 
    (HasReference left index leftResult
    , HasReference right index rightResult
    , Or leftResult rightResult result
    ) => HasReference (Application left right) index result 
else instance hasReferenceAbstraction :: 
    HasReference term (Succ index) result 
    => HasReference (Abstraction term) index result

-- Shifting
class ShiftTerm 
    (term :: Term) 
    (over :: Num) 
    (direction :: Boolean)
    (amount :: Num)
    (result :: Term) 
    | term over direction amount -> result

instance shiftTermApplication ::
    ( ShiftTerm left over sign amount leftResult
    , ShiftTerm right over sign amount rightResult )
    => ShiftTerm (Application left right) over sign amount (Application leftResult rightResult)
else instance shiftTermAbstraction ::
    ShiftTerm term (Succ over) sign amount result
    => ShiftTerm (Abstraction term) over sign amount (Abstraction result)
else instance shiftTermVar :: 
    ( Compare (Succ index) over ord
    , ShiftVar ord index sign amount result )
    => ShiftTerm (Var index) over sign amount (Var result) 

class ShiftVar 
    (ord :: Ordering) 
    (index :: Num) 
    (sign :: Boolean)
    (amount :: Num) 
    (result :: Num) 
    | index sign amount ord -> result

instance shiftVarGTPositive :: 
    Add index amount result
    => ShiftVar GT index True amount result
else instance shiftVarGTNegative :: 
    Sub index amount result
    => ShiftVar GT index False amount result
else instance shiftVarGeneral :: ShiftVar ord index sign amount index

-- Equality 
class TermsAreEqual (a :: Term) (b :: Term) (result :: Boolean) | a b -> result

instance termsAreEqualVar :: NAreEqual a b result => TermsAreEqual (Var a) (Var b) result
else instance termsAreEqualAbstraction :: TermsAreEqual a b result => TermsAreEqual (Abstraction a) (Abstraction b) result
else instance termsAreEqualApplication :: 
    ( TermsAreEqual left left' leftResult 
    , TermsAreEqual right right' rightResult
    , And leftResult rightResult result )
    => TermsAreEqual (Application left right) (Application left' right') result
else instance termsAreEqualGeneral :: TermsAreEqual a b False

-- Eta reduction
class EtaReduceAbstraction (input :: Term) (hasReference :: Boolean) (output :: Term) | input hasReference -> output

instance etaReduceLambdaFalse :: 
    ( ShiftTerm term Zero False (Succ Zero) result
    , DeepEtaReduce result deepResult )
    => EtaReduceAbstraction term False deepResult
else instance etaReduceLambdaTrue :: 
    DeepEtaReduce term result
    => EtaReduceAbstraction term True (Abstraction (Application result (Var Zero)))

class DeepEtaReduce (input :: Term) (output :: Term) | input -> output

instance deepEtaReduceAbstraction :: 
    ( HasReference term Zero hasReference
    , EtaReduceAbstraction term hasReference result) 
    => DeepEtaReduce (Abstraction (Application term (Var Zero))) result
else instance deepEtaReduceApplication :: 
    ( DeepEtaReduce left leftResult
    , DeepEtaReduce right rightResult) 
    => DeepEtaReduce (Application left right) (Application leftResult rightResult) 
else instance deepEtaReduceAbstraction' :: 
    DeepEtaReduce term result
    => DeepEtaReduce (Abstraction term) (Abstraction result)
else instance deepEtaReduceGeneral :: DeepEtaReduce a a

class EtaReduce (input :: Term) (output :: Term) | input -> output

instance etaReduceGeneral :: 
    ( DeepEtaReduce input reduced
    , TermsAreEqual input reduced equal
    , EtaReduceRec reduced equal result )
    => EtaReduce input result

class EtaReduceRec (reduced :: Term) (equal :: Boolean) (output :: Term) | reduced equal -> output

instance etaReduceTrue :: EtaReduceRec reduced True reduced
else instance etaReduceFalse :: EtaReduce reduced result => EtaReduceRec reduced False result

-- Conditions
class TermIf (bool :: Boolean)
    (onTrue :: Term)
    (onFalse :: Term)
    (output :: Term) 
    | bool onTrue onFalse -> output

instance ifTrue :: TermIf True onTrue onFalse onTrue
instance ifFalse :: TermIf False onTrue onFalse onFalse

-- Substitution 
class Substitute (index :: Num) (inside :: Term) (with :: Term) (result :: Term) | index inside with -> result

instance substituteVar :: 
    ( NAreEqual var index equal
    , TermIf equal with (Var var) result)
    => Substitute index (Var var) with result 
else instance substituteCall :: 
    ( Substitute index left with leftResult 
    , Substitute index right with rightResult 
    ) => Substitute index (Application left right) with (Application leftResult rightResult)
else instance subsituteAbstraction ::
    ( ShiftTerm with Zero True (Succ Zero) shifted 
    , Substitute (Succ index) term shifted result
    ) => Substitute index (Abstraction term) with (Abstraction result)

-- Beta reduction
class DeepBetaReduce (term :: Term) (result :: Term) | term -> result

instance deepBetaReduceAbstraction :: DeepBetaReduce term result => DeepBetaReduce (Abstraction term) (Abstraction result)
else instance deepBetaReudctionApplication ::
    ( DeepBetaReduce left leftReduced
    , DeepBetaReduce right rightReduced
    , ShiftTerm rightReduced Zero True (Succ Zero) rightShifted
    , Substitute Zero leftReduced rightShifted result
    , ShiftTerm result Zero False (Succ Zero) result'
    -- , DeepBetaReduce result' result''
    ) => DeepBetaReduce (Application (Abstraction left) right) result'
else instance deepBetaReduceApplication :: 
    ( DeepBetaReduce left leftResult 
    , DeepBetaReduce right rightResult 
    ) => DeepBetaReduce (Application left right) (Application leftResult rightResult)
else instance deepBetaReduceGeneral :: DeepBetaReduce a a

class BetaReduce (input :: Term) (output :: Term) | input -> output

instance betaReduceGeneral :: 
    ( DeepBetaReduce input reduced
    , TermsAreEqual input reduced equal
    , BetaReduceRec reduced equal result 
    ) => BetaReduce input result

class BetaReduceRec (reduced :: Term) (equal :: Boolean) (output :: Term) | reduced equal -> output

instance betaReduceTrue :: BetaReduceRec reduced True reduced
else instance betaReduceFalse :: BetaReduce reduced result => BetaReduceRec reduced False result

-- Church numerals
class ChurchNumeral (input :: Num) (output :: Term) | input -> output

instance churchNumeralGeneral :: 
    ChurchNumeralBody x result 
    => ChurchNumeral x (Abstraction (Abstraction result))

class ChurchNumeralBody (input :: Num) (output :: Term) | input -> output

instance churchNumeralZero :: ChurchNumeralBody Zero (Var Zero)
else instance churchNumeralSucc ::
    ChurchNumeralBody x result
    => ChurchNumeralBody (Succ x) (Application (Var (Succ Zero)) result)

-- Helpers
data TProxy (a :: Term) = TProxy

var :: forall a. NProxy a -> TProxy (Var a)
var _ = TProxy

abstract :: forall a. TProxy a -> TProxy (Abstraction a)
abstract _ = TProxy

application :: forall a b. TProxy a -> TProxy b -> TProxy (Application a b)
application _ _ = TProxy

infixl 1 application as -$-

hasReference :: forall term index result. 
    HasReference term index result => 
    TProxy term -> 
    NProxy index -> 
    BProxy result
hasReference _ _ = BProxy

etaReduce :: forall input output. EtaReduce input output => TProxy input -> TProxy output
etaReduce _ = TProxy

betaReduce :: forall input output. BetaReduce input output => TProxy input -> TProxy output
betaReduce _ = TProxy

reduce :: forall input output. Reduce input output => TProxy input -> TProxy output
reduce _ = TProxy

shiftVar :: forall term over amount result sign. 
    ShiftTerm term over sign amount result =>
    TProxy term ->
    NProxy over ->
    BProxy sign ->
    NProxy amount ->
    TProxy result
shiftVar _ _ _ _ = TProxy

churchNumeral :: forall int result. ChurchNumeral int result => NProxy int -> TProxy result
churchNumeral _ = TProxy

areEqual :: forall a b r. TermsAreEqual a b r => TProxy a -> TProxy b -> BProxy r
areEqual _ _ = BProxy 

-- Tests
referenceTest :: BProxy True
referenceTest = hasReference term index
    where
    term = abstract (abstract (application (var one) (var two)))
    index = zero

etaTest :: TProxy (Abstraction (Application (Var Zero) (Var (Succ Zero))))
etaTest = etaReduce expression
    where
    -- Equivalent of (\a b. (\b. b c) a b)
    expression = abstract (abstract (application (application (abstract (application (var zero) (var three))) (var one)) (var zero)))


churchZero :: TProxy (Abstraction (Abstraction (Var Zero)))
churchZero = churchNumeral zero

churchOne :: TProxy (Abstraction (Abstraction (Application (Var (Succ Zero)) (Var Zero))))
churchOne = churchNumeral one

churchThree :: TProxy (Abstraction (Abstraction (Application (Var (Succ Zero)) (Application (Var (Succ Zero)) (Application (Var (Succ Zero)) (Var Zero))))))
churchThree = churchNumeral three

churchAdd :: TProxy (Abstraction (Abstraction (Abstraction (Abstraction (Application (Application (Var (Succ (Succ (Succ Zero)))) (Var (Succ Zero))) (Application (Application (Var (Succ (Succ Zero))) (Var (Succ Zero))) (Var Zero)))))))
churchAdd = abstract (abstract (abstract (abstract body)))
    where
    body :: TProxy _
    body = (var three) -$- (var one) -$- ((var two) -$- (var one) -$- (var zero))

churchFiveUnReduced :: TProxy (Application (Application (Abstraction (Abstraction (Abstraction (Abstraction (Application (Application (Var (Succ (Succ (Succ Zero)))) (Var (Succ Zero))) (Application (Application (Var (Succ (Succ Zero))) (Var (Succ Zero))) (Var Zero))))))) (Abstraction (Abstraction (Application (Var (Succ Zero)) (Application (Var (Succ Zero)) (Application (Var (Succ Zero)) (Var Zero))))))) (Abstraction (Abstraction (Application (Var (Succ Zero)) (Var Zero)))))
churchFiveUnReduced = churchAdd -$- churchThree -$- churchOne

termConst :: TProxy (Abstraction (Abstraction (Var (Succ Zero))))
termConst = abstract (abstract (var one))

termIdentity :: TProxy (Abstraction (Var Zero))
termIdentity = abstract (var zero)

addTest :: BProxy True
addTest = areEqual fourRaw fourComputed
    where
    fourRaw = churchNumeral four 
    fourComputed = reduce (churchAdd -$- churchThree -$- churchOne) 

addTest' :: BProxy False
addTest' = areEqual fiveRaw fourComputed
    where
    fiveRaw = churchNumeral five 
    fourComputed = reduce (churchAdd -$- churchThree -$- churchOne) 

reductionTest :: TProxy (Abstraction (Abstraction (Var Zero)))
reductionTest = reduce (f -$- arg -$- arg)
    where
    f = termConst -$- termIdentity
    arg = churchZero

-- infinite :: _
-- infinite = reduce (abstract (t -$- t))
--     where
--     t = abstract (var one -$- (var zero -$- var zero))
