-- Thanks to Aaron Allen for assistence in the design of Nat and AtLeast
module Options.Commander.Arguments.NonEmpty where

import Data.Foldable
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce


newtype NonEmpty n a = NonEmpty {getNonEmpty :: Actual (NonEmpty n) a}
type instance Actual (NonEmpty n) = AtLeast (Nat2NatI n)

newtype NonEmptyTail n a = NonEmptyTail {getNonEmptyTail :: Actual (NonEmptyTail n) a}
type instance Actual (NonEmptyTail n) = AtLeastTail (Nat2NatI n)

type instance Actual [] = []

type family Actual (a :: * -> *) :: (* -> *)

data AtLeast :: NatI -> * -> * where
  (:|) :: a -> AtLeast n a -> AtLeast ('S n) a
  Tail :: [a] -> AtLeast 'Z a
infixr 5 :|

data AtLeastTail :: NatI -> * -> * where
  Head :: [a] -> AtLeastTail 'Z a
  (:>) :: AtLeastTail n a -> a -> AtLeastTail ('S n) a
infixl 5 :>

class FromList f where fromList :: forall a. [a] -> Maybe (f a)

deriving instance FromList (Actual (NonEmpty n)) => FromList (NonEmpty n)
instance FromList (AtLeast n) => FromList (AtLeast ('S n)) where
  fromList = \case
    x:xs -> (x :|) <$> fromList xs
    [] -> Nothing
instance FromList (AtLeast 'Z) where
  fromList = Just . Tail

deriving instance FromList (Actual (NonEmptyTail n)) => FromList (NonEmptyTail n)
instance KnownNat (NatI2Nat n) => FromList (AtLeastTail n) where
  fromList argNonEmpty = if l > minL
    then Just $ appendNonEmpty (Head excessHead) minTail
    else Nothing
    where
    l = length argNonEmpty
    minL = fromInteger $ natVal $ Proxy @(NatI2Nat n)
    (excessHead,minTail) = splitAt (l - minL) argNonEmpty
    appendNonEmpty :: forall a l m. AtLeastTail l a -> [a] -> AtLeastTail m a
    appendNonEmpty xs (y:ys) = appendNonEmpty (xs :> y) ys
    appendNonEmpty xs _      = unsafeCoerce xs

instance FromList [] where fromList = pure

data NatI = S NatI | Z

type family Nat2NatI a where
  Nat2NatI 0 = 'Z
  Nat2NatI n = 'S (Nat2NatI (n-1))

type family NatI2Nat a where
  NatI2Nat 'Z = 0
  NatI2Nat ('S n) = 1 + NatI2Nat n

instance (Show (Actual (NonEmpty n) a), KnownNat n) => Show (NonEmpty n a) where
  show x = "NonEmpty " <> (show $ natVal $ Proxy @n) <> " (" <> show (getNonEmpty x) <> ")"
instance Show a => Show (AtLeast 'Z a) where
  show (Tail xs) = "Tail " <> show xs
instance (Show (AtLeast n a), Show a) => Show (AtLeast ('S n) a) where
  show (x :| xs) = show x <> ":|" <> show xs

instance (Show (Actual (NonEmptyTail n) a), KnownNat n) => Show (NonEmptyTail n a) where
  show x = "NonEmptyTail " <> (show $ natVal $ Proxy @n) <> " (" <> show (getNonEmptyTail x) <> ")"
instance Show a => Show (AtLeastTail 'Z a) where
  show (Head xs) = "Head " <> show xs
instance (Show (AtLeastTail n a), Show a) => Show (AtLeastTail ('S n) a) where
  show (xs :> x) = show xs <> " :> " <> show x

deriving instance Eq (Actual (NonEmpty n) a) => Eq (NonEmpty n a)
deriving instance Eq a => Eq (AtLeast n a)

deriving instance Eq (Actual (NonEmptyTail n) a) => Eq (NonEmptyTail n a)
deriving instance Eq a => Eq (AtLeastTail n a)

deriving instance Foldable (Actual (NonEmpty n)) => Foldable (NonEmpty n)
instance Foldable (AtLeast 'Z) where
  foldr f e (Tail xs) = foldr f e xs
  toList (Tail xs) = xs
instance Foldable (AtLeast n) => Foldable (AtLeast ('S n)) where
  foldr f e xs = foldr f e (toList xs)
  toList (x :| xs) = x : toList xs

deriving instance Foldable (Actual (NonEmptyTail n)) => Foldable (NonEmptyTail n)
instance Foldable (AtLeastTail 'Z) where
  foldr f e = foldr f e . toList
  toList (Head xs) = xs
instance Foldable (AtLeastTail n) => Foldable (AtLeastTail ('S n)) where
  foldr f e (xs :> x) = foldr f (f x e) xs

