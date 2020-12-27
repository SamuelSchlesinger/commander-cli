-- Thanks to Aaron Allen for assistence in designing this module
module Options.Commander.Arguments.AtLeast where

import Data.Foldable
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce


type AtLeast n = AL [] (Nat2NatI n)
type Exact n = AL Proxy (Nat2NatI n)
data AL :: (* -> *) -> NatI -> * -> * where
  (:<) :: a -> AL f n a -> AL f ('S n) a
  Tail :: f a -> AL f 'Z a
infixr 5 :<

type AtLeastTail n = ALT [] (Nat2NatI n)
-- type ExactTail n = ALT Proxy (Nat2NatI n)
data ALT :: (* -> *) -> NatI -> * -> * where
  Head :: [a] -> ALT f 'Z a
  (:>) :: ALT f n a -> a -> ALT f ('S n) a
infixl 5 :>

class FromList f where fromList :: forall a. [a] -> Maybe (f a)

instance (FromList f, FromList (AL f n)) => FromList (AL f ('S n)) where
  fromList = \case
    x:xs -> (x :<) <$> fromList xs
    [] -> Nothing
instance FromList f => FromList (AL f 'Z) where
  fromList = fmap Tail . fromList

instance KnownNat (NatI2Nat n) => FromList (ALT f n) where
  fromList argAL = if l > minL
    then Just $ appendAL (Head excessHead) minTail
    else Nothing
    where
    l = length argAL
    minL = fromInteger $ natVal $ Proxy @(NatI2Nat n)
    (excessHead,minTail) = splitAt (l - minL) argAL
    appendAL :: forall a l m. ALT f l a -> [a] -> ALT f m a
    appendAL xs (y:ys) = appendAL (xs :> y) ys
    appendAL xs _      = unsafeCoerce xs

instance FromList [] where fromList = pure

instance FromList Proxy where fromList = const $ Just Proxy

data NatI = S NatI | Z

type family Nat2NatI a where
  Nat2NatI 0 = 'Z
  Nat2NatI n = 'S (Nat2NatI (n-1))

type family NatI2Nat a where
  NatI2Nat 'Z = 0
  NatI2Nat ('S n) = 1 + NatI2Nat n

instance (Show a, Show (f a)) => Show (AL f 'Z a) where
  show (Tail xs) = "Tail " <> show xs
instance (Show (AL f n a), Show a) => Show (AL f ('S n) a) where
  show (x :< xs) = show x <> " :< " <> show xs

instance Show a => Show (ALT f 'Z a) where
  show (Head xs) = "Head " <> show xs
instance (Show (ALT f n a), Show a) => Show (ALT f ('S n) a) where
  show (xs :> x) = show xs <> " :> " <> show x

deriving instance (Eq a, Eq (f a)) => Eq (AL f n a)

deriving instance Eq a => Eq (ALT f n a)

instance Foldable f => Foldable (AL f 'Z) where
  foldr f e (Tail xs) = foldr f e xs
  toList (Tail xs) = toList xs
instance Foldable (AL f n) => Foldable (AL f ('S n)) where
  foldr f e xs = foldr f e (toList xs)
  toList (x :< xs) = x : toList xs

instance Foldable (ALT f 'Z) where
  foldr f e = foldr f e . toList
  toList (Head xs) = toList xs
instance Foldable (ALT f n) => Foldable (ALT f ('S n)) where
  foldr f e (xs :> x) = foldr f (f x e) xs

instance Functor f => Functor (AL f 'Z) where
  fmap f (Tail x) = Tail $ fmap f x
instance Functor (AL f n) => Functor (AL f ('S n)) where
  fmap f (x :< y) = f x :< fmap f y

instance Functor f => Functor (ALT f 'Z) where
  fmap f (Head x) = Head $ fmap f x
instance Functor (ALT f n) => Functor (ALT f ('S n)) where
  fmap f (x :> y) = fmap f x :> f y

instance Traversable f => Traversable (AL f 'Z) where
  traverse f (Tail x) = Tail <$> traverse f x
instance Traversable (AL f n) => Traversable (AL f ('S n)) where
  traverse f (x :< xs) = (:<) <$> f x <*> traverse f xs

instance Traversable f => Traversable (ALT f 'Z) where
  traverse f (Head x) = Head <$> traverse f x
instance Traversable (ALT f n) => Traversable (ALT f ('S n)) where
  traverse f (xs :> x) = (:>) <$> traverse f xs <*> f x

