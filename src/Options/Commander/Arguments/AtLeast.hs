-- Thanks to Aaron Allen for assistence in designing this module
module Options.Commander.Arguments.AtLeast where

import Data.Foldable
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce


type AtLeast n = AL (Nat2NatI n)
data AL :: NatI -> * -> * where
  (:|) :: a -> AL n a -> AL ('S n) a
  Tail :: [a] -> AL 'Z a
infixr 5 :|

type AtLeastTail n a = ALT (Nat2NatI n) a
data ALT :: NatI -> * -> * where
  Head :: [a] -> ALT 'Z a
  (:>) :: ALT n a -> a -> ALT ('S n) a
infixl 5 :>

class FromList f where fromList :: forall a. [a] -> Maybe (f a)

instance FromList (AL n) => FromList (AL ('S n)) where
  fromList = \case
    x:xs -> (x :|) <$> fromList xs
    [] -> Nothing
instance FromList (AL 'Z) where
  fromList = Just . Tail

instance KnownNat (NatI2Nat n) => FromList (ALT n) where
  fromList argAL = if l > minL
    then Just $ appendAL (Head excessHead) minTail
    else Nothing
    where
    l = length argAL
    minL = fromInteger $ natVal $ Proxy @(NatI2Nat n)
    (excessHead,minTail) = splitAt (l - minL) argAL
    appendAL :: forall a l m. ALT l a -> [a] -> ALT m a
    appendAL xs (y:ys) = appendAL (xs :> y) ys
    appendAL xs _      = unsafeCoerce xs

instance FromList [] where fromList = pure

data NatI = S NatI | Z

type family Nat2NatI a where
  Nat2NatI 0 = 'Z
  Nat2NatI n = 'S (Nat2NatI (n-1))

type family NatI2Nat a where
  NatI2Nat 'Z = 0
  NatI2Nat ('S n) = 1 + NatI2Nat n

instance Show a => Show (AL 'Z a) where
  show (Tail xs) = "Tail " <> show xs
instance (Show (AL n a), Show a) => Show (AL ('S n) a) where
  show (x :| xs) = show x <> ":|" <> show xs

-- instance Show (ALT n a) where
--   show x = "ALT " <> (show $ natVal $ Proxy @n) <> " (" <> show (getALT x) <> ")"
instance Show a => Show (ALT 'Z a) where
  show (Head xs) = "Head " <> show xs
instance (Show (ALT n a), Show a) => Show (ALT ('S n) a) where
  show (xs :> x) = show xs <> " :> " <> show x

deriving instance Eq a => Eq (AL n a)

deriving instance Eq a => Eq (ALT n a)

instance Foldable (AL 'Z) where
  foldr f e (Tail xs) = foldr f e xs
  toList (Tail xs) = xs
instance Foldable (AL n) => Foldable (AL ('S n)) where
  foldr f e xs = foldr f e (toList xs)
  toList (x :| xs) = x : toList xs

instance Foldable (ALT 'Z) where
  foldr f e = foldr f e . toList
  toList (Head xs) = xs
instance Foldable (ALT n) => Foldable (ALT ('S n)) where
  foldr f e (xs :> x) = foldr f (f x e) xs

