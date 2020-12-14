module Options.Commander.Unrender where

import Data.Word
import Numeric.Natural
import Data.Typeable (Typeable)
import Data.Int
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack, unpack, find)
import Data.Text.Read (decimal, signed)
import Control.Applicative (Alternative((<|>)))


-- | A class for interpreting command line arguments into Haskell types.
class Typeable t => Unrender t where
  unrender :: Text -> Maybe t

instance Unrender String where
  unrender = Just . unpack

instance Unrender Text where
  unrender = Just

instance Unrender SBS.ByteString where
  unrender = Just . BS8.pack . unpack

instance Unrender LBS.ByteString where
  unrender = fmap LBS.fromStrict <$> unrender

-- | A useful default unrender for small, bounded data types.
unrenderSmall :: (Enum a, Bounded a, Show a) => Text -> Maybe a
unrenderSmall = flip Prelude.lookup [(pack $ show x, x) | x <- [minBound..maxBound]]

instance Unrender () where
  unrender = unrenderSmall

instance (Unrender a, Unrender b) => Unrender (Either a b) where
  unrender x = leftCase x <|> rightCase x where
    leftCase  = fmap Left  . unrender
    rightCase = fmap Right . unrender

instance Unrender Bool where
  unrender = unrenderSmall

newtype WrappedIntegral i = WrappedIntegral i
  deriving newtype (Num, Real, Ord, Eq, Enum, Integral)

instance (Typeable i, Integral i) => Unrender (WrappedIntegral i) where
  unrender = either (const Nothing) h . signed decimal where
    h (n, "") = Just (fromInteger n)
    h _ = Nothing

deriving via WrappedIntegral Integer instance Unrender Integer
deriving via WrappedIntegral Int instance Unrender Int
deriving via WrappedIntegral Int8 instance Unrender Int8
deriving via WrappedIntegral Int16 instance Unrender Int16
deriving via WrappedIntegral Int32 instance Unrender Int32
deriving via WrappedIntegral Int64 instance Unrender Int64

newtype WrappedNatural i = WrappedNatural i
  deriving newtype (Num, Real, Ord, Eq, Enum, Integral)

instance (Typeable i, Integral i) => Unrender (WrappedNatural i) where
  unrender = either (const Nothing) h . decimal where
    h (n, "") = if n >= 0 then Just (fromInteger n) else Nothing
    h _ = Nothing 

deriving via WrappedNatural Natural instance Unrender Natural
deriving via WrappedNatural Word instance Unrender Word
deriving via WrappedNatural Word8 instance Unrender Word8
deriving via WrappedNatural Word16 instance Unrender Word16
deriving via WrappedNatural Word32 instance Unrender Word32
deriving via WrappedNatural Word64 instance Unrender Word64

instance Unrender Char where
  unrender = find (const True)


