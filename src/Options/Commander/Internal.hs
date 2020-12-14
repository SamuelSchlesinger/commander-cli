{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Options.Commander.Internal where

import Data.Proxy (Proxy(Proxy))
import Data.String (IsString(fromString))
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.List.NonEmpty (NonEmpty, cons)
import Data.Foldable (toList)
import qualified Data.Maybe


showSymbol :: forall (a :: Symbol) b. (KnownSymbol a, IsString b) => b
showSymbol = fromString $ symbolVal $ Proxy @a

showTypeRep :: forall a b. (Typeable a, IsString b) => b
showTypeRep = fromString $ show $ typeRep $ Proxy @a

class SymbolList a where
  symbolList :: forall b. IsString b => NonEmpty b
instance (KnownSymbol s, SymbolList (s' ': ss)) => SymbolList (s ': s' ': ss) where
  symbolList = showSymbol @s `cons` symbolList @(s' ': ss)
instance KnownSymbol s => SymbolList '[s] where
  symbolList = pure $ showSymbol @s

altMay :: (Monad m, Foldable f) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
altMay f = g . toList
  where
  g = \case
    x:xs -> f x >>= \case
      Nothing -> g xs
      y -> pure y
    [] -> pure Nothing

intercalate :: forall f a. (Foldable f, Monoid a) => a -> f a -> a
intercalate x = f . toList
  where
  f = \case
    y:z:ys -> f $ y <> x <> z : ys
    [y] -> y
    [] -> mempty

infixr 8 >>>
(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

catMaybes :: Foldable f => f (Maybe a) -> [a]
catMaybes = Data.Maybe.catMaybes . toList

