module Options.Commander.Internal where

import Data.Proxy (Proxy(Proxy))
import Data.String (IsString(fromString))
import Type.Reflection (Typeable, typeRep)
-- import Type.Reflection (Typeable, typeRep, TypeRep, TyCon, tyConName, splitApps, someTypeRepTyCon)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.List.NonEmpty (NonEmpty, cons)
import Data.Foldable (toList)
-- import qualified Data.Maybe
-- import Language.Haskell.TH (TypeQ, conT, mkName, appT, litT, strTyLit, promotedConsT)


showSymbol :: forall (a :: Symbol) b. (KnownSymbol a, IsString b) => b
showSymbol = fromString $ symbolVal $ Proxy @a

showTypeRep :: forall a b. (Typeable a, IsString b) => b
showTypeRep = fromString $ show $ typeRep @a

-- symbolType :: String -> TypeQ
-- symbolType = litT . strTyLit
-- 
-- fromTypeable :: forall a . Typeable a => TypeQ
-- fromTypeable = fromTypeRep $ typeRep @a
--   where
--   fromTypeRep :: forall b. TypeRep b -> TypeQ
--   fromTypeRep = splitApps >>> \(x,xs) -> foldl appT (fromTypeCon x) $ fmap (fromTypeCon . someTypeRepTyCon) xs
--   fromTypeCon :: TyCon -> TypeQ
--   fromTypeCon = conT . mkName . tyConName

class SymbolList a where
  symbolList :: forall b. IsString b => NonEmpty b
instance (KnownSymbol s, SymbolList (s' ': ss)) => SymbolList (s ': s' ': ss) where
  symbolList = showSymbol @s `cons` symbolList @(s' ': ss)
instance KnownSymbol s => SymbolList '[s] where
  symbolList = pure $ showSymbol @s

-- altMay :: (Monad m, Foldable f) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
-- altMay f = g . toList
--   where
--   g = \case
--     x:xs -> f x >>= \case
--       Nothing -> g xs
--       y -> pure y
--     [] -> pure Nothing

intercalate :: forall f a. (Foldable f, Monoid a) => a -> f a -> a
intercalate x = f . toList
  where
  f = \case
    y:z:ys -> f $ y <> x <> z : ys
    [y] -> y
    [] -> mempty

-- infixr 8 >>>
-- (>>>) :: (a -> b) -> (b -> c) -> a -> c
-- (>>>) = flip (.)
-- 
-- catMaybes :: Foldable f => f (Maybe a) -> [a]
-- catMaybes = Data.Maybe.catMaybes . toList
-- 
-- promotedSymbolList :: Foldable f => f TypeQ -> TypeQ
-- promotedSymbolList = foldr (\x y -> promotedConsT `appT` x `appT` y) [t|('[] :: [Symbol])|]
-- 
-- class MaybeSymbol a where maybeSymbol :: IsString b => Maybe b
-- instance KnownSymbol a => MaybeSymbol ('Just a) where maybeSymbol = Just $ showSymbol @a
-- instance MaybeSymbol 'Nothing where maybeSymbol = Nothing

