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


showSymbol :: forall (a :: Symbol) b. (KnownSymbol a, IsString b) => b
showSymbol = fromString $ symbolVal $ Proxy @a

showTypeRep :: forall a b. (Typeable a, IsString b) => b
showTypeRep = fromString $ show $ typeRep $ Proxy @a

class SymbolList a where symbolList :: forall b. IsString b => [b]
instance (KnownSymbol s, SymbolList ss) => SymbolList (s ': ss) where symbolList = showSymbol @s : symbolList @ss
instance SymbolList '[] where symbolList = []

