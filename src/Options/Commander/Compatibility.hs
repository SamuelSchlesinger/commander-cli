{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Options.Commander.Compatibility {-# DEPRECATED "Switch to use Alternate" #-} where

import Options.Commander (Alternate(Alternate,Primary))


-- | Compatibility type for the old Unrender Either instance. Use altEither to convert AltEither to Either.
type AltEither a b = Alternate b a

-- | Compatibility type for the old Unrender Either instance.
altEither :: forall a b. AltEither a b -> Either a b
altEither = \case
  Alternate x -> Right x
  Primary   x -> Left  x

