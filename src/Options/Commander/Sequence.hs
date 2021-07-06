module Options.Commander.Sequence where


-- | The type level program sequencing combinator, taking two program types
-- and sequencing them one after another.
data (&) :: k -> * -> *
infixr 4 &

