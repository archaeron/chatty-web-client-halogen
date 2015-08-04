module Models.Channel where

import Prelude (Eq, eq)

data Channel = Channel
	{ id :: Int
	, name :: String
	}

unChannel (Channel c) = c

instance eqChannel :: Eq Channel where
	eq (Channel a) (Channel b) = a.id `eq` b.id
