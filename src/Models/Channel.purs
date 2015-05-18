module Models.Channel where

data Channel = Channel
	{ id :: Number
	, name :: String
	}

unChannel (Channel c) = c

instance eqChannel :: Eq Channel where
	(==) (Channel a) (Channel b) = a.id == b.id
	(/=) (Channel a) (Channel b) = a.id /= b.id
