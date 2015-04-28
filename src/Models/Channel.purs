module Models.Channel where

data Channel = Channel
	{ name :: String
	}

unChannel (Channel c) = c

instance eqChannel :: Eq Channel where
	(==) (Channel a) (Channel b) = a.name == b.name
	(/=) (Channel a) (Channel b) = a.name /= b.name
