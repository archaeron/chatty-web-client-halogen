module Models.Input where

data InputType
	= TextInput
	| CodeInput
	| FormulaInput
	| FileInput

instance eqInputType :: Eq InputType where
	(==) TextInput TextInput = true
	(==) CodeInput CodeInput = true
	(==) FormulaInput FormulaInput = true
	(==) FileInput FileInput = true
	(==) _ _ = false
	(/=) a b = not $ a == b
