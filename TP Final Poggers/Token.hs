module Token where

data Token =
    TokenPrint       |
    TokenAss         |
    TokenSemi        |
	TokenLeftBr  	 |
	TokenRightBr  	 |
	TokenLeftParen 	 |
	TokenRightParen	 |
	TokenComma     	 |
	TokenPlus     	 |
	TokenMinus     	 |
	TokenTimes     	 |
	TokenDiv     	 |
	TokenId String	 |
	TokenInt Int     |
    TokenBool Bool
    deriving (Eq,Show)
