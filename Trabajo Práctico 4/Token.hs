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
    	TokenOperAS Char |
    	TokenOperMD Char |
    	TokenId String	 |
    	TokenInt Int     |
        TokenBool Bool
        deriving (Eq,Show)
