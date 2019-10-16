{
module MyLexer where

import Token
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  "print"               { \s -> TokenPrint}
  $digit+				{ \s -> TokenInt (read s) }
  ":="				    { \s -> TokenAss }
  \;				    { \s -> TokenSemi }
  \{                    { \s -> TokenLeftBr}
  \}                    { \s -> TokenRightBr}
  \(                    { \s -> TokenLeftParen}
  \)                    { \s -> TokenRightParen}
  \,                    { \s -> TokenComma}
  \+                    { \s -> TokenPlus }
  \-                    { \s -> TokenMinus }
  \*                    { \s -> TokenTimes }
  \/                    { \s -> TokenDiv }
  $alpha [$alpha $digit \_ \']*		{ \s -> TokenId s }

{
-- Each action has type :: String -> Token

lexer = alexScanTokens
}
