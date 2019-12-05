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
  "print"               { \s -> TokenPrint }
  "set"                 { \s -> TokenTSet }
  "int"                 { \s -> TokenTInt }
  "bool"                { \s -> TokenTBool }
  "true"                { \s -> TokenTrue }
  "false"               { \s -> TokenFalse }
  "and"                 { \s -> TokenAnd }
  "or"                  { \s -> TokenOr }
  "subset"              { \s -> TokenSubset }
  "subsetEq"            { \s -> TokenSubsetEq }
  "in"                  { \s -> TokenIn }
  "first"               { \s -> TokenFirst }
  "second"              { \s -> TokenSecond }
  $digit+				{ \s -> TokenInt (read s) }
  ":="				    { \s -> TokenAss }
  \;				    { \s -> TokenSemi }
  \[                    { \s -> TokenLBr }
  \]                    { \s -> TokenRBr }
  \{                    { \s -> TokenLCurlyBr }
  \}                    { \s -> TokenRCurlyBr }
  \(                    { \s -> TokenLParen }
  \)                    { \s -> TokenRParen }
  \,                    { \s -> TokenComma }
  \+                    { \s -> TokenPlus }
  \-                    { \s -> TokenMinus }
  \*                    { \s -> TokenTimes }
  \/                    { \s -> TokenDiv }
  \%                    { \s -> TokenMod }
  \<                    { \s -> TokenLt }
  \>                    { \s -> TokenGt }
  \=                    { \s -> TokenEq }
  \#                    { \s -> TokenCard }
  "!="                  { \s -> TokenNEq }
  $alpha [$alpha $digit \_ \']*		{ \s -> TokenId s }

{
-- Each action has type :: String -> Token

lexer = alexScanTokens
}
