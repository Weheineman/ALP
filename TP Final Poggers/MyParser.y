{
module MyParser(parse) where

import MyLexer
import AST
import Token
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      int             { TokenInt $$ }
      id              { TokenId $$ }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '%'             { TokenMod }
      '<'             { TokenLt }
      '>'             { TokenGt }
      '='             { TokenEq }
      '!='            { TokenNEq }
      '#'             { TokenCard }
      print           { TokenPrint }
      tint            { TokenTInt }
      tbool           { TokenTBool }
      set             { TokenTSet }
      true            { TokenTrue }
      false           { TokenFalse }
      and             { TokenAnd }
      or              { TokenOr }
      subset          { TokenSubset }
      subsetEq        { TokenSubsetEq }
      in              { TokenIn }
      ':='            { TokenAss }
      ';'             { TokenSemi }
      ','             { TokenComma }
      '['             { TokenLBr }
      ']'             { TokenRBr }
      '{'             { TokenLCurlyBr }
      '}'             { TokenRCurlyBr }
      '('             { TokenLParen }
      ')'             { TokenRParen }

%right ';'
%left ':='
%left and or
%left subset subsetEq in
%left '=' '!='
%left '<' '>'
%left '+' '-'
%left '*' '/' '%'
%%

Stm
    : Type id ':=' Exp          { VarAssStm $1 $2 $4 }
    | Stm ';' Stm               { CompoundStm $1 $3 }
    | print '(' ExpList ')'     { PrintStm $3 }

ExpList
    : Exp ',' ExpList           { ExpList $1 $3 }
    | Exp                       { Exp $1 }

Exp
    : Atom                      { $1 }
    | BinOperation              { $1 }

BinOperation
    : Exp '+' Exp               { BinOp Add $1 $3 }
    | Exp '-' Exp               { BinOp Sub $1 $3 }
    | Exp '*' Exp               { BinOp Mul $1 $3 }
    | Exp '/' Exp               { BinOp Div $1 $3 }
    | Exp '%' Exp               { BinOp Mod $1 $3 }
    | Exp '<' Exp               { BinOp Lt $1 $3 }
    | Exp '>' Exp               { BinOp Gt $1 $3 }
    | Exp '=' Exp               { BinOp Eq $1 $3 }
    | Exp '!=' Exp              { BinOp NEq $1 $3 }
    | Exp and Exp               { BinOp And $1 $3 }
    | Exp or Exp                { BinOp Or $1 $3 }
    | Exp subset Exp            { BinOp Subset $1 $3 }
    | Exp subsetEq Exp          { BinOp SubsetEq $1 $3 }
    | Exp in Exp                { BinOp In $1 $3 }

Atom
    : int                       { Int $1 }
    | true                      { Bool True }
    | false                     { Bool False }
    | id                        { Var $1 }
    | '[' Exp ',' Exp ']'       { Pair $2 $4 }
    | '{' '}'                   { EmptySet }
    | '{' ExpList '}'           { Set $2 }
    | '(' Exp ')'               { $2 }

Type
    : tint                      { TInt }
    | tbool                     { TBool }
    | set '<' Type '>'          { TSet $3 }
    | '[' Type ',' Type ']'     { TPair $2 $4 }
{

}
