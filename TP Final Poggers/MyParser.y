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
      print           { TokenPrint }
      ':='            { TokenAss }
      ';'             { TokenSemi }
      ','             { TokenComma }
      '{'             { TokenLeftBr }
      '}'             { TokenRightBr }
      '('             { TokenLeftParen }
      ')'             { TokenRightParen }

%right ';'
%left ':='
%left '+' '-'
%left '*' '/'

%%

Stm
    : id ':=' Exp               { AssStm $1 $3 }
    | Stm ';' Stm               { CompoundStm $1 $3 }
    | print '(' ExpList ')'     { PrintStm $3 }

ExpList
    : Exp ',' ExpList           { EList $1 $3 }
    | Exp                       { Exp $1 }

Exp
    : Form                      { Form $1 }

Form
    :

SetExp
    : '{' '}'                   { EmptySet }
    | '{' ExpList '}'           { ExpList $2 }
    | '(' SetExp ')'            {  }

IntExp
    : int                       { Int $1 }
    | IntExp '+' IntExp         { OperAdd $1 $3 }
    | IntExp '-' IntExp         { OperSub $1 $3 }
    | IntExp '*' IntExp         { OperMul $1 $3 }
    | IntExp '/' IntExp         { OperDiv $1 $3 }
    | '(' IntExp ')'            { Term $1 }
{

}
