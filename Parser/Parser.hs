module Parser (expr) where

import Parselib

expr  :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)
expr   = term   `chainl1` addop
term   = factor `chainl1` mulop
factor = token digit +++ do {symb "("; n <- expr; symb ")"; return n}

addop  = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop  = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}