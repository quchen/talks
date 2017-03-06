{-# OPTIONS_GHC -Wall #-}

module BrainfuckParser where

import Parser

data BFInstruction =
      Add
    | Sub
    | MoveLeft
    | MoveRight
    | Read
    | Print
    | Loop BFProgram
    | Comment Char
    deriving (Show)

data BFProgram = BFProgram [BFInstruction]
    deriving (Show)

renderInstruction :: BFInstruction -> String
renderInstruction instr = case instr of
    Add         -> "+"
    Sub         -> "-"
    MoveLeft    -> "<"
    MoveRight   -> ">"
    Print       -> "."
    Read        -> ","
    (Loop prog) -> "[" ++ renderProgram prog ++ "]"
    Comment c   -> [c]

renderProgram :: BFProgram -> String
renderProgram (BFProgram p) = concat (map renderInstruction p)

bfLoop :: Parser BFInstruction
bfLoop  = do
    instructions <- between (char '[') (char ']') (many bfInstruction)
    pure (Loop (BFProgram instructions))

bfComment :: Parser BFInstruction
bfComment = do
    c <- noneOf "+-<>.,[]"
    pure (Comment c)

bfInstruction :: Parser BFInstruction
bfInstruction =
    let x ==> op = char x >> pure op
    in anyOf [ '+' ==> Add
             , '-' ==> Sub
             , '<' ==> MoveLeft
             , '>' ==> MoveRight
             , '.' ==> Print
             , ',' ==> Read
             , bfLoop
             , bfComment ]

bfProgram :: Parser BFProgram
bfProgram = do
    x <- many bfInstruction
    pure (BFProgram x)
