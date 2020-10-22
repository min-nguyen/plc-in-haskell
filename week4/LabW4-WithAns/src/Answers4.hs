module Answers4 where

-- local
import Compiler
import Parse

-- Exercise 1 - Regular Expressions and Finite Automata
--------------------------------------------------------------------------------

-- (i) b+ | b*(a(b+a))*b*

-- (ii) (see NFA.jpg - our answer is actually also a DFA)

-- Exercise 2 - Grammars
--------------------------------------------------------------------------------

-- (i)

{-

  Expr
  -> let var '=' Expr in Expr end                     -- Rule 5
  -> let var '=' Expr in Expr '+' Expr end            -- Rule 7
  -> let var '=' Expr in Expr '+' Expr '*' Expr end   -- Rule 6
  -> let var '=' Expr in Expr '+' Expr '*' num end    -- Rule 2
  -> let var '=' Expr in Expr '+' num '*' num end     -- Rule 2
  -> let var '=' Expr in var '+' num '*' num end      -- Rule 1
  -> let var '=' num in var '+' num '*' num end       -- Rule 2

-}

-- (ii)

{-

             Expr
              |
     let var '=' Expr in Expr end
                  |          \
                 num    Expr '+' Expr
                         |          \
                        var    Expr '*' Expr
                                |        |
                               num      num

-}

-- Exercise 4 - Bringing it Together
--------------------------------------------------------------------------------

-- (i)

-- | Parses and compiles a string in the 'Expr' language to stack machine code.
compString :: String -> [SInstr]
compString s = scomp (parseFromString s) []

-- (ii) (see IfExprLang)

-- (iii)

{-

PARSE STACK                             INPUT                           ACTION
   	                                    let z = 17 in z + 2 * 3 end     action: shift
let	                                    z = 17 in z + 2 * 3 end         action: shift
let var                                 = 17 in z + 2 * 3 end           action: shift
let var =                               17 in z + 2 * 3 end             action: shift
let var = num                           in z + 2 * 3 end                action: reduce (rule 2)
let var = Expr                          in z + 2 * 3 end                action: shift
let var = Expr in                       z + 2 * 3 end                   action: shift
let var = Expr in var                   z + 2 * 3 end                   action: reduce (rule 1)
let var = Expr in Expr                  + 2 * 3 end                     action: shift
let var = Expr in Expr +                2 * 3 end                       action: shift
let var = Expr in Expr + num            * 3 end                         action: reduce (rule 2)
let var = Expr in Expr + Expr           * 3 end                         action: shift
let var = Expr in Expr + Expr *         3 end                           action: shift
let var = Expr in Expr + Expr * num     end                             action: reduce (rule 2
let var = Expr in Expr + Expr * Expr    end                             action: reduce (rule 6)
let var = Expr in Expr + Expr           end                             action: reduce (rule 7)
let var = Expr in Expr                  end                             action: shift
let var = Expr in Expr end 	                                            action: reduce (rule 5)
Expr

-}

-- Exercise 5 - USQL
--------------------------------------------------------------------------------

{-
This answer covers the addition of an optional WHERE clause.

Changes in UsqlAbsyn.hs:

  ...
  data Stmt = Select [Expr] [String] (Maybe Expr)
  ...

Changes in UsqlLex.x

  ...
  tokens :-
       WHERE         { \s -> TokenWhere}
  ...
   data Token  = ...
               | TokenWhere
               | ...
               deriving (Show, Eq)
  ...

Changes in UsqlPar.y:

  ...
  %token
            where   { TokenWhere }

  Stmt    : select Exprs1 from Names1 where Expr   { Select $2 $4 $6 }
  ...

-}