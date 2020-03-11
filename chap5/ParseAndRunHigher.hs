
module ParseAndRunHigher where

-- from chap4 FunLang
import qualified HigherFun
import qualified Parse

run e = HigherFun.eval e []

{- Examples of higher-order programs, in concrete syntax -}

ex5 =
    Parse.parseFromString
      "let tw g = let app x = g (g x) in app end \n\
       \ in let mul3 x = 3 * x \n\
       \ in let quad = tw mul3 \n\
       \ in quad 7 end end end"

ex6 =
    Parse.parseFromString
     "let tw g = let app x = g (g x) in app end \n\
      \ in let mul3 x = 3 * x \n\
      \ in let quad = tw mul3 \n\
      \ in quad end end end";;

ex7 =
    Parse.parseFromString
      "let rep n = \n\
          \let rep1 g = \n\
              \let rep2 x = if n=0 then x else rep (n-1) g (g x) \n\
              \in rep2 end \n\
          \in rep1 end \n\
      \in let mul3 x = 3 * x \n\
      \in let tw = rep 2 \n\
      \in let quad = tw mul3 \n\
      \in quad 7 end end end end"

ex8 =
    Parse.parseFromString
      "let rep n = \n\
          \let rep1 g = \n\
              \let rep2 x = if n=0 then x else rep (n-1) g (g x) \n\
              \in rep2 end \n\
          \in rep1 end \n\
      \in let mul3 x = 3 * x \n\
      \in let twototen = rep 10 mul3 \n\
      \in twototen 7 end end end"

ex9 =
    Parse.parseFromString
      "let rep n = \n\
          \let rep1 g = \n\
              \let rep2 x = if n=0 then x else rep (n-1) g (g x) \n\
              \in rep2 end \n\
          \in rep1 end \n\
      \in let mul3 x = 3 * x \n\
      \in let twototen = (rep 10) mul3 \n\
      \in twototen 7 end end end"
