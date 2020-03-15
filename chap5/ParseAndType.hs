
module ParseAndType where

-- fromString = Parse.parseFromString

inferType = TypeInference.inferType
    
{- Well-typed examples ---------------------------------------- -}

-- In the let-body, f is polymorphic 

tex1 = inferType (fromString "let f x = 1 in f 7 + f false end")

-- In the let-body, g is polymorphic because f is 

tex2 = inferType (fromString "let g = let f x = 1 in f end in g 7 + g false end")

-- f is not polymorphic but used consistently 

tex3 = inferType (fromString "let g y = let f x = (x=y) in f 1 = f 3 end in g 7 end")

-- The twice function 

tex4 = inferType (fromString "let tw g = let app x = g (g x) in app end 
                              in let triple y = 3 * y in (tw triple) 11 end 
                              end")

tex5 = inferType (fromString "let tw g = let app x = g (g x) in app end 
                              in tw end")

-- Declaring a polymorphic function and rebinding it 

tex6 = inferType (fromString                 
                   "let id x = x 
                    in let i1 = id 
                    in let i2 = id 
                    in let k x = let k2 y = x in k2 end 
                    in (k 2) (i1 false) = (k 4) (i1 i2) end end end end ")

-- A large type 

tex7 = inferType (fromString 
                    "let pair x = let p1 y = let p2 p = p x y in p2 end in p1 end 
                    in let id x = x 
                    in let p1 = pair id id 
                    in let p2 = pair p1 p1 
                    in let p3 = pair p2 p2 
                    in let p4 = pair p3 p3 
                    in p4 end end end end end end")


-- A polymorphic function may be applied to itself *)

tex8 = inferType (fromString "let f x = x in f f end")


{- Ill-typed examples ----------------------------------------- -}

-- A function f is not polymorphic in its own right-hand side

teex1 () = inferType (fromString "let f x = f 7 + f false in 4 end")

-- f is not polymorphic in x because y is bound further out 

teex2 () = inferType (fromString "let g y = let f x = (x=y) in f 1 = f false end in g end")

-- circularity: function parameter h cannot be applied to itself 

teex3 () = inferType (fromString "let g h = h h in let f x = x in g f end end")
