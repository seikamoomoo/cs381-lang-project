-- Project 2020
-- PALINDROME
-- Team MKembers : Sriram Rakshith Kolar, Swetha Jayapath, Seika Muhmod 
-- DESC: 



-- Syntax
--     int  ::= (any integer)
--
--     var  ::= (any variable name)
--
--     expr ::= int                      literal integers
--           |  `-` expr                 integer negation
--           |  expr `+` expr            integer addition
--           |  var                      variable reference
--
--     expr ::= String
--           |  `-` expr                 integer negation
--           |  expr `+` expr            integer addition
--           |  var                      variable reference
--

--     test ::= expr `≤` expr            integer comparison
--           |  `!` test                 boolean negation
--           |  test `&&` test           boolean conjunction
--
--     stmt ::= var `:=` expr            variable assignment
--           |  `if` test `then` stmt    conditional statement
--              `else` stmt
--           |  `while` test `do` stmt   while loop
--           |  `begin` stmt* `end`      statement block
--
--     prog ::= `vars` var* `;` stmt     program



-- Grammer:

-- | Variable names.
type Var = String

-- | Integer expressions.
data Expr = Lit Int
          | Neg Expr
          | Add Expr Expr
          | Ref Var
  deriving (Eq,Show)

-- | Boolean expressions.
data Test = LTE Expr Expr
          | Not Test
          | And Test Test
  deriving (Eq,Show)

-- | Statements.
data Stmt = Set   Var  Expr
          | Cond  Test Stmt Stmt
          | While Test Stmt
          | Block [Stmt]
          | For Stmt Test Stmt Stmt
  deriving (Eq,Show)

-- | Program.
type Prog = ([Var], Stmt)

-- | Store 
type Store = [(Name, Int)]

type Name = String
-- Fix point funvtion for the while loops

fix :: (a -> a) -> a
fix f = let x = f x in x


-- Our Progs: 



-- | An example Imp program.
euclid :: Prog
euclid = (["a","b"], Block [a,b,loop])
  where
    a = Set "a" (Lit 1071)
    b = Set "b" (Lit 462)
    loop = While
             (Not (And (LTE (Ref "a") (Ref "b"))
                       (LTE (Ref "b") (Ref "a"))))
             (Cond
               (LTE (Ref "a") (Ref "b"))
               (Set "b" (Add (Ref "b") (Neg (Ref "a"))))
               (Set "a" (Add (Ref "a") (Neg (Ref "b")))))









-- for (i = 0, i< 10 i= i + 1)
-- while (i < 10)
-- i = i +1

-- Desugar :: Expr -> Expr

-- [(Name, Int)] :: Store

-- lookup :: [(a,b)] -> a -> Maybe b
-- get n env :: Name -> Store -> Int
-- set n i env :: Name -> Int -> Env -> Env

-- Semantic Domain:


-- | Semantics of integer expressions.
--   Semantic domain: Store -> Int

get :: Name -> Store -> Int
get n env = case lookup n env of
            Just i -> i
            Nothing -> 0



set :: Name -> Int -> Store -> Store
set n i env = (n,i):env 

new :: [Var] -> Store
new v = map (\x -> (x,0)) v


expr :: Expr -> Store -> Int
expr (Lit i)   = \_ -> i
expr (Neg e)   = \m -> negate (expr e m)
expr (Add l r) = \m -> expr l m + expr r m
expr (Ref x)   = \m -> get x m

-- | Semantics of boolean expressions.
--   Semantic domain: Store -> Bool
test :: Test -> Store -> Bool
test (LTE l r) = \m -> expr l m <= expr r m
test (Not e)   = \m -> not (test e m)
test (And l r) = \m -> test l m && test r m

-- | Semantics of statements.
--   Semantic domain: Store -> Store
stmt :: Stmt -> Store -> Store
stmt (Set   x e)   = \m -> set x (expr e m) m
stmt (Cond  c t e) = \m -> if test c m then stmt t m else stmt e m
stmt (While c b)   = fix (\f m -> if test c m then f (stmt b m) else m)
stmt (Block ss)    = \m -> stmts ss m  -- could also use foldl
  where
    stmts []     m = m
    stmts (s:ss) m = stmts ss (stmt s m)

-- | Semantics of programs.
--   Semantic domain: Store
prog :: Prog -> Store
prog (xs,s) = stmt s (new xs)
