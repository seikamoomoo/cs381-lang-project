-- Project 2020
-- Team MKembers : Sriram Rakshith Kolar, Swetha Jayapath, Seika Muhmod
-- DESC:



-- Syntax
--     int  ::= (any integer)
--
--     var  ::= (any variable Var)
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

-- Arrays are the core, strings cant be core.
-- return an error instead of 0
-- justification in the design doc !
--

-- Grammer:

-- | Variable Vars.
type Var = String

-- | Integer expressions.
data Expr = Lit Int
          | Neg Expr
          | Add Expr Expr
          | Mul Expr Expr
          | Ref Var
          | S String
          | Cat Expr Expr
          | L List

  deriving (Eq,Show)

-- | Boolean expressions.
data Test = LTE Expr Expr
          | Not Test
          | And Test Test
  deriving (Eq,Show)

-- | Lists
data List = Nil
           | Cons Expr List
  deriving (Eq,Show)


-- | Strings
-- data Str = S String
--          | Cat Str Str
--   deriving (Eq,Show)

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
type Store = [(Var, Int)]

type Fact = [Expr]

type ListInt = [List]

--type Var = String
-- Fix point funvtion for the while loops

fix :: (a -> a) -> a
fix f = let x = f x in x


-- Our Progs:



-- | An Imp program.


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



-- A factorial program which checks the lesser factorial value and sets b to zero
fact :: Prog
fact = (["a","b"], Block [a,b,cond])
   where
     a = Set "a" (Lit (factorial 6))
     b = Set "b" (Lit (factorial 5))
     cond = While
          (LTE (Ref "a") (Ref "b"))
          (Set "b" (Lit 0))


-- concatination of list of Strings

concatVar :: Prog
concatVar = (["a","b"], Block [a,b,cond])
    where
      a = Set "a" (S "abcd")
      b = Set "b" (S "efgh")
      cond = While
           (LTE (Lit 3)(Lit 4))
           (Set "b" (S (str (Cat (Ref "a")(Ref "b")))))


-- concatination of List of Integers

concatInt :: Prog
concatInt = (["a","b"], Block [a,b,cond])
    where
      a = Set "a" (L (Cons (Lit 5)(Cons (Lit 6)(Cons (Lit 7) Nil))))
      b = Set "b" (L (Cons (Lit 9)(Cons (Lit 10)(Cons (Lit 11) Nil))))
      cond = While
            (LTE (Lit 3)(Lit 4))
            (Set "b" (L(conInts (Ref "a")(Ref "b"))))




-- If then or else




-- factorial(while)
-- adding 3 numbers together
--





-- for (i = 0, i< 10 i= i + 1)
-- while (i < 10)
-- i = i +1

-- Desugar :: Expr -> Expr

-- [(Var, Int)] :: Store

-- lookup :: [(a,b)] -> a -> Maybe b
-- get n env :: Var -> Store -> Int
-- set n i env :: Var -> Int -> Env -> Env

-- Semantic Domain:

-- | Semantics of integer expressions.
--   Semantic domain: Store -> Int

-- | List
conInts :: Expr -> Expr -> List
conInts (L (Cons i x)) (L y) = case x of
                         Nil -> (Cons i y)
                         otherwise -> (Cons i (conInts (L x) (L y)))



factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


get :: Var -> Store -> Int
get n env = case lookup n env of
            Just i -> i
            Nothing -> 0

set :: Var -> Int -> Store -> Store
set n i env = (n,i):env

new :: [Var] -> Store
new v = map (\x -> (x,0)) v

-- | Semantics of strings
str :: Expr -> String
str (S x) = x
str (Cat x y) = case (Cat x y) of
                (Cat (S n) (S m)) -> (n ++ m)


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

-- 1. Define the syntax of types
data Type = TBool | TInt | TError | TString | TList
  deriving (Eq,Show)

-- Define the typing relation.
typeOfExp :: Expr -> Type
typeOfExp (Lit i)    = TInt
--typeOf (Neg e)    =
typeOfExp (Add l r)  = case (typeOfExp l, typeOfExp r) of
                          (TInt, TInt) -> TInt
                          _ -> TError
typeOfExp (Mul l r)  = case (typeOfExp l, typeOfExp r) of
                        (TInt, TInt) -> TInt
                        _ -> TError
typeOfExp (Ref s)  =   TString
typeOfExp (S _)  =   TString
typeOfExp (Cat l r)  =  case (typeOfExp l, typeOfExp r) of
                          (TString, TString) -> TString
                          _ -> TError
typeOfExp (L xs)  =   TList


typeOfTest :: Test -> Type
typeOfTest (LTE l r)    = case (typeOfExp l, typeOfExp r) of
                            (TInt, TInt) -> TBool
                            _ -> TError
typeOfTest (Not e) = case typeOfTest e of
                      (TBool) -> TBool
                      _ -> TError
typeOfTest (And l r)    = case (typeOfTest l, typeOfTest r) of
                          (TBool, TBool) -> TBool
                          _ -> TError


typeOfList :: List -> Type
typeOfList (Nil)    = TList
typeOfList (Cons e l)    = case (typeOfExp e, typeOfList l) of
                            (TInt, TList) -> TList
                            (TString, TList) -> TList
                            _ -> TError

typeOfStmt :: Stmt -> Type
typeOfStmt (Cond c t e) = case (typeOfTest c, typeOfStmt t, typeOfStmt e) of
                            (TBool, tt, te) -> if tt == te then tt else TError
                            _ -> TError
typeOfStmt (While c s) = case (typeOfTest c, typeOfStmt s) of
                          (TBool, t) -> t
                          _ -> TError
