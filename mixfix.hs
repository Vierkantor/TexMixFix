import Data.List hiding (group)
import Debug.Trace

import ParseLib

-- for building a list
(<~>) :: Parser s [a] -> Parser s [a] -> Parser s [a]
(<~>) p q = (++) <$> p <*> q
(~>) :: Parser s a -> Parser s [a] -> Parser s [a]
(~>) p q = (:) <$> p <*> q
(<~) :: Parser s [a] -> Parser s a -> Parser s [a]
(<~) p q = p <~> ((:[]) <$> q)

-- given a parser of expressions and a list of name parts, parse a list of expressions between those parts
between :: Eq s => Parser s a -> NameParts [s] -> Parser s [a]
_ `between` []       = error "you broke it!"
p `between` (x:[]) = [] <$ token x
p `between` (x : y : xs) = token x *> (p ~> (p `between` (y : xs)))

-- Left:  _+_+_ == (_+_)+_
-- Right: _+_+_ == _+(_+_)
-- Non:   _+_+_ == syntax error
data Associativity = Leftass | Rightass | Nonass deriving (Eq, Show)
-- describes how name parts and holes are arranged, every operator has holes between name parts
-- Prefix:  a hole after the last name part                     e.g. if_then_else_
-- Infix:   holes before the first and after the last name part e.g. _+_
-- Postfix: a hole before the first name part                   e.g. _[_]
-- Closes:  no extra holes                                      e.g. (_)
data Fixity = Prefix | Infix Associativity | Postfix | Closed deriving (Eq, Show)
type NameParts ss = [ss]
data Operator s = Operator Fixity (NameParts s) deriving Eq
instance Show s => Show (Operator s) where
	show op = fill op (repeat " _ ")

-- fill spaces between elements of the first list with elements of the second, does the inverse of between
-- assuming that len xs >= len ns - 1
fill' :: [[a]] -> [[a]] -> [a]
fill' []         _  = []
fill' (n:[])     _  = n
fill' (n:ns) (x:xs) = n ++ x ++ fill' ns xs

-- fill the holes of an operator with the given strings
fill :: Show s => Operator s -> [String] -> String
fill (Operator Closed    ns) xs = fill' (map show ns) xs
fill (Operator Prefix    ns) xs = fill' ((map show ns) ++ [""]) xs
fill (Operator Postfix   ns) xs = fill' ("" : (map show ns))  xs
fill (Operator (Infix a) ns) xs = fill' ("" : (map show ns) ++ [""]) xs

-- a directed (acyclic) graph denoting the precedence of operators
-- an edge from a to b means b binds tighter than a
-- i.e. that a can be the head of a parse tree with b as left- and/or rightmost leaf
-- (internal holes in operators accept everything)
type Precedence s = [Operator s]
data PrecedenceGraph s = Graph [Operator s] (Operator s -> Precedence s)

instance Show s => Show (PrecedenceGraph s) where
	show (Graph ops edges) = show $ zip ops (map edges ops)

-- the empty graph has no edges
emptyGraph :: [Operator s] -> PrecedenceGraph s
emptyGraph ops = Graph ops (const [])

-- construct the graph by adding an edge
(|+) :: Eq s => PrecedenceGraph s -> (Operator s, Operator s) -> PrecedenceGraph s
(Graph ops edges) |+ (from, to) = Graph ops edges'
	where edges' x | x == from = to : edges from
	               | otherwise = edges x
-- nicer syntax for adding an edge
(+->) :: a -> b -> (a, b)
a +-> b = (a, b)
infixl 5 |+
infix 6 +->

-- an expression in the abstract parse tree
data Expr s = Value s | Expr (Operator s) [Expr s] deriving (Eq)

instance Show s => Show (Expr s) where
	show (Value a) = show a
	show (Expr op exprs) = "(" ++ show op ++ ")" ++ show exprs

-- the inverse operation to parsing: prettyprinting
prettyprint :: Show s => Expr s -> String
prettyprint (Value s) = show s
prettyprint (Expr op xs) = fill op (map prettyprint xs)

-- example operators and graphs
if_then_else = Operator Prefix ["if", "then", "else"]
plusLeft = Operator (Infix Leftass) ["+"]
plusRight = Operator (Infix Rightass) ["+"]
access = Operator Postfix ["[", "]"]
parens = Operator Closed ["(", ")"]

predGraph :: PrecedenceGraph String
predGraph = emptyGraph [if_then_else, plusLeft, parens, access]
	|+ if_then_else +-> plusLeft
	|+ if_then_else +-> access
	|+ if_then_else +-> parens
	|+ plusLeft     +-> access
	|+ plusLeft     +-> parens

-- an example base value: a literal "_"
value :: Parser Char (Expr String)
value = Value <$> token "_"

-- parsing functions for mixfix expressions
-- (and for parts of those expressions)

-- parse a string with this operator as head
group :: (Eq s, Show s) => Parser s (Expr [s]) -> PrecedenceGraph [s] -> Operator [s] -> Parser s (Expr [s])
group b g op@(Operator  Closed          _) = Expr op <$> operator b g op
group b g op@(Operator (Infix Nonass)   _) = Expr op <$> up b g op ~> operator b g op <~ up b g op
group b g op@(Operator (Infix Leftass)  _) = left b g op
group b g op@(Operator (Infix Rightass) _) = right b g op
group b g op@(Operator  Prefix          _) = prefx b g op
group b g op@(Operator  Postfix         _) = post b g op

-- special handling for left recursive operators
left b g op = chainl (up b g op) (leftChain <$> operator b g op)
	where leftChain args x y = Expr op (x : args ++ [y])
post b g op = (Expr op <$> up b g op ~> operator b g op)
          <|> flip (<*>) (up b g op) (post' b g op)
post' :: (Eq s, Show s) => Parser s (Expr [s]) -> PrecedenceGraph [s] -> Operator [s] -> Parser s (Expr [s] -> Expr [s])
post' b g op = (postStep  <$> operator b g op)
           <|> (postChain <$> operator b g op <*> post' b g op)
	where postStep  args      x = Expr op (x : args)
	      postChain args half x = Expr op ((half x) : args)

-- special handling for right recursive operators
right b g op = Expr op <$> up b g op ~> operator b g op <~ (up b g op <|> right b g op)
prefx b g op = Expr op <$>              operator b g op <~ (up b g op <|> prefx b g op)

-- parse everything inside the name parts of this operator
operator b g (Operator _ ns) = expression b g `between` ns

-- parse everything tighter than the operator
up :: (Eq s, Show s) => Parser s (Expr [s]) -> PrecedenceGraph [s] -> Operator [s] -> Parser s (Expr [s])
up b g@(Graph operators edges) op = expression' b g (edges op)

-- parse the given operators
expression' :: (Eq s, Show s) => Parser s (Expr [s]) -> PrecedenceGraph [s] -> [Operator [s]] -> Parser s (Expr [s])
expression' b g ops = b <|> choice (map (group b g) ops)

-- parse any expression
expression :: (Eq s, Show s) => Parser s (Expr [s]) -> PrecedenceGraph [s] -> Parser s (Expr [s])
expression b g@(Graph ops _) = expression' b g ops

-- find all parse trees that satisfy the string
($$) :: Parser s a -> [s] -> [a]
p $$ xs = [a | (a, ys) <- parse p xs, null ys]
