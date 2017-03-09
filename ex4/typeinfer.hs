import Data.Char (isLetter, isDigit)
import Data.Map.Strict (empty, (!), insert, keys)


data Token = Id String | Dot | Lmd | OpenP | CloseP
data Expr = X String | Lamda String Expr | Seq Expr Expr

data Type = A Int | Fun Type Type
	deriving Eq

instance Show Type where
	show = show' False
		where
			show' _ (A id) = '@' : show id
			show' False (Fun t1 t2) = show' True t1 ++" -> "++ show' False t2
			show' True (Fun t1 t2) = "("++ show' True t1 ++" -> "++ show' False t2 ++")"

data Curry = Rule Type Type
data Sigma = Insert Int Type | TypeError



--	Lexer		--
identifyer [] = ("", [])
identifyer (c : cs) = if isLetter c || isDigit c then (c : id, cs') else ("", c : cs)
	where (id, cs') = identifyer cs

lexer [] = []
lexer (c : cs) = if isLetter c then let (id, cs') = identifyer cs in Id (c : id) : lexer cs' else case c of
	'.' -> Dot : lexer cs
	'\\' -> Lmd : lexer cs
	'(' -> OpenP : lexer cs
	')' -> CloseP : lexer cs
	_ -> lexer cs



--	Parser		--
parser (Id str : ts) = (ts, X str)
parser (OpenP : Lmd : Id str : Dot : ts) = (tail rest, Lamda str expr)
	where (rest, expr) = parser ts
parser (OpenP:ts) = let (rest', expr1) = parser ts
                        (rest, expr2) = parser rest'
					in (tail rest, Seq expr1 expr2)



--	TypeInfer	--
typeInfer id gamma (X str) = (id, A (gamma ! str), [])
typeInfer id gamma (Lamda str expr) = (id', Fun (A id) tau, c)
	where (id', tau, c) = typeInfer (id+1) (insert str id gamma) expr
typeInfer id gamma (Seq expr1 expr2) = let (id', sigma, c1) = typeInfer id gamma expr1
                                           (id'', tau, c2) = typeInfer id' gamma expr2
                                       in (id'' + 1, A id'', Rule sigma (Fun tau (A id'')) : (c1++c2))



--	Unify		--
exists id t = case t of
	(A i) -> id == i
	(Fun t1 t2) -> exists id t1 || exists id t2


replace id t (A i) = if i == id then t else A i
replace id t (Fun t1 t2) = Fun (replace id t t1) (replace id t t2)

mapC f (Rule a b) = Rule (f a) (f b)
replaceM id t = map (mapC (replace id t))

unify [] = []
unify (Rule t1 t2 : c) = if t1 == t2 then unify c else case (t1, t2) of
	(A id, t2) -> if exists id t2 then [TypeError] else Insert id t2 : unify (replaceM id t2 c)
	(t1, A id) -> if exists id t1 then [TypeError] else Insert id t1 : unify (replaceM id t1 c)
	(Fun t11 t12, Fun t21 t22) -> unify (Rule t11 t21 : Rule t12 t22 : c)
	_ -> [TypeError]



--	Final		--
finalize t [] = trd3 (number 0 empty t)
finalize t (Insert id t' : c) = finalize (replace id t' t) c

number next gamma (A id) = if id `elem` keys gamma then (next, gamma, A (gamma!id)) else (next + 1, insert id next gamma, A next)
number next gamma (Fun t1 t2) = let (next', gamma', t1') = number next gamma t1
                                    (next'', gamma'', t2') = number next' gamma' t2
                                in (next'', gamma'', Fun t1' t2')



--	Rest		--
snd3 (_, b, _) = b
trd3 (_, _, c) = c
sndTrd (_, b, c) = (b, c)


isError [] = False
isError (s:ss) = case s of
	TypeError -> True
	_ -> isError ss


execute input = let (t, c) = (sndTrd . typeInfer 0 empty . snd . parser . lexer) input
                    sigma = unify c
                in (if isError sigma then "type error" else show (finalize t sigma))



--	Main		--
main = do
	all <- getContents
	let list = (tail . lines) all
	putStr $ unlines (map execute list)