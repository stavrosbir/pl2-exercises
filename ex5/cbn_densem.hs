import Data.Char (isLower)
import Data.Map.Strict (empty, (!), insert, Map)


--	Syntax
type X  =  Char
data E  =  Lit Char | Write E E | Read E
        |  Var X | Lam X E | App E E


data V = Value X | Psi (D -> D)
type D = S -> (V, S)
type S = ([X], [X])
type Env = Map X D



-- --	Pretty-printing
-- instance Show E where
--   showsPrec p (Lit c) =
--     ('#' :) . (c :)
--   showsPrec p (Write e1 e2) =
--     ('+' :) . showsPrec p e1 . showsPrec p e2
--   showsPrec p (Read e) =
--     ('-' :) . showsPrec p e
--   showsPrec p (Var x) =
--     (x :)
--   showsPrec p (Lam x e) =
--     ('/' :) . (x :) . showsPrec p e
--   showsPrec p (App e1 e2) =
--     ('@' :) . showsPrec p e1 . showsPrec p e2



--	Parsing
next (x : r) = [(x, r)]
next _ = []


instance Read E where
  readsPrec p s =
    [(Lit c, r)        |  ('#', t) <- next s, (c, r) <- next t] ++
    [(Write e1 e2, r)  |  ('+', t1) <- next s,
                          (e1, t2) <- reads t1,
                          (e2, r) <- reads t2] ++
    [(Read e, r)       |  ('-', t) <- next s, (e, r) <- reads t] ++
    [(Var x, r)        |  (x, r) <- next s, isLower x] ++
    [(Lam x e, r)      |  ('/', t1) <- next s,
                          (x, t2) <- next t1, isLower x,
                          (e, r) <- reads t2] ++
    [(App e1 e2, r)    |  ('@', t1) <- next s,
                          (e1, t2) <- reads t1,
                          (e2, r) <- reads t2]



--	Semantics
sem :: E -> Env -> D
sem expr env s = case expr of
  (Lit c) -> (Value c, s)
  (Write e1 e2) -> let (Value v_out, s') = sem e1 env s
                       (v, (i, o)) = sem e2 env s'
                   in (v, (i, v_out:o))
  (Read e) -> let (i:is, o) = s in sem (App e (Lit i)) env (is, o)
  (Var c) -> (env!c) s
  (Lam c e) -> (Psi (\d -> sem e (insert c d env)), s)
  (App e1 e2) -> let (v, s') = sem e1 env s
                     d = sem e2 env
                 in phi v d s'


phi :: V -> D -> D
phi (Psi f) = f



--	Wrap it up!
main = do
  contents <- getContents
  let (phrase : input : _) = lines contents
  let e = read phrase
  let d = sem e empty
  let (_, (_, output)) = d (input, "")
  putStrLn output