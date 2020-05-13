
module Main where

import Parsers
import Control.Monad
import System.Environment
import System.IO
import NofibUtils

infixr 8 +.+ , +.. , ..+
infixl 7 <<< , <<*
infixr 6 |||

(+.+) = thn
(..+) = xthn
(+..) = thnx
(|||) = alt
(<<<) = using
(<<*) = using2
lit   :: Eq a => a -> Parser a a
lit   = literal
star  = rpt
anyC  = satisfy (const True)
butC cs = satisfy (not.(`elem` cs))
noC   "" = [("","")]
noC   _  = []

main = do
  (n:_) <- getArgs
  hSetEncoding stdin utf8
  input <- getContents
  replicateM_ (read n) $ do
    (_:args) <- getArgs
    parse_args input args

parse_args :: String -> [String] -> IO ()
parse_args input (regexp: files) =
	let acc = acceptor (fst(head(nnRegexp regexp)))
	    acc' = unlines . filter acc . lines
	in
	    print (hash (acc' input))
parse_args _ _ =
	getProgName >>= \progName ->

data NFANode
     	= NFAChar Char NFANode
	| NFAAny  NFANode

	| NFAEnd  NFANode
	| NFAFinal

nfaChar = NFAChar
nfaAny  = NFAAny
-- nfaEps  = NFAEps
nfaEps  = mkTable [] [] [] False . epsClosure
nfaEnd  = NFAEnd
nfaFinal= NFAFinal

mkTable pairs anys ends final []      = NFATable pairs anys ends final
mkTable pairs anys ends final (NFAChar c n:ns) = mkTable ((c,n):pairs) anys ends final ns
mkTable pairs anys ends final (NFAAny n:ns) = mkTable pairs (n:anys) ends final ns
mkTable pairs anys ends final (NFATable pairs' anys' ends' final':ns) = mkTable (pairs'++pairs) (anys'++anys) (ends'++ends) (final' || final) ns
mkTable pairs anys ends final (NFAEnd n:ns) = mkTable pairs anys (n:ends) final ns
mkTable pairs anys ends final (NFAFinal:ns) = mkTable pairs anys ends True ns
mkTable _ _ _ _ _ = error "illegal argument to mkTable"

type NFAproducer = NFANode -> NFANode

nnAtom :: Parser Char NFAproducer
nnAtom =
     lit '\\' ..+ lit '(' ..+ nnRegexp +.. lit '\\' +.. lit ')'
 ||| lit '\\' ..+ butC "|()"	 <<< nfaChar
 ||| lit '.'			 <<< const NFAAny
 ||| butC "\\.$"		 <<< nfaChar
 ||| lit '$' `followedBy` anyC <<< nfaChar

nnExtAtom :: Parser Char NFAproducer
nnExtAtom =
     nnAtom +.+ opt (lit '*' <<< const (\ at final ->
					 let at_init = at (nfaEps [final, at_init])
					 in  nfaEps [at_init, final])
		|||  lit '+' <<< const (\ at final ->
					 let at_init = at (nfaEps [final, at_init])
					 in  nfaEps [at_init])
		|||  lit '?' <<< const (\ at final ->
					 let at_init = at (nfaEps [final])
					 in  nfaEps [final, at_init]))
	<<< helper
     where
       helper (ea, []) = ea
       helper (ea, [f]) = f ea

nnFactor :: Parser Char NFAproducer
nnFactor =
     plus nnExtAtom	<<< foldr (.) id

nnRegexp :: Parser Char NFAproducer
nnRegexp =
     nnFactor +.+ star (lit '\\' ..+ lit '|' ..+ nnFactor) +.+ opt (lit '$')
	<<< helper
     where
       helper (ef, (efs, [])) = foldl combine ef efs
       helper (ef, (efs, _ )) = foldl combine ef efs . nfaEnd

nfaStep states c = {- epsClosure -} (concat (map step states))
  where
    step (NFAChar c' n') | c == c' = [n']
    step (NFAAny n') = [n']
    step (NFATable pairs anys ends finals) = [ n' | (c',n') <- pairs, c == c' ] ++ anys
    step _ = []

epsClosure [] = []
epsClosure (NFAEps ns:ns') = epsClosure (ns++ns')
epsClosure (n:ns) = n:epsClosure ns

acceptor :: NFAproducer -> String -> Bool
acceptor nfa str = nfaRun ( {- epsClosure -} [nfa nfaFinal]) str

nfaRun :: [NFANode] -> String -> Bool
nfaRun ns (c:cs) = nfaRun (nfaStep ns c) cs
nfaRun ns [] = not (null ( {- epsClosure -} (concat (map step ns))))
  where
    step (NFAEnd n') = [n']
    step (NFAFinal)  = [NFAFinal]
    step (NFATable pairs anys ends True) = [NFAFinal]
    step (NFATable pairs anys ends finals) = ends
    step _           = []
