module Solver where

import Debug.Trace
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import Data.Either
import Data.Array.IArray


import Learn
import Types
import ManualData
import Databases
import Utilities
import Indicators
import Evaluation
import LengthFunctions

------------------ CLUE PARSING MECHANICS FUNCTIONS ------------------------

infixr 3 +++
(+++) :: [a] -> [a] -> [a]
{-
Use this version to allow unknown def keywords...
xs +++ ys
  | null xs = ys
  | otherwise = xs
-}
xs +++ ys
  = xs ++ ys

lowerCase :: Clue -> Clue
lowerCase (Clue (xs, n))
  = Clue (filter (not . isPunctuation) (map toLower xs), n)

parse :: Clue -> [Parse]
parse clue
  = parseWithIndicator ws n ++ parseWithoutIndicator ws n
  where
    Clue (c, n) = lowerCase clue
    ws = words c

parseWithoutIndicator :: [String] -> Int -> [Parse]
parseWithoutIndicator ws n
  = [(unwords ws', "", p, n) |
       (ws', ws'') <- split2' ws,
       isInWordlist (unwords ws'),
       p <- parseClue ws'']
       -- +++
    -- [('?' : unwords ws', "", p, n) |
    --    (ws', ws'') <- split2' ws,
    --    p <- parseClue ws'']

parseWithIndicator :: [String] -> Int -> [Parse]
parseWithIndicator ws n
  = [(unwords ws', unwords ws'', p, n) |
       (ws', ws'', ws''') <- split3' ws,
       isInWordlist (unwords ws'),
       isDefIndicator ws'',
       p <- parseClue ws''']
       -- +++
    -- [('?' : unwords ws', unwords ws'', p, n) |
    --    (ws', ws'', ws''') <- split3' ws,
    --    isDefIndicator ws'',
    --    p <- parseClue ws''']

parseClue :: [String] -> [ParseTree]
parseClue [w]
  = parseWithoutConcat [w]
parseClue ws
  = parseWithoutConcat ws ++ parseWithConcat ws

parseWithConcat :: [String] -> [ParseTree]
parseWithConcat xs
  = map Concatenate ps
  where
    ps = concatMap (sequence . parseSubpart) (filter ((>1) . length) (partitions xs))
    parseSubpart part = [parseWithoutConcat subpart | subpart <- part]

parseWithoutConcat :: [String] -> [ParseTree]
parseWithoutConcat ws
  = parseSynonyms ws ++
    parseAnagrams pairs ++
    parseHiddenWords pairs ++
    parseInsertions triples ++
    parseSubtractions triples ++
    parseReversals pairs ++
    parseFirstLetters pairs ++
    parseLastLetters pairs ++
    parsePartOf pairs ++
    parseJuxtapositions triples
  where
    pairs = split2' ws
    triples = split3 ws

parseSynonyms :: [String] -> [ParseTree]
parseSynonyms ws
  = [Synonym (unwords ws)]

parseAnagrams :: Pairs -> [ParseTree]
parseAnagrams splits
  = [Anagram p p' |
       (p, p') <- splits,
       isAnagramIndicator p]

parseInsertions :: Triples -> [ParseTree]
parseInsertions splits
  = [Insertion ws' p p'' |
       (ws, ws', ws'') <- splits,
       isInsertionIndicator ws',
       p <- parseClue ws,
       p'' <- parseClue ws''] ++
    [Insertion ws' p'' p |
       (ws, ws', ws'') <- splits,
       isReverseInsertionIndicator ws',
       p <- parseClue ws,
       p'' <- parseClue ws'']

-- simplify' eliminates anagrams from the text being subtracted from,
-- e.g. (anagram of X) loses Y
parseSubtractions :: Triples -> [ParseTree]
parseSubtractions splits
  = [Subtraction ws' p'' p |
       (ws, ws', ws'') <- splits,
       isSubtractionIndicator ws',
       p <- map simplify' (parseClue ws),
       p'' <- map simplify' (parseClue ws'')] ++
    [Subtraction ws' p p'' |
       (ws'', ws', ws) <- splits,
       isReverseSubtractionIndicator ws',
       p <- map simplify' (parseClue ws),
       p'' <- map simplify' (parseClue ws'')]

parseReversals :: Pairs -> [ParseTree]
parseReversals splits
  = [Reversal ws p |
      (ws, ws') <- splits,
      isReversalIndicator ws,
      p <- parseClue ws']

parseHiddenWords :: Pairs -> [ParseTree]
parseHiddenWords splits
  = [HiddenWord ws ws' |
      (ws, ws') <- splits,
      isHiddenWordIndicator ws]

parseFirstLetters :: Pairs -> [ParseTree]
parseFirstLetters splits
  = [FirstLetter ws ws' |
      (ws, ws') <- splits,
      isFirstLetterIndicator ws]

parseLastLetters :: Pairs -> [ParseTree]
parseLastLetters splits
  = [LastLetter ws ws' |
      (ws, ws') <- splits,
      isLastLetterIndicator ws]

-- simplify' eliminates anagrams from the text being subtracted from,
-- e.g. partly (anagram of X)
parsePartOf :: Pairs -> [ParseTree]
parsePartOf splits
  = [PartOf ws p |
      (ws, ws') <- splits,
      isPartOfIndicator ws,
      p <- map simplify' (parseClue ws'),
      p /= Null]

parseJuxtapositions :: Triples -> [ParseTree]
parseJuxtapositions splits
  = [Juxtaposition ws' p p'' |
       (ws, ws', ws'') <- splits,
       isJuxtapositionIndicator ws',
       p <- parseClue ws,
       p'' <- parseClue ws''] ++
    [Juxtaposition ws' p'' p |
       (ws'', ws', ws) <- splits,
       isReverseJuxtapositionIndicator ws',
       p <- parseClue ws,
       p'' <- parseClue ws'']

-- Removes top-level anagrams.  Should really push this into subtrees...
simplify' (Concatenate ts)
  = simpleConcat (map simplify' ts)
simplify' (Anagram ind ws)
  = Null
simplify' t
  = t

-- Replaces top-level synonyms with Ident & removes everything else
-- Designed mainly for PartOf simplification
simplify (Concatenate ts)
  = simpleConcat (map simplify ts)
simplify (Synonym ws)
  = Ident ws
simplify t
  = Null

simpleConcat ts
  | any (==Null) ts = Null
  | otherwise = Concatenate ts

-------------- COST EVALUATION -------------

evalCost (s, k, t, n)
  | head s == '?' = 100000 + c
  | otherwise = c
  where
    c = cost t * (length_penalty s)

length_penalty ws
  = 60 + (length (words ws))

cost :: ParseTree -> Int
cost Null
  = 0
cost (Ident s)
  = 5
cost (Concatenate ts)
  = 20 * (length ts) + sum (map cost ts)
cost (Anagram ind strings)
  = 10
cost (HiddenWord ind strings)
  = 40
cost (Insertion ind t1 t2)
  = 10 + cost t1 + cost t2
cost (Subtraction ind t1 t2)
  = 30 + cost t1 + cost t2
cost (Reversal ind t)
  = 10 + cost t
cost (Synonym string)
  = 80 * length (words string)
cost (FirstLetter ind strings)
  = 20
cost (LastLetter ind strings)
  = 20
cost (PartOf ind c@(Concatenate ts))
  = 60 + 10 * cost c
cost (PartOf ind t)
  = 60 + cost t
cost (Juxtaposition ind t1 t2)
  = cost (Concatenate [t1, t2])

--------------------------- EVALUATION ----------------------------

checkSynonyms :: [Answer] -> Either [Answer] [Answer]
checkSynonyms answers
  | null sols = Left answers
  | otherwise = Right sols
  where
    sols = filter checkSynonym answers

checkSynonym :: Answer -> Bool
checkSynonym (Answer a (def, defkey, p, n))
  = Set.member a (Set.fromList (synonyms def))

checkValidWords :: [Answer] -> [Answer]
checkValidWords
  = filter isValidWord

isValidWord :: Answer -> Bool
isValidWord (Answer a expl)
  = isInWordlist a

sortByParseCost ts
  = zip labels (map snd . sort . map (\p -> (evalCost p, p)) $ ts)
  where
    labels = (zip [(0::Int)..] (repeat (length ts)))

constrainParseLengths :: Clue -> SynonymTable -> [Parse] -> [Parse]
constrainParseLengths (Clue (c, n)) synTable ts
  = filter hasValidLength ts
  where
    hasValidLength (_, _, clue, _)
      = (minLength clue synTable <= n) && (maxLength clue synTable >= n)

-- Synonym table caches synonyms sorted by length and then alphabetically.
makeTable :: String -> SynonymTable
makeTable s
  = [(w, (makeArray sorted mn mx, (mn, mx))) |
       w <- substrings (words s),
       let syns = synonyms (unwords w),
       let ns = map length syns,
       let sorted = sort (zip ns syns),
       let (ns', syns') = unzip sorted,
       let (mn, mx) = (head ns', last ns')]

makeArray :: [(Int, String)] -> Int -> Int -> Array Int [String]
makeArray syns mn mx
  = listArray (mn, mx) [sort [ws | (k, ws) <- syns, k == n] | n <- [mn..mx]]

solve c
  = (head' . checkSynonyms . flip evaluate synTable . sortByParseCost .
     constrainParseLengths c synTable . parse) c
  where
    Clue (s, _) = lowerCase c
    synTable = makeTable s

parses c
  = (sortByParseCost . constrainParseLengths c synTable . parse) c
  where
    Clue (s, _) = lowerCase c
    synTable = makeTable s

ans (Answer a (syn, def, t, n))
  = (a, syn)

lookfor s (Left (_, sols))
  = filter ((==s).fst) sols

head' (Left as)
  = (Left ("UNSOLVED", map ans as))
-- head' (Right sols)
--  = (Right ("SOLVED", sols))
head' (Right sols)
 = (Right ("SOLVED", sols))

------------- SAMPLE CLUES ------------

clue :: Int -> Clue
clue 1
  = Clue ("companion shredded corset",6) -- ESCORT
clue 2
  = Clue ("One name certain to give cover", 6)
  -- = Clue ("notice in flying coat", 6) -- JACKET
clue 3
  = Clue ("companion found in oklahoma terminal", 4)
clue 4
  = Clue ("a new member returned a woman", 6)
clue 5
  = Clue ("pause at these i fancy", 8) -- Everyman 3526, clue 1   ["athetise","hesitate"]
clue 6
  = Clue ("ankle was twisted in ballet", 8) -- Everyman 3526, clue 3
clue 7
  = Clue ("flyer needed by funfair manager", 6)
clue 8
  = Clue ("put food in this stuff on barge at sea", 9)
clue 9
  = Clue ("notice supervisor is going nuts at first", 4)
clue 10
  = Clue ("animal is mistake crossing one river", 7)
clue 11
  = Clue ("maria not a fickle lover", 9)
clue 12
  = Clue ("hope for high praise", 6)
clue 13
  = Clue ("Not fed partly twigged", 5)
clue 14
  = Clue ("Messy bit of lung next to part of kempton", 7)
clue 15
  = Clue ("berate without rodent insect", 3)
clue 16
  = Clue ("Man changing line around Gates head", 5)
clue 17
  = Clue ("Animal returns to grass", 4)
clue 18
  = Clue ("bums mix for deals without energy", 9)
clue 19
  = Clue ("liberal posh wearing platinum with fancy cars to give rich people", 10)
clue 20
  = Clue ("indications show surprising gains for example after recovery",7)
clue 21
  = Clue ("Scholarly head of languages brought in", 7)
clue 22
  = Clue ("A zebra dropping guts, munching bitter plant",6)
clue 23
  = Clue ("get rid of carter - about time!",7)
clue 24
  = Clue ("Heads turn in shock",4)
clue 25
  = Clue ("Proposals for heartless transgressors",6)
clue 26
  = Clue ("Servant, a sober worker, receives tip", 9)

-- Solveable but no synonym for answer
clue 101
  = Clue ("Poorly made Russian fighter returns with a bang",8) -- gimcrack
clue 102
  = Clue ("Skin disease going around English riding school",6) -- manege
clue 103
  = Clue ("Limit area where cattle may graze",5) -- range
clue 104
  = Clue ("They take advantage of a sailor needing employers",7) -- abusers
clue 105
  = Clue ("Package returned has address with added detail",9) -- elaborate
