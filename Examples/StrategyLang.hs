import Data.List
import Hagl

-----------------------------------------
-- Implementation of Strategy Language --
-----------------------------------------

data (Eq mv, Show mv) => 
     VStrategy mv = VStrategy [mv] [Rule mv] deriving (Eq, Show)

type Play mv = (mv,mv)
type Pattern mv = [Play mv]

data (Eq mv, Show mv) => 
     Rule mv = Rule (Match mv) mv deriving (Eq, Show)

data (Eq mv, Show mv) => 
     Match mv =
       Recent  [Pattern mv]
     | Initial [Pattern mv]
     | Forall  (Pattern mv)
     | Exists  (Pattern mv)
     | Default
     deriving (Eq, Show)

strategy :: (Eq mv, Show mv) => (VStrategy mv) -> Strategy mv ()
strategy (VStrategy init rules) =
  do n <- numGames
     if n < length init 
        then return (init !! n) 
        else matchr rules

matchr :: (Eq mv, Show mv) => [Rule mv] -> Strategy mv ()
matchr [] = error "No rules matched!"
matchr (Rule m mv : rules) = 
  do matched <- matchm m
     if matched then return mv else matchr rules

matchm :: (Eq mv, Show mv) => Match mv -> StratExec mv () Bool
matchm (Recent patterns) =
  do ps <- (prevn (length patterns)) plays
     return $ (length ps == length patterns) 
            && and (zipWith matchp patterns ps)
matchm (Initial patterns) = 
  do ps <- (firstn (length patterns)) plays
     return $ (length ps == length patterns) 
            && and (zipWith matchp patterns ps)
matchm (Forall pattern) =
  do ps <- every plays
     return . and $ map (matchp pattern) ps
matchm (Exists pattern) =
  do ps <- every plays
     return . or $ map (matchp pattern) ps
matchm Default = return True
        
matchp :: (Eq mv, Show mv) => Pattern mv -> Play mv -> Bool
matchp pattern play = or $ map (play ==) pattern

plays :: StratExec mv () (ByGame (mv, mv))
plays = do me <- my `each` every move
           he <- his `each` every move
           return . ByGame $ zipWith (,) me he

-------------------------
-- Regular Expressions --
-------------------------

data Regex mv = E | M mv 
              | Conj (Regex mv) (Regex mv) 
              | Disj (Regex mv) (Regex mv) 
              | Star (Regex mv)

data NRule mv = OnE Int Int
              | OnM mv Int Int
data NFA mv = NFA Int        -- start node
                  [Int]      -- accept nodes
                  [NRule mv] -- edges

data DRule mv = DRule mv Int Int
data DFA mv = DFA Int [Int] [DRule mv]

nxt :: Int -> Int
nxt = (*2)

regexToNFA :: (Eq mv, Show mv) => Regex mv -> Int -> (NFA mv, Int)
regexToNFA E i = (NFA i [i] [], nxt i)
regexToNFA (M mv) i = let i' = nxt i
                      in (NFA i [i'] [OnM mv i i'], nxt i')
regexToNFA (Conj r1 r2) i = let (NFA s1 as1 rs1, i') = regexToNFA r1 i
                                (NFA s2 as2 rs2, i'') = regexToNFA r2 i'
                            in (NFA s1 as2 (map (flip OnE s2) as1 ++ rs1 ++ rs2), nxt i'')
regexToNFA (Disj r1 r2) i = let (NFA s1 as1 rs1, i') = regexToNFA r1 i
                                (NFA s2 as2 rs2, i'') = regexToNFA r2 i'
                                s3 = nxt i''
                            in (NFA s3 (as1++as2) (OnE s3 s1 : OnE s3 s2 : rs1 ++ rs2), nxt s3)
regexToNFA (Star r) i = let (NFA s as rs, i') = regexToNFA r i
                            s' = nxt i'
                        in (NFA s' (s':as) (OnE s' s : map (flip OnE s) as ++ rs), nxt s')

nfaToDFA :: NFA mv -> DFA mv
nfaToDFA (NFA s as rs) = undefined

------------------------
-- Prisoner's Dilemma --
------------------------

data PD = C | D deriving (Show, Eq)

pd = matrix [C, D] [[2, 2], [0, 3], [3, 0], [1, 1]]

titForTat = player "Tit-for-Tat" $
  strategy $ VStrategy [] [Rule (Recent [[(C,D),(D,D)]]) D, Rule Default C]

ccd = player "(CCD)*" (periodic [C, C, D])
