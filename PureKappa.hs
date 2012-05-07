module PureKappa where

import KappaParser
import Data.List (intercalate, find, nub, (\\), lookup)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils
import Misc

moduleToKappa :: Module -> String
moduleToKappa m@(Module{ contactMap = cm }) =
  intercalate "\n\n" [ showSignatures cm
                     , showVars cm (vars m)
                     , showRules cm (rules m)
                     , showInits cm (inits m)
                     , showObss cm (obss m)
                     ]


repeatedSiteNames :: CMIntf -> [SiteName]
repeatedSiteNames = repeatedElems . map cmSiteName

-- NB: map cmSiteName . multisites = repeatedSiteNames
multisites :: CMIntf -> [CMSite]
multisites cmIntf = map getSite $ repeatedSiteNames cmIntf
  where getSite :: SiteName -> CMSite
        getSite x = fromJust $ find ((==x) . cmSiteName) cmIntf


showSignatures :: CM -> String
showSignatures cm = intercalate "\n" $ map showSign cm
  where showSign :: CMAgent -> String
        showSign (CMAgent name intf) = "%agent: " ++ name ++ "(" ++ intfStr ++ ")"
          where intfStr = intercalate "," . snd $ foldr getUniqueSite (multisiteCounter, []) intf
                multisiteCounter = zipmap (repeatedSiteNames intf) (repeat 0)
                mss = multisites intf

                getUniqueSite :: CMSite -> (Map.Map SiteName Int, [String]) -> (Map.Map SiteName Int, [String])
                getUniqueSite (CMSite name iss _) (m, xs) = if Map.member name m
                                                              then (Map.adjust (+1) name m, siteStr name' iss' : xs)
                                                              else (m, siteStr name iss : xs)
                  where name' = name ++ show n
                        n = fromJust $ Map.lookup name m
                        iss' = cmInternalStates . head $ filter ((==name) . cmSiteName) mss

                siteStr :: SiteName -> [InternalState] -> String
                siteStr name iss = concat $ name : map ('~':) iss


-- Rules
showRules :: CM -> [RuleWithName] -> String
showRules cm rs = intercalate "\n" $ zipWith showRule names rules
  where names = map fst rs
        rules = concatMap (expandMultisites cm . snd) rs

-- To expand multisites we need to compute the set of possible combinations of unique-sites names for them
-- For example, if we have an agent A(s~u, s), the combinations would be [(As1~u, As2), (As2~u, As1)]
-- Order does not matter! thus if we have an agent A(s, s), the only possible combination would be (As1, As2)

type UniqueSitePerm = [SiteName]

expandMultisites :: CM -> Rule -> [Rule]
expandMultisites cm r@(Rule isReversible lhs rhs rate) =
  do (lhs', rhs') <- zip lhsCommon rhsCommon
     del' <- uniqueExprs deleted deletedUSCs
     add' <- uniqueExprs added addedUSCs
     return $ Rule isReversible (lhs' ++ del') (rhs' ++ add') rate

  where n = largestPrefix r
        lhsPrefix = take n lhs
        rhsPrefix = take n rhs
        deleted = drop n lhs
        added = drop n rhs

        commonUSCs, deletedUSCs, addedUSCs :: [[UniqueSitePerm]]
        [commonUSCs, deletedUSCs, addedUSCs] = map (map $ uniqueSitePerms cm) [lhsPrefix, deleted, added]

        lhsCommon = uniqueExprs lhsPrefix commonUSCs
        rhsCommon = uniqueExprs rhsPrefix commonUSCs

uniqueExprs :: KExpr -> [[UniqueSitePerm]] -> [KExpr]
uniqueExprs expr uscs = cartesianProduct $ zipWith getUniques expr uscs
  where getUniques :: Agent -> [UniqueSitePerm] -> [Agent]
        getUniques a uscs = zipWith uniquify (repeat a) uscs

uniquify :: Agent -> UniqueSitePerm -> Agent
uniquify (Agent name intf) usc = Agent name (zipWith uniqueSite usc intf)
  where uniqueSite :: SiteName -> Site -> Site
        uniqueSite name' (Site _ internalState bindingState) = Site name' internalState bindingState

-- I need to tell multisites with same state (both internal and binding) from those with different state
-- because, as order doesn't matter, those with same state have less possible combinations
--
-- For example, let's consider agent A(s, s, s) that is presented as A(s, s) in rule 1 and A(s~u, s) in rule 2
--
-- In rule 1 we will have [A(s0, s1), A(s0, s2), A(s1, s2)],
-- so the length of the list is {n m}, where n=3 (multiplicity of site s in the contact map),
-- m=2 (multiplicity of site s in the pattern), and {n m} is the binomial coefficient of n over m.
-- The list contains the agents A(i from 0 to n-m, j from i to n-m+1, ...) as long as m-k > 0
-- We have here all the combinations without repetition for sites with same state
--
-- Instead, in rule 2 we will have [A(s0~u, s1), A(s0~u, s2), A(s1~u, s0), A(s1~u, s2), A(s2~u, s0), A(s2~u, s1)],
-- so the length of the list is n!/(n-m)! or {n m}*m! and we have all possible permutations (without repetition!)
-- for sites with different states
--
-- Finally, when we mix both, the list would be [A(s0~u, s1, s2), A(s1~u, s0, s2), A(s2~u, s0, s1)]
-- and its length  {n_i m_i}, where i is the set of sites with same state
--
-- I need more examples: A(s, s, s, s, s) => A(s~u, s~u, s, s):
-- [A(s0~u, s1~u, s2, s3), A(s0~u, s1~u, s2, s4), A(s0~u, s1~u, s3, s4),
--  A(s0~u, s2~u, s1, s3), A(s0~u, s2~u, s1, s4), A(s0~u, s2~u, s3, s4), ...]
--
-- NB n_i only depends on the site name of i, whereas m_i depends on the whole site (site name + state)
--
-- In short, we have all possible combinations for sites with same state and all possible permutations for sites
-- with different state

data Tree a = Node a [Tree a]
  deriving Show

uniqueSitePerms :: CM -> Agent -> [UniqueSitePerm]
uniqueSitePerms cm (Agent agentName intf) = do sitesWithIds <- allSitesPerms
                                               let (_, names) = foldr addSite (Map.fromList sitesWithIds, []) intf
                                               return names
  where
    allSitesPerms = map concat . cartesianProduct $ filter (not . null) perms -- cartesianProduct mixes different sites

    (CMAgent _ cmIntf) = getSig cm agentName
    siteNames = map cmSiteName cmIntf

    addSite :: Site -> (Map.Map Site [Int], UniqueSitePerm) -> (Map.Map Site [Int], UniqueSitePerm)
    addSite s@(Site name _ _) (sitesWithIdsMap, usp) = (sitesWithIdsMap', uniqueName : usp)
      where id:ids = sitesWithIdsMap Map.! s
            sitesWithIdsMap' = Map.insert s ids sitesWithIdsMap
            uniqueName = if lookup name nmap == Just 1
                           then name
                           else name ++ show id

    name2sites :: Map.Map SiteName [Site]
    name2sites = mapKeys getSites siteNames

    getSites :: SiteName -> [Site]
    getSites name = filter ((== name) . siteName) (nub intf)

    nmap :: [(SiteName, Int)] -- this is n, we index here by site name
    nmap = frequencies siteNames

    mmap :: [(Site, Int)] -- this is m, we index here by the whole site
    mmap = frequencies intf -- sites that have the same state are equal

    perms :: [[[(Site, [Int])]]]
    perms = map (uncurry getPerms) nmap

    getPerms :: SiteName -> Int -> [[(Site, [Int])]]
    getPerms name n = concatMap treeToList $ genTree ms []
      where
        sites = name2sites Map.! name
        sitesSet = Set.fromList sites

        -- for each site with the given name, we'll count how many times it appears in the agent (pattern)
        ms = filter (\(k, _) -> Set.member k sitesSet) mmap
        ns = [0..n-1]
        m = length ms

        -- we then generate the tree with all the possible ids for each of those sites
        genTree :: [(Site, Int)] -> [Int] -> [Tree (Site, [Int])]
        genTree [] _ = []
        genTree ((s,m):xs) seen = [ Node (s, ids) (genTree xs (ids ++ seen)) | ids <- getCombs m seen ]

        getCombs :: Int -> [Int] -> [[Int]]
        getCombs m seen = combinationsOf m (ns \\ seen)

        -- from the tree we get the lists containing the different-state sites (but same name) with their respective indices
        -- [(Site, [Int])] is one possible assignment of ids for each site
        treeToList :: Tree (Site, [Int]) -> [[(Site, [Int])]]
        treeToList (Node xs []) = [[xs]]
        treeToList (Node xs ts) = [ xs:ys | ys <- concatMap treeToList ts ]


-- Show
showRule :: RuleName -> Rule -> String
showRule name (Rule isReversible lhs rhs rate)
  | isReversible  = showUniRule name lhs rhs ++ "\n" ++ showUniRule (name ++ " inv") rhs lhs
  | otherwise     = showUniRule name lhs rhs
  where showUniRule name lhs rhs = "'" ++ name ++ "' " ++ showKExpr lhs ++ " -> " ++ showKExpr rhs ++ " @ " ++ showAExpr rate

showKExpr :: KExpr -> String
showKExpr e = intercalate ", " $ map showAgent e

showAgent :: Agent -> String
showAgent (Agent name intf) = name ++ "(" ++ intercalate ", " (map showSite intf) ++ ")"

showSite :: Site -> String
showSite (Site name "" Free) = name
showSite (Site name internalState Free) = name ++ "~" ++ internalState
showSite (Site name "" Unspecified) = ""
showSite (Site name internalState Unspecified) = name ++ "~" ++ internalState ++ "?"
showSite (Site name "" SemiLink) = name ++ "!_"
showSite (Site name internalState SemiLink) = name ++ "~" ++ internalState ++ "!_"
showSite (Site name "" (Bound i)) = name ++ "!" ++ show i
showSite (Site name internalState (Bound i)) = name ++ "~" ++ internalState ++ "!" ++ show i

showAExpr (Var x) = quote x
showAExpr (Integer n) = show n
showAExpr (Float n) = show n
showAExpr Infinity = "[inf]"
showAExpr (Uno Log ae)  = "[log] "  ++ showAExpr ae
showAExpr (Uno Sqrt ae) = "[sqrt] " ++ showAExpr ae
showAExpr (Uno Exp ae)  = "[exp] "  ++ showAExpr ae
showAExpr (Uno Sin ae)  = "[sin] "  ++ showAExpr ae
showAExpr (Uno Cos ae)  = "[cos] "  ++ showAExpr ae
showAExpr (Uno Tan ae)  = "[tan] "  ++ showAExpr ae
showAExpr (Uno Int ae)  = "[int] "  ++ showAExpr ae
showAExpr (Duo Add ae1 ae2)  = showAExpr ae1 ++ " + " ++ showAExpr ae2
showAExpr (Duo Sub ae1 ae2)  = showAExpr ae1 ++ " - " ++ showAExpr ae2
showAExpr (Duo Mult ae1 ae2) = showAExpr ae1 ++ " * " ++ showAExpr ae2
showAExpr (Duo Div ae1 ae2)  = showAExpr ae1 ++ " / " ++ showAExpr ae2

quote :: String -> String
quote s = "'" ++ s ++ "'"

-- Vars
showVars :: CM -> [Var] -> String
showVars cm vars = intercalate "\n" $ map showVar vars
  where showVar (name, Left kexpr) = intercalate "\n" $ kvars cm name kexpr
        showVar (name, Right aexpr) = "%var: '" ++ name ++ "' " ++ showAExpr aexpr

kvars :: CM -> String -> KExpr -> [String]
kvars cm name kexpr = mainVar : zipWith showKVar names kexprs
  where names = let namePrefix = name ++ "." in map ((namePrefix ++) . show) [0..length kexprs - 1]
        kexprs = expandMultisitesInExpr cm kexpr
        mainVar = "%var: '" ++ name ++ "' " ++ intercalate "+" (map quote names)
        showKVar name kexpr = "%var: '" ++ name ++ "' " ++ showKExpr kexpr

expandMultisitesInExpr :: CM -> KExpr -> [KExpr]
expandMultisitesInExpr cm kexpr = uniqueExprs kexpr $ map (uniqueSitePerms cm) kexpr


-- Obss
showObss :: CM -> [Obs] -> String
showObss cm obss = intercalate "\n" $ map showObs obss
  where showObs (Plot obs) = "%plot: '" ++ obs ++ "'"
        showObs (KExprWithName name ke) = "%obs: '" ++ name ++ "' " ++ showKExpr ke


-- Inits
showInits :: CM -> [Init] -> String
showInits cm inits = intercalate "\n" $ map showInit inits
  where showInit (n, init) = "%init: " ++ show n ++ " (" ++ sites ++ ")"
          where sites = intercalate ", " $ map (showAgent . nameMultisites cm) init

nameMultisites :: CM -> Agent -> Agent
nameMultisites cm (Agent name intf) = Agent name intf'
  where intf' = reverse . snd $ foldr getUniqueSite (multisiteCounter, []) intf
        multisiteCounter = zipmap (repeatedSiteNames cmIntf) (repeat 0)
        (CMAgent _ cmIntf) = getSig cm name

        getUniqueSite :: Site -> (Map.Map SiteName Int, Interface) -> (Map.Map SiteName Int, Interface)
        getUniqueSite s@(Site name is bs) (m, xs) = if Map.member name m
                                                      then (Map.adjust (+1) name m, Site name' is bs : xs)
                                                      else (m, s : xs)
          where name' = name ++ show n
                n = fromJust $ Map.lookup name m
