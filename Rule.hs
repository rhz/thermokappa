{-# LANGUAGE TupleSections #-}

module Rule where

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.List (sort)

import qualified KappaParser as KP
import qualified Mixture as M
import qualified Env as E
import PlainKappa (showAExpr)
import Utils

type Injection = Map.Map M.AgentId M.AgentId -- TODO should this be here or in Matching or elsewhere?

data Action = Add M.Mixture [M.AgentId]
            | Mod M.Endpoint M.InternalStateId
            | Brk M.Endpoint
            | Bnd M.Endpoint M.Endpoint
            | Del [M.AgentId]
  deriving (Show, Eq, Ord)

type Rate = KP.Rate
data Rule = Rule { lhs :: M.Mixture
                 , rhs :: M.Mixture
                 , rate :: Rate
                 , isReversible :: Bool
                 --, action :: [Action]
                 }
  deriving (Show, Eq)

evalRule :: E.Env -> KP.Rule -> Rule
evalRule env (KP.Rule isReversible lhs rhs rate) =
  Rule { lhs = M.evalKExpr env True lhs
       , rhs = M.evalKExpr env True rhs
       , rate = rate
       , isReversible = isReversible
       }


-- I adhere to the simpler convention
largestPrefix' :: M.Mixture -> M.Mixture -> Int
largestPrefix' lhs rhs = fromMaybe (Vec.length zippedAgents) (Vec.findIndex isDiff zippedAgents)
  where zippedAgents = Vec.zip (M.agents lhs) (M.agents rhs)
        isDiff (l, r) = M.agentName l /= M.agentName r

largestPrefix :: Rule -> Int
largestPrefix rule = largestPrefix' (lhs rule) (rhs rule)


data RuleEndpoint = Lhs M.Endpoint
                  | Rhs M.Endpoint
                  | Common M.Endpoint

modifiedSites :: Rule -> ([M.Endpoint], [M.Endpoint])
modifiedSites rule = (concatMap diff common ++ concatMap add deleted, concatMap diff common ++ concatMap add added)
  where
    prefixId = largestPrefix rule
    l = M.agents $ lhs rule
    r = M.agents $ rhs rule
    common  = Vec.toList . Vec.take prefixId . Vec.indexed $ Vec.zip l r
    deleted = Vec.toList . Vec.drop prefixId $ Vec.indexed l
    added   = Vec.toList . Vec.drop prefixId $ Vec.indexed r

    add :: (M.AgentId, M.Agent) -> [M.Endpoint]
    add (id, agent) = (id, ) <$> M.siteIds agent

    diff :: (M.AgentId, (M.Agent, M.Agent)) -> [M.Endpoint]
    diff (id, (aLhs, aRhs)) = (id, ) <$> Vec.toList sIds
      where sIds = Vec.findIndices (uncurry (/=)) (Vec.zip (M.interface aLhs) (M.interface aRhs))


actionScript :: M.Mixture -> M.Mixture -> [Action]
actionScript lhs rhs = added ++ common ++ deleted
  where
    prefixId = largestPrefix' lhs rhs
    [l, r] = M.agents <$> [lhs, rhs]
    addedIds   = [prefixId..Vec.length r - 1]
    deletedIds = [prefixId..Vec.length l - 1]
    added | null addedIds = []
          | otherwise     = [Add (M.inducedSSG rhs addedIds) addedIds]
    deleted | null deletedIds = []
            | otherwise       = [Del deletedIds]

    common = sort . removeDups . concatMap diff . Vec.toList . Vec.take prefixId . Vec.indexed $ Vec.zip l r

    diff :: (M.AgentId, (M.Agent, M.Agent)) -> [Action]
    diff (aId, (al, ar)) = intActions ++ lnkActions
      where
        intActions = concatMap (uncurry intDiff) sites
        lnkActions = concatMap (uncurry lnkDiff) sites
        sites = indexedList $ Vec.zip (M.interface al) (M.interface ar)

        intDiff sId (M.Site{ M.internalState = Just x },  M.Site{ M.internalState = Just y })
          | x == y    = []
          | otherwise = [Mod (aId, sId) y]
        intDiff sId (M.Site{ M.internalState = Nothing }, M.Site{ M.internalState = Just x }) = [Mod (aId, sId) x]
        intDiff _ _ = [] -- DCDW!

        lnkDiff sId (M.Site{ M.bindingState = M.Bound }, M.Site{ M.bindingState = M.Bound })
          | nbl == nbr = []
          | otherwise  = [break sId, bind sId] -- link permutation!
          where
            nbl = M.follow lhs (aId, sId) ? "Rule.actionScript: oops"
            nbr = M.follow rhs (aId, sId) ? "Rule.actionScript: oops"
        lnkDiff sId (M.Site{ M.bindingState = M.SemiLink },    M.Site{ M.bindingState = M.Bound })    = [break sId, bind sId]
        lnkDiff sId (M.Site{ M.bindingState = M.Bound },       M.Site{ M.bindingState = M.SemiLink }) = error "Rule.actionScript: bound -> semilink"
        lnkDiff sId (M.Site{ M.bindingState = M.Bound },       M.Site{ M.bindingState = M.Free })     = [break sId]
        lnkDiff sId (M.Site{ M.bindingState = M.Free },        M.Site{ M.bindingState = M.Bound })    = [bind sId]
        lnkDiff sId (M.Site{ M.bindingState = M.Unspecified }, M.Site{ M.bindingState = M.Bound })    = [break sId, bind sId]
        lnkDiff sId (M.Site{ M.bindingState = M.Unspecified }, M.Site{ M.bindingState = M.SemiLink }) = error "Rule.actionScript: unspecified -> semilink"
        lnkDiff sId (M.Site{ M.bindingState = M.Unspecified }, M.Site{ M.bindingState = M.Free })     = [break sId]
        lnkDiff _ _ = [] -- DCDW

        break sId = Brk (aId, sId)
        bind  sId = Bnd (aId, sId) nb
          where nb = M.follow rhs (aId, sId) ? "Rule.actionScript: no link for " ++ show (aId, sId)

    removeDups [] = []
    removeDups (x@(Bnd (a, i) (b, j)) : xs) = x : removeDups (filter (/= Bnd (b, j) (a, i)) xs)
    removeDups (x@(Brk (a, i)) : xs) = x : removeDups xs'
      where xs' | Just (b, j) <- M.follow lhs (a, i)  =  filter (/= Brk (b, j)) xs
                | otherwise                           =  xs


-- TODO How should I handle binding to new agents?
--      One possible solution: update not only mix but also inj with the application of each action
--      In this way, after adding a mixture you can add the corresponding rhs ids of the new agents
--      to the injection map and then bind to them in the usual way
apply :: [Action] -> (Injection, M.Mixture) -> (Injection, M.Mixture)
apply [] (inj, mix) = (inj, mix)
apply (Mod (aId, sId) x  : actions) (inj, mix) = apply actions (inj, M.setIntInMix (Just x) (inj Map.! aId, sId) mix)
apply (Bnd (a, i) (b, j) : actions) (inj, mix) = apply actions (inj, M.bind (inj Map.! a, i) (inj Map.! b, j) mix)
apply (Brk (a, i)        : actions) (inj, mix) = apply actions (inj, M.unbind (inj Map.! a, i) mix)

apply (Add mix' ids      : actions) (inj, mix) = apply actions (inj', M.disjointUnion mix mix')
  where inj' = inj `Map.union` zipmap ids [Vec.length (M.agents mix)..]

apply (Del aIds          : actions) (inj, mix) = apply actions (inj, mix''{ M.graph = replaceIds $ M.graph mix'' })
  where mix'' = mix'{ M.agents = Vec.ifilter (const . (`Set.member` idSet)) (M.agents mix') }
        idMap = fst . foldr newId (Map.empty, 0) $ M.agentIds mix'
        idSet = Set.fromList aIds
        mix'  = foldr unbind mix $ map (inj Map.!) aIds
        unbind aId mix = foldr (M.unbind . (aId, )) mix sites
          where sites = fst <$> filter (M.isBound . snd) (M.sitesWithId $ M.agents mix Vec.! aId)

        newId id (idMap, n) | id `Set.member` idSet = (idMap, n)
                            | otherwise = (Map.insert id n idMap, n+1)

        replaceIds graph = Map.fromList (replaceIds' <$> Map.toList graph)
          where replaceIds' ((a, i), (b, j)) = ((idMap Map.! a, i), (idMap Map.! b, j))

toKappa :: E.Env -> Rule -> String
toKappa env rule = M.toKappa env (lhs rule) ++ " -> " ++ M.toKappa env (rhs rule) ++ " @ " ++ showAExpr (rate rule)

