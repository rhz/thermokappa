{-# LANGUAGE TupleSections #-}

module Rule where

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import qualified KappaParser as KP
import qualified Mixture as M
import qualified Env as E
import Utils
import PlainKappa (showAExpr)

type Injection = Map.Map M.AgentId M.AgentId -- TODO should this be here or in Matching or elsewhere?

data Action = Mod M.Endpoint M.InternalStateId
            | Bnd M.Endpoint M.Endpoint
            | Brk M.Endpoint
            | Add M.Agent
            | Del M.AgentId
  deriving (Show, Eq)

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
actionScript lhs rhs = map Add added ++ removeDups [] (concatMap diff common) ++ map Del deletedIds
  where
    prefixId = largestPrefix' lhs rhs
    l = M.agents lhs
    r = M.agents rhs
    common = Vec.toList . Vec.take prefixId . Vec.indexed $ Vec.zip l r
    added  = Vec.toList $ Vec.drop prefixId r
    deletedIds = [prefixId..Vec.length l - 1]

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

    removeDups acc [] = acc
    removeDups acc (x@(Bnd (a, i) (b, j)) : xs) = removeDups (x:acc) (filter (/= Bnd (b, j) (a, i)) xs)
    removeDups acc (x@(Brk (a, i)) : xs) = removeDups (x:acc) xs'
      where xs' | Just (b, j) <- M.follow lhs (a, i) = filter (/= Brk (b, j)) xs
                | otherwise                          = xs
    removeDups acc (x:xs) = removeDups (x:acc) xs

-- TODO How should I handle binding to new agents?
apply :: [Action] -> Injection -> M.Mixture -> M.Mixture
apply [] _ mix = mix
apply (Mod (aId, sId) x  : actions) inj mix = apply actions inj $ M.setIntInMix (Just x) (inj !? aId ? "error 1", sId) mix
apply (Bnd (a, i) (b, j) : actions) inj mix = apply actions inj $ M.bind (inj !? a ? "error 2", i) (inj !? b ? "error 3: " ++ show (a, i) ++ " binds " ++ show (b, j) ++ " in " ++ show mix ++ " (emb: " ++ show inj ++ ")", j) mix
apply (Brk (a, i)        : actions) inj mix = apply actions inj $ M.unbind (inj !? a ? "error 4", i) mix
apply (Add agent         : actions) inj mix = apply actions inj mix{  M.agents = M.agents mix `Vec.snoc` agent } -- Beware! Vec.snoc = memory leak!!
apply (Del aId           : actions) inj mix = apply actions inj mix'{ M.agents = prefix Vec.++ Vec.tail suffix }
  where
    sites = map fst $ filter (M.isBound . snd) (indexedList . M.interface $ M.agents mix Vec.! aId')
    aId' = inj !? aId ? "error 5"
    mix' = foldr unbind mix sites
    unbind sId mix = M.unbind (aId', sId) mix
    (prefix, suffix) = Vec.splitAt aId' (M.agents mix')


toKappa :: E.Env -> Rule -> String
toKappa env rule = M.toKappa env (lhs rule) ++ " -> " ++ M.toKappa env (rhs rule) ++ " @ " ++ showAExpr (rate rule)

