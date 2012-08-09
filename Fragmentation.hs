{-# LANGUAGE TupleSections #-}

module Fragmentation where

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified KappaParser as KP
import qualified Mixture as M
import qualified Rule as R
import Matching
import Utils

type Obs = M.Mixture
type ODET = (R.Rate, [Obs]) -- ODE term
type ODE = [ODET]
type ODES = [(Obs, ODE)] -- ODE system, that is [( Obs, [(R.Rate, [Obs])] )]

-- TODO check if the first two tests are any good for performance
--      the first probably is, but the second one I'm not that sure
ccIso :: M.Mixture -> M.Mixture -> Bool
ccIso m1 m2 = agentCount m1 == agentCount m2 -- same amount of agents
           && typeCount  m1 == typeCount  m2 -- same 'typing map'
           && (agentCount m1 == 0 || any (match Set.empty Set.empty) anchors)
  where
    agentCount = Vec.length  . M.agents
    typeCount  = frequencies . map M.agentName . agentList
    agentList  = Vec.toList  . M.agents

    anchors = return . (0, ) <$> M.agentIds m2

    -- This function assumes the given m1 and m2 are connected components, it doesn't check if that's true.
    -- That's why we return True when we don't have anything else in the todo list,
    -- because that means we have visited all agents in both m1 and m2
    match :: Set.Set M.AgentId -> Set.Set M.AgentId -> [(M.AgentId, M.AgentId)] -> Bool
    match visited1 visited2 [] = True
    match visited1 visited2 ((id1, id2) : todo)
      | Set.member id1 visited1 && Set.member id2 visited2 = match visited1 visited2 todo -- TODO should I check that all nbs are visited?
      | Set.member id1 visited1 = error "Fragmentation.ccIso.match: id1 visited but not id2"
      | Set.member id2 visited2 = error "Fragmentation.ccIso.match: id2 visited but not id1"
      | a1 == a2 = match (Set.insert id1 visited1) (Set.insert id2 visited2) (todo ++ nbs)
      | otherwise = False
      where
        nbs = do (sId, (M.Site{ M.bindingState = M.Bound}, M.Site{ M.bindingState = M.Bound })) <- indexedList $ Vec.zip (M.interface a1) (M.interface a2)
                 let (nb1, _) = M.follow m1 (id1, sId) ? "Fragmentation.ccIso.match: disconnected graph (1)"
                     (nb2, _) = M.follow m2 (id2, sId) ? "Fragmentation.ccIso.match: disconnected graph (2)"
                 return (nb1, nb2)
        -- nbsVisited = all (visited1 `Set.member`) (map fst nbs) && all (visited2 `Set.member`) (map snd nbs)

        a1 = M.agents m1 Vec.!? id1 ? "Fragmentation.ccIso.match: agent id not found"
        a2 = M.agents m2 Vec.!? id2 ? "Fragmentation.ccIso.match: agent id not found"


fragment :: [R.Rule] -> [Obs] -> ODES
fragment rules obss = fragment' obss []
  where
    fragment' :: [Obs] -> [Obs] -> ODES
    fragment' [] _ = []
    fragment' (obs:todo) visited
      | any (ccIso obs) visited = fragment' todo visited
      | otherwise               = (obs, ode) : fragment' (todo ++ newFragments) (obs : visited)
      where
        ode = getODE obs =<< rules
        newFragments = snd =<< ode

    getODE :: Obs -> R.Rule -> ODE
    getODE obs rule = lhsTerms ++ rhsTerms
      where
        (msitesLhs, msitesRhs) = R.modifiedSites rule
        minglueingsLhs = minimalGlueings obs (R.lhs rule)
        minglueingsRhs = minimalGlueings obs (R.rhs rule)

        -- you have to filter out the minimal glueings that don't intersect in the pullback with the set of modified sites
        -- when you glue on the left, the new observable is just the minimal glueing
        -- when you glue on the right, the new observable is the inverse of the rule applied to the minimal glueing

        lhsRelevantMG = filter (isRelevant msitesLhs (R.lhs rule)) minglueingsLhs
        rhsRelevantMG = filter (isRelevant msitesRhs (R.rhs rule)) minglueingsRhs

        isRelevant :: [M.Endpoint] -> M.Mixture -> (M.Mixture, Injection, Injection, M.Mixture) -> Bool
        isRelevant msites m2 (m0, _, m2Inj, _) = any inPullback msites
          where
            inPullback (aId, sId) = agentInPullback && siteInPullback -- s0 `siteMatch` s2
              where id0 = m2Inj Map.! aId
                    agentInPullback = id0 < Vec.length (M.agents m0)

                    s0 = M.interface (M.agents m0 Vec.! id0) Vec.! sId
                    siteInPullback = not $ M.isUnspecified s0

        lhsTerms = (neg $ R.rate rule, ) <$> M.split <$> codomain    <$> lhsRelevantMG
        rhsTerms = (      R.rate rule, ) <$> M.split <$> invert rule <$> rhsRelevantMG

        codomain (_, _, _, m3) = m3

        invert :: R.Rule -> (M.Mixture, Injection, Injection, M.Mixture) -> M.Mixture
        invert rule (_, _, rhsInj, m3) = snd $ R.apply invertedScript (rhsInj, m3)
          where invertedScript = R.actionScript (R.rhs rule) (R.lhs rule)


neg :: KP.AExpr -> KP.AExpr
neg (KP.Integer n) = KP.Integer (negate n)
neg (KP.Float n) = KP.Float (negate n)
neg x = KP.Duo KP.Mult (KP.Integer (-1)) x

