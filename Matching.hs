module Matching where

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, partition, (\\))

import qualified Mixture as M
import qualified Env as E
import Utils

-- The idea here is to find all possible non-isomorphic superpositions of two or more kappa terms
-- (each of them can have one or more connected components)

type Matching = [(M.AgentId, M.AgentId)]


-- Match
agentMatch :: M.Agent -> M.Agent -> Bool
agentMatch a1 a2 = M.agentName a1 == M.agentName a2 &&
                   Vec.all siteMatch (Vec.zip (M.interface a1) (M.interface a2))
  where
    siteMatch :: (M.Site, M.Site) -> Bool
    siteMatch (s1, s2) = M.internalState s1 `intMatch` M.internalState s2 &&
                         M.bindingState  s1 `lnkMatch` M.bindingState  s2
      where
        intMatch :: Maybe M.InternalStateId -> Maybe M.InternalStateId -> Bool
        intMatch Nothing Nothing = True
        intMatch (Just int1) (Just int2) | int1 == int2  = True
                                         | otherwise     = False
        intMatch Nothing (Just _) = True
        intMatch (Just _) Nothing = False

        lnkMatch :: M.BindingState -> M.BindingState -> Bool
        lnkMatch M.Free M.Free = True
        lnkMatch M.Bound M.Bound = True
        lnkMatch M.SemiLink M.SemiLink = True
        lnkMatch M.SemiLink M.Bound = True
        lnkMatch M.Unspecified _ = True
        lnkMatch _ _ = False

superpose :: M.Mixture -> M.Mixture -> [Matching]
superpose e1 e2 = fst <$> foldM match ([], M.agentsWithId e2) (M.agentsWithId e1) -- TODO verify links
  where match (matching, as2) (id1, a1) = do ((id2, a2), as2') <- select as2
                                             guard $ agentMatch a1 a2
                                             return ((id1, id2) : matching, as2')



-- The functions above are useful for other purposes, but not for what we need here
-- Anyway, they are a good first approach to the generic problem of matchings
-- But what we need is to find the agent that can 'unify' two agents into one

-- Unify
-- Returns the unified agent (ie, the superposition of both) and the site matchings from t1 and t2 agents
-- Note that unify a1 a2 == unify a2 a1 (up to site reordering within a multisite)
unify :: M.Agent -> M.Agent -> Maybe M.Agent
unify a1 a2 = do guard $ M.agentName a1 == M.agentName a2
                 unifiedIntf <- Vec.foldM siteUnify Vec.empty (Vec.indexed sites)
                 return $ M.Agent{ M.agentName = M.agentName a1
                                 , M.interface = unifiedIntf
                                 }
  where
    sites = Vec.zip (M.interface a1) (M.interface a2)

    siteUnify :: M.Interface -> (M.SiteId, (M.Site, M.Site)) -> Maybe M.Interface
    siteUnify unifiedIntf (sId, (s1, s2)) =
      do unifiedInt <- M.internalState s1 `intUnify` M.internalState s2
         unifiedLnk <- M.bindingState  s1 `lnkUnify` M.bindingState  s2
         let unifiedSite = M.Site{ M.internalState = unifiedInt
                                 , M.bindingState  = unifiedLnk
                                 }
         return $ Vec.snoc unifiedIntf unifiedSite

      where -- More specific sites win
        intUnify :: Maybe M.InternalStateId -> Maybe M.InternalStateId -> Maybe (Maybe M.InternalStateId)
        intUnify Nothing Nothing = Just Nothing
        intUnify (Just int1) (Just int2) | int1 == int2  = Just $ Just int1
                                         | otherwise     = Nothing
        intUnify Nothing (Just int2) = Just $ Just int2
        intUnify (Just int1) Nothing = Just $ Just int1

        lnkUnify :: M.BindingState -> M.BindingState -> Maybe M.BindingState
        lnkUnify M.Free M.Free = Just M.Free
        lnkUnify M.Bound M.Bound = Just M.Bound
        lnkUnify M.SemiLink M.SemiLink = Just M.SemiLink

        lnkUnify M.SemiLink M.Bound = Just M.Bound
        lnkUnify M.Bound M.SemiLink = Just M.Bound

        lnkUnify lnk1 M.Unspecified = Just lnk1
        lnkUnify M.Unspecified lnk2 = Just lnk2

        lnkUnify _ _ = Nothing


-- unify was not the solution either, but it's perhaps useful for other things (the obviously non-useful part has been deleted)
-- maybe for computing the influence map, let's see then
-- what we need now is a function that takes the intersection of two agents (that is, the opossite of unify
-- in the sense that it keeps the less specific sites)

-- Intersect
intersect :: M.Agent -> M.Agent -> [M.Agent]
intersect a1 a2 =
  do guard $ M.agentName a1 == M.agentName a2
     intf3 <- Vec.foldM siteIntersect Vec.empty (Vec.indexed sites)
     return a1{ M.interface = intf3 }
  where
    sites = Vec.zip (M.interface a1) (M.interface a2)

    siteIntersect :: M.Interface -> (M.SiteId, (M.Site, M.Site)) -> [M.Interface]
    siteIntersect intf (sId, (s1, s2)) =
      do int <- M.internalState s1 `intIntersect` M.internalState s2
         lnk <- M.bindingState  s1 `lnkIntersect` M.bindingState  s2
         let s3 = M.Site{ M.internalState = int
                        , M.bindingState  = lnk
                        }
         return $ Vec.snoc intf s3

    -- Less specific sites win
    intIntersect :: Maybe M.InternalStateId -> Maybe M.InternalStateId -> [Maybe M.InternalStateId]
    intIntersect (Just int1) (Just int2) | int1 == int2  = [Just int1]
                                         | otherwise     = []
    intIntersect Nothing Nothing = [Nothing]
    intIntersect Nothing (Just int2) = [Nothing]
    intIntersect (Just int1) Nothing = [Nothing]

    lnkIntersect :: M.BindingState -> M.BindingState -> [M.BindingState]
    lnkIntersect M.Free M.Free = [M.Free]
    lnkIntersect M.Bound M.Bound = [M.Bound]
    lnkIntersect M.SemiLink M.SemiLink = [M.SemiLink]

    lnkIntersect M.SemiLink M.Bound = [M.SemiLink]
    lnkIntersect M.Bound M.SemiLink = [M.SemiLink]

    lnkIntersect M.Unspecified lnk2 = [M.Unspecified]
    lnkIntersect lnk1 M.Unspecified = [M.Unspecified]

    lnkIntersect _ _ = []


type PendingLink = (M.Endpoint, M.SiteId)

-- Returns all possible pull-backs for the two mixtures
-- TODO how could I avoid using nub? why are there so many replicates? (ie, why are there so many ways to create the same intersection?)
intersections :: M.Mixture -> M.Mixture -> [(M.Mixture, (Matching, Matching))]
intersections m1 m2 = nub $ do (m3, m1Matching, m2Matching, _, _) <- pullbacks [(M.empty, [], [], ids1, ids2)]
                               return (m3, (m1Matching, m2Matching))
  where
    ids1 = Set.fromList $ M.agentIds m1
    ids2 = Set.fromList $ M.agentIds m2

    isFinished :: (M.Mixture, Matching, Matching, Set.Set M.AgentId, Set.Set M.AgentId) -> Bool
    isFinished (_, _, _, ids1, ids2) = Set.null ids1 || Set.null ids2

    pullbacks :: [(M.Mixture, Matching, Matching, Set.Set M.AgentId, Set.Set M.AgentId)] -> [(M.Mixture, Matching, Matching, Set.Set M.AgentId, Set.Set M.AgentId)]
    pullbacks intersections
      | null partialIntersections = completedIntersections
      | otherwise = completedIntersections ++ extendedIntersections
      where
        (completedIntersections, partialIntersections) = partition isFinished intersections
        extendedIntersections = pullbacks $
          do (mix, m1Matching, m2Matching, ids1, ids2) <- partialIntersections
             id2 <- Set.toList ids2
             let id1 = Set.findMin ids1
             (mix, m1Matching, m2Matching, Set.delete id1 ids1, ids2) : intersectAndExtend mix [((id1, id2), [])] ids1 ids2 m1Matching m2Matching

    intersectAndExtend :: M.Mixture -> [((M.AgentId, M.AgentId), [PendingLink])] -> Set.Set M.AgentId -> Set.Set M.AgentId -> Matching -> Matching -> [(M.Mixture, Matching, Matching, Set.Set M.AgentId, Set.Set M.AgentId)]
    intersectAndExtend mix [] ids1 ids2 m1Matching m2Matching = return (mix, m1Matching, m2Matching, ids1, ids2)
    intersectAndExtend mix (((id1, id2), pendingLinks):todo) ids1 ids2 m1Matching m2Matching
      | Set.member id1 ids1 && Set.member id2 ids2 =
        do a3 <- intersect a1 a2
           -- for every bound site in a3, intersect and extend the neighbours
           let nbs = do (sId, M.Site{ M.bindingState = M.Bound }) <- indexedList $ M.interface a3
                        let (nb1, nbSiteId1) = M.follow m1 (id1, sId) ? "Matching.intersections.intersectAndExtend: disconnected graph (1)"
                            (nb2, nbSiteId2) = M.follow m2 (id2, sId) ? "Matching.intersections.intersectAndExtend: disconnected graph (2)"
                            -- a pending link holds all the information that's needed to create the link
                            -- with a neighbour until the agent id of the neighbour in m3 is known
                            pendingLink = ((id3, sId), nbSiteId1)

                        if nbSiteId1 /= nbSiteId2
                          then error "Matching.intersections.intersectAndExtend: site ids in neighbour differ"
                          else return ((nb1, nb2), pendingLink)

               nbs' = map (fst . head |.| map snd) $ groupWith fst nbs -- group by neighbour
               mix' = M.Mixture{ M.agents = Vec.snoc (M.agents mix) a3
                               , M.graph = foldr insertLink (M.graph mix) pendingLinks
                               }

           intersectAndExtend mix' (todo ++ nbs') (Set.delete id1 ids1) (Set.delete id2 ids2) m1Matching' m2Matching'

      | Set.member id1 ids1 || Set.member id2 ids2 = [] -- TODO this is a little bit hacky... I should skip (id1, id2) beforehand to prevent the otherwise case and this should be the otherwise
      | otherwise = intersectAndExtend mix todo ids1 ids2 m1Matching m2Matching -- skip (id1, id2) -- this happens when coming back through a link

      where a1 = M.agents m1 Vec.! id1
            a2 = M.agents m2 Vec.! id2
            id3 = Vec.length (M.agents mix)

            m1Matching' = (id1, id3) : m1Matching
            m2Matching' = (id2, id3) : m2Matching

            insertLink :: PendingLink -> M.Graph -> M.Graph
            insertLink (ep, sId) = M.addLink ep (id3, sId)


type AgentMap = Map.Map M.AgentId M.AgentId

-- TODO this should be :: E.Env -> [M.Mixture] -> [(M.Mixture, [AgentMap])]
minimalGlueings :: M.Mixture -> M.Mixture -> [(M.Mixture, (AgentMap, AgentMap))]
minimalGlueings m1 m2 =
  do (m3, (m1Matchings, m2Matchings)) <- intersections m1 m2
     let (m3' , m1AgentMap) = extend m1 m1Matchings m3
         (m3'', m2AgentMap) = extend m2 m2Matchings m3'
     return (m3'', (m1AgentMap, m2AgentMap))
  where
    extend :: M.Mixture -> Matching -> M.Mixture -> (M.Mixture, AgentMap)
    extend m1 matchings m3 = addSites missingSites (m3, agentMap)
      where
        agentMap = Map.fromList matchings

        -- Find the sites missing in the agents that have been intersected
        missingSites = do (aId1, aId3) <- matchings
                          (sId, M.Site{ M.internalState = Nothing, M.bindingState = M.Unspecified }) <- indexedList $ M.interface (M.agents m3 Vec.! aId3)
                          guard . M.isBound $ M.interface (M.agents m1 Vec.! aId1) Vec.! sId
                          return ((aId1, sId), aId3)

        addSites :: [(M.Endpoint, M.AgentId)] -> (M.Mixture, AgentMap) -> (M.Mixture, AgentMap)
        addSites [] (m3, agentMap)
          | null remainingAgents  =  (m3, agentMap)
          | otherwise             =  addSites newSites (m3', agentMap)
          where
            remainingAgents = M.agentIds m1 \\ Map.keys agentMap

            aId1 = head remainingAgents
            aId3 = Vec.length (M.agents m3)

            a1 = M.agents m1 Vec.! aId1
            m3' = m3{ M.agents = Vec.snoc (M.agents m3) a1 }
            newSites = do (sId, M.Site{ M.bindingState = M.Bound }) <- indexedList $ M.interface a1
                          return ((aId1, sId), aId3)

        addSites (((aId1, sId), aId3):missingSites) (m3, agentMap)
          | nbId1 `Map.member` agentMap  =  addSites  missingSites              (m3' , agentMap') -- link
          | otherwise                    =  addSites (missingSites ++ newSites) (m3'', agentMap') -- append agent and link
          where
            (nbId1, nbSiteId) = M.follow m1 (aId1, sId) ? "Matching.minimalGlueing.extend: missing link"

            agent  = M.agents m3 Vec.! aId3
            agent' = agent{ M.interface = M.interface agent Vec.// [(sId, site')] }
            agents' = M.agents m3 Vec.// [(aId3, agent')]

            site  = M.interface agent Vec.! sId
            site' = site{ M.bindingState = M.Bound }

            nbId3 = Map.findWithDefault (Vec.length agents') nbId1 agentMap
            graph' = M.addLink (aId3, sId) (nbId3, nbSiteId) (M.graph m3) -- add link
            m3' = m3{ M.agents = agents', M.graph  = graph' }

            -- Add neighbour
            nb1 = M.agents m1 Vec.! nbId1
            m3'' = m3'{ M.agents = Vec.snoc agents' nb1 }

            agentMap' = Map.insert aId1 aId3 $ Map.insert nbId1 nbId3 agentMap

            -- Collect new sites from nb1
            newSites = do (nbSiteId', M.Site{ M.bindingState = M.Bound }) <- indexedList $ M.interface nb1
                          guard $ nbSiteId' /= nbSiteId
                          return ((nbId1, nbSiteId'), nbId3)

