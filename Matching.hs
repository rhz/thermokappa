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
siteMatch :: M.Site -> M.Site -> Bool
siteMatch s1 s2 = M.internalState s1 `intMatch` M.internalState s2
               && M.bindingState  s1 `lnkMatch` M.bindingState  s2
  where
    intMatch :: Maybe M.InternalStateId -> Maybe M.InternalStateId -> Bool
    intMatch Nothing _ = True
    intMatch i1 i2 = i1 == i2

    lnkMatch :: M.BindingState -> M.BindingState -> Bool
    lnkMatch M.Unspecified _ = True
    lnkMatch M.SemiLink M.Bound = True
    lnkMatch b1 b2 = b1 == b2

agentMatch :: M.Agent -> M.Agent -> Bool
agentMatch a1 a2 = M.agentName a1 == M.agentName a2 -- same name
                && Vec.and (Vec.zipWith siteMatch (M.interface a1) (M.interface a2)) -- and same interface

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
                 unifiedIntf <- Vec.foldM siteUnify' [] sites
                 return $ a1{ M.interface = Vec.fromList $ reverse unifiedIntf }
  where
    sites = Vec.zip (M.interface a1) (M.interface a2)

    siteUnify' unifiedIntf (s1, s2) = do unifiedSite <- siteUnify s1 s2
                                         return (unifiedSite : unifiedIntf)

siteUnify :: M.Site -> M.Site -> Maybe M.Site
siteUnify s1 s2 =
  do unifiedInt <- M.internalState s1 `intUnify` M.internalState s2
     unifiedLnk <- M.bindingState  s1 `lnkUnify` M.bindingState  s2
     return M.Site{ M.internalState = unifiedInt
                  , M.bindingState  = unifiedLnk
                  }
  where -- More specific sites win
    intUnify :: Maybe M.InternalStateId -> Maybe M.InternalStateId -> Maybe (Maybe M.InternalStateId)
    intUnify (Just int1) (Just int2) | int1 == int2  = Just (Just int1)
                                     | otherwise     = Nothing
    intUnify Nothing Nothing = Just Nothing
    intUnify Nothing int2 = Just int2
    intUnify int1 Nothing = Just int1

    lnkUnify :: M.BindingState -> M.BindingState -> Maybe M.BindingState
    lnkUnify M.Free M.Free = Just M.Free
    lnkUnify M.Bound M.Bound = Just M.Bound
    lnkUnify M.SemiLink M.SemiLink = Just M.SemiLink

    lnkUnify M.SemiLink M.Bound = Just M.Bound
    lnkUnify M.Bound M.SemiLink = Just M.Bound

    lnkUnify lnk1 M.Unspecified = Just lnk1
    lnkUnify M.Unspecified lnk2 = Just lnk2

    lnkUnify _ _ = Nothing


-- unify was not the solution either, but it's perhaps useful for other things
-- maybe for computing the influence map
-- what we need now is a function that takes the intersection of two agents
-- (that is, the opossite of unify in the sense that it keeps the less specific sites)

-- Intersect
intersect :: M.Agent -> M.Agent -> [M.Agent]
intersect a1 a2 =
  do guard $ M.agentName a1 == M.agentName a2
     intf3 <- Vec.foldM siteIntersect [] (Vec.indexed sites)
     return a1{ M.interface = Vec.fromList $ reverse intf3 }
  where
    sites = Vec.zip (M.interface a1) (M.interface a2)

    siteIntersect :: [M.Site] -> (M.SiteId, (M.Site, M.Site)) -> [[M.Site]]
    siteIntersect intf (sId, (s1, s2)) =
      do int <- M.internalState s1 `intIntersect` M.internalState s2
         lnk <- M.bindingState  s1 `lnkIntersect` M.bindingState  s2
         let s3 = M.Site{ M.internalState = int
                        , M.bindingState  = lnk
                        }
         return (s3 : intf)

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


type PendingLinks = Map.Map M.SiteId M.Endpoint
type TodoMap = Map.Map (M.AgentId, M.AgentId) PendingLinks

-- Returns all possible pull-backs for the two mixtures
-- TODO how could I avoid using nub? why are there so many replicates? (ie, why are there so many ways to create the same intersection?)
--      is it because isomorphisms? if so, why are them all created in a syntacticly equivalent manner?
intersections :: M.Mixture -> M.Mixture -> [(M.Mixture, Matching, Matching)]
intersections m1 m2 = nub $ do (agents, graph, m1Matching, m2Matching, _, _, _) <- pullbacks [([], Map.empty, [], [], 0, ids1, ids2)]
                               let m3 = M.Mixture { M.agents = Vec.fromList $ reverse agents
                                                  , M.graph  = graph
                                                  }
                               return (m3, m1Matching, m2Matching)
  where
    ids1 = Set.fromList $ M.agentIds m1
    ids2 = Set.fromList $ M.agentIds m2

    isFinished :: ([M.Agent], M.Graph, Matching, Matching, M.AgentId, Set.Set M.AgentId, Set.Set M.AgentId) -> Bool
    isFinished (_, _, _, _, _, ids1, ids2) = Set.null ids1 || Set.null ids2

    pullbacks :: [([M.Agent], M.Graph, Matching, Matching, M.AgentId, Set.Set M.AgentId, Set.Set M.AgentId)]
              -> [([M.Agent], M.Graph, Matching, Matching, M.AgentId, Set.Set M.AgentId, Set.Set M.AgentId)]
    pullbacks intersections
      | null partialIntersections = completedIntersections
      | otherwise = completedIntersections ++ extendedIntersections
      where
        (completedIntersections, partialIntersections) = partition isFinished intersections
        extendedIntersections = pullbacks $
          do (agents, graph, m1Matching, m2Matching, id3, ids1, ids2) <- partialIntersections
             id2 <- Set.toList ids2
             let (id1, ids1') = Set.deleteFindMin ids1
                 intersections' = intersectAndExtend agents graph m1Matching m2Matching id1 id2 id3 ids1 ids2 Map.empty Map.empty
             (agents, graph, m1Matching, m2Matching, id3, ids1', ids2) : intersections'

    intersectAndExtend :: [M.Agent] -> M.Graph -> Matching -> Matching -> M.AgentId -> M.AgentId -> M.AgentId -> Set.Set M.AgentId -> Set.Set M.AgentId
                       -> PendingLinks -> TodoMap
                       -> [([M.Agent], M.Graph, Matching, Matching, M.AgentId, Set.Set M.AgentId, Set.Set M.AgentId)]
    intersectAndExtend agents graph m1Matching m2Matching id1 id2 id3 ids1 ids2 pendingLinks todo
      | Set.member id1 ids1 && Set.member id2 ids2 =
        do a3 <- intersect a1 a2
           -- for every bound site in a3, intersect and extend the neighbours
           let nbs = do (sId, M.Site{ M.bindingState = M.Bound }) <- M.sitesWithId a3
                        -- skip the sites you are coming from
                        guard $ sId `Map.notMember` pendingLinks
                        let nb1 = M.follow m1 (id1, sId) ? "Matching.intersections.intersectAndExtend: disconnected graph (1)"
                            nb2 = M.follow m2 (id2, sId) ? "Matching.intersections.intersectAndExtend: disconnected graph (2)"
                        return (sId, nb1, nb2)

               nonExtendibleSites = [ sId | (sId, (_, i), (_, j)) <- nbs, i /= j ]

               -- a pending link holds all the information that's needed to create the link
               -- with a neighbour until the agent id of the neighbour in m3 is known
               nbs'  = [ ((a, b), (i, (id3, sId))) | (sId, (a, i), (b, j)) <- nbs, i == j ] -- pending link: ((id3, sId), i)
               nbs'' = fst . head |.| Map.fromList . map snd <$> groupWith fst nbs' -- group pending links by neighbour
               todo' = Map.unionWith (Map.union) todo (Map.fromList nbs'')

               --agents' = foldl' (M.setLnk M.SemiLink) a3 semiLinks : agents
               agents' = a3 : agents
               graph'  = Map.foldrWithKey insertLink graph pendingLinks

               ids1' = Set.delete id1 ids1
               ids2' = Set.delete id2 ids2

           if null nonExtendibleSites
             then if Map.null todo'
                    then return (agents', graph', m1Matching', m2Matching', id3+1, ids1', ids2') -- there's nothing else to do, we are done
                    else let (((id1', id2'), pendingLinks'), todo'') = Map.deleteFindMin todo' in -- pick the next one and continue exteding
                         intersectAndExtend agents' graph' m1Matching' m2Matching' id1' id2' (id3+1) ids1' ids2' pendingLinks' todo''
             else [] -- there are sites that make m3 impossible to extend completely

      | otherwise = []
      where
        a1 = M.agents m1 Vec.! id1
        a2 = M.agents m2 Vec.! id2

        m1Matching' = (id1, id3) : m1Matching
        m2Matching' = (id2, id3) : m2Matching

        insertLink :: M.SiteId -> M.Endpoint -> M.Graph -> M.Graph
        insertLink sId ep = M.addLink ep (id3, sId)


type Injection = Map.Map M.AgentId M.AgentId
data LinkInfo = T1 | T2 | None

-- TODO this should be :: [M.Mixture] -> [(M.Mixture, [Injection], M.Mixture)]
minimalGlueings :: M.Mixture -> M.Mixture -> [(M.Mixture, Injection, Injection, M.Mixture)]
minimalGlueings m1 m2 =
  do (m0, m1Matchings, m2Matchings) <- intersections m1 m2
     (m3, m1Inj, m2Inj) <- refine m0 m1Matchings m2Matchings
     let (m3' , m1Inj') = addAndExtend m1 (m3 , m1Inj)
         (m3'', m2Inj') = addAndExtend m2 (m3', m2Inj)
     return (m0, m1Inj', m2Inj', m3'')
  where
    refine :: M.Mixture -> Matching -> Matching -> [(M.Mixture, Injection, Injection)]
    refine m0 m1Matchings m2Matchings = foldM refineAgent (m0, m1Inj, m2Inj) (M.agentIds m0)
      where
        m1Inj = Map.fromList m1Matchings
        m2Inj = Map.fromList m2Matchings
        bwdMap = Map.fromList [ (id3, (id1, id2)) | [(id1, id3), (id2, _)] <- groupWith snd (m1Matchings ++ m2Matchings) ]

        refineAgent :: (M.Mixture, Injection, Injection) -> M.AgentId -> [(M.Mixture, Injection, Injection)]
        refineAgent mg id3 = foldM refineSite' mg (M.siteIds a1) -- note that M.siteIds a1 == M.siteIds a2 == M.siteIds a3
          where
            (id1, id2) = bwdMap Map.! id3
            a1 = M.agents m1 Vec.! id1
            a2 = M.agents m2 Vec.! id2

            refineSite' :: (M.Mixture, Injection, Injection) -> M.SiteId -> [(M.Mixture, Injection, Injection)]
            refineSite' (m3, m1Inj, m2Inj) sId =
              do s3' <- toList $ siteUnify s1 s2
                 let a3' = a3{ M.interface = M.interface a3 Vec.// [(sId, s3')] }
                     m3' = m3{ M.agents    = M.agents    m3 Vec.// [(id3, a3')] }
                 -- TODO isn't there a better way to express this?
                 case linkInfo s1 s2 s3 of
                   T1   -> let (m3'', m1Inj') = extend m1 [(id1, id3, sId)] (m3', m1Inj) in return (m3'', m1Inj', m2Inj)
                   T2   -> let (m3'', m2Inj') = extend m2 [(id2, id3, sId)] (m3', m2Inj) in return (m3'', m1Inj, m2Inj')
                   None -> return (m3', m1Inj, m2Inj)
              where
                a3 = M.agents m3 Vec.! id3
                s1 = M.interface a1 Vec.! sId
                s2 = M.interface a2 Vec.! sId
                s3 = M.interface a3 Vec.! sId

                toList Nothing  = []
                toList (Just x) = [x]

                linkInfo :: M.Site -> M.Site -> M.Site -> LinkInfo
                linkInfo s1 s2 s3
                  | M.isBound s1 && not (M.isBound s2) && not (M.isBound s3) = T1
                  | not (M.isBound s1) && M.isBound s2 && not (M.isBound s3) = T2
                  | otherwise                                                = None

    addAndExtend :: M.Mixture -> (M.Mixture, Injection) -> (M.Mixture, Injection)
    addAndExtend m1 (m3, inj)
      | null remainingAgents  =  (m3, inj)
      | otherwise             =  addAndExtend m1 $ extend m1 sites (m3', inj')
      where
        remainingAgents = M.agentIds m1 \\ Map.keys inj
        id1  = head remainingAgents
        id3  = Vec.length (M.agents m3)
        a1   = M.agents m1 Vec.! id1
        m3'  = m3{ M.agents = Vec.snoc (M.agents m3) a1 } -- TODO beware!! Vec.snoc here!! possible memory leak!!
        inj' = Map.insert id1 id3 inj

        sites = do (sId, M.Site{ M.bindingState = M.Bound }) <- M.sitesWithId a1
                   return (id1, id3, sId)

extend :: M.Mixture -> [(M.AgentId, M.AgentId, M.SiteId)] -> (M.Mixture, Injection) -> (M.Mixture, Injection)
extend m1 [] (m3, inj) = (m3, inj)
extend m1 ((id1, id3, sId):sites) (m3, inj)
  | nbId1 `Map.member` inj = extend m1  sites            (M.bind (id3, sId) (nbId3, nbSiteId) m3 , inj )
  | otherwise              = extend m1 (sites ++ sites') (M.bind (id3, sId) (nbId3, nbSiteId) m3', inj')
  where
    (nbId1, nbSiteId) = M.follow m1 (id1, sId) ? "Matching.extend: missing link"
    nbId3  = Map.findWithDefault nbId3' nbId1 inj
    nbId3' = Vec.length (M.agents m3)

    -- add neighbour
    nb1  = M.agents m1 Vec.! nbId1
    m3'  = m3{ M.agents = Vec.snoc (M.agents m3) nb1 } -- Beware! Vec.snoc here! possible memory leak!!
    inj' = Map.insert nbId1 nbId3 inj

    -- collect new sites from nb1
    sites' = do (nbSiteId', M.Site{ M.bindingState = M.Bound }) <- M.sitesWithId nb1
                guard $ nbSiteId /= nbSiteId'
                return (nbId1, nbId3, nbSiteId')


{-
  where
    extend :: M.Mixture -> Matching -> Matching -> [(M.Mixture, (Injection, Injection))]
    extend m3 m1Matchings m2Matchings = extend' m3 m1Inj m2Inj sitesInM3
      where
        m1Inj = Map.fromList m1Matchings
        m2Inj = Map.fromList m2Matchings
        bwdMap = Map.fromList [ (id3, (id1, id2)) | [((id1, id3), (id2, _))] <- groupWith snd (m1Matchings ++ m2Matchings) ]

        sitesInM3 = do (id3, a3) <- M.agentsWithId m3
                       let (id1, id2) = bwdMap Map.! id3
                           a1 = m1 Vec.! id1
                           a2 = m2 Vec.! id2
                       (sId, s3) <- M.sitesWithId a3
                       let s1 = M.interface a1 Vec.! sId
                           s2 = M.interface a2 Vec.! sId
                       (s3', linkInfo) <- refineSite s1 s2 s3 -- this should be a Maybe
                       return (id3, (sId, (s3', linkInfo)))

        --extend' :: [(M.AgentId, (M.SiteId, (M.Site, LinkInfo)))] -> [(M.Mixture, (Injection, Injection))]
        refineAndExtend m3 m1Inj m2Inj [] = ...
        refineAndExtend m3 m1Inj m2Inj sitesInM3 = foldr extendSite
          where m3' = m3{ M.agents = M.agents m3 Vec.// refinedAgents }

                refinedAgents = do (id3, sitesInA3) <- fst . head |.| map snd <$> groupWith fst sitesInM3
                                   return (id3, refineAgent (M.agents m3 Vec.! id3) sitesInA3)

                --refineAgent :: M.Agent -> [(M.SiteId, (M.Site, LinkInfo))]) -> M.Agent
                refineAgent a3 sitesInA3 = a3{ M.interface = M.interface a3 Vec.// sites' }
                  where sites' = [ (sId, s3) | (sId, (s3, _)) <- sitesInA3 ]

                nbs = do (id3, (sId, (_, Just (nb1, m1)))) <- sitesInM3
                         return  ((id3, sId), (nb1, m1))

                -- get neighbours and check if they are in agent maps
                -- if they are not, add them to m3
                -- link them
                -- perhaps in some intermediate state I should remove duplicate links, e.g. when two sites in sitesInM3 must be linked
-}

