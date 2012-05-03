module Matching where

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (delete, nub, nubBy, unzip4, intercalate, lookup, partition, (\\))
import Data.Tuple (swap)
import Data.Maybe (catMaybes, fromJust)
import System.IO.Unsafe

import qualified Mixture as M
import qualified Env as E
import Utils

-- The idea here is to find all possible non-isomorphic superpositions of two or more kappa terms
-- (each of them can have one or more connected components)

type SiteMatching = (M.SiteId, M.SiteId)
type SiteMatchings = Vec.Vector [SiteMatching] -- indexed by multisite id

type Matching = (M.AgentId, M.AgentId)
type Glueing = [(Matching, SiteMatchings)] -- TODO this should be named CompleteMatching, as glueings are something else


indexedList :: Vec.Vector a -> [(Int, a)]
indexedList = Vec.toList . Vec.indexed

agentIds :: M.Mixture -> [M.AgentId]
agentIds m = [0..Vec.length (M.agents m) - 1]

agentsWithId :: M.Mixture -> [(M.AgentId, M.Agent)]
agentsWithId = indexedList . M.agents

fillMultiSite :: E.Env -> (M.AgentNameId, M.MultiSiteId) -> M.MultiSite -> M.MultiSite
fillMultiSite env (agentNameId, multisiteId) mss = mss Vec.++ Vec.replicate (numSites - Vec.length mss) unspecifiedSite
  where numSites = E.numSites env (agentNameId, multisiteId)
        unspecifiedSite = M.Site{ M.internalState = Nothing, M.bindingState = M.Unspecified }

removeUnspecified :: M.MultiSite -> M.MultiSite
removeUnspecified = Vec.filter (not . isUnspecified)

isUnspecified :: M.Site -> Bool
isUnspecified (M.Site{ M.internalState = Nothing, M.bindingState = M.Unspecified }) = True
isUnspecified _ = False

-- Match
agentMatch :: M.Agent -> M.Agent -> [SiteMatchings]
agentMatch a1 a2 = do guard $ M.agentName a1 == M.agentName a2
                      Vec.foldM siteMatch Vec.empty $ Vec.zip (M.interface a1) (M.interface a2)
  where siteMatch :: SiteMatchings -> (M.MultiSite, M.MultiSite) -> [SiteMatchings]
        siteMatch matching (ms1, ms2) = map (Vec.snoc matching . fst) $ Vec.foldM multiMatch ([], indexedList ms2) (Vec.indexed ms1)

          where multiMatch (m, l2) (siteId1, s1) = [ ((siteId1, siteId2) : m, delete (siteId2, s2) l2) |
                                                     (siteId2, s2) <- l2 ,
                                                     M.internalState s1 `intMatch` M.internalState s2 &&
                                                     M.bindingState  s1 `lnkMatch` M.bindingState  s2 ]

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

superpose :: M.Mixture -> M.Mixture -> [Glueing]
superpose e1 e2 = map fst $ foldM match ([], agentsWithId e2) (agentsWithId e1) -- FIXME verify links
  where match (m, l2) (id1, a1) = do (id2, a2) <- l2
                                     siteMatchings <- agentMatch a1 a2
                                     return (((id1, id2), siteMatchings) : m, delete (id2, a2) l2)



-- The functions above are useful for other purposes, but not for what we need here
-- Anyway, they are a good first approach to the generic problem of matchings
-- But what we need is to find the agent that can 'unify' two agents into one

-- Unify
-- Returns the unified agent (ie, the superposition of both) and the site matchings from t1 and t2 agents
-- Note that unify a1 a2 == unify a2 a1 (up to site reordering within a multisite)
unify :: E.Env -> M.Agent -> M.Agent -> [(M.Agent, SiteMatchings, SiteMatchings)]
unify env a1 a2 = nub $
  do guard $ M.agentName a1 == M.agentName a2

     let sites = Vec.zip (M.interface a1) (M.interface a2)
     (unifiedIntf, smatchings1, smatchings2) <- Vec.foldM siteUnify (Vec.empty, Vec.empty, Vec.empty) (Vec.indexed sites)

     let unifiedAgent = M.Agent{ M.agentName = M.agentName a1
                               , M.interface = unifiedIntf
                               }
     return (unifiedAgent, smatchings1, smatchings2)

  where siteUnify ::  (M.Interface, SiteMatchings, SiteMatchings) -> (M.MultiSiteId, (M.MultiSite, M.MultiSite))
                  -> [(M.Interface, SiteMatchings, SiteMatchings)]
        siteUnify (unifiedIntf, smatchings1, smatchings2) (multisiteId, (mss1, mss2)) =
          do (unifiedSite, smatching1, smatching2, nonUnifiedSites) <- Vec.foldM multiUnify (Vec.empty, [], [], mss2') mss1'
             guard $ null nonUnifiedSites -- every site from a1 and a2 must be unified
             return ( Vec.snoc unifiedIntf (removeUnspecified unifiedSite)
                    , Vec.snoc smatchings1 smatching1
                    , Vec.snoc smatchings2 smatching2
                    )

          where mss1' = Vec.indexed $ fillMultiSite env (M.agentName a1, multisiteId) mss1
                mss2' = indexedList $ fillMultiSite env (M.agentName a2, multisiteId) mss2

                multiUnify (ms, smatching1, smatching2, l2) (siteId1, s1) =
                  do ((siteId2, s2), l2') <- select l2
                     unifiedInt <- M.internalState s1 `intUnify` M.internalState s2
                     unifiedLnk <- M.bindingState  s1 `lnkUnify` M.bindingState  s2
                     let unifiedSite = M.Site{ M.internalState = unifiedInt
                                             , M.bindingState  = unifiedLnk
                                             }
                         siteId3 = Vec.length ms
                         smatching1' = (siteId1, siteId3) : smatching1
                         smatching2' = (siteId2, siteId3) : smatching2
                     return (Vec.snoc ms unifiedSite, smatching1', smatching2', l2')

                -- More specific sites win
                intUnify :: Maybe M.InternalStateId -> Maybe M.InternalStateId -> [Maybe M.InternalStateId]
                intUnify Nothing Nothing = [Nothing]
                intUnify (Just int1) (Just int2) | int1 == int2  = [Just int1]
                                                 | otherwise     = []
                intUnify Nothing (Just int2) = [Just int2]
                intUnify (Just int1) Nothing = [Just int1]

                lnkUnify :: M.BindingState -> M.BindingState -> [M.BindingState]
                lnkUnify M.Free M.Free = [M.Free]
                lnkUnify M.Bound M.Bound = [M.Bound]
                lnkUnify M.SemiLink M.SemiLink = [M.SemiLink]

                lnkUnify M.SemiLink M.Bound = [M.Bound]
                lnkUnify M.Bound M.SemiLink = [M.Bound]

                lnkUnify lnk1 M.Unspecified = [lnk1]
                lnkUnify M.Unspecified lnk2 = [lnk2]

                lnkUnify _ _ = []



-- unify was not the solution either, but it's perhaps useful for other things (the obviously non-useful part has been deleted)
-- maybe for computing the influence map, let's see then
-- what we need now is a function that takes the intersection of two agents (that is, the opossite of unify
-- in the sense that it keeps the less specific sites)

-- Intersect
type PendingLink = (M.Endpoint, ((M.MultiSiteId, M.MultiSiteId), (M.SiteId, M.SiteId)))

-- Returns all possible pull-backs for the two mixtures
--intersections :: E.Env -> M.Mixture -> M.Mixture -> [(M.Mixture, Glueing, Glueing)]
intersections env m1 m2 = nub $ pullbacks [(M.empty, [], [], ids1, ids2, [])] -- TODO why is nub necessary?
  where
    ids1 = Set.fromList $ agentIds m1
    ids2 = Set.fromList $ agentIds m2

    --isFinished :: (M.Mixture, Glueing, Glueing, Set.Set M.AgentId, Set.Set M.AgentId) -> Bool
    isFinished (_, _, _, ids1, ids2, _) = Set.null ids1 || Set.null ids2

    --pullbacks :: [(M.Mixture, Glueing, Glueing, Set.Set M.AgentId, Set.Set M.AgentId)] -> [(M.Mixture, Glueing, Glueing, Set.Set M.AgentId, Set.Set M.AgentId)]
    pullbacks intersections
      | null partialIntersections = completedIntersections
      | otherwise = completedIntersections ++ extendedIntersections
      where (completedIntersections, partialIntersections) = partition isFinished intersections
            extendedIntersections = pullbacks $
              do (mix, m1Glueing, m2Glueing, ids1, ids2, seeds) <- partialIntersections
                 id2 <- Set.toList ids2
                 let id1 = Set.findMin ids1
                     ext = do (mix, m1Glueing, m2Glueing, ids1', ids2') <- intersectAndExtend mix [((id1, id2), [])] ids1 ids2 m1Glueing m2Glueing
                              return (mix, m1Glueing, m2Glueing, ids1', ids2', ((id1, ids1, ids1'), (id2, ids2, ids1')):seeds)
                 (mix, m1Glueing, m2Glueing, Set.delete id1 ids1, ids2, seeds) : ext

    --intersectAndExtend :: M.Mixture -> [((M.AgentId, M.AgentId), [PendingLink])] -> Set.Set M.AgentId -> Set.Set M.AgentId -> Glueing -> Glueing -> [(M.Mixture, Glueing, Glueing, Set.Set M.AgentId, Set.Set M.AgentId)]
    intersectAndExtend mix [] ids1 ids2 m1Glueing m2Glueing = return (mix, m1Glueing, m2Glueing, ids1, ids2)
    intersectAndExtend mix (((id1, id2), pendingLinks):todo) ids1 ids2 m1Glueing m2Glueing
      | Set.member id1 ids1 && Set.member id2 ids2 =
        do (a3, smatchings1, smatchings2) <- intersect env a1 a2
           let m1Glueing' = ((id1, id3), smatchings1) : m1Glueing
               m2Glueing' = ((id2, id3), smatchings2) : m2Glueing
               -- For every bound site in a3, intersect and extend the neighbours
               nbs = [ ((nb1, nb2), (ep3, ((nbMultisiteId1, nbMultisiteId2), (nbSiteId1, nbSiteId2)))) |
                       (multisiteId, (mss, sm1, sm2)) <- indexedList $ Vec.zip3 (M.interface a3) smatchings1 smatchings2 ,
                       (siteId3, M.Site{ M.bindingState = M.Bound }) <- indexedList mss ,
                       let siteId1 = lookup siteId3 (map swap sm1) ? "Matching.intersections.intersectAndExtend: incomplete site matchings (1)"
                           siteId2 = lookup siteId3 (map swap sm2) ? "Matching.intersections.intersectAndExtend: incomplete site matchings (2)"
                           ep1 = (id1, multisiteId, siteId1)
                           ep2 = (id2, multisiteId, siteId2)
                           ep3 = (id3, multisiteId, siteId3)
                           (nb1, nbMultisiteId1, nbSiteId1) = M.follow m1 ep1 ? "Matching.intersections.intersectAndExtend: disconnected graph (1)"
                           (nb2, nbMultisiteId2, nbSiteId2) = M.follow m2 ep2 ? "Matching.intersections.intersectAndExtend: disconnected graph (2)" ]

               nbs' = map (fst . head |.| map snd) $ groupWith fst nbs
               mix' = M.Mixture{ M.agents = Vec.snoc (M.agents mix) a3
                               , M.graph = foldr (insertLink smatchings1 smatchings2) (M.graph mix) pendingLinks
                               }

           intersectAndExtend mix' (todo ++ nbs') (Set.delete id1 ids1) (Set.delete id2 ids2) m1Glueing' m2Glueing'

      | Set.member id1 ids1 || Set.member id2 ids2 = [] -- TODO this is kind of hacky... I should skip (id1, id2) beforehand to prevent the otherwise case and this should be the otherwise
      | otherwise = intersectAndExtend mix todo ids1 ids2 m1Glueing m2Glueing -- skip (id1, id2) -- this happens when coming back through a link

      where a1 = M.agents m1 Vec.! id1
            a2 = M.agents m2 Vec.! id2
            id3 = Vec.length (M.agents mix)

            -- TODO I should use forward/backward maps here instead of glueings
            insertLink :: SiteMatchings -> SiteMatchings -> PendingLink -> M.Graph -> M.Graph
            insertLink smatchings1 smatchings2 (ep, ((multisiteId1, multisiteId2), (siteId1, siteId2)))
              | multisiteId1 == multisiteId2 && siteId3 == siteId3' = M.addLink ep (id3, multisiteId1, siteId3)
              | otherwise = error "Matching.intersections.intersectAndExtend: oops"
              where siteId3  = lookup siteId1 (smatchings1 Vec.! multisiteId1) ? "Matching.intersections.insertLink: missing site in matching (1)"
                    siteId3' = lookup siteId2 (smatchings2 Vec.! multisiteId2) ? "Matching.intersections.insertLink: missing site in matching (2)"


intersect :: E.Env -> M.Agent -> M.Agent -> [(M.Agent, SiteMatchings, SiteMatchings)]
intersect env a1 a2 =
  do guard $ M.agentName a1 == M.agentName a2
     (intf3, smatchings1, smatchings2) <- Vec.foldM multiIntersect (Vec.empty, Vec.empty, Vec.empty) (Vec.indexed sites)
     let a3 = a1{ M.interface = intf3 }
     return (a3, smatchings1, smatchings2)
  where
    sites = Vec.zip (M.interface a1) (M.interface a2)

    multiIntersect :: (M.Interface, SiteMatchings, SiteMatchings) -> (M.MultiSiteId, (M.MultiSite, M.MultiSite)) -> [(M.Interface, SiteMatchings, SiteMatchings)]
    multiIntersect (intf, smatchings1, smatchings2) (multisiteId, (mss1, mss2)) =
      do (multisite, smatching1, smatching2, remainingSites) <- Vec.foldM siteIntersect (Vec.empty, [], [], mss2') mss1'
         return ( Vec.snoc intf multisite
                , Vec.snoc smatchings1 smatching1
                , Vec.snoc smatchings2 smatching2
                )

      where mss1' = Vec.indexed $ fillMultiSite env (M.agentName a1, multisiteId) mss1
            mss2' = indexedList $ fillMultiSite env (M.agentName a2, multisiteId) mss2

            siteIntersect :: (M.MultiSite, [SiteMatching], [SiteMatching], [(M.SiteId, M.Site)]) -> (M.SiteId, M.Site) -> [(M.MultiSite, [SiteMatching], [SiteMatching], [(M.SiteId, M.Site)])]
            siteIntersect (ms, smatching1, smatching2, l2) (siteId1, s1) =
              do ((siteId2, s2), l2') <- select l2
                 int <- M.internalState s1 `intIntersect` M.internalState s2
                 lnk <- M.bindingState  s1 `lnkIntersect` M.bindingState  s2
                 let s3 = M.Site{ M.internalState = int
                                , M.bindingState  = lnk
                                }
                     siteId3 = Vec.length ms
                     smatching1' = (siteId1, siteId3) : smatching1
                     smatching2' = (siteId2, siteId3) : smatching2
                 if isUnspecified s3
                   then return (ms, smatching1, smatching2, l2')
                   else return (Vec.snoc ms s3, smatching1', smatching2', l2')

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

    lnkIntersect M.SemiLink M.Bound = [M.SemiLink] -- TODO is this correct??
    lnkIntersect M.Bound M.SemiLink = [M.SemiLink]

    lnkIntersect M.Unspecified lnk2 = [M.Unspecified]
    lnkIntersect lnk1 M.Unspecified = [M.Unspecified]

    lnkIntersect _ _ = []


type FwdMap = Map.Map M.Endpoint M.Endpoint
type BwdMap = Map.Map M.Endpoint M.Endpoint
type Embedding = (FwdMap, BwdMap)

type AgentMap = Map.Map M.AgentId M.AgentId

mkFwdMap :: Glueing -> FwdMap
mkFwdMap glueings = Map.fromList $
  do ((aId1, aId2), msmatchings) <- glueings
     (msId, smatchings) <- indexedList msmatchings
     (sId1, sId2) <- smatchings
     return ((aId1, msId, sId1), (aId2, msId, sId2))


minimalGlueings :: E.Env -> M.Mixture -> M.Mixture -> [(M.Mixture, (FwdMap, AgentMap), (FwdMap, AgentMap))]
minimalGlueings env m1 m2 =
  do (m3  , m1Glueings, m2Glueings, _, _, _) <- intersections env m1 m2
     let (m3' , m1FwdMap, m1AgentMap) = extend m1 m1Glueings m3
         (m3'', m2FwdMap, m2AgentMap) = extend m2 m2Glueings m3'
     return (m3'', (m1FwdMap, m1AgentMap), (m2FwdMap, m2AgentMap))
  where
    extend :: M.Mixture -> Glueing -> M.Mixture -> (M.Mixture, FwdMap, AgentMap)
    extend m1 glueings m3 = addSites missingSites (m3, fwdMap, agentMap)
      where
        fwdMap = mkFwdMap glueings
        agentMap = Map.fromList $ map fst glueings

        -- Find the sites missing in the agents that have been intersected
        missingSites = do (aId1, aId3) <- map fst glueings
                          (msId, ms) <- indexedList $ M.interface (M.agents m1 Vec.! aId1)
                          (sId1, _)  <- indexedList ms
                          guard $ Map.notMember (aId1, msId, sId1) fwdMap
                          return ((aId1, msId, sId1), aId3)

        addSites :: [(M.Endpoint, M.AgentId)] -> (M.Mixture, FwdMap, AgentMap) -> (M.Mixture, FwdMap, AgentMap)
        addSites [] (m3, fwdMap, agentMap)
          | null remainingAgents = (m3, fwdMap, agentMap)
          | otherwise = addSites newSites (m3', fwdMap, agentMap)
          where remainingAgents = agentIds m1 \\ Map.keys agentMap

                aId1 = head remainingAgents
                a1 = M.agents m1 Vec.! aId1
                a3 = a1{ M.interface = M.emptyInterface env (M.agentName a1) }
                aId3 = Vec.length (M.agents m3)

                m3' = m3{ M.agents = Vec.snoc (M.agents m3) a3 }
                newSites = do (msId, ms) <- indexedList $ M.interface a1
                              (sId1, _)  <- indexedList ms
                              return ((aId1, msId, sId1), aId3)

        addSites ((ep1@(aId1, msId, sId1), aId3):missingSites) (m3, fwdMap, agentMap)
          | nbId1 `Map.member` agentMap  =  addSites  missingSites              (m3' , fwdMap' , agentMap') -- link
          | otherwise                    =  addSites (missingSites ++ newSites) (m3'', fwdMap'', agentMap') -- append agent and link
          where
            nbEp1@(nbId1, nbMsId, nbSiteId1) = M.follow m1 ep1 ? "Matching.minimalGlueing.extend: missing link"

            agent  = M.agents m3 Vec.! aId3
            agent' = agent{ M.interface = M.interface agent Vec.// [(msId, multisite')] }
            agents' = M.agents m3 Vec.// [(aId3, agent')]

            multisite  = M.interface agent Vec.! msId
            multisite' = Vec.snoc multisite site -- add site

            site = M.interface (M.agents m1 Vec.! aId1) Vec.! msId Vec.! sId1
            sId3 = Vec.length multisite

            nbEp3@(nbId3, _, nbSiteId3) = Map.findWithDefault nbEp3' nbEp1 fwdMap
            nbEp3' = (Vec.length agents', nbMsId, 0)

            graph' | M.isBound site = M.addLink (aId3, msId, sId3) nbEp3 (M.graph m3) -- add link
                   | otherwise = M.graph m3

            m3' = m3{ M.agents = agents', M.graph  = graph' }

            -- Add neighbour
            nb1 = M.agents m1 Vec.! nbId1
            nbSite = M.interface (M.agents m1 Vec.! nbId1) Vec.! nbMsId Vec.! nbSiteId1
            nbIntf = M.emptyInterface env (M.agentName nb1) Vec.// [(nbMsId, Vec.singleton nbSite)]
            nb3 = nb1{ M.interface = nbIntf }

            agents'' = Vec.snoc agents' nb3
            m3'' = m3'{ M.agents = agents'' }

            fwdMap'  = Map.insert ep1 (aId3, msId, sId3) fwdMap
            fwdMap'' = Map.insert nbEp1 nbEp3 fwdMap'

            agentMap' = Map.insert aId1 aId3 $ Map.insert nbId1 nbId3 agentMap

            -- Collect new sites from nb1
            newSites = do (nbMsId, ms) <- indexedList $ M.interface nb1
                          (nbSiteId1, _) <- indexedList ms
                          guard $ (nbId1, nbMsId, nbSiteId1) /= nbEp1
                          return ((nbId1, nbMsId, nbSiteId1), nbId3)


toDot :: E.Env -> M.Mixture -> M.Mixture -> M.Mixture -> (FwdMap, AgentMap) -> (FwdMap, AgentMap) -> String
toDot env m1 m2 m3 (m1FwdMap, m1AgentMap) (m2FwdMap, m2AgentMap) =
  "digraph {\n" ++
  "  graph [ overlap = \"scale\" ];\n" ++
  "  node [ shape = \"box\" ];\n" ++
  "  m1 [ label = \"" ++ M.toKappa env m1 ++ "\" ];\n" ++
  "  m2 [ label = \"" ++ M.toKappa env m2 ++ "\" ];\n" ++
  "  m3 [ label = \"" ++ M.toKappa env m3 ++ "\" ];\n" ++
  "  m1 -> m3 [ label = \"" ++ show (Map.toList m1AgentMap) ++ "\" ];\n" ++
  "  m2 -> m3 [ label = \"" ++ show (Map.toList m2AgentMap) ++ "\" ];\n" ++
  "}\n"

