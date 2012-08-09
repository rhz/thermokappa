{-# LANGUAGE BangPatterns #-}

module Mixture where

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Maybe (isJust)

import qualified KappaParser as KP
import qualified Env as E
import Utils

-- Sites
type InternalStateId = Int
data BindingState = Free | SemiLink | Bound | Unspecified -- WLD = Unspecified
  deriving (Show, Eq, Ord)
data Site = Site { internalState :: Maybe InternalStateId
                 , bindingState  :: BindingState
                 }
  deriving (Show, Eq, Ord)

type SiteId = Int
type Interface = Vec.Vector Site -- indexed by SiteId

-- Agents
data Agent = Agent { agentName :: E.AgentNameId
                   , interface :: Interface
                   }
  deriving (Show, Eq, Ord)

foldInterface :: (SiteId -> Site -> a -> a) -> a -> Agent -> a
foldInterface f acc = Vec.ifoldr f acc . interface

foldInterfaceM :: (Monad m) => (a -> SiteId -> Site -> m a) -> a -> Agent -> m a
foldInterfaceM f acc = Vec.foldM g acc . Vec.indexed . interface
  where g acc (siteId, site) = f acc siteId site

isBound :: Site -> Bool
isBound Site{ bindingState = Bound } = True
isBound _ = False

isFree :: Site -> Bool
isFree Site{ bindingState = Free } = True
isFree _ = False

isUnspecified :: Site -> Bool
isUnspecified site = site == unspecifiedSite

unspecifiedSite :: Site
unspecifiedSite = Site{ internalState = Nothing
                      , bindingState = Unspecified
                      }

emptyInterface :: E.Env -> E.AgentNameId -> Interface
emptyInterface env agentName = Vec.replicate (E.numSites env agentName) unspecifiedSite

setLnk :: BindingState -> Agent -> SiteId -> Agent
setLnk lnk a sId = a{ interface = interface a Vec.// [(sId, s')] }
  where s = interface a Vec.!? sId ? "Mixture.setLnk: site id not found"
        s' = s{ bindingState = lnk }

setInt :: Maybe InternalStateId -> Agent -> SiteId -> Agent
setInt int a sId = a{ interface = interface a Vec.// [(sId, s')] }
  where s = interface a Vec.!? sId ? "Mixture.setLnk: site id not found"
        s' = s{ internalState = int }

siteIds :: Agent -> [SiteId]
siteIds a = [0..Vec.length (interface a) - 1]

sitesWithId :: Agent -> [(SiteId, Site)]
sitesWithId = indexedList . interface

-- Mixtures
type AgentId = Int
type ComponentId = Int
type MixtureId = Int

type Agents = Vec.Vector Agent -- indexed by AgentId

type Endpoint = (AgentId, SiteId)
type Graph = Map.Map Endpoint Endpoint

data Mixture = Mixture { agents :: Agents
                       , graph :: Graph
                       }
  deriving (Show, Eq, Ord)

empty :: Mixture
empty = Mixture{ agents = Vec.empty
               , graph  = Map.empty
               }

agentIds :: Mixture -> [AgentId]
agentIds m = [0..Vec.length (agents m) - 1]

agentsWithId :: Mixture -> [(AgentId, Agent)]
agentsWithId = indexedList . agents

data Link = Link AgentId SiteId
          | Closed
type LinkMap = Map.Map KP.BondLabel Link
data Context = Ctxt { freshId :: !AgentId
                    , linkMap :: !LinkMap
                    }

evalAgent :: E.Env -> Bool -> ([Agent], Graph, LinkMap, AgentId) -> KP.Agent -> ([Agent], Graph, LinkMap, AgentId)
evalAgent env isPattern (!mix, !graph, !linkMap, !agentId) (KP.Agent agentName intf) = (agent:mix, graph', linkMap', agentId+1)
  where
    agent = Agent{ agentName = agentNameId
                 , interface = interface
                 }
    agentNameId = E.idOfAgent env agentName ? "Mixture.evalAgent: agent '" ++ agentName ++ "' is not mentioned in contact map"
    missingSite siteName = "site '" ++ siteName ++ "' in agent '" ++ agentName ++ "' is not mentioned in contact map"

    interface = emptyInterface env agentNameId Vec.// sites
    sites = map getSite intf
    getSite (KP.Site siteName int lnk) = (siteId, Site { internalState = internalStateId int
                                                       , bindingState = bindingState lnk
                                                       })
      where siteId = E.idOfSite env (agentNameId, siteName) ? "Mixture.evalAgent: " ++ missingSite siteName

            internalStateId "" | isPattern = Nothing
                               | otherwise = Just (E.defaultInternalState env (agentNameId, siteId))
            internalStateId intState = Just int
              where int = E.idOfIntState env (agentNameId, siteId, intState) ? "Mixture.evalAgent: internal state '" ++ intState ++ "' in agent '" ++ agentName ++ "' and site '" ++ siteName ++ "' is not mentioned in contact map"

            bindingState KP.Free = Free
            bindingState (KP.Bound _) = Bound
            bindingState KP.SemiLink | isPattern = SemiLink
                                     | otherwise = error "Mixture.evalAgent: only patterns are allowed to have semi links"
            bindingState KP.Unspecified | isPattern = Unspecified
                                        | otherwise = error "Mixture.evalAgent: only patterns are allowed to have unspecified binding states"

    links = do (KP.Site siteName _ (KP.Bound bondLabel)) <- intf
               let siteId = E.idOfSite env (agentNameId, siteName) ? "Mixture.evalAgent: " ++ missingSite siteName
               return (siteId, bondLabel)

    (graph', linkMap') = foldl' updateGraph (graph, linkMap) links

    updateGraph (!graph, !linkMap) (siteId, bondLabel) =
      case Map.lookup bondLabel linkMap of
        Nothing          ->  (                                 graph, Map.insert bondLabel (Link agentId siteId) linkMap)
        Just (Link b j)  ->  (addLink (agentId, siteId) (b, j) graph, Map.insert bondLabel Closed                linkMap)
        Just Closed      ->  error $ "Mixture.evalAgent: bond label " ++ show bondLabel ++ " is binding more than two sites"


evalKExpr :: E.Env -> Bool -> KP.KExpr -> Mixture
evalKExpr env isPattern kexpr
  | not $ null incompleteBonds = error $ "Mixture.evalKExpr: incomplete bond(s) " ++ show incompleteBonds
  | otherwise = Mixture { agents = Vec.fromList $ reverse agents -- TODO I can probably save this reverse if I leave out the reverse in KP.unpackChains
                        , graph = graph
                        }
  where (agents, graph, linkMap, _) = foldl' (evalAgent env isPattern) ([], Map.empty, Map.empty, 0) kexpr
        incompleteBonds = map fst $ filter (not . isClosed . snd) (Map.toList linkMap)
        isClosed Closed = True
        isClosed _ = False


follow :: Mixture -> Endpoint -> Maybe Endpoint
follow = flip Map.lookup . graph

addLink :: Endpoint -> Endpoint -> Graph -> Graph
addLink ep1 ep2 = Map.insert ep1 ep2 . Map.insert ep2 ep1

removeLink :: Endpoint -> Endpoint -> Graph -> Graph
removeLink ep1 ep2 = Map.delete ep1 . Map.delete ep2

setLnkInMix :: BindingState -> Endpoint -> Mixture -> Mixture
setLnkInMix lnk (aId, sId) mix = mix{ agents = agents mix Vec.// [(aId, a')] }
  where a  = agents mix Vec.!? aId ? "Mixture.setLnk: agent id not found"
        a' = setLnk lnk a sId

setIntInMix :: Maybe InternalStateId -> Endpoint -> Mixture -> Mixture
setIntInMix int (aId, sId) mix = mix{ agents = agents mix Vec.// [(aId, a')] }
  where a  = agents mix Vec.!? aId ? "Mixture.setLnk: agent id not found"
        a' = setInt int a sId

bind :: Endpoint -> Endpoint -> Mixture -> Mixture
bind (a, i) (b, j) mix = setLnkInMix Bound (a, i) $ setLnkInMix Bound (b, j) mix{ graph = addLink (a, i) (b, j) (graph mix) }

unbind :: Endpoint -> Mixture -> Mixture
unbind (a, i) mix = setLnkInMix Free (a, i) $ setLnkInMix Free (b, j) mix{ graph = removeLink (a, i) (b, j) (graph mix) }
  where (b, j) = follow mix (a, i) ? "Mixture.unbind: " ++ show (a, i) ++ " is not bound"

links :: Mixture -> Set.Set (Endpoint, Endpoint)
links mix = Map.foldrWithKey add Set.empty (graph mix)
  where add  ep1  ep2 linkSet
          | (ep2, ep1) `Set.member` linkSet = linkSet
          | otherwise = Set.insert (ep1, ep2) linkSet

toKappa :: E.Env -> Mixture -> String
toKappa env mix = intercalate ", " . Vec.toList $ Vec.imap agentStr (agents mix)
  where
    linkSet = Set.toList $ links mix
    linkMap = zipmap (map fst linkSet) [1..] `Map.union` zipmap (map snd linkSet) [1..]

    agentStr :: AgentId -> Agent -> String
    agentStr agentId agent = agentNameStr ++ "(" ++ intercalate ", " sites ++ ")"
      where
        agentNameId  = agentName agent
        agentNameStr = E.agentOfId env agentNameId ? "Mixture.toKappa: missing agent name id"
        sites = filter (not . null) . Vec.toList $ Vec.imap siteStr (interface agent)

        siteStr :: SiteId -> Site -> String
        siteStr _ (Site{ internalState = Nothing, bindingState = Unspecified }) = ""
        siteStr siteId site = siteName ++ internalStateStr (internalState site) ++ bindingStateStr (bindingState site)
          where
            siteName = E.siteOfId env (agentNameId, siteId) ? "Mixture.toKappa: missing site id"

            internalStateStr Nothing = ""
            internalStateStr (Just int) = "~" ++ intStateStr
              where intStateStr = E.intStateOfId env (agentNameId, siteId, int) ? "Mixture.toKappa: missing internal state id"

            bindingStateStr Free = ""
            bindingStateStr Unspecified = "?"
            bindingStateStr SemiLink = "!_"
            bindingStateStr Bound = "!" ++ show bondLabel
              where bondLabel = Map.lookup (agentId, siteId) linkMap ? "Mixture.toKappa: couldn't find endpoint " ++ show (agentId, siteId)

valid :: Mixture -> Bool
valid mix@(Mixture{ graph = graph }) = all isInMix (Map.toList graph) && Vec.all isInGraph boundSites
  where boundSites = do (aId, agent) <- Vec.indexed $ agents mix
                        (sId, Site{ bindingState = Bound }) <- Vec.indexed $ interface agent
                        return (aId, sId)

        isInMix (ep1, ep2) = ep1 `Vec.elem` boundSites && ep2 `Vec.elem` boundSites

        isInGraph ep1 = (Map.lookup ep1 graph >>= flip Map.lookup graph) == Just ep1


-- Connected components
type AgentIdSet = Set.Set AgentId

-- Returns the list with all the agents that are in the same component as agentId
component :: Mixture -> AgentId -> AgentIdSet
component Mixture{ agents = agents, graph = graph } agentId = explore [agentId] Set.empty
  where explore :: [AgentId] -> AgentIdSet -> AgentIdSet
        explore [] visited = visited
        explore (aId:todo) visited
          | Set.member aId visited  =  explore todo visited -- skip this agent
          | otherwise               =  explore (todo ++ nbs) (Set.insert aId visited)
          where
            nbs = do (sId, Site{ bindingState = Bound }) <- sitesWithId (agents Vec.!? aId ? "Mixture.component: agent id '" ++ show aId ++ "' not found")
                     return . fst $ graph Map.! (aId, sId)

components :: Mixture -> [AgentIdSet]
components mix = components' [] (Set.fromList $ agentIds mix)
  where components' acc ids
          | Set.null ids = acc
          | otherwise    = components' (cc:acc) (ids Set.\\ cc)
          where cc = component mix (Set.findMin ids)

split :: Mixture -> [Mixture]
split mix = map (inducedSSG mix . Set.elems) (components mix)

-- SSG = site-subgraph
inducedSSG :: Mixture -> [AgentId] -> Mixture
inducedSSG mix ids = foldr (setLnkInMix SemiLink) mix' semilinks
  where mix' = Mixture{ agents = agents', graph = graph' }
        agents' = Vec.backpermute (agents mix) (Vec.fromList ids)

        amap = zipmap ids [0..] -- map the original agent ids onto their new ids
        (graph', semilinks) = Map.foldrWithKey updateId (Map.empty, []) (graph mix)
        updateId (a, i) (b, j) (graph, semilinks)
          | Map.member a amap && Map.member b amap  =  (Map.insert (amap Map.! a, i) (amap Map.! b, j) graph, semilinks)
          | Map.member a amap                       =  (graph, (amap Map.! a, i) : semilinks)
          | Map.member b amap                       =  (graph, (amap Map.! b, j) : semilinks)
          | otherwise                               =  (graph, semilinks)


disjointUnion :: Mixture -> Mixture -> Mixture
disjointUnion m1 m2 = Mixture{ agents = agents m1 Vec.++ agents m2
                             , graph  = graph m1 `Map.union` Map.fromList (replaceIds <$> Map.toList (graph m2))
                             }
  where size1 = Vec.length (agents m1)
        replaceIds ((a, i), (b, j)) = ((a + size1, i), (b + size1, j))



{- Old code
-- Returns the list with all the agents that are in the same component as agentId
component :: E.Env -> Agents -> Graph -> AgentIdSet -> AgentId -> ([AgentId], AgentIdSet)
component env agents graph visited agentId = explore ([agentId], [], visited)
  where explore :: ([AgentId], [AgentId], AgentIdSet) -> ([AgentId], AgentIdSet)
        explore ([], cc, visited) = (cc, visited)
        explore (agentId:todo, cc, visited)
          | Set.member agentId visited  = explore (todo, cc, visited) -- skip this agent
          | otherwise                   = explore (todo ++ nbs, agentId:cc, Set.insert agentId visited)
          where
            nbs = foldInterface exploreSite [] (agents Vec.!? agentId ? missingAgentId env "Mixture.component" agentId)

            exploreSite :: MultiSiteId -> SiteId -> Site -> [AgentId] -> [AgentId]
            exploreSite multisiteId siteId (Site{ bindingState = Bound }) nbs = nbId:nbs
              where (nbId, _, _) = Map.lookup (agentId, multisiteId, siteId) graph ? "Mixture.component: site '" ++ siteName ++ "' in agent '" ++ agentNme ++ "' is bound but the link is not in the graph"
                    -- error-reporting info
                    agentNme = E.agentOfId env agentNameId ? "Mixture.component: this should never happen"
                    siteName = E.siteOfId env (agentNameId, multisiteId) ? "Mixture.component: this should never happen"
                    agentNameId = agentName (agents Vec.! agentId)

            exploreSite _ _ _ nbs = nbs


createMixture :: E.Env -> MixtureId -> Agents -> Graph -> Mixture
createMixture env mixId agents graph = Mixture { agents = agents
                                               , graph = graph
                                               , arity = length components
                                               , getId = mixId
                                               , idsOfNames = idsOfNames
                                               , ccOfId = ccOfId
                                               , sizeOfCc = Vec.fromList sizeOfCc
                                               }
  where numAgents = Vec.length agents
        (components, visited) = foldr addComponent ([], Set.empty) [0..numAgents-1]
        addComponent agentId (components, visited)
          | Set.member agentId visited  = (components, visited) -- skip this agent
          | otherwise                   = (cc:components, visited')
          where (cc, visited') = component env agents graph visited agentId

        ccsWithIds = concat $ zipWith zip components (map repeat [0..])
        ccOfId = Vec.replicate numAgents (-1) Vec.// ccsWithIds
        sizeOfCc = map length components

        idsOfNames = Map.fromList . map toSet . groupWith fst . sortWith fst . Vec.toList $ Vec.imap nameAndCc agents
        toSet xs = (fst $ head xs, Set.fromList $ map snd xs)

        nameAndCc :: AgentId -> Agent -> ((AgentNameId, ComponentId), AgentId)
        nameAndCc agentId (Agent{ agentName = agentNameId }) = ((agentNameId, ccId), agentId)
          where ccId = ccOfId Vec.! agentId
-}

