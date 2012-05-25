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
  deriving (Show, Eq)
data Site = Site { internalState :: Maybe InternalStateId
                 , bindingState :: BindingState
                 }
  deriving (Show, Eq)

type SiteId = Int
type Interface = Vec.Vector Site -- indexed by SiteId

-- Agents
data Agent = Agent { agentName :: E.AgentNameId
                   , interface :: Interface
                   }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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

addAgent :: E.Env -> Bool -> (Agents, Graph, LinkMap) -> KP.Agent -> (Agents, Graph, LinkMap)
addAgent env isPattern (!mix, !graph, !linkMap) (KP.Agent agentName intf) = (Vec.snoc mix agent, graph', linkMap')
  where
    agent = Agent{ agentName = agentNameId
                 , interface = interface
                 }
    agentId = Vec.length mix
    agentNameId = E.idOfAgent env agentName ? "Mixture.addAgent: " ++ missingAgent agentName

    interface = emptyInterface env agentNameId Vec.// sites
    sites = map getSite intf
    getSite (KP.Site siteName int lnk) = (siteId, Site { internalState = internalStateId int
                                                       , bindingState = bindingState lnk
                                                       })
      where siteId = E.idOfSite env (agentNameId, siteName) ? "Mixture.addAgent: " ++ missingSite agentName siteName

            internalStateId "" | isPattern = Nothing
                               | otherwise = Just (E.defaultInternalState env (agentNameId, siteId))
            internalStateId intState = Just int
              where int = E.idOfIntState env (agentNameId, siteId, intState) ? "Mixture.addAgent: " ++ missingIntState agentName siteName intState

            bindingState KP.Free = Free
            bindingState (KP.Bound _) = Bound
            bindingState KP.SemiLink | isPattern = SemiLink
                                     | otherwise = error "Mixture.addAgent: only patterns are allowed to have semi links"
            bindingState KP.Unspecified | isPattern = Unspecified
                                        | otherwise = error "Mixture.addAgent: only patterns are allowed to have unspecified binding states"

    links = do (KP.Site siteName _ (KP.Bound bondLabel)) <- intf
               let siteId = E.idOfSite env (agentNameId, siteName) ? "Mixture.addAgent: " ++ missingSite agentName siteName
               return (siteId, bondLabel)

    (graph', linkMap') = foldl' updateGraph (graph, linkMap) links

    updateGraph (graph, linkMap) (siteId, bondLabel) =
      case Map.lookup bondLabel linkMap of
        Nothing          ->  (graph,                                  Map.insert bondLabel (Link agentId siteId) linkMap)
        Just (Link b j)  ->  (addLink (agentId, siteId) (b, j) graph, Map.insert bondLabel Closed                linkMap)
        Just Closed      ->  error $ "Mixture.addAgent: bond label " ++ show bondLabel ++ " is binding more than two sites"


evalKExpr :: E.Env -> Bool -> KP.KExpr -> Mixture
evalKExpr env isPattern kexpr
  | not $ null incompleteBonds = error $ "Mixture.evalKExpr: incomplete bond(s) " ++ show incompleteBonds
  | otherwise = Mixture { agents = agents
                        , graph = graph
                        }
  where (agents, graph, linkMap) = foldl' (addAgent env isPattern) (Vec.empty, Map.empty, Map.empty) kexpr
        incompleteBonds = map fst $ filter (not . isClosed . snd) (Map.toList linkMap)
        isClosed Closed = True
        isClosed _ = False


follow :: Mixture -> Endpoint -> Maybe Endpoint
follow = flip Map.lookup . graph

addLink :: Endpoint -> Endpoint -> Graph -> Graph
addLink ep1 ep2 = Map.insert ep1 ep2 . Map.insert ep2 ep1

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


-- Error reporting
missingAgent :: KP.AgentName -> String
missingAgent agentName =
  "agent '" ++ agentName ++ "' is not mentioned in contact map"

missingSite :: KP.AgentName -> KP.SiteName -> String
missingSite agentName siteName =
  "site '" ++ siteName ++ "' in agent '" ++ agentName ++ "' is not mentioned in contact map"

missingIntState :: KP.AgentName -> KP.SiteName -> KP.InternalState -> String
missingIntState agentName siteName intState =
  "internal state '" ++ intState ++ "' in agent '" ++ agentName ++ "' and site '" ++ siteName ++ "' is not mentioned in contact map"


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

