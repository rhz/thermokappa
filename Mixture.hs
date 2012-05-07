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
type MultiSiteId = Int
type MultiSite = Vec.Vector Site
type Interface = Vec.Vector MultiSite -- indexed by MultiSiteId then by SiteId

-- Agents
type AgentNameId = Int

data Agent = Agent { agentName :: AgentNameId
                   , interface :: Interface
                   }
  deriving (Show, Eq)

foldInterface :: (MultiSiteId -> SiteId -> Site -> a -> a) -> a -> Agent -> a
foldInterface f acc = Vec.ifoldr g acc . interface
  where g multisiteId multisite acc = Vec.ifoldr (f multisiteId) acc multisite

foldInterfaceM :: (Monad m) => (a -> MultiSiteId -> SiteId -> Site -> m a) -> a -> Agent -> m a
foldInterfaceM f acc = Vec.foldM g acc . Vec.indexed . interface
  where g acc (multisiteId, multisite) = Vec.foldM h acc (Vec.indexed multisite)
          where h acc (siteId, site) = f acc multisiteId siteId site

isBound :: Site -> Bool
isBound Site{ bindingState = Bound } = True
isBound _ = False

emptyInterface :: E.Env -> AgentNameId -> Interface
emptyInterface env agentName = Vec.replicate (E.numMultiSites env agentName) Vec.empty

-- Mixtures
type AgentId = Int
type ComponentId = Int
type MixtureId = Int

type Agents = Vec.Vector Agent -- indexed by AgentId

type Endpoint = (AgentId, MultiSiteId, SiteId)
type Graph = Map.Map Endpoint Endpoint

data Mixture = Mixture { agents :: Agents
                       , graph :: Graph
                       }
  deriving (Show, Eq)

{-
type Agent2Cc = Vec.Vector ComponentId -- indexed by AgentId

type Name2Ids = Map.Map (AgentNameId, ComponentId) AgentIdSet
type AgentIdSet = Set.Set AgentId

data Mixture = Mixture { agents :: Agents
                       , graph :: Graph
                       , arity :: Int
                       , getId :: MixtureId
                       , idsOfNames :: Name2Ids
                       , ccOfId :: Agent2Cc -- indexed by AgentId
                       , sizeOfCc :: Vec.Vector Int -- indexed by ComponentId
                       }
  deriving (Show, Eq)
-}

empty :: Mixture
empty = Mixture{ agents = Vec.empty
               , graph = Map.empty
               }

type LinkMap = Map.Map KP.BondLabel [Endpoint]

addAgent :: E.Env -> Bool -> KP.Agent -> (Agents, Graph, LinkMap) -> (Agents, Graph, LinkMap)
addAgent env isPattern (KP.Agent agentName intf) (mix, graph, linkMap) = (Vec.snoc mix agent, graph', linkMap')
  where agent = Agent{ agentName = agentNameId
                     , interface = interface
                     }
        agentId = Vec.length mix
        agentNameId = E.idOfAgent env agentName ? missingAgent "Mixture.addAgent" agentName

        emptyInterface = Vec.replicate (E.numMultiSites env agentNameId) Vec.empty
        (interface, graph', linkMap') = foldl' addSite (emptyInterface, graph, linkMap) intf


        addSite :: (Interface, Graph, LinkMap) -> KP.Site -> (Interface, Graph, LinkMap)
        addSite (intf, graph, linkMap) (KP.Site siteName int lnk) = (intf', graph', linkMap')
          where intf' = intf Vec.// [(multisiteId, Vec.snoc multisite site)]
                site = Site { internalState = internalStateId int
                            , bindingState = bindingState lnk
                            }
                multisiteId = E.idOfSite env (agentNameId, siteName) ? missingSite "Mixture.addAgent" agentName siteName

                internalStateId "" | isPattern = Nothing
                                   | otherwise = Just (E.defaultInternalState env (agentNameId, multisiteId))
                internalStateId int = Just (E.idOfIntState env (agentNameId, multisiteId, int) ? missingIntState "Mixture.addAgent" agentName siteName int)

                bindingState KP.Free = Free
                bindingState (KP.Bound _) = Bound
                bindingState KP.SemiLink | isPattern = SemiLink
                                         | otherwise = error "Mixture.addAgent: only patterns are allowed to have semi links"
                bindingState KP.Unspecified | isPattern = Unspecified
                                            | otherwise = error "Mixture.addAgent: only patterns are allowed to have unspecified binding states"

                multisite = intf Vec.!? multisiteId ? missingSite "Mixture.addAgent" agentName siteName
                siteId = Vec.length multisite

                (graph', linkMap') = updateGraph lnk
                updateGraph (KP.Bound bondLabel)
                  | null endpoints         = (graph, linkMap')
                  | length endpoints == 1  = (addLink (head endpoints) endpoint graph, linkMap')
                  | otherwise              = error $ "Mixture.addAgent: bond label " ++ show bondLabel ++ " is binding more than two sites"
                  where linkMap' = Map.insert bondLabel (endpoint:endpoints) linkMap
                        endpoint = (agentId, multisiteId, siteId)
                        endpoints = Map.findWithDefault [] bondLabel linkMap
                updateGraph _ = (graph, linkMap)


{-
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


evalKExpr :: E.Env -> Bool -> KP.KExpr -> Mixture
evalKExpr env isPattern kexpr
  | not $ null incompleteBonds = error $ "Mixture.evalKExpr: incomplete bond(s) " ++ show incompleteBonds
  | otherwise = Mixture { agents = agents
                        , graph = graph
                        }
  where (agents, graph, linkMap) = foldl' (flip $ addAgent env isPattern) (Vec.empty, Map.empty, Map.empty) kexpr
        incompleteBonds = map fst $ filter ((/= 2) . length . snd) (Map.toList linkMap)


follow :: Mixture -> Endpoint -> Maybe Endpoint
follow = flip Map.lookup . graph

addLink :: Endpoint -> Endpoint -> Graph -> Graph
addLink ep1 ep2 = Map.insert ep1 ep2 . Map.insert ep2 ep1



toKappa :: E.Env -> Mixture -> String
toKappa env mix = intercalate ", " . Vec.toList . Vec.imap agentStr $ agents mix
  where
    (linkMap, _) = Map.foldrWithKey addLink (Map.empty, 1) (graph mix)
    addLink ep1 ep2 (linkMap, n) | Map.member ep1 linkMap  = (linkMap, n)
                                 | otherwise               = (Map.insert ep1 n $ Map.insert ep2 n linkMap, n + 1)

    agentStr :: AgentId -> Agent -> String
    agentStr agentId agent = agentNameStr ++ "(" ++ intercalate ", " sites ++ ")"
      where
        agentNameId  = agentName agent
        agentNameStr = E.agentOfId env agentNameId ? "Mixture.toKappa: missing agent name id"
        sites = concat . Vec.toList $ Vec.imap siteStr (interface agent)

        siteStr multisiteId = Vec.toList . Vec.imap multisiteStr
          where
            multisiteStr siteId site = siteName ++ internalStateStr (internalState site) ++ bindingStateStr (bindingState site)
              where
                siteName = E.siteOfId env (agentNameId, multisiteId) ? "Mixture.toKappa: missing site id"

                internalStateStr Nothing = ""
                internalStateStr (Just int) = "~" ++ (E.intStateOfId env (agentNameId, multisiteId, int) ? "Mixture.toKappa: missing internal state id")

                bindingStateStr Free = ""
                bindingStateStr Unspecified = "?"
                bindingStateStr SemiLink = "!_"
                bindingStateStr Bound = "!" ++ show (Map.lookup (agentId, multisiteId, siteId) linkMap ?
                                                          "Mixture.toKappa: couldn't find endpoint " ++ show (agentId, multisiteId, siteId))

valid :: Mixture -> Bool
valid mix@(Mixture{ graph = graph }) = all isInMix (Map.toList graph) && Vec.all isInGraph boundSites
  where boundSites = do (aId, agent) <- Vec.indexed $ agents mix
                        (msId, ms) <- Vec.indexed $ interface agent
                        (sId, Site{ bindingState = Bound }) <- Vec.indexed ms
                        return (aId, msId, sId)

        isInMix (ep1, ep2) = ep1 `Vec.elem` boundSites && ep2 `Vec.elem` boundSites

        isInGraph ep1 = (Map.lookup ep1 graph >>= flip Map.lookup graph) == Just ep1


-- Error reporting
missingAgent :: String -> KP.AgentName -> String
missingAgent fnName agentName = fnName ++ ": agent '" ++ agentName ++ "' is not mentioned in contact map"

missingAgentId :: E.Env -> String -> AgentId -> String
missingAgentId env fnName agentId = missingAgent fnName (E.agentOfId env agentId ? "Mixture.missingAgentId: missing agent name id")

missingSite :: String -> KP.AgentName -> KP.SiteName -> String
missingSite fnName agentName siteName = fnName ++ ": site '" ++ siteName ++ "' in agent '" ++ agentName ++ "' is not mentioned in contact map"

missingSiteId :: E.Env -> String -> AgentNameId -> MultiSiteId -> String
missingSiteId env fnName agentNameId multisiteId = missingSite fnName agentName siteName
  where agentName = E.agentOfId env agentNameId ? "Mixture.missingSiteId: misssing agent name id"
        siteName = E.siteOfId env (agentNameId, multisiteId) ? "Mixture.missingSiteId: missing site id"

missingIntState :: String -> KP.AgentName -> KP.SiteName -> KP.InternalState -> String
missingIntState fnName agentName siteName intState =
  fnName ++ ": internal state '" ++ intState ++ "' in agent '" ++ agentName ++ "' and site '" ++ siteName ++ "' is not mentioned in contact map"

