module Env( Env(..), createEnv, AgentNameId, SiteNameId, InternalStateId
          , agentOfId, siteOfId, intStateOfId
          , idOfAgent, idOfSite, idOfIntState
          , defaultInternalState, numSites
          ) where

import qualified Data.Vector as Vec

import qualified KappaParser as KP
import Utils

-- All these identifiers are locally unique
type AgentNameId = Int
type SiteNameId = Int
type InternalStateId = Int

-- This data structure stores the name of everything and assigns it an id
data Env = Env { agentNames :: Vec.Vector KP.AgentName -- indexed by AgentNameId
               , siteNames :: Vec.Vector (Vec.Vector KP.SiteName) -- indexed by AgentNameId then SiteNameId
               , intStates :: Vec.Vector (Vec.Vector (Vec.Vector KP.InternalState)) -- indexed by AgentNameId then SiteNameId then InternalStateId
               , ruleNames :: Vec.Vector KP.RuleName -- indexed by RuleId
               , varNames :: Vec.Vector KP.VarName -- indexed by variable id, observables are considered as variables here
               }
  deriving (Show, Eq)

-- Note: don't confuse AgentNameId with AgentId, the former is a "global" id while the latter is a "local" (ie, per mixture) id

createEnv :: KP.Module -> Env
createEnv m@(KP.Module{ KP.contactMap = cm }) =
  Env { agentNames = agentNames
      , siteNames = Vec.fromList $ map fst sites
      , intStates = Vec.fromList $ map snd sites
      , ruleNames = ruleNames
      , varNames  = varNames
      }
  where agentNames = Vec.fromList $ map KP.cmAgentName cm
        ruleNames  = Vec.fromList $ map fst (KP.rules m)
        varNames   = Vec.fromList $ map fst (KP.vars  m)
        sites = map getSites cm

        getSites agent = (siteNames, intStates)
          where siteNames = Vec.fromList $ map KP.cmSiteName (KP.cmInterface agent)
                intStates = Vec.fromList $ map (Vec.fromList . KP.cmInternalStates) (KP.cmInterface agent)


agentOfId :: Env -> AgentNameId -> Maybe KP.AgentName
agentOfId env agentId = agentNames env Vec.!? agentId

siteOfId :: Env -> (AgentNameId, SiteNameId) -> Maybe KP.SiteName
siteOfId env (agentId, multisiteId) = siteNames env Vec.!? agentId >>= (Vec.!? multisiteId)

intStateOfId :: Env -> (AgentNameId, SiteNameId, InternalStateId) -> Maybe KP.InternalState
intStateOfId env (agentId, multisiteId, intStateId) = intStates env Vec.!? agentId >>= (Vec.!? multisiteId) >>= (Vec.!? intStateId)

{-
ruleOfId :: Env -> RuleId -> Maybe KP.RuleName
shapeOfId :: Env -> ShapeId -> Maybe KP.ShapeName
varOfId :: Env -> VarId -> Maybe KP.VarName
-}


idOfAgent :: Env -> KP.AgentName -> Maybe AgentNameId
idOfAgent = flip Vec.elemIndex . agentNames

idOfSite :: Env -> (AgentNameId, KP.SiteName) -> Maybe SiteNameId
idOfSite env (agentId, siteName) = Vec.elemIndex siteName intf
  where intf = siteNames env Vec.!? agentId ? "Env.idOfSite: missing agent id"

idOfIntState :: Env -> (AgentNameId, SiteNameId, KP.InternalState) -> Maybe InternalStateId
idOfIntState env (agentId, multisiteId, intState) = Vec.elemIndex intState states
  where states = (intStates env Vec.!? agentId >>= (Vec.!? multisiteId)) ? "Env.idOfIntState: missing site id"

{-
idOfRule :: Env -> KP.RuleName -> Maybe RuleId
idOfShape :: Env -> KP.ShapeName -> Maybe ShapeId
idOfVar :: Env -> KP.VarName -> Maybe VarId

isRule :: Env -> RuleId -> Bool -- check if ruleId < Vec.length ruleNames
-}


-- TODO this should return Maybe InternalStateId
defaultInternalState :: Env -> (AgentNameId, SiteNameId) -> InternalStateId
defaultInternalState env (agentId, multisiteId)
  | Vec.length states == 0  = error $ "Env.defaultInternalState: no default internal state for agent '" ++ agentName ++ "' and site '" ++ siteName ++ "'"
  | otherwise               = 0
  where states = (intStates env Vec.!? agentId >>= (Vec.!? multisiteId)) ? "Env.defaultInternalState: site '" ++ show siteName ++ "' in agent '" ++ show agentName ++ "' is not mentioned in contact map"
        agentName = agentOfId env agentId ? "Env.defaultInternalState: missing agent name id"
        siteName = siteOfId env (agentId, multisiteId) ? "Env.defaultInternalState: missing site name id in agent '" ++ show agentName ++ "'"


-- TODO this should return Maybe Int
numSites :: Env -> AgentNameId -> Int
numSites env agentId = Vec.length intf
  where intf = siteNames env Vec.!? agentId ? "Env.numMultiSites: missing agent id"

