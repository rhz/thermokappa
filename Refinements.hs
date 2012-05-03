module Refinements where

import KappaParser
import Utils
import Misc

import Data.Maybe (catMaybes, fromJust)
import Data.List (find)
import qualified Data.MultiSet as MultiSet

-- TODO this module should use ids instead of strings everywhere

data RefinedLink = FreeL | SemiL | BoundL AgentName SiteName
  deriving Show
data RefineAction = SpecifyLink RefinedLink
                  | SpecifyState String
  deriving Show
type AgentIndex = Int
type SiteIndex = Int
data ReactionSide = Lhs | Rhs
  deriving Show
data RefinableSite = Common AgentIndex SiteIndex [RefineAction]
                   | NonCommon ReactionSide AgentIndex SiteIndex [RefineAction]
  deriving Show

-- When making changes to lhs, those that affect the prefix should be mirrored to rhs
-- When rule is reversible, changes should be make to deleted agents, common agents (prefix) and added agents.
-- Otherwise, only to deleted and common.

refinableSitesInNonCommon :: CM -> ReactionSide -> Agent -> AgentIndex -> [RefinableSite]
refinableSitesInNonCommon cm xhs (Agent agentName intf) agentIndex =
  catMaybes $ zipWith refineSite intf [0..]
  where refineSite :: Site -> SiteIndex -> Maybe RefinableSite
        refineSite (Site siteName internalState bindingState) siteIndex =
          if null refineActions
            then Nothing
            else Just $ NonCommon xhs agentIndex siteIndex refineActions
          where refineActions = refinedStates internalState ++ refinedLinks bindingState

                refinedStates "" = map SpecifyState $ internalStatesInCM cm agentName siteName
                refinedStates _ = []

                refinedLinks Unspecified = [SpecifyLink FreeL, SpecifyLink SemiL]
                refinedLinks SemiLink = map specifyBound $ bindingStatesInCM cm agentName siteName
                refinedLinks _ = []

refinableSitesInCommon :: CM -> Agent -> Agent -> AgentIndex -> [RefinableSite]
refinableSitesInCommon cm (Agent agentName lintf) (Agent _ rintf) agentIndex =
  catMaybes $ zipWith3 refineSite lintf rintf [0..]
  where refineSite :: Site -> Site -> SiteIndex -> Maybe RefinableSite
        refineSite (Site siteName lis lbs) (Site _ ris rbs) siteIndex =
          if null refineActions
            then Nothing
            else Just $ Common agentIndex siteIndex refineActions
          where refineActions = refinedStates lis ris ++ refinedLinks lbs rbs

                refinedStates "" "" = map SpecifyState $ internalStatesInCM cm agentName siteName
                refinedStates _ _ = []

                refinedLinks Unspecified Unspecified = [SpecifyLink FreeL, SpecifyLink SemiL]
                refinedLinks SemiLink SemiLink = map specifyBound $ bindingStatesInCM cm agentName siteName
                refinedLinks SemiLink Free = map specifyBound $ bindingStatesInCM cm agentName siteName
                refinedLinks _ _ = []

specifyBound :: CMBindingState -> RefineAction
specifyBound (CMBound agentName siteName) = SpecifyLink $ BoundL agentName siteName

internalStatesInCM :: CM -> AgentName -> SiteName -> [InternalState]
internalStatesInCM cm agentName siteName = iss
  where (CMAgent _ cmIntf) = getSig cm agentName
        (CMSite _ iss _) = fromJust $ find isSite cmIntf
        isSite (CMSite name _ _) = name == siteName

bindingStatesInCM :: CM -> AgentName -> SiteName -> [CMBindingState]
bindingStatesInCM cm agentName siteName = bss
  where (CMAgent _ cmIntf) = getSig cm agentName
        (CMSite _ _ bss) = fromJust $ find isSite cmIntf
        isSite (CMSite name _ _) = name == siteName

refinableSites :: CM -> Rule -> [RefinableSite]
refinableSites cm r@(Rule isReversible lhs rhs _) = if isReversible
                                                      then common ++ del ++ add
                                                      else common ++ del
  where n = largestPrefix r
        lhsPrefix = take n lhs
        rhsPrefix = take n rhs
        deleted = drop n lhs
        added = drop n rhs
        common = concat $ zipWith3 (refinableSitesInCommon cm) lhsPrefix rhsPrefix [0..]
        del = concat $ zipWith (refinableSitesInNonCommon cm Lhs) deleted [n..]
        add = concat $ zipWith (refinableSitesInNonCommon cm Rhs) added [n..]

genRefinement :: CM -> Rule -> RefinableSite -> [Rule]
genRefinement cm r@(Rule isReversible lhs rhs rate) (Common agentIndex siteIndex actions) =
  map (refineCommon cm r agentIndex siteIndex) actions

genRefinement cm r@(Rule isReversible lhs rhs rate) (NonCommon xhs agentIndex siteIndex actions) =
  map (refineNonCommon cm r xhs agentIndex siteIndex) actions


refineNonCommon :: CM -> Rule -> ReactionSide -> AgentIndex -> SiteIndex -> RefineAction -> Rule
refineNonCommon cm r@(Rule isReversible lhs rhs rate) Lhs agentIndex siteIndex (SpecifyState state) =
  Rule isReversible (specifyState lhs agentIndex siteIndex state) rhs rate
refineNonCommon cm r@(Rule isReversible lhs rhs rate) Rhs agentIndex siteIndex (SpecifyState state) =
  Rule isReversible lhs (specifyState rhs agentIndex siteIndex state) rate

refineNonCommon cm r@(Rule isReversible lhs rhs rate) Lhs agentIndex siteIndex (SpecifyLink FreeL) =
  Rule isReversible (specifyLink lhs agentIndex siteIndex Free) rhs rate
refineNonCommon cm r@(Rule isReversible lhs rhs rate) Rhs agentIndex siteIndex (SpecifyLink FreeL) =
  Rule isReversible lhs (specifyLink rhs agentIndex siteIndex Free) rate

refineNonCommon cm r@(Rule isReversible lhs rhs rate) Lhs agentIndex siteIndex (SpecifyLink SemiL) =
  Rule isReversible (specifyLink lhs agentIndex siteIndex SemiLink) rhs rate
refineNonCommon cm r@(Rule isReversible lhs rhs rate) Rhs agentIndex siteIndex (SpecifyLink SemiL) =
  Rule isReversible lhs (specifyLink rhs agentIndex siteIndex SemiLink) rate
  
refineNonCommon cm r@(Rule isReversible lhs rhs rate) Lhs agentIndex siteIndex (SpecifyLink (BoundL a' s')) =
  Rule isReversible (createPartner cm lhs agentIndex siteIndex a' s') rhs rate
refineNonCommon cm r@(Rule isReversible lhs rhs rate) Rhs agentIndex siteIndex (SpecifyLink (BoundL a' s')) =
  Rule isReversible lhs (createPartner cm rhs agentIndex siteIndex a' s') rate

specifyLink :: KExpr -> AgentIndex -> SiteIndex -> BindingState -> KExpr
specifyLink kexpr agentIndex siteIndex bs' = pre ++ a' : post
  where (pre, (Agent name intf):post) = splitAt agentIndex kexpr
        a' = Agent name (preSite ++ site' : postSite)
        (preSite, (Site sn is _):postSite) = splitAt siteIndex intf
        site' = Site sn is bs'

specifyState :: KExpr -> AgentIndex -> SiteIndex -> InternalState -> KExpr
specifyState kexpr agentIndex siteIndex is' = pre ++ a' : post
  where (pre, (Agent name intf):post) = splitAt agentIndex kexpr
        a' = Agent name (preSite ++ site' : postSite)
        (preSite, (Site sn _ bs):postSite) = splitAt siteIndex intf
        site' = Site sn is' bs

createPartner :: CM -> KExpr -> AgentIndex -> SiteIndex -> AgentName -> SiteName -> KExpr
createPartner cm kexpr agentIndex siteIndex partnerName partnerSite = pre ++ a' : partner : post
  where (pre, (Agent name intf):post) = splitAt agentIndex kexpr
        a' = Agent name (preSite ++ site' : postSite)
        (preSite, (Site sn is _):postSite) = splitAt siteIndex intf
        site' = Site sn is (Bound n)
        n = foldr max 0 bondLabels
        bondLabels = concatMap bondLabelsInAgent kexpr
        bondLabelsInAgent (Agent _ intf) = map bondLabel intf
        bondLabel (Site _ _ (Bound n)) = n
        bondLabel _ = 0
        partner = Agent partnerName [Site partnerSite "" (Bound n)]


refineCommon :: CM -> Rule -> AgentIndex -> SiteIndex -> RefineAction -> Rule
refineCommon cm r@(Rule isReversible lhs rhs rate) agentIndex siteIndex (SpecifyState state) =
  Rule isReversible (specifyState lhs agentIndex siteIndex state) (specifyState rhs agentIndex siteIndex state) rate
refineCommon cm r@(Rule isReversible lhs rhs rate) agentIndex siteIndex (SpecifyLink FreeL) =
  Rule isReversible (specifyLink lhs agentIndex siteIndex Free) (specifyLink rhs agentIndex siteIndex Free) rate
refineCommon cm r@(Rule isReversible lhs rhs rate) agentIndex siteIndex (SpecifyLink SemiL) =
  Rule isReversible (specifyLink lhs agentIndex siteIndex SemiLink) (specifyLink rhs agentIndex siteIndex SemiLink) rate
refineCommon cm r@(Rule isReversible lhs rhs rate) agentIndex siteIndex (SpecifyLink (BoundL a' s')) =
  Rule isReversible (createPartner cm lhs agentIndex siteIndex a' s') (createPartner cm rhs agentIndex siteIndex a' s') rate

-- Main function! Only this one should be exported
nextRefinement :: CM -> Rule -> [Rule]
nextRefinement cm r = concatMap (genRefinement cm r') (refinableSites cm r')
  where r' = fillRule cm r

fillRule :: CM -> Rule -> Rule
fillRule cm (Rule isReversible lhs rhs rate) = Rule isReversible lhs' rhs' rate
  where [lhs', rhs'] = map (map $ fillAgent cm) [lhs, rhs]

fillAgent :: CM -> Agent -> Agent
fillAgent cm (Agent name intf) = Agent name (fillIntf cm name intf)

fillIntf :: CM -> AgentName -> Interface -> Interface
fillIntf cm agentName intf = intf ++ implicitSites
  where (CMAgent _ cmIntf) = getSig cm agentName
        explicitSites = MultiSet.fromList $ map siteName intf
        implicitSites = map createSite . MultiSet.toList $ MultiSet.fromList (map cmSiteName cmIntf) MultiSet.\\ explicitSites
        createSite siteName = Site siteName "" Unspecified

-- Before refining a rule to get a particular refined rule,
-- we want to know if it's possible to get that refinement
--isRefinableInto :: Rule -> Rule -> Bool

