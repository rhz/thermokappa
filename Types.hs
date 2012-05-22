{-# LANGUAGE TupleSections #-}

module Types where

import qualified KappaParser as KP
import KappaParser
import PlainKappa (showAgent)
import Utils
import Misc

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad (mapM_)
import Data.List (find)

-- Checking
data Error = LinkError Agent SiteName SiteName Agent
           | StateError Agent SiteName String

check :: Module -> IO ()
check m@(Module{ contactMap = cm }) = mapM_ printErrors (ruleErrors ++ initErrors ++ varErrors)
  where ruleErrors = map (uncurry errorInRule) (rules m)
        errorInRule ruleName (Rule _ lhs rhs _) = ("rule '" ++ show ruleName ++ "'", checkExpr cm lhs ++ checkExpr cm rhs)

        initErrors = zipWith errorInInit (inits m) [1..]
        errorInInit (_, kexpr) n = ("init " ++ show n, checkExpr cm kexpr)

        varErrors = map errorInVar (vars m)
        errorInVar (name, Left kexpr) = ("var '" ++ name ++ "'", checkExpr cm kexpr)
        errorInVar _ = ("", [])

        printErrors (name, errors) = mapM_ printError errors
          where printError (LinkError a i j b) =
                  putStrLn $ "Error in " ++ name ++ ": Link between agents '" ++ showAgent a ++ "' and '" ++ showAgent b ++ "' " ++
                             "is not allowed through sites '" ++ i ++ "' and '" ++ j ++ "', respectively"
                printError (StateError a i state) =
                  putStrLn $ "Error in " ++ name ++ ": State '" ++ state ++ "' is not allowed on site '" ++ i ++ "' " ++
                             "in agent '" ++ showAgent a ++ "'"


checkExpr :: CM -> KExpr -> [Error]
checkExpr cm kexpr = concatMap stateErrors kexpr ++ Map.foldr linkErrors [] lm
  where stateErrors :: Agent -> [Error]
        stateErrors a@(Agent name intf) = mapMaybe stateError intf
          where stateError :: Site -> Maybe Error
                stateError (Site sn state _) = if state == "" || Set.member (name, sn, state) allowedStates
                                                 then Nothing
                                                 else Just $ StateError a sn state

        allowedStates = Set.fromList $ concatMap getStates4Agent cm
        getStates4Agent (CMAgent name intf) = concatMap getStates4Site intf
          where getStates4Site (CMSite sn iss _) = map (name, sn,) iss

        lm = linkMap kexpr
        linkErrors :: Link -> [Error] -> [Error]
        linkErrors (a@(Agent aname aintf), i, j, b@(Agent bname bintf)) errors
            | isLinkInCM = errors
            | otherwise  = LinkError a i j b : errors
          where isLinkInCM = any (== CMBound bname j) bss
                (CMAgent _ cmIntf) = getSig cm aname
                (CMSite _ _ bss) = fromJust $ find isSite cmIntf
                isSite (CMSite sn _ _) = sn == i

-- Inference
inferCM :: [KExpr] -> CM
inferCM kexprs = map replicateSites . toCM $ foldr addKExpr Map.empty kexprs
  where
    addKExpr kexpr cm = foldr addAgent cm kexpr
      where
        lm = linkMap kexpr

        addAgent (Agent agentName intf) cm = Map.insert agentName cmIntf' cm
          where
            cmIntf  = Map.findWithDefault Map.empty agentName cm
            cmIntf' = foldr addSite cmIntf intf

            addSite (Site siteName int lnk) cmIntf = Map.insert siteName cmSite' cmIntf
              where
                cmSite  = Map.findWithDefault (CMSite siteName [] []) siteName cmIntf
                cmSite' = addInt int $ addLnk lnk cmSite

                addInt int (CMSite siteName ints lnks)
                  | int `elem` ints  =  CMSite siteName      ints  lnks
                  | otherwise        =  CMSite siteName (int:ints) lnks

                addLnk (Bound bondLabel) (CMSite siteName ints lnks)
                  | lnk `elem` lnks  =  CMSite siteName ints      lnks
                  | otherwise        =  CMSite siteName ints (lnk:lnks)
                  where
                    (Agent an1 _, sn1, sn2, Agent an2 _) = Map.lookup bondLabel lm ? "Types.inferCM: bond label " ++ show bondLabel ++ " not found"
                    lnk | agentName == an1 && siteName == sn1  =  CMBound an2 sn2
                        | agentName == an2 && siteName == sn2  =  CMBound an1 sn1

                addLnk _ cmSite = cmSite

    toCM cm = map toCMAgent $ Map.toList cm
    toCMAgent (agentName, cmIntf) = CMAgent agentName (Map.elems cmIntf)

    siteCount = foldr (Map.unionWith max) Map.empty $ concatMap (map countSite) kexprs

    countSite (Agent agentName intf) = Map.fromList $ frequencies siteNames
      where siteNames = map (agentName, ) $ map siteName intf

    replicateSites (CMAgent agentName intf) = CMAgent agentName intf'
      where intf' = foldr replicateSite [] intf

            replicateSite site@(CMSite siteName _ _) intf = replicate count site ++ intf
              where count = Map.lookup (agentName, siteName) siteCount ? "Types.inferCM: site not found"

