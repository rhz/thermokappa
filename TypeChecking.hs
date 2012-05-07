{-# LANGUAGE TupleSections #-}

module TypeChecking where

import qualified KappaParser as KP
import KappaParser -- FIXME
import PureKappa (showAgent)
import Utils
import Misc

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad (mapM_)
import Data.List (find)

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

