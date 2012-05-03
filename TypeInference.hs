{-# LANGUAGE TupleSections #-}

module TypeInference where

import qualified Data.Map as Map

import KappaParser
import Utils
import Misc


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
                    (Agent an1 _, sn1, sn2, Agent an2 _) = Map.lookup bondLabel lm ? "TypeInference.inferCM: bond label " ++ show bondLabel ++ " not found"
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
              where count = Map.lookup (agentName, siteName) siteCount ? "TypeInference.inferCM: site not found"

