module Misc where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified KappaParser as KP


largestPrefix :: KP.Rule -> Int
largestPrefix (KP.Rule _ lhs rhs _) = fst . foldr findMismatch (0, False) $ zip lhs rhs
  where findMismatch _ (n, True) = (n, True)
        findMismatch (KP.Agent lname lintf, KP.Agent rname rintf) (n, False) =
          if lname == rname && Set.fromList (map KP.siteName lintf) == Set.fromList (map KP.siteName rintf)
            then (n+1, False)
            else (n, True)


-- Link Map
data TmpLink = Half KP.Agent KP.SiteName | Complete KP.Agent KP.SiteName KP.SiteName KP.Agent
  deriving (Show, Eq, Ord)
type TmpLinkMap = Map.Map KP.BondLabel TmpLink

type Link = (KP.Agent, KP.SiteName, KP.SiteName, KP.Agent)
type LinkMap = Map.Map KP.BondLabel Link

linkMap :: KP.KExpr -> LinkMap
linkMap kexpr = toLinkMap $ foldr addAgent Map.empty kexpr
  where addAgent :: KP.Agent -> TmpLinkMap -> TmpLinkMap
        addAgent a1@(KP.Agent _ intf) lm = foldr addBond lm $ filter isBond intf
          where isBond :: KP.Site -> Bool
                isBond (KP.Site _ _ (KP.Bound _)) = True
                isBond _ = False

                addBond :: KP.Site -> TmpLinkMap -> TmpLinkMap
                addBond (KP.Site s1 _ (KP.Bound l)) lm =
                  case Map.lookup l lm of
                    Just (Half a2 s2) -> Map.insert l (Complete a2 s2 s1 a1) lm
                    Just _ -> error $ "linkMap: bond label " ++ show l ++ " appears too many times in expression"
                    Nothing -> Map.insert l (Half a1 s1) lm
                addBond (KP.Site s1 _ _) _ = error $ "linkMap: site " ++ s1 ++ " must be bound"

        toLinkMap :: TmpLinkMap -> LinkMap
        toLinkMap lm = Map.mapWithKey toLink lm
          where toLink _ (Complete a1 s1 s2 a2) = (a1, s1, s2, a2)
                toLink l _ = error $ "linkMap: bond label " ++ show l ++ " appears only once in expression"

