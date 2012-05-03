module Shapes where

import KappaParser

-- For a Kappa rule to be compatible with an energy shape the following conditions must be met:
-- - The shape must match the lhs of the rule

type Energy = Maybe AExpr -- Nothing means the energy is undefined

deltaEnergy :: Shape -> Rule -> Energy
deltaEnergy shape (Rule _ lhs rhs _) = foldr (liftM2 $ Duo Add) $ zipWith delta (match shape lhs) (match shape rhs)
  where delta :: Matching -> Matching -> Energy
        delta (PartialMatch _) (PartialMatch _) = Nothing -- Undefined
        delta (Match m1) (Match m2) = Just $ Float 0
        delta (Match m1) (PartialMatch m2) = ...
        delta m1 m2 = delta m2 m1

--isCompatible :: Shape -> Rule -> Bool
--isCompatible shape (Rule _ lhs rhs _) = ...

{- This is a bad idea!!
-- It's probably better to use enable from Dynamics.hs in kappa1

-- Matchings, Glueings and so on
type AgentIdx = Int
type Glueing  = Map.Map AgentIdx AgentIdx
data Matching = PartialMatch Glueing
              | Match Glueing

match :: KExpr -> KExpr -> [Matching]
match e1 e2 = ...


type InterfaceGlueing = Map.Map Site Site

type AgentMatching = PartialMatch InterfaceGlueing
                   | Match InterfaceGlueing

agentMatch :: Agent -> Agent -> [


type ParallelBonds = (BondLabel, BondLabel)
type ParallelPaths = [ParallelBonds]

siteMatch :: Site -> Site -> (Bool, Maybe ParallelBonds)
siteMatch (Site name1 is1 bs1) (Site name2 is2 bs2) = ...
-}

