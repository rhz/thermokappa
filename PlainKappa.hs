module PlainKappa where

import KappaParser
import Data.List (intercalate, find, nub, (\\), lookup)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Utils
import Misc

moduleToKappa :: Module -> String
moduleToKappa m@(Module{ contactMap = cm }) =
  intercalate "\n\n" [ showSignatures cm
                     , showVars cm (vars m)
                     , showRules cm (rules m)
                     , showInits cm (inits m)
                     , showObss cm (obss m)
                     ]

showSignatures :: CM -> String
showSignatures cm = intercalate "\n" $ map showSign cm
  where showSign :: CMAgent -> String
        showSign (CMAgent name intf) = "%agent: " ++ name ++ "(" ++ intercalate ", " (map siteStr intf) ++ ")"
          where siteStr :: CMSite -> String
                siteStr (CMSite name iss _) = concat $ name : map ('~':) iss


-- Rules
showRules :: CM -> [RuleWithName] -> String
showRules cm rs = intercalate "\n" $ map showRule rs

showRule :: RuleWithName -> String
showRule (name, Rule isReversible lhs rhs rate)
  | isReversible  = showUniRule name lhs rhs ++ "\n" ++ showUniRule (name ++ " inv") rhs lhs
  | otherwise     = showUniRule name lhs rhs
  where
    showUniRule name lhs rhs = "'" ++ name ++ "' " ++ showKExpr lhs ++ " -> " ++ showKExpr rhs ++ " @ " ++ showAExpr rate

showKExpr :: KExpr -> String
showKExpr e = intercalate ", " $ map showAgent e

showAgent :: Agent -> String
showAgent (Agent name intf) = name ++ "(" ++ intercalate ", " (map showSite intf) ++ ")"

showSite :: Site -> String
showSite (Site name "" Free) = name
showSite (Site name internalState Free) = name ++ "~" ++ internalState
showSite (Site name "" Unspecified) = ""
showSite (Site name internalState Unspecified) = name ++ "~" ++ internalState ++ "?"
showSite (Site name "" SemiLink) = name ++ "!_"
showSite (Site name internalState SemiLink) = name ++ "~" ++ internalState ++ "!_"
showSite (Site name "" (Bound i)) = name ++ "!" ++ show i
showSite (Site name internalState (Bound i)) = name ++ "~" ++ internalState ++ "!" ++ show i

showAExpr (Var x) = "'" ++ x ++ "'"
showAExpr (Integer n) = show n
showAExpr (Float n) = show n
showAExpr Infinity = "[inf]"
showAExpr (Uno Log ae)  = "[log] "  ++ showAExpr ae
showAExpr (Uno Sqrt ae) = "[sqrt] " ++ showAExpr ae
showAExpr (Uno Exp ae)  = "[exp] "  ++ showAExpr ae
showAExpr (Uno Sin ae)  = "[sin] "  ++ showAExpr ae
showAExpr (Uno Cos ae)  = "[cos] "  ++ showAExpr ae
showAExpr (Uno Tan ae)  = "[tan] "  ++ showAExpr ae
showAExpr (Uno Int ae)  = "[int] "  ++ showAExpr ae
showAExpr (Duo Add ae1 ae2)  = showAExpr ae1 ++ " + " ++ showAExpr ae2
showAExpr (Duo Sub ae1 ae2)  = showAExpr ae1 ++ " - " ++ showAExpr ae2
showAExpr (Duo Mult ae1 ae2) = showAExpr ae1 ++ " * " ++ showAExpr ae2
showAExpr (Duo Div ae1 ae2)  = showAExpr ae1 ++ " / " ++ showAExpr ae2

-- Vars
showVars :: CM -> [Var] -> String
showVars cm vars = intercalate "\n" $ map showVar vars
  where showVar (name, Left  kexpr) = "%var: '" ++ name ++ "' " ++ showKExpr kexpr
        showVar (name, Right aexpr) = "%var: '" ++ name ++ "' " ++ showAExpr aexpr


-- Obss
showObss :: CM -> [Obs] -> String
showObss cm obss = intercalate "\n" $ map showObs obss
  where showObs (Plot obs) = "%plot: '" ++ obs ++ "'"
        showObs (KExprWithName name ke) = "%obs: '" ++ name ++ "' " ++ showKExpr ke


-- Inits
showInits :: CM -> [Init] -> String
showInits cm inits = intercalate "\n" $ map showInit inits
  where showInit (n, init) = "%init: " ++ show n ++ " (" ++ showKExpr init ++ ")"

