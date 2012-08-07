module Main where

import qualified KappaParser as KP
import qualified Env as E
import qualified Mixture as M
import qualified Rule as R
import qualified Fragmentation as F
import qualified PlainKappa as K
import Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment (getArgs)
import Data.List (intercalate, nubBy)

main :: IO ()
main = do inputFilename : nStr : _ <- getArgs
          m <- KP.parseFromFile inputFilename

          let env   = E.createEnv m
              rules = [ R.evalRule env rule | (_, rule) <- KP.rules m ]
              obss  = [ (M.evalKExpr env True obs, name) | KP.KExprWithName name obs <- KP.obss m ]

              odes = take (read nStr) $ F.fragment rules (map fst obss)
              fragments = map fst odes ++ (snd =<< snd =<< odes)

              names = reverse $ addFrags 1 (reverse obss) fragments -- reverse forth and back
              addFrags _ obss [] = obss
              addFrags n obss (obs:todo)
                | (obs', name):_ <- filter (F.ccIso obs . fst) obss = addFrags n ((obs, name):obss) todo -- reuse name
                | otherwise = addFrags (n+1) ((obs, "F" ++ show n):obss) todo -- assign a name

          mapM_ (printFrag env) (nubBy ((==) `on` snd) names)
          putStrLn ""
          mapM_ (printODE (Map.fromList names)) odes
  where
    printFrag :: E.Env -> (F.Obs, String) -> IO ()
    printFrag env (obs, name) = putStrLn $ name ++ " := " ++ M.toKappa env obs

    printODE :: Map.Map F.Obs String -> (F.Obs, F.ODE) -> IO ()
    printODE names (obs, ode) = putStrLn $ names Map.! obs ++ " = " ++ odeRhs
      where
        odeRhs | null ode  = "0"
               | otherwise = intercalate " + " (map showODET ode)

        showODET :: F.ODET -> String
        showODET (rate, obss) = K.showAExpr rate ++ " " ++ intercalate " " (map (names Map.!) obss)

