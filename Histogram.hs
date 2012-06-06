module Main where

import qualified KappaParser as KP
import qualified Types as T
import qualified Mixture as M
import qualified Env as E
import Utils

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Monad (zipWithM_)
import System.Environment (getArgs)
import System.FilePath (dropExtension)

type Depth = Int
type Histogram = Map.Map Depth Int

histogram :: E.AgentNameId -> E.SiteNameId -> E.SiteNameId -> E.SiteNameId -> M.Mixture -> (Histogram, Histogram, Histogram)
histogram glucose c1 c4 c6 mix = (makeHist $ depth 1 root, makeHist $ branch 1 (Just root), makeHist $ chainLen 0 (Just root))
  where depth :: Int -> M.AgentId -> [Depth]
        depth n aId = case (nb4, nb6) of
                        (Nothing, Nothing)       -> [n]
                        (Nothing, Just nb6Id)    ->  n : depth (n+1) nb6Id
                        (Just nb4Id, Nothing)    -> depth (n+1) nb4Id
                        (Just nb4Id, Just nb6Id) -> depth (n+1) nb4Id ++ depth (n+1) nb6Id
          where nb4 = fst <$> M.follow mix (aId, c4)
                nb6 = fst <$> M.follow mix (aId, c6)

        branch :: Int -> Maybe M.AgentId -> [Depth]
        branch _ Nothing = []
        branch n (Just aId) = case nb6 of
                                Nothing -> branch (n+1) nb4
                                _       -> n : branch 1 nb6 ++ branch 1 nb4
          where nb4 = fst <$> M.follow mix (aId, c4)
                nb6 = fst <$> M.follow mix (aId, c6)

        chainLen :: Int -> Maybe M.AgentId -> [Depth]
        chainLen n Nothing | n == 0    = []
                           | otherwise = [n]
        chainLen n (Just aId) = branch (n+1) nb4 ++ branch 0 nb6
          where nb4 = fst <$> M.follow mix (aId, c4)
                nb6 = fst <$> M.follow mix (aId, c6)

        root = findRoot 0
        makeHist = Map.fromList . frequencies

        findRoot :: M.AgentId -> M.AgentId
        findRoot possibleRootId -- the root needs to be a glucose and have the C1 site free
          | name == glucose && M.isFree (intf Vec.! c1) = possibleRootId
          | otherwise                                   = findRoot nbId
          where M.Agent{ M.agentName = name, M.interface = intf } = M.agents mix Vec.! possibleRootId
                (nbId, _) = M.follow mix (possibleRootId, c1) ? "Histogram: site C1 is not free nor bound"

makeTable :: Histogram -> String
makeTable hist = intercalate "\n" . map toRow $ Map.toAscList hist
  where toRow (x, freq) = show x ++ " " ++ show freq

main :: IO ()
main = do inputFilename : glucose : c1 : c4 : c6 : _ <- getArgs
          m <- KP.parseFromFile inputFilename
          let kexprs = map snd $ KP.inits m
              cm = T.inferCM kexprs
              env = E.createEnv KP.emptyModule{ KP.contactMap = cm }

              glucoseId = E.idOfAgent env glucose ? "Histogram: no '" ++ glucose ++ "' agent"
              c1Id = E.idOfSite env (glucoseId, c1) ? "Histogram: no '" ++ c1 ++ "' site in '" ++ glucose ++ "' agent"
              c4Id = E.idOfSite env (glucoseId, c4) ? "Histogram: no '" ++ c4 ++ "' site in '" ++ glucose ++ "' agent"
              c6Id = E.idOfSite env (glucoseId, c6) ? "Histogram: no '" ++ c6 ++ "' site in '" ++ glucose ++ "' agent"

              (depths, branchs, chainLens) = unzip3 $ map (histogram glucoseId c1Id c4Id c6Id . M.evalKExpr env False) kexprs

              basename = dropExtension inputFilename
              outputFilenames suffix = map (makeOutFn suffix) [1..length kexprs]
              makeOutFn suffix n = basename ++ "-" ++ show n ++ "-" ++ suffix ++ ".hist"

          zipWithM_ writeFile (outputFilenames "depth")     (map makeTable depths)
          zipWithM_ writeFile (outputFilenames "branch")    (map makeTable branchs)
          zipWithM_ writeFile (outputFilenames "chain-len") (map makeTable chainLens)

