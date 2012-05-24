module Main where

import qualified KappaParser as KP
import qualified Types as T
import qualified Mixture as M
import qualified Env as E
import Utils

import qualified Data.Vector as Vec
import Control.Monad (zipWithM_)
import System.Environment (getArgs)
import System.FilePath (dropExtension)

glucopyranose :: Int -> String -> String -> String
glucopyranose _ c4 c6 = "C1C(O)C(O)C(C(O1)CO" ++ c6 ++ ")O" ++ c4

toSmiles :: E.AgentNameId -> E.SiteNameId -> E.SiteNameId -> E.SiteNameId -> M.Mixture -> String
toSmiles glucose c1 c4 c6 mix = "O" ++ smile 1 (findRoot 0)
  where smile :: Int -> Maybe M.AgentId -> String
        smile _ Nothing = ""
        smile n (Just aId) = glucopyranose n (smile n nb4) (smile (n+1) nb6)
          where nb4 = fst <$> M.follow mix (aId, c4)
                nb6 = fst <$> M.follow mix (aId, c6)

        findRoot :: M.AgentId -> Maybe M.AgentId
        findRoot possibleRootId -- the root needs to be a glucose and have the C1 site free
          | name == glucose && M.isFree (intf Vec.! c1) = Just possibleRootId
          | otherwise = do (nbId, _) <- M.follow mix (possibleRootId, c1)
                           findRoot nbId
          where M.Agent{ M.agentName = name, M.interface = intf } = M.agents mix Vec.! possibleRootId

main :: IO ()
main = do inputFilename : glucose : c1 : c4 : c6 : _ <- getArgs
          m <- KP.parseFromFile inputFilename
          let kexprs = map snd $ KP.inits m
              cm = T.inferCM kexprs
              env = E.createEnv KP.emptyModule{ KP.contactMap = cm }

              glucoseId = E.idOfAgent env glucose ? "Smiles: no '" ++ glucose ++ "' agent"
              c1Id = E.idOfSite env (glucoseId, c1) ? "Smiles: no '" ++ c1 ++ "' site in '" ++ glucose ++ "' agent"
              c4Id = E.idOfSite env (glucoseId, c4) ? "Smiles: no '" ++ c4 ++ "' site in '" ++ glucose ++ "' agent"
              c6Id = E.idOfSite env (glucoseId, c6) ? "Smiles: no '" ++ c6 ++ "' site in '" ++ glucose ++ "' agent"

              smiles = map (toSmiles glucoseId c1Id c4Id c6Id . M.evalKExpr env False) kexprs

              basename = dropExtension inputFilename
              outputFilenames = map makeOutFn [1..length smiles]
              makeOutFn n = basename ++ "-" ++ show n ++ ".smi"

          zipWithM_ writeFile outputFilenames smiles

