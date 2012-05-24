module Main where

import qualified KappaParser as KP
import qualified Types as T
import qualified Mixture as M
import qualified Env as E
import Utils

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)
import Control.Monad (zipWithM_)
import System.Environment (getArgs)
import System.FilePath (dropExtension)


toDot :: E.Env -> M.Mixture -> String
toDot env mix =
  "graph {\n" ++
  "  overlap = \"scale\";\n" ++
  "  sep = \"1\";\n" ++
  "  node [ shape = \"circle\" ];\n\n" ++
  concatMap nodeDot (M.agentsWithId mix) ++
  concatMap linkDot links ++
  "}\n"
  where nodes = Vec.imap nodeName $ M.agents mix
        nodeName i  agent  = agentName agent ++ show i
        nodeDot (i, agent) = "  " ++ nodeName i agent ++ " [ label = \"" ++ agentName agent ++ "\" ];\n"

        links = Set.toList $ M.links mix
        linkDot ((aId1, sId1), (aId2, sId2)) =
          "  "   ++ (nodes Vec.! aId1) ++
          " -- " ++ (nodes Vec.! aId2) ++
          " [ headlabel = \"" ++ siteName aId1 sId1 ++ "\"" ++
          " , taillabel = \"" ++ siteName aId2 sId2 ++ "\" ];\n"

        agentName agent = E.agentOfId env (M.agentName agent) ? "Matching.detailedDot: missing agent name id"
        siteName aId sId = E.siteOfId env (M.agentName (M.agents mix Vec.! aId), sId) ? "Matching.detailedDot: missing site id"

main :: IO ()
main = do inputFilename : _ <- getArgs
          m <- KP.parseFromFile inputFilename
          let kexprs = map snd $ KP.inits m
              cm = T.inferCM kexprs
              env = E.createEnv KP.emptyModule{ KP.contactMap = cm }
              mixs = map (M.evalKExpr env False) kexprs
              dots = map (toDot env) mixs

              basename = dropExtension inputFilename
              outputFilenames = map makeOutFn [1..length dots]
              makeOutFn n = basename ++ "-" ++ show n ++ ".dot"

          zipWithM_ writeFile outputFilenames dots



