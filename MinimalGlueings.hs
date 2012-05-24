module Main where

import qualified KappaParser as KP
import qualified Types as T
import qualified Env as E
import qualified Mixture as M
import Matching
import Utils

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, nub)
import Control.Monad (zipWithM_)
import System.Environment (getArgs)
import System.FilePath (dropExtension)

main :: IO ()
main = do inputFilename : _ <- getArgs
          kexprs <- KP.parseKExprsFromFile inputFilename
          let cm = T.inferCM kexprs
              env = E.createEnv KP.emptyModule{ KP.contactMap = cm }

              m1 : m2 : _ = map (M.evalKExpr env True) kexprs

              m3s = minimalGlueings m1 m2

              cDot = condensedDot env m1 m2 m3s
              dDots = map (detailedDot env m1 m2) m3s

              basename = dropExtension inputFilename
              outputFilenames = map makeOutFn [1..length dDots]
              makeOutFn n = basename ++ "-" ++ show n ++ ".dot"

          writeFile (basename ++ ".dot") cDot
          zipWithM_ writeFile outputFilenames dDots


-- GraphViz
condensedDot :: E.Env -> M.Mixture -> M.Mixture -> [(M.Mixture, (AgentMap, AgentMap))] -> String
condensedDot env m1 m2 m3s =
  "digraph {\n" ++
  "  overlap = \"prism\";\n" ++
  "  node [ shape = \"box\" ];\n" ++
  "  m1 [ label = \"" ++ M.toKappa env m1 ++ "\" ];\n" ++
  "  m2 [ label = \"" ++ M.toKappa env m2 ++ "\" ];\n\n" ++
  intercalate "\n" (zipWith m3Dot [3..] m3s) ++
  "}\n"
  where m3Dot i (m3, (m1AgentMap, m2AgentMap)) =
          "  m" ++ show i ++ " [ label = \"" ++ M.toKappa env m3 ++ "\" ];\n" ++
          "  m1 -> m" ++ show i ++ " [ label = \"" ++ show (Map.toList m1AgentMap) ++ "\", color = \"firebrick3\",  fontcolor = \"firebrick3\"  ];\n" ++
          "  m2 -> m" ++ show i ++ " [ label = \"" ++ show (Map.toList m2AgentMap) ++ "\", color = \"dodgerblue3\", fontcolor = \"dodgerblue3\" ];\n"

detailedDot :: E.Env -> M.Mixture -> M.Mixture -> (M.Mixture, (AgentMap, AgentMap)) -> String
detailedDot env m1 m2 (m3, (m1AgentMap, m2AgentMap)) =
  "digraph {\n" ++
  "  overlap = \"scale\";\n" ++
  "  sep = \"1\";\n" ++
  "  node [ shape = \"circle\" ];\n\n" ++
  exprDot m1 "M1" ++ "\n" ++
  exprDot m2 "M2" ++ "\n" ++
  exprDot m3 "M3" ++ "\n" ++
  matchingsDot m1AgentMap m1 "M1" m3 "M3" "firebrick3" ++ "\n" ++
  matchingsDot m2AgentMap m2 "M2" m3 "M3" "dodgerblue3" ++
  "}\n"
  where exprDot mix prefix = "  subgraph {\n" ++
                             concatMap nodeDot (M.agentsWithId mix) ++
                             concatMap linkDot links ++
                             "  }\n"
          where nodes = Vec.imap nodeName $ M.agents mix
                nodeName i  agent  = prefix ++ agentName  agent ++ show i
                nodeDot (i, agent) = "    " ++ nodeName i agent ++ " [ label = \"" ++ agentName agent ++ "\" ];\n"

                links = Set.toList $ M.links mix
                linkDot ((aId1, sId1), (aId2, sId2)) =
                  "    " ++ (nodes Vec.!? aId1 ? "Matching.detailedDot: " ++ show aId1 ++ ", " ++ show nodes ++ ", " ++ show (M.toKappa env mix)) ++
                  " -> " ++ (nodes Vec.!? aId2 ? "Matching.detailedDot: " ++ show aId2 ++ ", " ++ show nodes ++ ", " ++ show (M.toKappa env mix)) ++
                  " [ headlabel = \"" ++ siteName aId1 sId1 mix ++ "\"" ++
                  " , taillabel = \"" ++ siteName aId2 sId2 mix ++ "\", arrowhead = \"none\" ];\n"

        agentName agent = E.agentOfId env (M.agentName agent) ? "Matching.detailedDot: missing agent name id"
        siteName aId sId mix = E.siteOfId env (M.agentName (M.agents mix Vec.! aId), sId) ? "Matching.detailedDot: missing site id"

        matchingsDot agentMap sourceMix sourcePrefix targetMix targetPrefix color = concatMap matchingDot $ Map.toList agentMap
          where matchingDot (sourceId, targetId) = "  "   ++ nodeName sourceId sourceMix sourcePrefix ++
                                                   " -> " ++ nodeName targetId targetMix targetPrefix ++
                                                   " [ style = \"dashed\", color = \"" ++ color ++ "\" ];\n"

                nodeName aId mix prefix = prefix ++ agentName (M.agents mix Vec.! aId) ++ show aId

{-
toDot :: E.Env -> M.Mixture -> M.Mixture -> M.Mixture -> (FwdMap, AgentMap) -> (FwdMap, AgentMap) -> String
toDot env m1 m2 m3 (m1FwdMap, m1AgentMap) (m2FwdMap, m2AgentMap) =
  "digraph {\n" ++
  "  graph [ overlap = \"scale\" ];\n" ++
  "  node [ shape = \"box\" ];\n" ++
  "  m1 [ label = \"" ++ M.toKappa env m1 ++ "\" ];\n" ++
  "  m2 [ label = \"" ++ M.toKappa env m2 ++ "\" ];\n" ++
  "  m3 [ label = \"" ++ M.toKappa env m3 ++ "\" ];\n" ++
  "  m1 -> m3 [ label = \"" ++ show (Map.toList m1AgentMap) ++ "\" ];\n" ++
  "  m2 -> m3 [ label = \"" ++ show (Map.toList m2AgentMap) ++ "\" ];\n" ++
  "}\n"
-}

