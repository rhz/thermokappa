module Main where

import qualified KappaParser as KP
import qualified TypeInference as TI
import qualified Env as E
import qualified Mixture as M
import Matching

import Control.Monad (zipWithM_)
import System.Environment (getArgs)
import System.FilePath (dropExtension)

main :: IO ()
main = do inputFilename : _ <- getArgs
          kexprs <- KP.parseKExprsFromFile inputFilename
          let cm = TI.inferCM kexprs
              env = E.createEnv KP.emptyModule{ KP.contactMap = cm }

              m1 : m2 : _ = map (M.evalKExpr env True) kexprs

              m3s = do (m3, ((m1FwdMap, m1AgentMap), (m2FwdMap, m2AgentMap))) <- minimalGlueings env m1 m2
                       return (m3, (m1AgentMap, m2AgentMap))

              cDot = condensedDot env m1 m2 m3s
              dDots = map (detailedDot env m1 m2) m3s

              basename = dropExtension inputFilename
              outputFilenames = map (++ ".dot") . map ((basename ++ "-") ++) $ map show [0..length m3s]

          writeFile (basename ++ ".dot") cDot
          zipWithM_ writeFile outputFilenames dDots

