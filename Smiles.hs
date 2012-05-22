module Main where

import KappaParser
import Utils
import Misc

import qualified Data.Map as Map
import Data.List (find)
import Control.Monad (zipWithM_)
import System.Environment (getArgs)
import System.FilePath (dropExtension)

glucopyranose :: String -> String -> String
glucopyranose c4 c6 = "C1C(O)C(O)C(C(CO" ++ c6 ++ ")O1)O" ++ c4

-- TODO a better way is to take any agent and go to the agent at C1
findFirst :: AgentName -> SiteName -> KExpr -> Maybe Agent
findFirst glucose c1 = find isFirst
  where isFirst (Agent agentName intf) = agentName == glucose && any isFreeOnC1 intf
        isFreeOnC1 (Site siteName _ Free) = siteName == c1
        isFreeOnC1 _ = False

toSmiles :: AgentName -> SiteName -> SiteName -> SiteName -> KExpr -> String
toSmiles glucose c1 c4 c6 kexpr = "O" ++ smile (Just anchor)
  where lm = linkMap kexpr
        anchor = findFirst glucose c1 kexpr ? "Smiles.toSmiles: could not find a free site '" ++ c1 ++ "'"

        smile :: Maybe Agent -> String
        smile Nothing = ""
        smile (Just (Agent _ intf)) = glucopyranose (smile $ neighbourAt "C4") (smile $ neighbourAt "C6")
          where neighbourAt sn = do (Site _ _ (Bound bl)) <- find ((== sn) . siteName) intf
                                    (a1, sn1, sn2, a2) <- Map.lookup bl lm
                                    if sn1 == sn
                                      then return a2
                                      else return a1

main :: IO ()
main = do inputFilename : glucose : c1 : c4 : c6 : _ <- getArgs
          m <- parseFromFile inputFilename
          let kexprs = map snd $ inits m
              smiles = map (toSmiles glucose c1 c4 c6)  kexprs

              basename = dropExtension inputFilename
              outputFilenames = map (++ ".smi") . map ((basename ++ "-") ++) $ map show [0..length smiles]

          zipWithM_ writeFile outputFilenames smiles

