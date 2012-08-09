{-# LANGUAGE BangPatterns #-}

module KappaParser( SiteName, InternalState, BondLabel, BindingState(..), Site(..), Interface, siteName, agentName
                  , AgentName, Agent(..), KExpr, Rate, Rule(..), RuleName, RuleWithName
                  , CMBindingState(..), CMSite(..), CMIntf, CMAgent(..), CM -- CM types
                  , getSig, cmAgentName, cmInterface, cmSiteName, cmInternalStates, cmBindingStates -- CM helper functions
                  , AExpr(..), Unop(..), Duop(..)
                  , Obs(..), Shape, ShapeName, Init, Expr, Var, VarName, Module(..), emptyModule
                  , agent, kexpr, rule, aexpr, moduleParser
                  , fileParse, simpleParse, parseAgent, parseKExpr, parseRule, parseCM, parseModule, parseFromFile
                  , kappaDef, parens, decimal, naturalOrFloat, comma, commaSep, commaSep1, semiSep1, symbol, reservedOp, reserved, identifier, whiteSpace
                  ) where

import Prelude hiding (init)
import Control.Applicative ((<*), (<$>))
import Control.Monad (liftM)
import Data.List (delete)

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Text.Parsec.Error
import Text.Parsec.Indent


type Parser a = IndentParser String () a

-- Types
type SiteName = String
type InternalState = String
type BondLabel = Int
data BindingState = Free | SemiLink | Bound !BondLabel | Unspecified
  deriving (Show, Eq, Ord)
data Site = Site !SiteName !InternalState !BindingState
  deriving (Show, Eq, Ord)

siteName :: Site -> SiteName
siteName (Site name _ _) = name

type AgentName = String
type Interface = [Site]
data Agent = Agent !AgentName !Interface
  deriving (Show, Eq, Ord)

agentName :: Agent -> AgentName
agentName (Agent name _) = name

type KExpr = [Agent]

type Rate = AExpr
data Rule = Rule Bool KExpr KExpr Rate
  deriving (Show, Eq)

-- Language definition
kappaDef = emptyDef{ T.commentStart = "{-"
                   , T.commentEnd = "-}"
                   , T.commentLine = "--"
                   , T.nestedComments = True
                   , T.identStart = letter
                   , T.identLetter = alphaNum <|> oneOf "_'"
                   , T.opStart = oneOf "=<-@+*/^"
                   , T.opLetter = oneOf "=<->@+*/^"
                   , T.reservedOpNames = ["=", "->", "<->", "@", "+", "-", "*", "/", "^"]
                   , T.reservedNames = ["contact-map:", "init:", "obs:", "shape:", "shapes:", "rule:", "rules:", "...",
                                        "log", "exp", "mod", "sqrt", "sin", "cos", "tan", "int", "inf"]
                   }

T.TokenParser{ T.parens = parens
             , T.decimal = decimal
             , T.naturalOrFloat = naturalOrFloat
             , T.comma = comma
             , T.commaSep = commaSep
             , T.commaSep1 = commaSep1
             , T.semiSep1 = semiSep1
             , T.symbol = symbol
             , T.reservedOp = reservedOp
             , T.reserved = reserved
             , T.identifier = identifier
             , T.whiteSpace = whiteSpace } = T.makeTokenParser kappaDef


-- Kappa parsers
agent :: Parser Agent
agent = do name <- identifier <?> "agent"
           intf <- parens interface <?> "interface"
           return $ Agent name intf

interface :: Parser Interface
interface = commaSep site

site :: Parser Site
site = do siteName <- identifier <?> "site name"
          internalState <- (symbol "~" >> identifier) <|> return ""
          bindingState  <- (symbol "!" >> (bondLabel <|> semiLink))
                       <|> (symbol "?" >> return Unspecified)
                       <|> return Free
          return $! Site siteName internalState bindingState
  where bondLabel = do bondLabel <- decimal <?> "bond label"
                       return $ Bound (fromIntegral bondLabel)
        semiLink  = symbol "_" >> return SemiLink

createChain :: Agent -> Agent -> Agent -> KExpr
createChain first@(Agent fname fintf) second@(Agent sname sintf) last@(Agent lname lintf)
  | not (fname == sname && fname == lname) =
      error $ "KappaParser.createChain: all agents in a chain must be of the same type"
  | not (hasSameSites fintf sintf && hasSameSites fintf lintf) =
      error $ "KappaParser.createChain: all agents in a chain must have the same sites in their interface"
  | firstLink /= firstLink' =
      error $ "KappaParser.createChain: first and second agents in chain must be bound by sites '" ++ rightSite ++ "' and '" ++ leftSite ++ "', respectively"
  | otherwise =
      first : take n agentsInChain ++ [last]
  where
    agentsInChain = iterate nextAgentInChain second
    n = (lastLink - firstLink) `quot` step

    (Site leftSite _ _) : (Site rightSite _ (Bound firstLink)) : fOtherSites = fintf
    (Site _ _ (Bound firstLink')) : (Site _ _ (Bound secondLink)) : sOtherSites = sintf
    (Site _ _ (Bound lastLink)) : _ = lintf

    step = secondLink - firstLink
    steps = step : step : zipWith diff fOtherSites sOtherSites

    diff :: Site -> Site -> Int
    diff (Site _ _ (Bound i)) (Site _ _ (Bound j)) = j-i
    diff _ _ = 0

    nextAgentInChain :: Agent -> Agent
    nextAgentInChain (Agent name intf) = Agent name intf'
      where intf' = zipWith incSite intf steps

    incSite :: Site -> Int -> Site
    incSite (Site name is (Bound i)) step = Site name is (Bound $ i+step)
    incSite s _ = s

    hasSameSites :: Interface -> Interface -> Bool
    hasSameSites i1 i2 = map siteName i1 == map siteName i2

-- TODO I should get rid of all these reverses
unpackChains :: KExpr -> [Either () Agent] -> KExpr
unpackChains acc [] = reverse acc
unpackChains acc ((Right a1):(Right a2):(Left ()):(Right a3):xs) = unpackChains (reverse (createChain a1 a2 a3) ++ acc) xs
unpackChains acc ((Right a):xs) = unpackChains (a:acc) xs
unpackChains acc xs = error $ "malformed chain expression"

kexpr :: Parser KExpr
kexpr = unpackChains [] <$> commaSep (liftM Right agent <|> liftM Left ellipsis) <?> "kappa expression" -- commaSep or commaSep1?
  where ellipsis = reserved "..."

rule :: Parser Rule
rule = do lhs <- kexpr
          isReversible <- ((reservedOp "->" <?> "arrow") >> return False) <|>
                          ((reservedOp "<->" <?> "bidirectional arrow") >> return True)
          rhs <- kexpr
          reservedOp "@"
          rate <- aexpr
          return $ Rule isReversible lhs rhs rate

-- Contact Map
data CMBindingState = CMBound AgentName SiteName
  deriving (Show, Eq)
data CMSite = CMSite !SiteName ![InternalState] ![CMBindingState]
  deriving (Show, Eq)
type CMIntf = [CMSite]
data CMAgent = CMAgent !AgentName !CMIntf
  deriving (Show, Eq)
type CM = [CMAgent]

-- CM helper functions
getSig :: CM -> AgentName -> CMAgent
getSig cm name = head $ filter ((== name) . cmAgentName) cm

cmAgentName :: CMAgent -> AgentName
cmAgentName (CMAgent cmName _) = cmName

cmInterface :: CMAgent -> CMIntf
cmInterface (CMAgent _ cmIntf) = cmIntf

cmSiteName :: CMSite -> SiteName
cmSiteName (CMSite name _ _) = name

cmInternalStates :: CMSite -> [InternalState]
cmInternalStates (CMSite _ iss _) = iss

cmBindingStates :: CMSite -> [CMBindingState]
cmBindingStates (CMSite _ _ bss) = bss

-- CM parsers
cmAgent :: Parser CMAgent
cmAgent = do name <- identifier <?> "agent signature"
             intf <- parens cmIntf <?> "signature interface"
             return $ CMAgent name intf

cmIntf :: Parser CMIntf
cmIntf = commaSep cmSite

states :: Parser a -> String -> Parser [a]
states stateParser s = (symbol s >> stateSet) <|> return []
  where stateSet = do symbol "{"
                      xs <- commaSep1 stateParser
                      symbol "}"
                      return xs
                   <|>
                   do x <- stateParser
                      return [x]

cmBindingState :: Parser CMBindingState
cmBindingState = do agentName <- identifier
                    char '.'
                    siteName <- identifier
                    return $ CMBound agentName siteName

cmSite :: Parser CMSite
cmSite = do siteName <- identifier <?> "site name"
            internalStates <- states identifier "~"
            bindingStates <- states cmBindingState "!"
            return $ CMSite siteName internalStates bindingStates

-- Algebraic Expressions
data AExpr = Var String | Integer Int | Float Double | Uno Unop AExpr | Duo Duop AExpr AExpr | Infinity
  deriving (Show, Eq)

data Unop = Log | Sqrt | Exp | Sin | Cos | Tan | Int
  deriving (Show, Eq)
data Duop = Add | Sub | Mult | Div | Mod | Pow
  deriving (Show, Eq)

aexpr :: Parser AExpr
aexpr = buildExpressionParser table term <?> "algebraic expression"

table = [ [Prefix (reserved "log" >> return (Uno Log)),
           Prefix (reserved "exp" >> return (Uno Exp)),
           Prefix (reserved "sin" >> return (Uno Sin)),
           Prefix (reserved "cos" >> return (Uno Cos)),
           Prefix (reserved "tan" >> return (Uno Tan)),
           Prefix (reserved "int" >> return (Uno Int)),
           Prefix (reserved "sqrt" >> return (Uno Sqrt))]
        , [Infix (reservedOp "^" >> return (Duo Pow)) AssocLeft]
        , [Infix (reservedOp "*" >> return (Duo Mult)) AssocLeft,
           Infix (reservedOp "/" >> return (Duo Div)) AssocLeft,
           Infix (reserved "mod" >> return (Duo Mod)) AssocLeft]
        , [Infix (reservedOp "+" >> return (Duo Add)) AssocLeft,
           Infix (reservedOp "-" >> return (Duo Sub)) AssocLeft]
        ]

numParser :: Parser (Either Int Double)
numParser = do s <- char '+' <|> char '-' <|> return '+'
               n <- naturalOrFloat
               return $ toInt (s == '+') n
  where toInt isPositive (Left x)  = Left . fromInteger $ neg isPositive x
        toInt isPositive (Right x) = Right $ neg isPositive x
        neg isPositive x | isPositive = x
                         | otherwise  = negate x

term = parens aexpr <|> (reservedOp "inf" >> return Infinity) <|> fmap Var identifier <|>
       do n <- numParser
          return $ case n of
                     Left n -> Integer n
                     Right n -> Float n

type ShapeName = String
type Shape = (KExpr, AExpr)

type VarName = String
type Var = (VarName, Expr)

type RuleName = String
type RuleWithName = (RuleName, Rule)

type Init = (Int, KExpr)
type Expr = Either KExpr AExpr
data Obs = KExprWithName VarName KExpr | Plot VarName
  deriving (Show, Eq)

data Module = Module{ contactMap :: CM
                    , shapes :: [Shape]
                    , rules :: [RuleWithName]
                    , inits :: [Init]
                    , obss :: [Obs]
                    , vars :: [Var]
                    }
  deriving (Show, Eq)

emptyModule = Module{ contactMap = []
                    , shapes = []
                    , rules = []
                    , inits = []
                    , obss = []
                    , vars = []
                    }

initP :: Parser Init
initP = do reserved "init:"
           n <- decimal
           whiteSpace
           e <- kexpr
           return (fromIntegral n, e)

obsP :: Parser Obs
obsP = do reserved "obs:"
          getKExprWithName <|> getId
  where getId = identifier >>= return . Plot
        getKExprWithName = do char '\''
                              name <- many $ noneOf "'"
                              char '\''
                              whiteSpace
                              ke <- kexpr
                              return $ KExprWithName name ke

varP :: Parser Var
varP = do name <- identifier
          reservedOp "="
          expr <- liftM Right aexpr <|> liftM Left kexpr
          return (name, expr)

energyShape :: Parser Shape
energyShape = do expr <- kexpr
                 reservedOp "@"
                 energy <- aexpr
                 return (expr, energy)


shapeP :: Parser Shape
shapeP = reserved "shape:" >> energyShape

shapesP :: Parser [Shape]
shapesP = reserved "shapes:" >> block energyShape

ruleWithName :: Parser RuleWithName
ruleWithName = do name <- try (identifier <* reservedOp "=") <|> return ""
                  r <- rule
                  return (name, r)

ruleP :: Parser RuleWithName
ruleP = reserved "rule:" >> ruleWithName

rulesP :: Parser [RuleWithName]
rulesP = reserved "rules:" >> block ruleWithName

-- FIXME cmP should be indentation-aware
cmP :: Parser CM
cmP = reserved "contact-map:" >> commaSep1 cmAgent

data Decl = CMDecl CM
          | ShapeDecl Shape
          | ShapesDecl [Shape]
          | RuleDecl RuleWithName
          | RulesDecl [RuleWithName]
          | InitDecl Init
          | ObsDecl Obs
          | VarDecl Var

createModule :: [Decl] -> Module
createModule decls = foldr (flip addDecl) emptyModule decls
  where addDecl m (CMDecl cm)     = m{ contactMap = cm }
        addDecl m (ShapesDecl ss) = m{ shapes = ss ++ shapes m }
        addDecl m (ShapeDecl s)   = m{ shapes = s : shapes m }
        addDecl m (RulesDecl rs)  = m{ rules = rs ++ rules m }
        addDecl m (RuleDecl r)    = m{ rules = r : rules m }
        addDecl m (InitDecl i)    = m{ inits = i : inits m }
        addDecl m (ObsDecl o)     = m{ obss = o : obss m }
        addDecl m (VarDecl v)     = m{ vars = v : vars m }

moduleParser :: Parser Module
moduleParser = whiteSpace >> kfParser <* eof
  where kfParser :: Parser Module
        kfParser = do decls <- many declParser
                      return $ createModule decls

        declParser :: Parser Decl
        declParser = liftM CMDecl cmP
                 <|> liftM ShapeDecl shapeP
                 <|> liftM ShapesDecl shapesP
                 <|> liftM RuleDecl ruleP
                 <|> liftM RulesDecl rulesP
                 <|> liftM InitDecl initP
                 <|> liftM ObsDecl obsP
                 <|> liftM VarDecl varP


-- Helper functions
fileParse :: String -> Parser a -> String -> a
fileParse filename p s = case runIndent filename $ runParserT p () filename s of
                           Left e -> error $ show e -- ParseError
                           Right result -> result

simpleParse :: Parser a -> String -> a
simpleParse = fileParse ""

parseAgent :: String -> Agent
parseAgent = simpleParse agent

parseKExpr :: String -> KExpr
parseKExpr = simpleParse kexpr

parseRule :: String -> Rule
parseRule = simpleParse rule

parseCM :: String -> CM
parseCM = simpleParse cmP

parseModule :: String -> Module
parseModule = simpleParse moduleParser

parseFromFile :: String -> IO Module
parseFromFile filename = readFile filename >>= return . fileParse filename moduleParser

