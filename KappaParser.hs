module KappaParser( SiteName, InternalState, BondLabel, BindingState(..), Site(..), Interface, siteName, agentName
                  , AgentName, Agent(..), KExpr, Rate, Rule(..), RuleName, RuleWithName
                  , CMBindingState(..), CMSite(..), CMIntf, CMAgent(..), CM -- CM types
                  , getSig, cmAgentName, cmInterface, cmSiteName, cmInternalStates, cmBindingStates -- CM helper functions
                  , AExpr(..), Unop(..), Duop(..)
                  , Obs(..), Shape, ShapeName, Init, Expr, Var, VarName, Module(..), emptyModule
                  , agent, kexpr, rule, aexpr, moduleParser
                  , simpleParse, parseAgent, parseKExpr, parseRule, parseModule, parseFromFile, parseKExprsFromFile
                  ) where

import Prelude hiding (init)
import Control.Applicative ((<*))
import Control.Monad (liftM)
import Data.List (delete)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Error
import Text.Parsec.Indent


type Parser a = IndentParser String () a

-- Types
type SiteName = String
type InternalState = String
type BondLabel = Int
data BindingState = Free | SemiLink | Bound BondLabel | Unspecified
  deriving (Show, Eq, Ord)
data Site = Site SiteName InternalState BindingState
  deriving (Show, Eq, Ord)

siteName :: Site -> SiteName
siteName (Site name _ _) = name

type AgentName = String
type Interface = [Site]
data Agent = Agent AgentName Interface
  deriving (Show, Eq, Ord)

agentName :: Agent -> AgentName
agentName (Agent name _) = name

type KExpr = [Agent]

type Rate = AExpr
data Rule = Rule Bool KExpr KExpr Rate
  deriving (Show, Eq)

-- Language definition
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , commentLine = "--"
              , nestedComments = True
              , identStart = letter
              , identLetter = alphaNum <|> oneOf "_'"
              , opStart = oneOf "=<-@+*/^"
              , opLetter = oneOf "=<->@+*/^"
              , reservedOpNames = ["=", "->", "<->", "@", "+", "-", "*", "/", "^"]
              , reservedNames = ["contact-map:", "init:", "obs:", "shape:", "shapes:", "rule:", "rules:", "...",
                                 "log", "exp", "mod", "sqrt", "sin", "cos", "tan", "int", "inf"]
              }

TokenParser{ parens = m_parens
           , decimal = m_decimal
           , naturalOrFloat = m_naturalOrFloat
           , comma = m_comma
           , commaSep = m_commaSep
           , commaSep1 = m_commaSep1
           , semiSep1 = m_semiSep1 -- only used for parseKExprsFromFile
           , symbol = m_symbol
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , identifier = m_identifier
           , whiteSpace = m_whiteSpace } = makeTokenParser def


-- Kappa parsers
agent :: Parser Agent
agent = do name <- m_identifier <?> "agent"
           intf <- m_parens interface <?> "interface"
           return $ Agent name intf

interface :: Parser Interface
interface = m_commaSep site

site :: Parser Site
site = do siteName <- m_identifier <?> "site name"
          internalState <- (m_symbol "~" >> m_identifier) <|> return ""
          bindingState <- (m_symbol "!" >> (do bondLabel <- m_decimal
                                               return . Bound . fromIntegral $ bondLabel
                                        <|> do m_symbol "_"
                                               return SemiLink))
                          <|> (m_symbol "?" >> return Unspecified)
                          <|> return Free
          return $ Site siteName internalState bindingState

createChain :: Agent -> Agent -> Agent -> KExpr
createChain first@(Agent fname fintf) second@(Agent sname sintf) last@(Agent lname lintf) =
  if fname == sname && fname == lname
    then if hasSameSites fintf sintf && hasSameSites fintf lintf
           then if firstLink == firstLink'
                  then chain
                  else error $ "createChain: first and second agents in chain must be bound by sites '" ++ rightSite ++ "' and '" ++ leftSite ++ "', respectively"
           else error "createChain: all agents in a chain must have the same sites in their interface"
    else error "createChain: all agents in a chain must be of the same type"

  where
    chain = first : take n agentsInChain ++ [last]
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

chainExpr :: Parser KExpr
chainExpr = do first <- agent
               m_comma
               second <- agent
               m_comma
               m_reserved "..."
               m_comma
               last <- agent
               return $ createChain first second last

kexpr :: Parser KExpr
kexpr = do xs <- m_commaSep1 (try chainExpr <|> singleAgentExpr) <?> "kappa expression"
           return $ concat xs
  where singleAgentExpr = do a <- agent
                             return [a]

rule :: Parser Rule
rule = do lhs <- kexpr
          isReversible <- ((m_reservedOp "->" <?> "arrow") >> return False) <|>
                          ((m_reservedOp "<->" <?> "bidirectional arrow") >> return True)
          rhs <- kexpr
          m_reservedOp "@"
          rate <- aexpr
          return $ Rule isReversible lhs rhs rate

-- Contact Map
data CMBindingState = CMBound AgentName SiteName
  deriving (Show, Eq)
data CMSite = CMSite SiteName [InternalState] [CMBindingState]
  deriving (Show, Eq)
type CMIntf = [CMSite]
data CMAgent = CMAgent AgentName CMIntf
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
cmAgent = do name <- m_identifier <?> "agent signature"
             intf <- m_parens cmIntf <?> "signature interface"
             return $ CMAgent name intf

cmIntf :: Parser CMIntf
cmIntf = m_commaSep cmSite

states :: Parser a -> String -> Parser [a]
states stateParser s = (m_symbol s >> stateSet) <|> return []
  where stateSet = do m_symbol "{"
                      xs <- m_commaSep1 stateParser
                      m_symbol "}"
                      return xs
                   <|>
                   do x <- stateParser
                      return [x]

cmBindingState :: Parser CMBindingState
cmBindingState = do agentName <- m_identifier
                    char '.'
                    siteName <- m_identifier
                    return $ CMBound agentName siteName

cmSite :: Parser CMSite
cmSite = do siteName <- m_identifier <?> "site name"
            internalStates <- states m_identifier "~"
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

table = [ [Prefix (m_reserved "log" >> return (Uno Log)),
           Prefix (m_reserved "exp" >> return (Uno Exp)),
           Prefix (m_reserved "sin" >> return (Uno Sin)),
           Prefix (m_reserved "cos" >> return (Uno Cos)),
           Prefix (m_reserved "tan" >> return (Uno Tan)),
           Prefix (m_reserved "int" >> return (Uno Int)),
           Prefix (m_reserved "sqrt" >> return (Uno Sqrt))]
        , [Infix (m_reservedOp "^" >> return (Duo Pow)) AssocLeft]
        , [Infix (m_reservedOp "*" >> return (Duo Mult)) AssocLeft,
           Infix (m_reservedOp "/" >> return (Duo Div)) AssocLeft,
           Infix (m_reserved "mod" >> return (Duo Mod)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft,
           Infix (m_reservedOp "-" >> return (Duo Sub)) AssocLeft]
        ]

numParser :: Parser (Either Int Double)
numParser = do s <- char '+' <|> char '-' <|> return '+'
               n <- m_naturalOrFloat
               return $ toInt (s == '+') n
  where toInt isPositive (Left x)  = Left . fromInteger $ neg isPositive x
        toInt isPositive (Right x) = Right $ neg isPositive x
        neg isPositive x | isPositive = x
                         | otherwise  = negate x

term = m_parens aexpr <|> (m_reservedOp "inf" >> return Infinity) <|> fmap Var m_identifier <|>
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
initP = do m_reserved "init:"
           n <- m_decimal
           m_whiteSpace
           e <- kexpr
           return (fromIntegral n, e)

obsP :: Parser Obs
obsP = do m_reserved "obs:"
          getKExprWithName <|> getId
  where getId = m_identifier >>= return . Plot
        getKExprWithName = do char '\''
                              name <- many $ noneOf "'"
                              char '\''
                              m_whiteSpace
                              ke <- kexpr
                              return $ KExprWithName name ke

varP :: Parser Var
varP = do name <- m_identifier
          m_reservedOp "="
          ke <- kexpr
          if null ke
            then do ae <- aexpr
                    return (name, Right ae)
            else return (name, Left ke)

energyShape :: Parser Shape
energyShape = do expr <- kexpr
                 m_reservedOp "@"
                 energy <- aexpr
                 return (expr, energy)


shapeP :: Parser Shape
shapeP = m_reserved "shape:" >> energyShape

shapesP :: Parser [Shape]
shapesP = m_reserved "shapes:" >> block energyShape

ruleWithName :: Parser RuleWithName
ruleWithName = do name <- try (m_identifier <* m_reservedOp "=") <|> return ""
                  r <- rule
                  return (name, r)

ruleP :: Parser RuleWithName
ruleP = m_reserved "rule:" >> ruleWithName

rulesP :: Parser [RuleWithName]
rulesP = m_reserved "rules:" >> block ruleWithName

-- FIXME cmP should be indentation-aware
cmP :: Parser CM
cmP = m_reserved "contact-map:" >> m_commaSep1 cmAgent

data Decl = CMDecl CM
          | ShapeDecl Shape
          | ShapesDecl [Shape]
          | RuleDecl RuleWithName
          | RulesDecl [RuleWithName]
          | InitDecl Init
          | ObsDecl Obs
          | VarDecl Var

createModule :: [Decl] -> Module
createModule decls = foldr addDecl emptyModule decls
  where addDecl (CMDecl cm) m = m{ contactMap = cm }
        addDecl (ShapesDecl ss) m = m{ shapes = ss ++ shapes m }
        addDecl (ShapeDecl s) m = m{ shapes = s : shapes m }
        addDecl (RulesDecl rs) m = m{ rules = rs ++ rules m }
        addDecl (RuleDecl r) m = m{ rules = r : rules m }
        addDecl (InitDecl i) m = m{ inits = i : inits m }
        addDecl (ObsDecl o) m = m{ obss = o : obss m }
        addDecl (VarDecl v) m = m{ vars = v : vars m }

moduleParser :: Parser Module
moduleParser = m_whiteSpace >> kfParser <* eof
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
simpleParse :: Parser a -> String -> a
simpleParse p s = case runIndent "" $ runParserT p () "" s of
                    Left e -> error $ show e -- ParseError
                    Right result -> result

parseAgent :: String -> Agent
parseAgent = simpleParse agent

parseKExpr :: String -> KExpr
parseKExpr = simpleParse kexpr

parseRule :: String -> Rule
parseRule = simpleParse rule

parseModule :: String -> Module
parseModule = simpleParse moduleParser


parseFromFile :: String -> IO Module
parseFromFile filename = do s <- readFile filename
                            case runIndent filename $ runParserT moduleParser () filename s of
                              Left e -> error $ show e
                              Right kappaModule -> return kappaModule

-- This function is only useful for MinimalGlueings.hs
parseKExprsFromFile :: String -> IO [KExpr]
parseKExprsFromFile filename = do s <- readFile filename
                                  case runIndent filename $ runParserT (m_semiSep1 kexpr) () filename s of
                                    Left e -> error $ show e
                                    Right kexprs -> return kexprs

