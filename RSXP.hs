module RSXP (
    XMLAST (Element, Body, Comment)
  , parseXML
  , getAllBodies
  , getBodiesByName
  , getAllElements
  , getElementsByName
  , getElementsByPath
) where

import Text.ParserCombinators.Parsec

data XMLAST =  
    Element Name [Attribute] [XMLAST]
  | Body String
  | Comment String
  | CouldNotParse
  deriving Show

type Name      = String
type Attribute = (Key, Value)
type Key       = String
type Value     = String 


parseXML :: String -> XMLAST
parseXML str =
  f ast where
      ast = parse (spaces >> xmlParser) "" str
      f (Right x) = x
      f (Left x) = CouldNotParse
      
xmlParser :: Parser XMLAST
xmlParser = 
  try withoutExplictCloseTag <|>  withExplicitCloseTag


withExplicitCloseTag :: Parser XMLAST
withExplicitCloseTag = 
  do
    (name, attr) <- openTag
    innerXML <- many innerXML
    closeTag name
    return (Element name attr innerXML)

innerXML = comment <|> xmlParser <|> parseBody

parseBody = fmap Body $ many1 $ noneOf "<>"

comment :: Parser XMLAST
comment =
  do
    try $ string "<!--"
    body <- manyTill anyChar (string "-->")
    return (Comment body)

openTag :: Parser (String, [(String,String)])
openTag =
  do
    try $ char '<' >> notFollowedBy (char '/')
    tag <- many (letter <|> digit)
    spaces
    a <- try (many keyValue)
    char '>'
    return (tag, a)

closeTag :: String -> Parser ()
closeTag str =
  do
    try $ string "</"
    spaces
    string str
    spaces
    char '>'
    return ()

withoutExplictCloseTag :: Parser XMLAST
withoutExplictCloseTag = 
  do
    try $ char '<' >> notFollowedBy (char '/')
    name <- many (letter <|> digit)
    spaces
    a <- try (many keyValue)
    spaces
    string "/>"
    return (Element name a [])

keyValue :: Parser (String, String)
keyValue = 
  do
    key <- many1 (letter <|> digit)
    spaces
    char '='
    spaces
    value <- quotedString
    spaces
    return (key, value)

quotedString :: Parser String
quotedString = do
  q <- (try (char '"')) <|> char '\''
  value <- fmap concat $ many
    $ many1 (noneOf ['\\', q])
      <|> try (string ['\\', q])
      <|> try (string "\\")
  char q
  return value


getAllElements :: XMLAST -> [(XMLAST, String, XMLAST)]
getAllElements ast = getAllElements' ast "" ast
getAllElements' pe pp element@(Element n a es) = concat $ map (getAllElements' element (pp ++ "/" ++ n)) es
getAllElements' pe pp x = [(pe, pp, x)]

getElementsByName :: String -> XMLAST -> [(XMLAST, String, XMLAST)]
getElementsByName str ast = filter (\e -> f e) (getAllElements ast) where
                  f ((Element n _ _), _, _) = n == str
                  f _ = False

getElementsByPath :: String -> XMLAST -> [(XMLAST, String, XMLAST)]
getElementsByPath str ast = filter (\e -> f e) (getAllElements ast) where
                  f (_ , p, _) = p == str


getAllBodies :: XMLAST -> [(String, String)]
getAllBodies = getAllBodies' "" where
  getAllBodies' :: String -> XMLAST -> [(String, String)]
  getAllBodies' p (Body str) = [(p, str)]
  getAllBodies' p (Element n a es) =
               let v2 = concat $ map (getAllBodies' (fixUp p n)) es
                   fixUp x y = x ++ "/" ++ y
               in v2
  getAllBodies' p _ = []

getBodiesByName :: String -> XMLAST -> [String]
getBodiesByName name xmlast= map snd $ filter (\(n,v) -> n == name) (getAllBodies xmlast)