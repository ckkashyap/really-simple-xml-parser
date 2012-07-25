module RSXP (
    XMLAST (Element, Body, Comment)
  , parseXML
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

innerXML = comment <|> xmlParser <|> getBody

getBody = fmap Body $ many1 $ noneOf "<>"

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

