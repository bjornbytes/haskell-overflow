module BEncode where

import Text.ParserCombinators.Parsec;
import Data.Map;

data BData = BString String
           | BInteger Int
           | BList [BData]
           | BDictionary (Map BData BData)
             deriving Eq

instance Show BData where
    show (BString s) = s
    show (BInteger i) = (show i)
    show (BList xs) = (show xs)
    show (BDictionary m) = (show $ toList m)

decode :: String -> IO ()
decode s = case (parse bdata "" s) of
             Left err -> print err
             Right xs -> print xs

bdata = bstring
    <|> binteger
    <|> blist
    <|> bdictionary

bstring :: Parser BData
bstring = do
  n <- number
  char ':'
  s <- count n anyChar
  return $ BString s

--TODO: Adhere more closely to the BEncoding spec.
binteger :: Parser BData
binteger = do
  char 'i'
  n <- number
  char 'e'
  return $ BInteger n

blist :: Parser BData
blist = do
  char 'l'
  xs <- many1 bdata
  char 'e'
  return $ BList xs

bdictionary :: Parser BData
bdictionary = do
  char 'd'
  xs <- many1 bdictionaryElement
  char 'e'
  return $ BDictionary (fromAscList xs)

bdictionaryElement :: Parser (BData, BData)
bdictionaryElement = do
  s <- bstring
  d <- bdata
  return (s,d)

number :: Parser Int
number = do { n <- many1 digit; return $ read n }