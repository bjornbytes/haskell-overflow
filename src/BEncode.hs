module BEncode where

import Text.ParserCombinators.Parsec;
import Data.Map;
import Data.Maybe;

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

decode :: String -> Maybe BData
decode s = case (parse bdata "" s) of
             Right xs -> Just xs
             otherwise -> Nothing

decodeBString :: String -> Maybe String
decodeBString s = case decode s of
                    Just (BString x) -> Just x
                    otherwise -> Nothing

decodeBInteger :: String -> Maybe Int
decodeBInteger s = case decode s of
                     Just (BInteger x) -> Just x
                     otherwise -> Nothing

decodeBList :: String -> Maybe [BData]
decodeBList s = case decode s of
                  Just (BList x) -> Just x
                  otherwise -> Nothing

decodeBDictionary :: String -> Maybe (Map BData BData)
decodeBDictionary s = case decode s of
                        Just (BDictionary x) -> Just x
                        otherwise -> Nothing

bdata :: Parser BData
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