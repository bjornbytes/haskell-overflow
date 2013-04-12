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

extractBString :: BData -> Maybe String
extractBString (BString s) = Just s
extractBString _ = Nothing

extractBInteger :: BData -> Maybe Int
extractBInteger (BInteger x) = Just x
extractBInteger _ = Nothing

extractBList :: BData -> Maybe [BData]
extractBList (BList xs) = Just xs
extractBList _ = Nothing

extractBDictionary :: BData -> Maybe (Map BData BData)
extractBDictionary (BDictionary m) = Just m
extractBDictionary _ = Nothing

assumeBString :: BData -> String
assumeBString x = case extractBString x of
                    Just x -> x
                    Nothing -> error $ "assumeBString applied to " ++ show x

assumeBInteger :: BData -> Int
assumeBInteger x = case extractBInteger x of
                    Just x -> x
                    Nothing -> error $ "assumeBInteger applied to " ++ show x

assumeBList :: BData -> [BData]
assumeBList x = case extractBList x of
                    Just x -> x
                    Nothing -> error $ "assumeBList applied to " ++ show x

assumeBDictionary :: BData -> (Map BData BData)
assumeBDictionary x = case extractBDictionary x of
                    Just x -> x
                    Nothing -> error $ "assumeBDictionary applied to " ++ show x

decode :: String -> Maybe BData
decode s = case (parse bdata "" s) of
             Left err -> Nothing
             Right xs -> Just xs

decodeBString :: String -> String
decodeBString = assumeBString . fromJust . decode

decodeBInteger :: String -> Int
decodeBInteger = assumeBInteger . fromJust . decode

decodeBList :: String -> [BData]
decodeBList = assumeBList . fromJust . decode

decodeBDictionary :: String -> (Map BData BData)
decodeBDictionary = assumeBDictionary . fromJust . decode

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