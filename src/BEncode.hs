module BEncode where

import Text.ParserCombinators.Parsec hiding (Parser);
import Text.Parsec.ByteString;
import qualified Data.Map as M;
import Data.Maybe;
import qualified Data.ByteString as B;
import qualified Data.ByteString.Char8 as C;

type BDict = M.Map String BData

data BData = BString B.ByteString
           | BInteger Int
           | BList [BData]
           | BDictionary BDict
             deriving (Eq, Ord)

instance Show BData where
    show (BString s) = show s
    show (BInteger i) = show i
    show (BList xs) = show xs
    show (BDictionary m) = (show $ M.toList m)

extractBString :: BData -> Maybe B.ByteString
extractBString (BString s) = Just s
extractBString _ = Nothing

extractBInteger :: BData -> Maybe Int
extractBInteger (BInteger x) = Just x
extractBInteger _ = Nothing

extractBList :: BData -> Maybe [BData]
extractBList (BList xs) = Just xs
extractBList _ = Nothing

extractBDictionary :: BData -> Maybe BDict
extractBDictionary (BDictionary m) = Just m
extractBDictionary _ = Nothing

assumeBString :: BData -> B.ByteString
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

assumeBDictionary :: BData -> BDict
assumeBDictionary x = case extractBDictionary x of
                    Just x -> x
                    Nothing -> error $ "assumeBDictionary applied to " ++ show x

decode :: B.ByteString -> Maybe BData
decode s = case (parse bdata "" s) of
             Left err -> Nothing
             Right xs -> Just xs

decodeBString :: B.ByteString -> B.ByteString
decodeBString = assumeBString . fromJust . decode

decodeBInteger :: B.ByteString -> Int
decodeBInteger = assumeBInteger . fromJust . decode

decodeBList :: B.ByteString -> [BData]
decodeBList = assumeBList . fromJust . decode

decodeBDictionary :: B.ByteString -> BDict
decodeBDictionary = assumeBDictionary . fromJust . decode

encode :: BData -> B.ByteString
encode (BString s) = B.concat [C.pack (show $ B.length s), C.singleton ':', s]
encode (BInteger x) = B.concat [C.singleton 'i', C.pack $ show x, C.singleton 'e']
encode (BList xs) = B.concat [C.singleton 'l', B.concat $ map encode xs, C.singleton 'e']
encode (BDictionary x) = B.concat [C.singleton 'd', B.concat $ map encodeElement $ M.toList x, C.singleton 'e']
  where encodeElement (key, val) = B.append (encode $ BString $ C.pack key) (encode val)

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
  return $ BString $ read (show s)

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
  return $ BDictionary (M.fromAscList xs)

bdictionaryElement :: Parser (String, BData)
bdictionaryElement = do
  n <- number
  char ':'
  s <- count n anyChar
  d <- bdata
  return (s,d)

number :: Parser Int
number = do { n <- many1 digit; return $ read n }
