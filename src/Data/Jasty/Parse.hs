module Data.Jasty.Parse (parse) where

import           Data.Char
import           Data.Void

import           Data.Scientific
import           Text.Megaparsec      hiding (parse, token)
import           Text.Megaparsec.Char

import           Data.Jasty.AST

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) JValue
parse = runParser (pValue <* eof) ""

---

pValue :: Parser JValue
pValue = choice $ map (try . pWhitespaces)
  [ JObject <$> pObject
  , JArray  <$> pArray
  , JString <$> pString
  , JNumber <$> pNumber
  , JTrue   <$  pTrue
  , JFalse  <$  pFalse
  , JNull   <$  pNull
  ]

pObject :: Parser [Member]
pObject = choice $ map (try . braces)
  [ mempty <$ pWhitespace
  , pMembers
  ]

pArray :: Parser [JValue]
pArray = choice $ map (try . brackets)
  [ mempty <$ pWhitespace
  , pValues
  ]

pString :: Parser String
pString = quotes $ many pChar

pNumber :: Parser Scientific
pNumber = read <$> pCoefficient <> pOptionExponent

pTrue :: Parser String
pTrue = string "true"

pFalse :: Parser String
pFalse = string "false"

pNull :: Parser String
pNull = string "null"

---

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

quotes :: Parser a -> Parser a
quotes = between (char '"') (char '"')

pWhitespaces :: Parser a -> Parser a
pWhitespaces = between pWhitespace pWhitespace

pWhitespace :: Parser String
pWhitespace = many $ choice
  [ pSpace
  , pLineFeed
  , pCarriageReturn
  , pHorizontalTab
  ]

pSpace :: Parser Char
pSpace = char ' '

pLineFeed :: Parser Char
pLineFeed = char '\n'

pCarriageReturn :: Parser Char
pCarriageReturn = char '\r'

pHorizontalTab :: Parser Char
pHorizontalTab = char '\t'

pMembers :: Parser [Member]
pMembers = pMember `sepBy1` pComma

pMember :: Parser Member
pMember = (,) <$> pKey <* pColon <*> pValue

pKey :: Parser String
pKey = pWhitespaces pString

pValues :: Parser [JValue]
pValues = pValue `sepBy1` pComma

pComma :: Parser Char
pComma = char ','

pColon :: Parser Char
pColon = char ':'

pChar :: Parser Char
pChar = choice $ map try
  [ pNormalChar
  , pSpecialChar
  ]

pNormalChar :: Parser Char
pNormalChar = satisfy isNormalChar

pSpecialChar :: Parser Char
pSpecialChar = char '\\' *> choice
  [ oneOf ['"', '\\', '/']
  , '\b' <$ char 'b'
  , '\f' <$ char 'f'
  , '\n' <$ char 'n'
  , '\r' <$ char 'r'
  , '\t' <$ char 't'
  , char 'u' *> pHexChar
  ]

pHexChar :: Parser Char
pHexChar = hex2Char <$> p4HexDigits

p4HexDigits :: Parser String
p4HexDigits = count 4 hexDigitChar

pCoefficient :: Parser String
pCoefficient = pOptionSign <> pInteger <> pOptionFraction

pOptionSign :: Parser String
pOptionSign = choice [pMinus, mempty]

pInteger :: Parser String
pInteger = choice $ map try [pZero, pNonZero]

pZero :: Parser String
pZero = string "0"

pNonZero :: Parser String
pNonZero = (:) <$> pNonZeroDigit <*> pDigits

pNonZeroDigit :: Parser Char
pNonZeroDigit = oneOf ['1'..'9']

pOptionFraction :: Parser String
pOptionFraction = choice [try pFraction, mempty]

pFraction :: Parser String
pFraction = (:) <$> pDot <*> pDigits1

pDot :: Parser Char
pDot = char '.'

pOptionExponent :: Parser String
pOptionExponent = choice [try pExponent, mempty]

pExponent :: Parser String
pExponent = (:) <$> char' 'e' <*> pExponentSign <> pDigits1

pExponentSign :: Parser String
pExponentSign = choice [pMinus, pPlus, mempty]

pMinus :: Parser String
pMinus = string "-"

pPlus :: Parser String
pPlus = string "+"

pDigits :: Parser String
pDigits = many digitChar

pDigits1 :: Parser String
pDigits1 = some digitChar

 ---

isNormalChar :: Char -> Bool
isNormalChar c = isNotControl c && notElem c ['"', '\\']

isNotControl :: Char -> Bool
isNotControl = not . isControl

hex2Char :: String -> Char
hex2Char = chr . hex2Dec

hex2Dec :: String -> Int
hex2Dec = foldl (\acc c -> 16 * acc + digitToInt c) 0
