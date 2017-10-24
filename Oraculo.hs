module Oraculo
( Opciones
, Oraculo(..)
, crearOraculo
, prediccion
, pregunta
, opciones
, respuesta
, ramificar
) where

-- Imports
--------------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char

import Text.ParserCombinators.ReadP
import Control.Applicative((<|>))


-- Data types
--------------------------------------------------------------------------------
data Oraculo   = Prediccion String | Pregunta String Opciones | Nada
type Opciones  = Map.Map String Oraculo


-- Construcción
--------------------------------------------------------------------------------
crearOraculo :: String -> Oraculo
crearOraculo = Prediccion


-- Acceso
--------------------------------------------------------------------------------
prediccion :: Oraculo -> String
prediccion (Prediccion s) = s
prediccion _ = error "Este Oráculo no es una predicción."

pregunta :: Oraculo -> String
pregunta (Pregunta s _) = s
pregunta _ = error "Este Oráculo no es una pregunta."

opciones :: Oraculo -> Opciones
opciones (Pregunta _ o) = o
opciones _ = error "No hay opción si el oráculo es una predicción."

respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta p o) r =
  case Map.lookup r o of
    Nothing -> error "No existe tal respuesta!"
    Just o  -> o


-- Modificación
--------------------------------------------------------------------------------
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar rs os s = Pregunta s ops
  where ops = Map.fromList $ zip rs os


-- Instancias
--------------------------------------------------------------------------------
instance Show Oraculo where
  show (Prediccion s)   = "=>" ++ s ++ "<="
  show (Pregunta s ops) = s ++ "?" ++ "\n\n" ++ showOps
    where showOps = Map.foldr (++) [] $ Map.mapWithKey (\k v -> '{':k ++ "::" ++ show v ++ "}\n") ops

instance Read Oraculo where
  readsPrec _ = readP_to_S pOraculo

instance Eq Oraculo where
  Nada         == Nada            = True
  Prediccion a == Prediccion b    =   a == b 
  Pregunta p o == Pregunta p' o'  =   p == p' && o == o' 
  Pregunta _ _ == _               = False
  Prediccion _ == _               = False
  Nada         == _               = False

-- Parser (Para uso en Read Oraculo)
--------------------------------------------------------------------------------
pPrediccion :: ReadP Oraculo
pPrediccion = do
  text <- between (string "=>") (string "<=") $ many get
  return (Prediccion text)

pPreguntaOnly :: ReadP String
pPreguntaOnly = do
  text <- many1 get
  ques <- char '?'
  return text

pRowCapsule :: ReadP (String, Oraculo)
pRowCapsule = between (char '{') (char '}') pRow

pRow :: ReadP (String, Oraculo)
pRow = do
  skipSpaces
  respuesta <- many1 get
  string "::"
  oraculo <- pOraculo
  skipSpaces
  return (respuesta, oraculo)

pOraculo :: ReadP Oraculo
pOraculo = do
  skipSpaces
  pPrediccion <|> do
    pregunta <- pPreguntaOnly
    skipSpaces
    resOps <- many1 pRowCapsule
    return (Pregunta pregunta (Map.fromList resOps))
