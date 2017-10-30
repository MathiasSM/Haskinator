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

-- IMPORTS
--------------------------------------------------------------------------------
import qualified Data.Map as Map		-- Manejar mapas eficientes
import qualified Data.List as List	-- Funciones extras para manejo de listas
import qualified Data.Char as Char	-- Funciones extras para manejo de caracteres


-- DATA TYPES
--------------------------------------------------------------------------------
data Oraculo   = Prediccion String | Pregunta String Opciones | Nada deriving (Eq, Show, Read)
type Opciones  = Map.Map String Oraculo


-- FUNCIONES
--------------------------------------------------------------------------------

-- Construcción 
crearOraculo :: String -> Oraculo
crearOraculo = Prediccion

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar rs os s = Pregunta s ops
  where ops = Map.fromList $ zip rs os


-- Acceso
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
respuesta (Pregunta p opts) s =
  case Map.lookup s opts of
    Just o  -> o
    Nothing -> error $ "No existe tal respuesta!"   ++"\nRespuesta:\t"++(show s)++"\nOpciones:\t"++(show opts)
