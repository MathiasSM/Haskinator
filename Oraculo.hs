-- Se supone que los errores los lancemos para catch y eso? 
-- Habrá que hacer el main y hacer los catch allá de alguna manera
-- Porque si no se pueden modificar estas funciones para que sean Either...

-- Imports
--------------------------------------
import qualified Data.Map as M 

-- No sé si se supone que usemos los tipos tal cual lo especificaron o podemos definir otros por conveniencia
-- Tipos
--------------------------------------
type Opciones  = M.Map String Oraculo
data Oraculo   = Prediccion String | Pregunta String Opciones


-- Construcción
--------------------------------------
crearOraculo :: String -> Oraculo
crearOraculo = Prediccion

-- Acceso
--------------------------------------
prediccion :: Oraculo -> String
prediccion (Prediccion s) = s
prediccion _ = "Lo siento, este Oráculo no es una predicción."

pregunta :: Oraculo -> String
pregunta (Pregunta s _) = s
pregunta _ = "Lo siento, este Oráculo no es una pregunta."

opciones :: Oraculo -> Opciones
opciones (Pregunta _ o) = o
-- opciones _ = "Lo siento, no hay opción si es una predicción."

respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta p o) r = 
	case M.lookup r o of
		Nothing -> Pregunta ("Error: No existe tal respuesta!\n\n"++p) o
		Just o  -> o


-- Modificación
--------------------------------------
-- ramificar :: [String] -> [Oraculo] -> String -> Oraculo
-- Sinceramente no entiendo qué se supone que piden en esta función

-- Instancias
--------------------------------------
instance Show Oraculo where
	show (Prediccion s)   = show s
	show (Pregunta s ops) = show $ s ++ '\n':'\n':"Y aqui las opciones"
  
-- NPI de como hacer el parser (yet)
--
-- readsOraculo (x:xs) = [ (f, r) | (f, '\n':r) <- readsOraculo xs ]
-- readsOraculo s		= [ (f, t) | (f,t) <- reads s ]  
-- instance Read Oraculo where
-- 	readsPrec _ s = readsOraculo s
