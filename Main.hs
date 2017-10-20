import qualified Oraculo as O
import qualified Data.Map as M

crearOraculo :: IO ()
crearOraculo = do
	putStrLn "Para crear un oráculo desde cero, ingrese una predicción:"
	prediccion <- getLine
	putStrLn  "Se ha creado un oraculo con la siguiente predicción:"
	putStrLn prediccion
	return IO $ O.crearOraculo prediccion

preguntaFinal :: String -> String
preguntaFinal s = "Estás pensando en..." ++ s ++ "? [s/n]"

addOraculo :: O.Oraculo -> IO ()
addOraculo (O.Prediccion s) = do
	putStrLn "Hmm... Ok. Dime la respuesta correcta, por favor:"
	respuesta <- getLine
	putStrLn "¿Qué pregunta podría diferenciar la respuesta de mi predicción?:"
	pregunta <- getLine
	putStrLn "Dime qué opción/respuesta corresponde a la respuesta correcta"
	opcionBuena <- getLine
	putStrLn "Dime qué opción/respuesta corresponde a mi predicción original:"
	opcionMala <- getLine
	return ()
	-- Crear Oráculo y pegarlo al árbol


predecir :: O.Oraculo -> IO ()
predecir (O.Prediccion s) = do
	putStrLn $ preguntaFinal s
	respuesta <- getLine
	-- when (head respuesta != 's') {- Agregar Oraculo -} 
	salir
predecir (O.Pregunta p ops) = do
	putStrLn p
	mapM_ (\(s,o) -> putStrLn '-':s) ops
	respuesta <- getLine
	-- when (respuesta == "Ninguna") IO () -- Npi de qué debe hacer	
	if M.member respuesta ops 
		then 
			let o = lookup respuesta ops
				in predecir o
		else 
			predecir (O.Pregunta p ops)
	
	

salir :: IO()
salir = putStrLn "Gracias por usar el glorioso Haskinator!"

opciones = M.fromList [ ("Crear Oráculo Nuevo",        crearOraculo),
							("Hacer una Predicción",       predecir),
							("Guardar el Oráculo actual",  persistir),
							("Cargar un Oráculo",          cargar),
							("Consultar Pregunta Crucial", crucial),
							("Salir",                      salir)] 

fakeMain = do
	putStrLn "Por favor, elige una opción:"
	mapM_ (\(s,_) -> putStrLn '-':s) opciones
	respuesta <- getLine
	if M.member respuesta opciones 
		then return $ M.lookup respuesta opciones
		else do 
			putStrLn "...Hmmm... No entendí lo que quieres."
			putStrLn "Escribe la opción que deseas" 
			fakeMain

main = do
	putStrLn "Bienvenido al glorioso Haskinator!"

