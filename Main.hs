import qualified Oraculo as O
import qualified Data.Map as M

crearOraculo :: IO O.Oraculo
crearOraculo = do
	putStrLn "Para crear un oráculo desde cero, ingrese una predicción:"
	prediccion <- getLine
	putStrLn  "Se ha creado un oraculo con la siguiente predicción:"
	putStrLn prediccion
	return $ O.crearOraculo prediccion

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
	mapM_ (\(k, v) -> putStrLn ('-':k)) (M.toList ops)
	respuesta <- getLine
	-- when (respuesta == "Ninguna") IO () -- Npi de qué debe hacer
	if M.member respuesta ops
		then do
			let o = lookup respuesta (M.toList ops)
			case o of
			  Nothing -> salir
			  Just o -> predecir o
				--in predecir o
	else
		predecir (O.Pregunta p ops)


persistir :: O.Oraculo -> IO ()
persistir o = do
	putStrLn "Ingrese el nombre del archivo donde desea guardar su oráculo"
	fileName <- getLine
	putStrLn "Guardando oráculo..."
	writeFile fileName (show o)


salir :: IO()
salir = putStrLn "Gracias por usar el glorioso Haskinator!"

{-opciones = M.fromList [ ("Crear Oráculo Nuevo",        crearOraculo),
												("Hacer una Predicción",       predecir),
												("Guardar el Oráculo actual",  persistir),
												("Cargar un Oráculo",          cargar),
												("Consultar Pregunta Crucial", crucial),
												("Salir",                      salir)]-}

opciones = M.fromList [ ("Crear Oráculo Nuevo",        crearOraculo)]

fakeMain = do
	putStrLn "Por favor, elige una opción:"
	mapM_ (\(k, v) -> putStrLn ('-':k)) (M.toList opciones)
	respuesta <- getLine
	if M.member respuesta opciones
		then do
			let c = M.lookup respuesta opciones
			case c of
				Just c -> c


	else do
		putStrLn "...Hmmm... No entendí lo que quieres."
		putStrLn "Escribe la opción que deseas"
		fakeMain

main = do
	putStrLn "Bienvenido al glorioso Haskinator!"
