import System.Exit
import System.IO
import System.Directory
import qualified Oraculo as O
import qualified Data.Map as M

type Direcciones = [String]

haskiTalks = "|@| "
haskiTabed = "    "
userTalks  = "|?| "

{-SECCION DE UTILIDADES Y FUNCIONES AUXILIARES-}

-- Util: Función que presenta la Predicción final alcanza al usuario a través del stdout
preguntaFinal :: String -> String
preguntaFinal s = "Estás pensando en... " ++ s ++ " ?"


-- Util: Función que presenta al usuario las posibles opciones (si/no) que puede responder
-- cuando tiene que decidir si la prediccion obtenida es correcta o no.
yesno :: IO Bool
yesno = do
  putStr " [y/n] "
  hFlush stdout
  respuesta <- getLine
  if respuesta == "y"
    then return True
    else if respuesta == "n"
      then return False
      else do
        putStr $ "\n" ++ haskiTabed ++ "Intenta otra vez:"
        yesno

-- Util: Función que recibe una respuesta por parte del usuario a través del stdin y verifica
-- que dicha entrada no sea vacía
getLine' :: IO String
getLine' = do
  putStr haskiTabed
  hFlush stdout
  s <- getLine
  if (not . null) s
    then return s
    else do
      putStrLn $ haskiTalks ++ "No puede ser un string vacío! Intenta otra vez:"
      getLine'


-- Util: dada una raíz y una lista de direcciones, dame el oráculo resultante
-- Recibe las direcciones al revés
currentO' :: Direcciones -> O.Oraculo -> O.Oraculo
currentO' []     o = o
currentO' (d:ds) o = currentO' ds $ O.respuesta o d

currentO :: Direcciones -> O.Oraculo -> O.Oraculo
currentO ds = currentO' $ reverse ds



-- Util: Lowest  Common Ancestor
-- Dadas dos listas [a_0, ..., a_n] y [a_0, ... , b_n] que representan un camino
-- desde a_0 hasta a_n y b_n, devuelve el ancestro común más bajo entre a_n y b_n
lcaO :: Eq a => [a] -> [a] -> Maybe a
lcaO [] _ = Nothing
lcaO _ [] = Nothing
lcaO (x:xs) (y:ys)
    | x /= y              = Nothing
    | null xs || null ys  = Just x
    | head xs == head ys  = lcaO xs ys
    | otherwise           = Just x


-- Misma versión de lcaO pero adaptado a oráculos y tuplas
lcaO' :: [(O.Oraculo,a)] -> [(O.Oraculo,a)] -> Maybe (O.Oraculo,(a,a))
lcaO' [] _ = Nothing
lcaO' _ [] = Nothing
lcaO' ((ox,ix):xs) ((oy,iy):ys)
    | ox /= oy                            = Nothing
    | null xs || null ys                  = Just (ox,(ix,iy))
    | (fst . head) xs == (fst . head) ys  = lcaO' xs ys
    | otherwise                           = Just (ox,(ix,iy))


-- Util
-- Algoritmo de DFS adaptado a Oráculos para verificar si una cierta Predicción
-- existe dentro del árbol de oráculos actual
dfs s o@(O.Prediccion p)    = if s == p then Just [(o,"")] else Nothing
dfs s o@(O.Pregunta p opts) = case ( filter (((/=) Nothing) . fst) $ map (\(k,v) -> (dfs s v, k)) (M.toList opts) ) of
                                ((Just x,k):xs) -> Just ((o,k):x)
                                otherwise     -> Nothing


{- SECCIÓN DE OPERACIONES SOBRE EL ÁRBOL DE ORÁCULOS Y LOS OŔACULOS-}

-- Dada una predicción, se crea un oráculo nuevo con dicha predicción
newO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
newO o dirs = do
  putStrLn $ haskiTalks ++ "Para crear un oráculo desde cero, ingrese una predicción:"
  prediccion <- if o == O.Nada then getLine' else getLine
  if null prediccion
    then do
      putStrLn $ haskiTalks ++ "No se ha creado oráculo alguno. El anterior se mantiene intacto."
      return (o, dirs)
    else do
      let new_o = O.crearOraculo prediccion
      putStrLn $ haskiTalks ++ "Se ha creado un nuevo oráculo con la siguiente predicción:"
      putStrLn $ haskiTabed ++ O.prediccion new_o
      return (new_o, [])


-- Función que al momento de insertar un nuevo oráculo/pregunta en el árbol actual,
-- reorganiza toda la información previamente existente. Recibe inicialmente
-- las direcciones invertidas, el oráculo raíz y la nueva hoja
normalizarO' :: Direcciones -> O.Oraculo -> O.Oraculo -> O.Oraculo
normalizarO' [] _ h = h
normalizarO' (k:ks) o h = O.Pregunta p (M.insert k newChild opts)
  where
    (p, opts)   = (O.pregunta o, O.opciones o)
    newChild    = normalizarO' ks oldChild h
    oldChild    = O.respuesta o k

normalizarO :: Direcciones -> O.Oraculo -> O.Oraculo -> O.Oraculo
normalizarO ds = normalizarO' $ reverse ds


-- Función que agrega una nueva pregunta al oráculo actual.
addPreguntaO :: O.Oraculo -> Direcciones -> O.Oraculo -> IO (O.Oraculo, Direcciones)
addPreguntaO o ds oraculo_actual = do
  putStrLn $ haskiTalks ++ "Hmm... Ok. Dime la predicción correcta, por favor:"
  p2' <- getLine'
  putStrLn $ haskiTalks ++ "¿Qué pregunta podría diferenciar la respuesta de mi predicción?:"
  pregunta <- getLine'
  putStrLn $ haskiTalks ++ "Dime qué opción/respuesta corresponde a la respuesta correcta"
  op2 <- getLine'
  putStrLn $ haskiTalks ++ "Dime qué opción/respuesta corresponde a mi predicción original:"
  op1 <- getLine'
  let new_root = normalizarO ds o new_current
        where (p1, p2)    = (oraculo_actual, O.crearOraculo p2')
              new_current = O.ramificar [op1, op2] [p1, p2] pregunta
  putStrLn $ haskiTalks ++ "He agregado la nueva posible predicción!"
  return ( new_root, [] )


-- Función que se encarga de agregar una nueva opcion al oráculo si el usuario ha contestado "Ninguna"
addOpcionO :: O.Oraculo -> Direcciones -> O.Oraculo -> IO (O.Oraculo, Direcciones)
addOpcionO o ds oraculo_actual = do
  putStrLn $ haskiTalks ++ "Oh! Ok, dime la respuesta correcta a la pregunta (en este caso): "
  respuestaCorrecta <- getLine'
  putStrLn $ haskiTalks ++ "No sé mucho sobre eso, así que me deberás decir en qué pensabas (la predicción):"
  in_prediccion <- getLine'
  let new_root = normalizarO ds o new_actual
        where
          new_actual = O.Pregunta (O.pregunta oraculo_actual) new_opciones
          new_opciones = M.insert respuestaCorrecta prediccion $ O.opciones oraculo_actual
          prediccion = O.crearOraculo in_prediccion
  return (new_root, [])


-- Función que inicia el proceso de prediccion del Oráculo actual.
-- Si el oráculo actual es una Predicción, arroja dicha predicción automáticamente
-- de lo contrario, se presentan las preguntas que contiene el oráculo actual
predecirO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
predecirO o ds = do
  let oraculo_actual = currentO ds o
  case oraculo_actual of
    O.Prediccion p -> do
      putStr $ haskiTalks ++ preguntaFinal p
      yes <- yesno
      if yes
        then do
          putStrLn $ haskiTalks ++ "Wubbalubbadubdub!"
          return $ (o, [])
        else addPreguntaO o ds oraculo_actual
    O.Pregunta p opts -> do
      putStrLn $ haskiTalks ++ p
      mapM_ (\(i, _) -> putStrLn (haskiTabed++"- "++i)) $ M.toList opts
      putStrLn $ haskiTabed ++ "- Ninguna"
      respuesta <- getLine'
      if respuesta == "Ninguna"
        then addOpcionO o ds oraculo_actual
        else do
          case M.lookup respuesta opts of
            Just _ -> predecirO o (respuesta:ds)
            otherwise -> do
              putStrLn $ haskiTalks ++ "Lo siento. No sé a qué opción te refieres. "
              predecirO o ds



-- Función que se encarga de almacenar en un archivo de texto indicado por el usuario
-- la estructura del oráculo actual
persistirO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
persistirO o ds = do
  putStrLn $ haskiTalks ++ "Ingresa el nombre del archivo donde deseas guardar tu oráculo:"
  filename <- getLine'
  putStrLn $ haskiTalks ++ "Guardando oráculo..."
  withFile filename WriteMode (\file -> hPrint file o) -- TODO: catchear errores
  return (o, ds)

-- Función que se encarga de cargar a partir de un archivo de texto, cuyo nombre introduce
-- el usuario, un oráculo previamente almacenado
cargarO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
cargarO o ds = do
  putStrLn $ haskiTalks ++ "Ingresa el nombre del archivo de donde deseas cargar un oráculo:"
  filename <- getLine'
  fileExists <- doesFileExist filename
  if fileExists
    then do
      putStrLn $ haskiTalks ++ "Cargando oráculo..."
      s <- readFile filename
      let new_o = read s :: O.Oraculo
      return (new_o, ds)
  else do
    putStrLn "El archivo no existe!"
    return (o, ds)


-- Función que le pide al usuario dos predicciones que pueden estar o no dentro del
-- árbol que contiene el oráculo actual y devuelve la pregunta que eventualmente
-- llevará a decidir entre tales predicciones ingresadas por el usuario
crucial :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
crucial o ds = do
  putStrLn $ haskiTalks ++ "Ingresa una primera predicción a consultar:"
  p1 <- getLine'
  putStrLn $ haskiTalks ++ "Ingresa una segunda predicción a consultar:"
  p2 <- getLine'
  let l1 = maybe [] id $ dfs p1 o
      l2 = maybe [] id $ dfs p2 o
      lca = maybe (O.Nada,([],[])) id $ lcaO' (l1) (l2)
  putStrLn $ haskiTalks ++ "La pregunta que decide entre ambas opciones es:"
  putStrLn $ haskiTabed ++ (O.pregunta $ fst lca)
  putStrLn $ haskiTabed ++ "("++p1++") \t- " ++ (fst $ snd lca)
  putStrLn $ haskiTabed ++ "("++p2++") \t- " ++ (snd $ snd lca)
  return (o, ds)


-- Termina la ejecución de Haskinator
salirO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
salirO _ _ = do
  putStrLn $ haskiTalks ++ "Gracias por usar el glorioso Haskinator!"
  exitSuccess



-- Menu
--------------------------------------------------------------------------------

menu = [ ("Crear Oráculo Nuevo",        newO),
         ("Hacer una Predicción",       predecirO),
         ("Guardar el Oráculo actual",  persistirO),
         ("Cargar un Oráculo",          cargarO),
         ("Consultar Pregunta Crucial", crucial),
         ("Salir",                      salirO) ]

menu' :: [Int]
menu' = [0,3,5]

-- Función que muestra el menu con todas las opciones de Hakinator
mostrarMenu :: Int -> IO ()
mostrarMenu i = do
  if i == length menu
    then return ()
    else do
      putStrLn $ haskiTabed ++ (show $ i+1) ++ " - " ++ ( fst $ menu!!i )
      mostrarMenu (i+1)


-- Función que muestra una parte de todas las opciones disponibles de Hakinator.
-- Se crea con la intensión de mostrar únicamente las opciones válidas la primera
-- vez que se ejecuta el cliente
mostrarMenu' :: Int -> IO ()
mostrarMenu' i = do
  if i == length menu'
    then return ()
    else do
      let j = menu'!!i
      putStrLn $ haskiTabed ++ (show $ j+1) ++ " - " ++ (fst $ menu!!j)
      mostrarMenu' (i+1)



-- Main, Init y Loop
--------------------------------------------------------------------------------
mainLoop :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
mainLoop o stk = do
  putStrLn $ haskiTalks ++ "Por favor, escribe el número de una opción:"
  mostrarMenu 0
  respuesta' <- getLine'
  let respuesta = ((+) (-1)) $ read respuesta' :: Int
  let opt = if respuesta >= length menu || respuesta < 0 then Nothing else Just $ snd $ menu!!respuesta
  case opt of
    Just f -> do
      result <- f o stk
      mainLoop (fst result) (snd result)
      where
    Nothing -> do
      putStrLn $ haskiTalks ++ "...Hmmm... No entendí lo que quieres."
      mainLoop o stk

-- Función que le permite al usuario empezar a utilizar el Hakinator. Permite crear un
-- nuevo oráculo o cargar uno previamente guardado.
mainInit :: IO (O.Oraculo, Direcciones)
mainInit = do
  putStrLn $ haskiTalks ++ "Para empezar, escribe el número de una opción:"
  mostrarMenu' 0
  respuesta' <- getLine'
  let respuesta = ((+) (-1)) $ read respuesta' :: Int
  let opt = if (not . elem respuesta) menu' then Nothing else Just $ snd $ menu!!respuesta
  case opt of
    Just f -> f O.Nada []
    Nothing -> do
      putStrLn $ haskiTalks ++ "...Hmmm... No entendí lo que quieres."
      mainInit

-- Inicia la ejecución de Hakinator
main :: IO ()
main = do
  putStrLn $ haskiTalks ++ "Bienvenido al glorioso Haskinator!"
  o <- mainInit
  mainLoop (fst o) (snd o)
  return ()