import System.Exit
import System.IO
import qualified Oraculo as O
import qualified Data.Map as M

type Direcciones = [String]

haskiTalks = "|@| "
haskiTabed = "    "
userTalks  = "|?| "

-- Util
preguntaFinal :: String -> String
preguntaFinal s = "Estás pensando en... " ++ s ++ " ?"

-- Util
yesno :: IO Bool
yesno = do
  putStr " [y/n] "
  respuesta <- getLine
  if respuesta == "y" 
    then return True
    else if respuesta == "n"
      then return False
      else do
        putStr $ "\n" ++ haskiTabed ++ "Intenta otra vez:"
        yesno

-- Util
getLine' :: IO String
getLine' = do
  putStr haskiTabed
  s <- getLine
  if (not . null) s -- Deberíamos revisar si el string es "visible" (haya algo que no sea puro whitespace)
    then return s
    else do 
      putStrLn $ haskiTalks ++ "No puede ser un string vacío! Intenta otra vez:"
      getLine' 


-- Util: dada una raíz y una lista de direcciones, dame el oráculo resultante
-- Reciba las direcciones al revés
currentO :: Direcciones -> O.Oraculo -> O.Oraculo
currentO []     o = o
currentO (d:ds) o = currentO ds $ O.respuesta o d

-- Util LCA
lca :: Eq a => [a] -> [a] -> Maybe a
lca [] _ = Nothing
lca _ [] = Nothing
lca (x:xs) (y:ys) 
    | x == y && (take 1 xs) == (take 1 ys)                = lca (xs) (ys)
    | x == y && (take 1 xs) /= (take 1 ys)                = Just x
    | otherwise                                           = Nothing



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




-- Recibe inicialmente las direcciones invertidas, el oráculo raíz y la nueva hoja
normalizarO :: Direcciones -> O.Oraculo -> O.Oraculo -> O.Oraculo
normalizarO [k] o h = O.Pregunta (O.pregunta o) nuOpts
  where nuOpts = M.insert k h $ O.opciones o
normalizarO (k:ks) o h = O.Pregunta (O.pregunta o) nuOpts
  where 
    nuOpts  = M.insert k nuChild $ O.opciones o
    nuChild = normalizarO ks (O.respuesta o k) h
normalizarO [] _ h = h


addPreguntaO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
addPreguntaO o ds = do
  let p = currentO ds o
  putStrLn $ haskiTalks ++ "Hmm... Ok. Dime la predicción correcta, por favor:"
  in_p' <- getLine'
  let p' = O.crearOraculo in_p'
  putStrLn $ haskiTalks ++ "¿Qué pregunta podría diferenciar la respuesta de mi predicción?:"
  pregunta <- getLine'
  putStrLn $ haskiTalks ++ "Dime qué opción/respuesta corresponde a la respuesta correcta"
  op' <- getLine'
  putStrLn $ haskiTalks ++ "Dime qué opción/respuesta corresponde a mi predicción original:"
  op <- getLine'
  let o' = O.ramificar [op', op] [p', p] pregunta
  let new_root = normalizarO (reverse ds) o o'
  putStrLn $ haskiTalks ++ "He agregado la nueva posible predicción!"
  return ( new_root, [] )


predecirO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
predecirO o ds = do
  let oraculo_actual = currentO ds o
  case oraculo_actual of 
    O.Prediccion _ -> do 
      putStr $ haskiTalks ++ (preguntaFinal . O.prediccion) oraculo_actual
      yes <- yesno
      if yes 
        then do
          putStrLn $ haskiTalks ++ "Wubbalubbadubdub!"
          return $ (o, [])
        else addPreguntaO o ds 
    O.Pregunta _ _ -> do
      putStrLn $ haskiTalks ++ O.pregunta oraculo_actual
      mapM_ (\(p, o) -> putStrLn (haskiTabed++p)) $ M.toList $ O.opciones oraculo_actual
      putStrLn $ haskiTabed++"Ninguna"
      respuesta <- getLine'
      if respuesta == "Ninguna" 
        then do
          putStrLn $ haskiTalks ++ "Oh! Ok, dime la respuesta correcta a la pregunta (en este caso): "
          respuestaCorrecta <- getLine'
          putStrLn $ haskiTalks ++ "No sé mucho sobre eso, así que me deberás decir en qué pensabas (la predicción):"
          in_prediccion <- getLine'
          let prediccion = O.crearOraculo in_prediccion
          let new_actual = O.Pregunta (O.pregunta oraculo_actual) new_opciones
                where new_opciones = M.insert respuestaCorrecta prediccion $ O.opciones oraculo_actual
          return (normalizarO ds o new_actual, respuestaCorrecta:ds)
        else do
          case M.lookup respuesta $ O.opciones oraculo_actual of
            Just o' -> predecirO o (respuesta:ds)
            otherwise -> do
              putStrLn $ haskiTalks ++ "Lo siento. No sé a qué opción te refieres. "
              predecirO o ds




persistirO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
persistirO o ds = do
  putStrLn $ haskiTalks ++ "Ingresa el nombre del archivo donde deseas guardar tu oráculo:"
  filename <- getLine'
  putStrLn $ haskiTalks ++ "Guardando oráculo..."
  withFile filename WriteMode (\file -> hPrint file o) -- TODO: catchear errores
  return (o, ds)


cargarO :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
cargarO o ds = do
  putStrLn $ haskiTalks ++ "Ingresa el nombre del archivo de donde deseas cargar un oráculo:"
  filename <- getLine'
  putStrLn $ haskiTalks ++ "Cargando oráculo..."
  withFile filename ReadMode (\file -> do
    s <- hGetContents file
    readIO s) -- TODO: catchear errores


crucial :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
crucial _ _ = do
  die "Falta implementar"

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

mostrarMenu :: Int -> IO ()
mostrarMenu i = do
  if i == length menu
    then return ()
    else do
      putStrLn $ haskiTabed ++ (show $ i+1) ++ " - " ++ ( fst $ menu!!i )
      mostrarMenu (i+1)

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

main :: IO ()
main = do
  putStrLn $ haskiTalks ++ "Bienvenido al glorioso Haskinator!"
  o <- mainInit
  mainLoop (fst o) (snd o)
  return ()
