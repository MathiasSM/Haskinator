import System.Exit
import System.IO
import System.Directory
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
currentO' :: Direcciones -> O.Oraculo -> O.Oraculo
currentO' []     o = o
currentO' (d:ds) o = currentO' ds $ O.respuesta o d

currentO :: Direcciones -> O.Oraculo -> O.Oraculo 
currentO ds = currentO' $ reverse ds 


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
normalizarO' :: Direcciones -> O.Oraculo -> O.Oraculo -> O.Oraculo
normalizarO' [] _ h = h
normalizarO' (k:ks) o h = O.Pregunta p (M.insert k newChild opts)
  where 
    (p, opts)   = (O.pregunta o, O.opciones o)  
    newChild    = normalizarO' ks oldChild h
    oldChild    = O.respuesta o k

normalizarO :: Direcciones -> O.Oraculo -> O.Oraculo -> O.Oraculo
normalizarO ds = normalizarO' $ reverse ds 

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


crucial :: O.Oraculo -> Direcciones -> IO (O.Oraculo, Direcciones)
crucial o ds = do
  putStrLn $ haskiTalks ++ "Introduzca su primera predicción a consultar:"
  prediccion1 <- getLine'
  putStrLn $ haskiTalks ++ "Introduzca su segunda predicción a consultar:"
  prediccion2 <- getLine'
  if not (prediccion1 `elem` ds) || not (prediccion2 `elem` ds)
    then do
      putStrLn $ haskiTalks ++ "Alguna de las predicciones ingresadas no se encuentra dentro del oráculo."
      return (o, ds)
  else do
    return (o, ds)



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
