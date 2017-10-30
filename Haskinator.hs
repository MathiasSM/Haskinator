-- IMPORTS
--------------------------------------------------------------------------------
import System.Exit              -- Poder salir del programa
import System.IO                -- Funciones de IO
import System.Directory         -- Manejar archivos
import Control.Monad            -- Funciones para manejar cosas en Monads
import Control.Exception         -- Catchear excepciones de lectura/escritura
import qualified Data.Map as M  -- Manejar mapas eficientes (qualified as M)
import qualified Oraculo as O   -- Módulo de Oraculo (qualified as O)



-- ALIASES
--------------------------------------------------------------------------------
type Direccion    = String
type Direcciones  = [String]

-- Prefijos para output
haskiTalks = "|@| "
haskiTabed = "    "
userTalks  = "|?| "



-- FUNCIONES AUXILIARES
--------------------------------------------------------------------------------


-- IO General
--------------------

-- Pregunta repetidamente sí o no ("y" o "n") hasta obtener una respuesta.
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

-- Lee una linea de input repetidamente hasta obtener una no-vacía.
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


-- ORACULOS
--------------------

-- Reemplaza recursivamente los oráculos en el camino que han sido desechados por los nuevos
normalizarO :: Direcciones -> O.Oraculo -> O.Oraculo -> O.Oraculo
normalizarO ds = normaliza $ reverse ds
  where 
    normaliza [] _ h = h
    normaliza (k:ks) o h = O.Pregunta p (M.insert k newChild opts)
      where
        (p, opts)   = (O.pregunta o, O.opciones o)
        newChild    = normaliza ks oldChild h
        oldChild    = O.respuesta o k

-- Agrega una nueva pregunta al oráculo actual 
addPreguntaO :: O.Oraculo -> Direcciones -> O.Oraculo -> IO O.Oraculo
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
  return new_root

-- Agrega una nueva opción a la pregunta actual
addOpcionO :: O.Oraculo -> Direcciones -> O.Oraculo -> IO O.Oraculo
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
  return new_root

-- El LCA de dos nodos, dados los caminos respectivos desde la raíz
lcaO :: [(O.Oraculo,a)] -> [(O.Oraculo,a)] -> Maybe (O.Oraculo,(a,a))
lcaO [] _ = Nothing
lcaO _ [] = Nothing
lcaO ((ox,ix):xs) ((oy,iy):ys)
    | ox /= oy                            = Nothing
    | null xs || null ys                  = Just (ox,(ix,iy))
    | (fst . head) xs == (fst . head) ys  = lcaO xs ys
    | otherwise                           = Just (ox,(ix,iy))


-- OPERACIONES PRINCIPALES
--------------------------------------------------------------------------------

-- Pide una nueva predicción para generar un oráculo nuevo con solamente esa predicción
new :: O.Oraculo -> IO O.Oraculo
new o = do
  putStrLn $ haskiTalks ++ "Para crear un oráculo desde cero, ingrese una predicción:"
  prediccion <- if o == O.Nada then getLine' else getLine
  if null prediccion
    then do
      putStrLn $ haskiTalks ++ "No se ha creado oráculo alguno. El anterior se mantiene intacto."
      return o
    else do
      let new_o = O.crearOraculo prediccion
      putStrLn $ haskiTalks ++ "Se ha creado un nuevo oráculo con la siguiente predicción:"
      putStrLn $ haskiTabed ++ O.prediccion new_o
      return new_o

-- Realiza una nueva predicción a partir del oráculo actual y las respuestas del usuario
predecir :: O.Oraculo -> IO O.Oraculo
predecir o = predecir' o []
  where 
    predecir' o ds = do
      let oraculo_actual = nextWith (reverse ds) o
            where nextWith []     o = o
                  nextWith (d:ds) o = nextWith ds $ O.respuesta o d
      case oraculo_actual of
        O.Prediccion p -> do
          putStr $ haskiTalks ++ "Estás pensando en... " ++ p ++ " ?"
          yes <- yesno
          if yes
            then do
              putStrLn $ haskiTalks ++ "Wubbalubbadubdub!"
              return o
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
                Just _ -> predecir' o (respuesta:ds)
                otherwise -> do
                  putStrLn $ haskiTalks ++ "Lo siento. No sé a qué opción te refieres. "
                  predecir' o ds

-- Guarda el oráculo completo en el archivo especificado por el usuario
persistir :: O.Oraculo -> IO O.Oraculo
persistir o = do
  putStrLn $ haskiTalks ++ "Ingresa el nombre del archivo donde deseas guardar tu oráculo:"
  filename <- getLine'
  putStrLn $ haskiTalks ++ "Guardando oráculo..."
  withFile filename WriteMode (\file -> hPrint file o)
  return o

-- Carga un oráculo del archivo especificado por el usuario
cargar :: O.Oraculo -> IO O.Oraculo
cargar o = catch rO hE
            where
              hE :: IOError -> IO O.Oraculo
              hE _ = do
                putStrLn $ haskiTalks ++ "F***, hubo un problema al leer el archivo."
                putStrLn $ haskiTabed ++ "Intenta otra vez!"
                cargar o
              rO :: IO O.Oraculo
              rO = do
                putStrLn $ haskiTalks ++ "Ingresa el nombre del archivo de donde deseas cargar un oráculo:"
                filename <- getLine
                if null filename 
                  then do
                    putStrLn $ haskiTalks ++ "El oráculo anterior quedó intacto." 
                    return o 
                  else do
                    putStrLn $ haskiTalks ++ "Cargando oráculo..."
                    s <- readFile filename
                    let o = read s :: O.Oraculo
                    putStrLn $ haskiTabed ++ "... Se cargó el oraculo exitosamente!"
                    return o


-- Indica la única pregunta que decide entre dos predicciones ingresadas por el usuario
crucial :: O.Oraculo -> IO O.Oraculo
crucial o = do
  putStrLn $ haskiTalks ++ "Ingresa una primera predicción a consultar:"
  p1 <- getLine'
  putStrLn $ haskiTalks ++ "Ingresa una segunda predicción a consultar:"
  p2 <- getLine'
  if p1==p2 
    then do
      putStrLn $ haskiTalks ++ "Por favor, dime dos predicciones diferentes!"
      return o
  else do
    let 
      deepIn os f = map (\(k,v) -> (f v, k)) (M.toList os)
      fstIsSmt    = ((/=) Nothing) . fst  
      dirsHastaO p o@(O.Prediccion p')    = if p' == p then Just [(o,"")] else Nothing
      dirsHastaO p o@(O.Pregunta p' opts) = case ( filter fstIsSmt $ deepIn opts $ dirsHastaO p) of
                                              ((Just x,k):xs) -> Just ((o,k):x)
                                              otherwise       -> Nothing 
    let l1 = dirsHastaO p1 o 
    case l1 of 
      Nothing -> do 
        putStrLn $ haskiTalks ++ "Hey, lo siento pero no se qué es \"" ++ p1 ++"\"."
        return o
      Just l1 -> do
        let l2 = dirsHastaO p2 o
        case l2 of 
          Nothing -> do 
            putStrLn $ haskiTalks ++ "Hey, lo siento pero no se qué es \"" ++ p2 ++"\"."
            return o
          Just l2 -> do
            let lca = maybe (O.Nada,([],[])) id $ lcaO l1 l2
            case fst lca of
              O.Pregunta p _ -> do
                putStrLn $ haskiTalks ++ "La pregunta que decide entre ambas opciones es:"
                putStrLn $ haskiTabed ++ p
                putStrLn $ haskiTabed ++ "("++p1++") \t- " ++ (fst $ snd lca)
                putStrLn $ haskiTabed ++ "("++p2++") \t- " ++ (snd $ snd lca)
                return o
              otherwise -> do
                putStrLn $ haskiTalks ++ "No hay pregunta que diferencie ambas predicciones!"
                return o

-- Termina la ejecución de Haskinator
salir :: O.Oraculo -> IO O.Oraculo
salir _ = do
  putStrLn $ haskiTalks ++ "Gracias por usar el glorioso Haskinator!"
  exitSuccess


-- LÓGICA DEL JUEGO
--------------------------------------------------------------------------------

-- Menú de opciones del loop principal
menu :: [(String, (O.Oraculo -> IO O.Oraculo))]
menu = [ ("Crear Oráculo Nuevo",        new),
         ("Hacer una Predicción",       predecir),
         ("Guardar el Oráculo actual",  persistir),
         ("Cargar un Oráculo",          cargar),
         ("Consultar Pregunta Crucial", crucial),
         ("Salir",                      salir) ]

-- Loop principal de Haskinator. Presenta las opciones del juego.
loop :: O.Oraculo -> IO O.Oraculo
loop o = do
  let menui = if o == O.Nada then [0,3,5] else [0..length menu-1]
  putStrLn $ haskiTalks ++ "Por favor, escribe el número de una opción:"
  forM_ (zip [0..] menu) $ \(a,(b,c)) -> if notElem a menui then return () else putStrLn $ haskiTabed ++ (show a) ++ " - " ++ b
  in_opt <- getLine'
  let i     = read in_opt :: Int
      opt   = if notElem i menui then Nothing else Just $ snd $ menu!!i
  case opt of
    Just f -> do
      result <- f o
      loop result
    Nothing -> do
      putStrLn $ haskiTalks ++ "...Hmmm... No entendí lo que quieres."
      loop o

-- Inicia la ejecución de Hakinator
main :: IO ()
main = do
  putStrLn $ haskiTalks ++ "Bienvenido al glorioso Haskinator!"
  o <- loop O.Nada
  loop o
  return ()
