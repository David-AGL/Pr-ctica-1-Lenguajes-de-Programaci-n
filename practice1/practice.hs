import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un Articulo
data Articulo = Articulo {
    nombre :: String,
    categoria :: String
} deriving (Show, Read)

-- Función para registrar la entrada de un artículo al Inventario
registrarEntrada :: String -> String -> [Articulo] -> [Articulo]
registrarEntrada nombre categoria inventario =
    Articulo nombre categoria : inventario

-- Función para buscar artículos por categoría
buscarArticulos :: String -> [Articulo] -> [Articulo]
buscarArticulos categoriaArticulo inventario =
    filter (\v -> categoriaArticulo == categoria v) inventario

-- Función para guardar la información de los artículos en un archivo de texto
guardarInventario :: [Articulo] -> IO ()
guardarInventario inventario = do
    withFile "inventario.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarArticulo inventario))
    putStrLn "Inventario guardado en el archivo inventario.txt."

-- Función para cargar la información de los artículos desde un archivo de texto
cargarInventario :: IO [Articulo]
cargarInventario = do
    contenido <- withFile "inventario.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerArticulo lineas)
    where
        leerArticulo linea = read linea :: Articulo

-- Función para mostrar la información de un artículo como cadena de texto
mostrarArticulo :: Articulo -> String
mostrarArticulo (Articulo nombre categoria) =
    "Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\"}"

-- Función para listar los artículos en el inventario
listarArticulos :: [Articulo] -> IO ()
listarArticulos [] = putStrLn "No hay artículos en el inventario."
listarArticulos articulos = do
    putStrLn "Artículos en el inventario:"
    mapM_ (putStrLn . mostrarArticulo) articulos

-- Función para contar los artículos de una categoría específica
contarArticulosPorCategoria :: String -> [Articulo] -> Int
contarArticulosPorCategoria categoriaArticulo inventario =
    length $ filter (\a -> categoria a == categoriaArticulo) inventario

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el inventario desde el archivo de texto
    inventario <- cargarInventario
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"

    -- Ciclo principal del programa
    cicloPrincipal inventario

-- Función para el ciclo principal del programa
cicloPrincipal :: [Articulo] -> IO ()
cicloPrincipal inventario = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de artículo"
    putStrLn "2. Buscar artículos por categoría"
    putStrLn "3. Listar artículos"
    putStrLn "4. Mostrar cantidad de artículos por categoría"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del artículo:"
            nombreArticulo <- getLine
            putStrLn "Ingrese la categoría del artículo:"
            categoriaArticulo <- getLine
            let inventarioActualizado = registrarEntrada nombreArticulo categoriaArticulo inventario
            putStrLn $ "Artículo '" ++ nombreArticulo ++ "' ingresado al inventario."
            guardarInventario inventarioActualizado
            cicloPrincipal inventarioActualizado

        "2" -> do
            putStrLn "Ingrese la categoría a buscar:"
            categoriaBusqueda <- getLine
            let resultados = buscarArticulos categoriaBusqueda inventario
            case resultados of
                [] -> putStrLn $ "No se encontraron artículos en la categoría '" ++ categoriaBusqueda ++ "'."
                _  -> do
                    listarArticulos resultados
            cicloPrincipal inventario

        "3" -> do
            listarArticulos inventario
            cicloPrincipal inventario

        "4" -> do
            putStrLn "Ingrese la categoría para contar artículos:"
            categoriaConsulta <- getLine
            let totalPorCategoria = contarArticulosPorCategoria categoriaConsulta inventario
            putStrLn $ "Hay " ++ show(totalPorCategoria) ++ " artículos en la categoría " ++ categoriaConsulta
            cicloPrincipal inventario


        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal inventario