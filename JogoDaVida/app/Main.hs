module Main where

import Data.Matrix (Matrix, fromLists, prettyMatrix)
import qualified Data.Matrix as M
import Data.Maybe (fromMaybe)

lerInicial :: String -> IO (Matrix Bool)
lerInicial fp = do
    texto <- readFile fp
    let linhas = lines texto -- lines junta 
    let booleanos = map (map (=='#')) linhas
    return (fromLists booleanos)

atualizar :: Matrix Bool -> Matrix Bool
atualizar m = M.mapPos (atualizarCelula m) m

atualizarCelula :: Matrix Bool -> (Int, Int) -> Bool -> Bool
atualizarCelula m (i,j) v = (v && (vivos == 2) || (vivos == 3)) || vivos == 3 
    where
        deslocamentos = [(-1,-1), (-1, 0),(-1,1),
                         (0, -1),         (0, 1),
                         (1, -1), (1, 0), (1, 1)]
        vizinhos = map (\(di,dj) -> M.safeGet (i+di) (j+dj) m) deslocamentos
        vizinhos' = map (fromMaybe False) vizinhos
        vivos = length (filter (\x -> x ) vizinhos')


main :: IO ()
main = do

    let fp = "teste.txt"
    m <- lerInicial fp
    run m
    where 
        run m = do
            putStrLn (prettyMatrix m)
            run (atualizar m)
