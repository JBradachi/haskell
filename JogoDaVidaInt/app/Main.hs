module Main where

import Data.Matrix (Matrix, fromLists, prettyMatrix, toLists)
import qualified Data.Matrix as M
import Data.Maybe (fromMaybe)
import Graphics.Gloss

type Grid = Matrix Int

lerInicial :: String -> IO (Grid)
lerInicial fp = do
    texto <- readFile fp
    let linhas = lines texto -- lines junta 
    let idade = map (map (\x -> if x == '#' then 1 else 0 )) linhas
    -- let booleanos = map (map (== '#')) linhas
    return (fromLists idade)

atualizar :: Grid -> Grid
atualizar m = M.mapPos (atualizarCelula m) m

atualizarCelula :: Grid -> (Int, Int) -> Int -> Int
atualizarCelula m (i,j) v 
    | v > 0 = if vivos ==2 || vivos == 3 then v+1 else 0
    | otherwise = if vivos == 3 then 1 else 0
    where
        deslocamentos = [(-1,-1), (-1, 0),(-1,1),
                         (0, -1),         (0, 1),
                         (1, -1), (1, 0), (1, 1)]
        vizinhos = map (\(di,dj) -> M.safeGet (i+di) (j+dj) m) deslocamentos
        vizinhos' = map (fromMaybe 0) vizinhos
        vivos = length (filter (> 0) vizinhos')


main :: IO ()
main = do

    let fp = "teste.txt"
    m <- lerInicial fp
    run m
    where 
        run m = do
            let w = M.ncols m
            let h = M.nrows m
            let tamanho = ((w+1)*cellSize, (h+1)*cellSize)
            let evolve _ _ = atualizar
            simulate
                (InWindow "Jogo da Vida" tamanho (0, 0))
                black 12 m toPicture evolve


--mostraMatriz :: Matrix Bool -> String
--mostraMatriz m = foldr (\a b -> a ++ "\n" ++ b) "" strings
--    where
--        linhas = (toLists m)
--        strings = map (map (\v -> if v then '#' else '.')) linhas
 
cellSize :: Int
cellSize = 10
 
toPicture :: Grid -> Picture
toPicture grid =
    Pictures [
        Color white (rectangleWire w' h'),
        Scale 1 (-1) $
            Translate (-w'/2.0) (-h'/2.0) $
                Pictures (map Pictures (toLists cells))
    ]
    where
        w' = fromIntegral $ M.ncols grid * cellSize
        h' = fromIntegral $ M.nrows grid * cellSize
        cells = M.mapPos toPicture' grid
 
toPicture' :: (Int, Int) -> Int -> Picture
toPicture' (i,j) v =
    Translate x y $
        Color (makeColor 0.5 1 0.5 (if v>0 then 1 else 0)) $
            rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
    where
        x = (fromIntegral j - 0.5) * fromIntegral cellSize
        y = (fromIntegral i - 0.5) * fromIntegral cellSize