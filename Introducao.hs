-- Programação funcional em haskell

import Data.List (partition, lookup)
import Data.Maybe (fromMaybe)

-- Baseada na aplicação de funções 
-- - imutabilidade
-- - mais fácil de usar funções de ordem superior (funções que recebem funções como parametro)
-- - avaliação preguiçosa
-- - funções definidas em arquivos <nome.hs>
-- - usadas no interpretador ghci (escrever no terminal) :l <nome do arquivo> >>> carrega o arquivo

-- SEMPRE definidos em funções <nome> <parametros> = <expressão> 
-- todo nome de tipo é maiúsculo
-- nas declarações de parametros das funções o ultimo item é o retorno
delta :: Double->Double->Double-> Double
delta a b c = b**2 - 4*a*c

bhaskara :: Double->Double->Double-> [Double]-- descrição dos parametros

-- se tiver duvida do tipo que a função usa, utilize :t <nome_da_função> no terminal

bhaskara a b c
    | d < 0 = []        -- pipe são ifs aninnhados
    | d == 0 = [x']
    | otherwise = [x', x''] -- otherwise é o else
    where  -- descrição de variaveis
        d = delta a b c
        x' = (-b + sqrt d) / (2*a)
        x'' = (-b - sqrt d) / (2*a)


-- exemplo de recursão fibonacci (não possui laços de repetição)

fib :: Int -> Int

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- as variaveis não mudam valores, para isso tem q chamar a função novamente com um novo valor

fib2 :: Int -> Int

-- exemplo de if

fib2 0 = 0
fib2 1 = 1
fib2 n = fib' 0 1 2
    where
        fib' n2 n1 i =          -- forma de "fazer" um laço, vai empilhando as funções necessárias para ter o resultado
            if i == n
                then n2 +n1
                else fib' n1 (n2+n1) (i+1)

-- forma mais eficiente
fibSeq = 0 : 1 : zipWith (+) fibSeq (tail fibSeq)

-- listas em haskell:
-- - [1, 2, 3] = 1: 2 : 3 : [] os :     são contrutores e descontrutores
-- - [0..10] >> cria lista de 0 a 10 de 1 em 1
-- - [0, 2..10] >>> cria lista de 2 em 2 até 10
-- - possuem listas infinitas >>> [0, 2..]
-- - String são considerados lista de char

-- - possuem varias funções prontas >>> 
--      length Retorna o tamanho de uma lista                                                       > length "abcxyz"            >>> 6
--      reverse Inverte os elementos de uma lista                                                   > reverse [4,5,2,2]          >>> [2,2,5,4]
--      head Retorna o primeiro elemento da lista                                                   > head "abc"                 >>> ’a’
--      tail Retorna o corpo da lista                                                               > tail "abc"                 >>> "bc"
--      take Gera uma lista com os n primeiros elementos da lista original                          > take 2 [’d’,’f’,’g’,’r’]   >>> "df"
--      drop Retira n elementos do inicio da lista                                                  > drop 3 [3,3,4,4,5,5]       >>> [4,5,5]
--      sum Retorna a soma dos elementos da lista                                                   > sum [4,5,7,2,1]            >>> 19
--      product Retorna o produto dos elementos da lista                                            > product [5,3,6,1]          >>> 90
--      maximum Retorna o maior elemento de uma lista                                               > maximum [4,5,1,2]          >>> 5
--      minimum Retorna o menor elemento de uma lista                                               > minimum [5.2,0.3,7.2]      >>> 0.3
--      elem Verifica se um elemento pertence a lista                                               > elem 5 [1,5,10]            >>> True
--      (!!) Retorna o valor da lista do indice enviado                                             > [1,3,5,7,9] !!0            >>> 1
--      replicate Constroi uma lista replicando um elemento                                         > replicate 4 ’c’            >>> "cccc"
--      takeWhile Retorna o maior segmento inicial de uma lista que satisfaz uma condição           > takeWhile (<10) [1,3,13,4] >>> [1,3]
--      dropWhile Retira o maior segmento inicial de uma lista que satisfaz uma condicao            > dropWhile (<10) [1,3,13,4] >>> [13,4]
--      splitAt Divide uma lista num par de sub-listas fazendo a divisao numa determinada posicao   > splitAt 2 [3,4,2,1,5]      >>> ([3,4],[2,1,5])
--      zip Recebe duas listas como entrada e retorna uma lista de pares                            > zip [1,2] [’a’,’b’]        >>> [(1,’a’),(2,’b’)]
--      (++) Concatena duas listas                                                                  > [1,2,3]++[4,5,6]           >>> [1,2,3,4,5,6]
--      concat Recebe uma lista de listas e as concatena                                            > concat [[1,2],[3,4]]       >>>  [1,2,3,4] 
--      last Retorna o ultimo elemento da lista                                                     > last [4,3,2]               >>> 2
--      null Retorna verdadeiro se uma lista é vazia                                                > null []                    >>> True
--
--

-- como percorrer uma lista
-- - ir chamando a a lista com o tail restante

{-

f :: [a] -> Int
f [] = 0
f(h:t)  = <faça aqui o processamento do dado em h> f t <chama novamente a função para o resto da lista> -- descontrutor h >>> head t >>> tail

-}

-- contando quantos itens tem a lista 

len :: [a] -> Int
len [] = 0
len(h:t)  = 1 + len t -- descontrutor h >>> head t >>> tail

{-
f[1, 2, 3] = 1 + f[2, 3]
f[2, 3] = 1 + 1 + f[3]
f[3] = 1 + 1 + 1 + f[]
1+1+1
-}

-- função soma

sum' :: [Int] -> Int
sum' [] = 0
sum' (h:t)  = h + sum' t



foldr' :: (a->b->b) -> b -> [a] -> b -- exemplo de declaração de parametro utilizando função como parametro "(a->b->b)"
foldr' f z [] = z
foldr' f z (h:t) = f h (foldr' f z t)

-- transforma os elementos da lista em um valor
{-
    foldr' (+) 0 [1, 2, 3] = (+) 1 (foldr' (+) 0 [2, 3])
                           = (+) 1 ((+) 2 (foldr' (+) 0 [3]))
                           = (+) 1 ((+) 2 ((+)3 (foldr' (+) 0 []))
                           = (+) 1 ((+) 2 ((+)3 0)
                           = (+) 1 ((+) 2 3)
                           = (+) 1 5)
                           = 6

-}

-- filtrar a lista
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (h:t) =
     if f h
        then h : filter' f t
        else filter' f t

-- filter' (\x -> x `mod` 2 == 1) [0..25] retorna uma sublista com uma função especifica, nesse caso numeros impares

-- map aplica uma fução em todos os itens da lista
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (h:t) = f h : map' f t

-- map' (/2) [0..10]


-- quicksort em haskell

quicksort :: (Ord a) => [a] -> [a]

quicksort [] = []
quicksort (pivot:t) = quicksort lt ++ [pivot] ++ quicksort gt
    where
        lt = filter (< pivot) t
        gt = filter (>= pivot) t

-- quicksort em haskell, utilizando partition
quicksort2 :: (Ord a) => [a] -> [a]

quicksort2 [] = []
quicksort2 (pivot:t) = quicksort2 lt ++ [pivot] ++ quicksort2 gt
    where (lt, gt) = partition (< pivot) t -- desconstrução de tupla


-- valores nulos representado por Nothing e Maybe
-- Existem funções prontas que ajudam
-- - fromMaybe
-- - fromJust
-- - isNothing 
-- - isUpper
-- - catMaybes
-- - listToMaybe
-- - maybeToList
-- - lookup dado um valor do elemento da tupla, talvez ele retorna um elemento, caso contrário ele retorna nothing 


-- procura uo indice que aparece a letra
indiceLetra:: Char -> Int
indiceLetra c =
    fromMaybe 0 m -- substitui o case
--    case m of  
--        Nothing -> 0
--        Just i -> i
    where m = lookup c (zip ['A'..'Z'] [1..])

-- filter mais complexo, retorna o primeiro elemento encontrado que satisfaz a função, ou retorna Nothing
lookupBy :: (a -> Bool) -> [a] -> Maybe a
lookupBy f [] = Nothing 
lookupBy f (h:t) = 
    if f h 
        then Just h
        else lookupBy f t

-- lookupBy (\p -> head p == 'a') ["livro", "asas", "casa", "abacate"]
