-- Programação funcional em haskell

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
--      length (tamanho da lista) 
--      reverse ()
--      head (primeiro elemento)
--      tail (todos menos o primeiro elemento)
--      take ()
--      drop ()
--      sum ()
--      product ()
--      maximum ()
--      minimum ()

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

{-
    foldr' (+) 0 [1, 2, 3] = (+) 1 (foldr' (+) 0 [2, 3])
                           = (+) 1 ((+) 2 (foldr' (+) 0 [3]))
                           = (+) 1 ((+) 2 ((+)3 (foldr' (+) 0 [3]))
                           = (+) 1 (foldr' (+ 0 [2, 3]))

-}