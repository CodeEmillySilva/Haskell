import Data.List(sortBy)
type Doc = String
type Line = String
type Word' = String

--a/ numerar linhas
-- Lines já existe na biblioteca

--b/ numerar as linhas do documento

numLines :: [Line] -> [(Int,Line)]
numLines xs = numLines' 1 xs

numLines' _ [] = []
numLines' n (x:xs) = (n,x):numLines' (n+1) xs

--c/ associar a cada ocorrencia de uma palavra no documento o num da linha que ocorrencia

allNumWords :: [(Int,Line)] -> [(Int,Word')]
allNumWords [] = []
allNumWords ((n,w):xs) = allNumWords' n (words w) ++ allNumWords xs

allNumWords' _ [] = []
allNumWords' n (x:xs) = if length (x) < 3 then allNumWords' n xs
                        else (n,x) : allNumWords' n xs

--d/ sortby, ordenar alfabeticamente as ocorrencias de palavras no texto

sortLs :: [(Int,Word')] -> [(Int,Word')]
sortLs [] = []
sortLs xs = sortBy (\(_,w1)(_,w2) -> compare w1 w2) xs

--e/ produzir para cada palvara a lista dos números de linhas em que a palavra ocorre

almalgamate :: [(Int,Word')] -> [([Int],Word')]
almalgamate [] = []
almalgamate xs = almalgamate' xs []

almalgamate' [] jun = jun
almalgamate' ((n,w):xs) jun = almalgamate' xs (juncao n w jun)

juncao n w [] = [([n],w)]
juncao n w ((m,v):xs) = if w==v then (m++[n],w):xs
                        else (m,v) : juncao n w xs

--f/ Eliminar, da lista de numeros de linhas, numeros repetidos

shorten :: [([Int],Word')] -> [([Int],Word')]
shorten = map (\(n,w) -> (removerRep n,w))

removerRep [] = []
removerRep (x:xs) = if pertence x xs then removerRep xs
                    else x:removerRep xs

pertence :: Eq a => a -> [a] -> Bool
pertence e [] = False
pertence e (x:xs) = if e==x then True else pertence e xs

-- implementar o arquivo texto nas funções 
makeIndex :: Doc -> [([Int], Word')]
makeIndex texto = (shorten . almalgamate . sortLs . allNumWords . numLines . lines) texto

-- função para mostrar na tela o resultado final demakeIndex
impressao :: [([Int],Word')] -> IO ()
impressao texto = sequence_ (map (\(x,y) -> putStrLn (show x ++ " - " ++ y)) texto)

-- main
main = do   putStrLn "Mudança do arquivo de:"
            texto <- readFile "Arquivos.txt"
            putStrLn texto
            putStrLn "Para:"
            impressao (makeIndex texto)
