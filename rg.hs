import System.Random
import qualified Data.Matrix as Matrix 
import qualified Data.Set as Set

type Chrome = (Int, Int) 
type Gene = [Chrome] -- list order corresponds to innovation
-- TODO: add possibility of empty mapping or missing chromisone

type Pheno = [Bool] -- list of bools representing which chromisones in the gene are expressed

data Circuit = INPUT | OUTPUT | AND | OR | NOT | MUX
    deriving (Read, Show, Enum, Eq)
-- these circuit objects have some constraints
-- NOT should only have one input
-- INPUT should have no inputs
-- OUTPUT should have no output and only one input
-- MUX must differentiate select signals from other inputs 
-- Possible solution: convert to matrix form and check the above reqirements, cull those who don't meet
-- Remaining problem: certain cells require specific ports for connections (ie select and a b in mux)

circuitAtoms = [INPUT , OUTPUT , AND , OR , NOT , MUX]
maxNodes = 10 :: Int

randomGene :: Int -> Int -> Gene
randomGene size seed =
    let randomNumbers = take (2*size) $ randoms $ mkStdGen seed in
    let source = (flip mod maxNodes) <$> abs <$> take size randomNumbers in
    let destination = (flip mod maxNodes) <$> abs <$> drop size randomNumbers in
    zip source destination

randomNode :: StdGen -> (Circuit, StdGen)
randomNode state = 
    let (randomValue, newState) = next state in
    let index = mod randomValue $ length circuitAtoms in
    (circuitAtoms !! index, newState)

randomNodes :: StdGen -> Int -> [Circuit]
randomNodes state size
    | size > 0 = let (symbol, newState) = randomNode state in [symbol] ++ (randomNodes newState $ size - 1)
    | otherwise = []

cross :: StdGen -> [a] -> [a] -> [a]
cross state (ah:at) (bh:bt) =
    let (randomValue, newState) = next state in
    let head = if 0 /= mod randomValue 2 then ah else bh in
    let tail = cross newState at bt in 
    (head:tail)
cross state [] [] = []
-- TODO: what if lists are of different lengths

edgeCount :: Gene -> (Int, Int) -> Int
edgeCount g p =
    if Set.member p $ Set.fromList g then 1 else 0

geneTable :: Gene -> Matrix.Matrix Int 
geneTable g =
    let dim = length g in
    Matrix.matrix dim dim $ edgeCount g

--checkInputs Gene -> [Circuit] -> Bool
--checkInputs g (h:t) = sum $ Matrix.getRow index
--checkOutputs Gene -> [Circuit] -> Bool
--checkOutputs = sum $ Matrix.getCol index

-- TODO: Add mutation mechanism
-- relevant mutations:
--     Change node type
--     Activate/Deactivate an existing edge
--     Inovate a new edge
--     Inovate a new node 
--mutate :: Gene -> Gene
--mutate a = 




-- TODO: Implement cost and reward function
-- big cost to larger graphs (graph size measured in edges and nodes?)
-- exponential reward for coverage (a small increase in coverage when wealready have high coverage is a big deal)
-- heavy penalties for invalid circuits (ie, two lines going into a not)


-- TODO JSON to Graph


-- Graph to Graphviz Dot lang
drawEdge :: [Circuit] -> Gene -> String
drawEdge nodes ((str, fin):t) = "    " ++ (show $ nodes !! str) ++ show str ++ " -> " ++ (show $ nodes !! fin) ++ show fin ++ "\n" ++ drawEdge nodes t 
drawEdge nodes [] = "" 

draw :: [Circuit] -> Gene -> String
draw nodes x = "digraph g {\n" ++ drawEdge nodes x ++ "}"

main = do
    putStrLn $ draw (randomNodes (mkStdGen 0) 10) $ randomGene 10 0
    putStrLn $ draw (randomNodes (mkStdGen 5) 10) $ randomGene 10 5 
    putStrLn $ draw (cross (mkStdGen 0) (randomNodes (mkStdGen 0) 10) (randomNodes (mkStdGen 5) 10)) $ cross (mkStdGen 0) (randomGene 10 0) $ randomGene 10 5
    
    putStrLn $ show $ geneTable $ randomGene 10 0
