import Data.List
import Data.List.Split
import System.IO
import Control.Monad


hasList([],lst) = False
hasList(curList:rest,lst) =  if curList==lst then True else hasList(rest,lst)

data Graph =Graph {vertices:: [(Integer,[Integer])]}

buildVertices(0) = []
buildVertices(iter) = (iter,[]):buildVertices(iter-1)

buildEdges( [] ,vertices ) = vertices
buildEdges(fromV:toV:rest,vertices) = buildEdges(rest,newVertices)
	where newVertices = map(\(el,verts) -> if el==fromV then (el,toV:verts) else (el,verts)) vertices

buildGraph(nvertex,vertexList) = Graph(buildEdges(vertexList,buildVertices(nvertex)))


makeStep(graph,[]) = []
makeStep(graph,currVertex:rest) =  case lookup currVertex (vertices(graph)) of
										Just vToList ->  vToList++makeStep(graph,rest)
										Nothing -> makeStep(graph,rest)

makeMove(graph,0,currVertices) = currVertices
makeMove(graph,diceValue,currVertices) =
	 makeMove(graph,diceValue-1, 
	 	foldl (\newLst el -> if (elem el newLst == True) then newLst else el:newLst) []  (makeStep(graph,currVertices)))



diceProcess(endNode,graph,depth,[],initialDiceList,currVertices,borderLists) = 
	if hasList(borderLists,sort(currVertices)) then -1 
		else diceProcess(endNode,graph,depth,initialDiceList,initialDiceList,currVertices,sort(currVertices):borderLists)
diceProcess(endNode,graph,depth,diceValue:rest,initialDiceList,currVertices,borderLists) = 
	if elem endNode currVertices == True then depth
		else diceProcess(endNode,graph,depth+1,rest,initialDiceList,makeMove(graph,diceValue,currVertices),borderLists)


dice(nvertex,vertexList,diceList) = 
		diceProcess(nvertex,buildGraph(nvertex,vertexList),0,diceList,diceList,[1],[])


main::IO()
main = do
	hSetBuffering stdin LineBuffering
	s <- getLine
	let ntasks = read s :: Integer
	
	forM_ [1..ntasks] (\i -> do
		params <- getLine
		let graphMetadata = map (\x -> read x :: Integer) (splitOn " " params)
		params <- getLine
		let edges = map (\x -> read x :: Integer) (splitOn " " params)
		params <- getLine
		let dices = map (\x -> read x :: Integer) (splitOn " " params)
		let ans = dice(head(graphMetadata),edges,dices)
		print ans
		)


