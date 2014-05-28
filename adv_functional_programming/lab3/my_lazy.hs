import Data.List

createList([],el:lst,currListLength,maxIndex) = createList([el],lst,1,maxIndex)
createList(currList,el:rest,currListLength,maxIndex) | maxIndex < currListLength = currList
createList(currList,el:rest,currListLength,maxIndex) =
	createList(currList++[el]++currList,rest,currListLength*2+1,maxIndex)

reach_element(0,lst)=lst 
reach_element(index,el:rest) =reach_element(index-1,rest)

elsSumm(0,el:rest) = el
elsSumm(ind,el:rest) = el+elsSumm(ind-1,rest)

lazy :: Integer -> Integer -> [Integer] -> Integer
lazy minIndex maxIndex infList = elsSumm(maxIndex-minIndex,reach_element(minIndex-1,createList([],infList,0,maxIndex+1)))

