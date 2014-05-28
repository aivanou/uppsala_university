import Data.List
import Data.List.Split
import System.IO
import Control.Monad
import qualified Data.MemoCombinators as Memo


getRoadLength([]) = 0
getRoadLength((tiles,speed):rest) = tiles+getRoadLength(rest)

splitEvenOdd [] = ([], [])
splitEvenOdd [x] = ([x], [])
splitEvenOdd (x:y:xs) = (x:xp, y:yp) where (xp, yp) = splitEvenOdd xs

possibleAccselerations(0,0) =[0]
possibleAccselerations(0,minusSpeed) = -minusSpeed:possibleAccselerations(0,minusSpeed-10)
possibleAccselerations(plusSpeed,val) = plusSpeed:possibleAccselerations(plusSpeed-10,val)

minSpeedRegion([],currTile,currMinSpeed,minTile,maxTile) = currMinSpeed
minSpeedRegion((tiles,speed):rest,currTile,currMinSpeed,minTile,maxTile) =
	if and[currTile>=minTile, currTile<=maxTile, currMinSpeed>speed] then
		minSpeedRegion(rest,currTile+tiles,speed,minTile,maxTile)
	else minSpeedRegion(rest,currTile+tiles,currMinSpeed,minTile,maxTile)

rally(road,[],currTile,_,moves,_,roadLength) | currTile >= roadLength = moves
rally(road,[],currTile,_,_,_,roadLength) | currTile < roadLength = -1
rally(road,racs,currTile,currSpeed,moves,initialAcs,roadLength) | currTile >= roadLength = moves
rally(road,acs:restAcs,currTile,currSpeed,moves,initialAcs,roadLength)  | (currSpeed+acs) <=0 = 
	rally(road,restAcs,currTile,currSpeed,moves,initialAcs,roadLength)

rally(road,acs:restAcs,currTile,currSpeed,moves,initialAcs,roadLength) = 
	if minSpeedRegion(road,0,10000,currTile+1,currTile+(div (currSpeed+acs) 10)) < (currSpeed+acs) then
		rally(road,restAcs,currTile,currSpeed,moves,initialAcs,roadLength)
	else 
		if mvs1 == -1 then rally(road,restAcs,currTile,currSpeed,moves,initialAcs,roadLength) 
			else mvs1 where 
				mvs1 = rally(road,initialAcs,currTile + (div (currSpeed+acs) 10),currSpeed+acs,moves+1,initialAcs,roadLength)


main::IO()
main = do
	hSetBuffering stdin LineBuffering
	s <- getLine
	let ntasks = read s :: Integer
	
	forM_ [1..ntasks] (\i -> do
		params <- getLine
		let roadMetadata = map (\x -> read x :: Integer) (splitOn " " params)
		params <- getLine
		let rawRoad = map (\x -> read x :: Integer) (splitOn " " params)
		let (rodd,reven) = splitEvenOdd(rawRoad)
		let road = init(zip rodd reven)
		let	initialAcs = sortBy (\x y -> if x>y then LT else GT) (possibleAccselerations(head(roadMetadata),head(tail(roadMetadata))))
		let rl = getRoadLength(road)
		let ans = rally(road,initialAcs,0,0,0,initialAcs,rl)
		print ans
		)
