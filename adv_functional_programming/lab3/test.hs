import qualified Data.MemoCombinators as Memo


nums = [1,2,3,4]

n = let len [] 0 
		len (x:xs) = len xs +1
	in len nums

fuc 0 = 1
fuc 1 = 1 
fuc n = n* fuc(n-1)

buyable n = iter n (True : replicate 19 False)
    where iter 0 lst = lst !! 0
          iter n lst = iter (n-1) ((lst !! 5 || lst !! 8 || lst !! 19) : take 19 lst)


fib = Memo.integral fib'
    where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n-1) + fib (n-2)