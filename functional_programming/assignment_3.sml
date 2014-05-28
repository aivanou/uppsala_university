(*I. Tail Recursion*)

fun average lst=
	let
	    fun compute([],summ)=summ
	      | compute(el::tail,summ)=compute(tail,Real.+(summ,el))
	in
	    compute(lst,0.0)/real(length lst)
	end

(*J. Specification*)

(*K. Use of Higher-order Functions*)

fun append lst el = foldl (op ::) lst [el];

exception Domain;

fun last_l([]) = raise Domain
  | last_l(el::lst) = foldl (fn(x,y) => x) el lst;

fun last_r([]) = raise Domain
  | last_r(el::[]) = el
  | last_r(el::lst) = foldr (fn(x,y) => if y=el then x else y) el lst;

fun member_l lst el =
	let
	    fun f(x,b) = if x=el then [true]::b else b
	in
	    if length (foldl f [] lst) = 1 then true else false
	end;

fun member_r lst el = if length (foldr (fn(x,b)=> if x=el then [true]::b
									      else b) [] lst) = 1 then true else false;

fun reverse_l lst = foldl (op ::) [] lst;

fun reverse_r lst = foldr (fn(x,y)=>y@[x]) [] lst;

fun max lst = foldl (fn(x,b) => if x>b then x else b) ~1 lst;

fun filter lst f = 
	let
		fun check cond (el,lst) = if cond el then el::lst else lst;
	in
	    foldl (check f) [] lst
	end;

(* L. Binary Search Trees*)

datatype tree = Void | Node of tree * int * tree ;

fun sub_tree low high tree= 
	let
	    fun add_sub_tree (Void) node = node
	      | add_sub_tree (Node(left,tree_el,right)) node = if tree_el>=low andalso tree_el<high
	      	 then Node((add_sub_tree left node),tree_el, (add_sub_tree right node))
	      	 else add_sub_tree (left) (add_sub_tree right node)
	in
	    add_sub_tree tree Void
	end;
	
(*M. Complexity
	The worst case time will be, when we add nodes, and each new node lies only to the left or to the right
	as a result, to add N nodes, we need O(N). For each node, we should find the last node,
	finally, the worst case : O(N^2)

	average : O(K*log K) where K - amout of appropriate nodes
*)