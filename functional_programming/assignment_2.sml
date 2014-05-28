
fun iota(n) = 
	let
	    fun iota'(max_el,current_el) = if current_el=0 then [] 
	    											   else (max_el-current_el)::iota'(max_el,current_el-1)
	in
	    iota'(n,n)
	end;

fun interUnord s1 s2 =
	let
	    fun inList(v,[]) = false
	      | inList(v,x::lst) = if v=x then true else inList(v,lst)

	    fun interUnord'(lst1,[]) = []
	      | interUnord'(lst1,x::lst2) = if inList(x,lst1) then x::interUnord'(lst1,lst2)
	      												  else interUnord'(lst1,lst2)
	in
		interUnord'(s1,s2)    
	end;

fun interOrd s1 s2 =
	let
	    fun interOrd'([],[]) = []
	      | interOrd'(x1::lst1,[]) = []
	      | interOrd'([],x2::lst2) = []
	      | interOrd'(x1::lst1,x2::lst2)= if x1=x2 then x1::interOrd'(lst1,x2::lst2)
	      										   else if x1>x2 then interOrd'(x1::lst1,lst2)
	      										   				  else interOrd'(lst1,x2::lst2)
	in
	    interOrd'(s1,s2)
	end;

fun real_time f =
    let
	val rt = Timer.startRealTimer()
	val result = f ()
	val time = Timer.checkRealTimer rt
    in
	(time, result)
    end;


val result = 
    let
	val s1 = iota 2
	val s2 = iota 3
	val slow_time_and_res = real_time (fn () => interUnord s1 s2)
	val fast_time_and_res = real_time (fn () => interOrd s1 s2)
    in
	(slow_time_and_res, fast_time_and_res)
    end;

datatype fruit = 
    apple of real
  | banana of real
  | lemon of int;


fun sumPrice(fruits,apple_price,banana_price,lemon_price)=
	let
	    fun getPrice(nil) = 0.0
	      | getPrice((apple (weigth))::lst) = weigth*apple_price+getPrice(lst)
	      | getPrice((banana (weigth))::lst) =weigth*banana_price+getPrice(lst)
	      | getPrice((lemon (amount))::lst) = real(amount)*lemon_price+getPrice(lst);
	in
	    getPrice(fruits)
	end;

datatype 'a tree = node of {value:'a,children: 'a tree list};


fun buildTree values init_value= 
	let
	    fun add new_value (node{value,children}) = if length children = 3 then seek(new_value,[],children)
	      			else node{value=value,children=node{value=new_value,children=[]}::children}
	    
	    and seek(new_value,next_queue, []) = seek(new_value,[],next_queue)
	      | seek(new_value,next_queue, (node{value,children})::lst) = if length children=3 
	    										then seek(new_value,(next_queue)@(children),lst)
	    										else node{value=value,children=node{value=new_value,children=[]}::children}
 	      
	    fun buildTree'(nd ,[])= nd
	      | buildTree'(nd ,el::lst)=buildTree'(add el nd ,lst);
	in
	    buildTree'(node{value=init_value,children=[]},values)
	end;

fun nodesAmount (node{value,children})= 
	let
	    fun compute([]) = 0
	      | compute(el::lst)=nodesAmount(el)+compute(lst)
	in
	    1+compute(children)
	end;

fun getLabels (node{value,children})=
	let
	    fun iterate([]) = []
	      | iterate(el::lst) = (getLabels el)@(iterate(lst))
	in
	    value::(iterate(children))
	end;

fun treeSearch s_value (node{value,children}) =
	let
	    fun iterate([]) = false
	      | iterate(el::lst) = if (treeSearch s_value el)=true then true
	      													   else iterate(lst)
	in
		if s_value=value then true
						 else iterate(children)			    
	end;

fun treeHeight (node{value,children}) = 
	let
	    fun max a b = if a>b then a else b
	    fun iterate ([]) = 0
	      | iterate (el::lst)= max(treeHeight el) (iterate(lst))
	in
	    1+iterate(children)
	end;










