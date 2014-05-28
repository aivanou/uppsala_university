(*

fun product n =
    if n = 1 then 1
    else n * product (n-1)



A.1. product 3 
->if 3=1 then 1 else 3*product(2)
->if  false then 1 else 3*product(2)
->3*product(2)
->3*(if 2=1 then 1 else 2*product(1))
->3*(if false then 1 else 2*product(1))
->3*(2*product(1))
->3*2*(if 1=1 then 1 else product(0))
->3*2(if true then 1 else product(0))
->3*2*1 
->6
A.1.

A.2.
product of the first n digits, also there is no condition, if n<1, 
as a result it can be an eternal cycle
A.2.
*)

(*
A.3.
product n
   TYPE: int -> int
   PRE:  n >= 1
   POST: the product of the first n digits
   EXAMPLES: product 1 = 1
             product 4 = 24
A.3.
*)
(*
A.4. Give a variant for the function. - 
 VARIANT: n 
A.4.
 *)

(*
B.1.
*)
		val plus=fn (x,y)=>x+y;

		fun temp_p x y = x+y;
		val plus=temp_p;
(*B.1.

B.2.
	
	This line will create variable called foo, that will contain the result of computing plus(4,5) => (9)
B.2.

B.3.

	This line will create another function bar, that will have 1 constant parameter 4.
	Then this function can be called like this: bar 42;
B.3.

B.4.
	plus 3 4;
	( fn 3 4 => 3 + 4;
	  fn 3 4 => 7;
	  return 7;
	)
B.4.
*)


(*

C.1. int -> int :  val f=fn x=>x*2; C.1.
C.2.int -> int -> int

  fun f x y = x*y*2; 
  fun f x = fn y=>x+y;
C.2.
C.3.
	int -> int * int
	  	val f=fn x=>(x,2*x);
C.3.
C.4.
	int * int -> int  
		val f= fn(x,y)=>1*x*y;
C.4.
C.5.
	int -> real -> string -> string  
		fun f a b c = if a=1 andalso b>1.0 then c^" yes" else c^" no";
C.5.
C.6.
	int * ( string * string * int) -> int * string: 
		fun f(a,(b,c,d))=(a*size(b)+size(c)*d,b^c);
C.6.

*)

exception badNumber;
(*
D.1.
now it returns 1 if the input number is less then 1 
  "You must decide how to handle the case when n < 1." - very ambiguous condition
*)

fun lcm n=
	let
		fun gcd(a,b)=if a=0 then b else gcd(b mod a,a)
		fun slcm(a1,b1) = a1*b1 div gcd(a1,b1)
	in
		if n>1 then slcm(n,lcm(n-1)) else 1
	end;
(*D.1*)
