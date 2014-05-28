(* Ivanou Aliaksandr - Aliaksandr.Ivanou.1364@student.uu.se *)

(*-------------------------------------------------------------------*)
(*          Assignment 4 - Functional Programming                    *)
(*-------------------------------------------------------------------*)


(*-------------------------------------------------------------------*)
(*                 N. Valuations: Interface                          *)
(*-------------------------------------------------------------------*)


signature VALUATION = sig
  type VT
  val empty: VT
  val insert: string * bool * VT -> VT
  val lookup: string * VT -> bool option
  val getValues: VT ->(string * bool)  list

end;

(*-------------------------------------------------------------------*)
(*                 N. Valuations: implementaion                      *)
(*-------------------------------------------------------------------*)


structure Valuation :>VALUATION  = struct

type VT = (string * bool) list

val empty = []

fun insert(key,value,lst) = (key, value) :: lst

fun lookup (seekKey , []) = NONE
   | lookup (seekKey,(key, value) :: ls) =
     if seekKey=key then SOME value else lookup(seekKey,ls)

fun getValues (l)  =
    let
        fun gv ([],l2)  = l2
          | gv (el::l1,l2) =  gv(l1,el::l2)

    in
        gv(l,[])
    end

end;

datatype  formula = True
                 | False
                 | Var of  string
                 | Not of  formula
                 | And of  formula *  formula
                 | Or of   formula *  formula;



functor Semantics (V: VALUATION) = 
struct
        
    fun truth_value(valuation,fr) =
        let
                fun compute_tr_value(True) = true
                  | compute_tr_value(False) = false
                  | compute_tr_value(Var(str)) = if  Valuation.lookup(str,valuation) = NONE then raise Domain
                                                    else valOf(Valuation.lookup(str,valuation))
                  | compute_tr_value(And(f1,f2)) = if compute_tr_value(f1) andalso compute_tr_value(f2) then true else false
                  | compute_tr_value(Or(f1,f2)) = if compute_tr_value(f1) orelse compute_tr_value(f2) then true else false
                  | compute_tr_value(Not(f1)) = if compute_tr_value(f1) then false else true

        in
            compute_tr_value(fr)
        end



    fun is_tautology(fr)=
        let
            fun collectVariables(True,lst) = lst
              | collectVariables(False,lst) = lst
              | collectVariables(Var(str),lst) = str::lst
              | collectVariables(And(f1,f2),lst) = collectVariables(f1,lst)@collectVariables(f2,lst)
              | collectVariables(Or(f1,f2),lst) = collectVariables(f1,lst)@collectVariables(f2,lst)
              | collectVariables(Not(f1),lst) = collectVariables(f1,lst) 

            fun check_tautology(f,[],valuationList) = truth_value(valuationList,f)
              | check_tautology(f,el::lst,valuationList)= if check_tautology(f,lst,Valuation.insert(el,true, valuationList)) 
                                                            andalso check_tautology(f,lst,Valuation.insert(el,false,valuationList))
                                                             then true
                                                          else false

        in
            check_tautology(fr,collectVariables(fr,[]),Valuation.empty)
        end

end;

(* examples *)
local
    open Valuation
in
    val A = "a";
    val B = "b";
    val valuationList = empty;
    val valuationList = insert (A ,true, valuationList);
    val valuationList = insert (B ,false ,valuationList);
    val lookup_A = lookup( "A", valuationList);
    val f1=And(Var "a" ,Var "b");
    val f2=Or(And(Var "a",Var "b"),Not(Var "b"));
    val f3= Or (Var "x", Not (Var "x"));
    structure  s1=Semantics(Valuation);
    val r1=s1.truth_value(valuationList,f1);
    val r2=s1.truth_value(valuationList,f2);
    val r3=s1.is_tautology(f3);

end
