signature Matrix = 
sig
	type  T
	val makeMatrix: int*int -> T
	val update: int vector vector*int*int*int -> T
end


(*datatype matrix = cell of int*int;*)

datatype matrix = matr of {cells:int vector vector,mRows:int,mCols:int};

structure Matrix_Impl = 
struct
	
	type T = matrix

	fun makeMatrix(rows,cols)  = 
		let
			fun insRow(0) = []
			|   insRow(size) = 0::insRow(size-1)

			fun mkMatrix(0)=[]
			|	mkMatrix(rNumber) = Vector.fromList(insRow(cols))::mkMatrix(rNumber-1)
		in
			matr{cells=Vector.fromList(mkMatrix(rows)),mRows=rows,mCols=cols}
		end

	fun update(matr{cells,mRows,mCols},value,row,col) =
			matr{cells=Vector.update(cells,row,Vector.update(Vector.sub(cells,row),col,value)),mRows=mRows,mCols=mCols}

	fun get(matr{cells,mRows,mCols},row,col) = Vector.sub(Vector.sub(cells,row),col)

	fun getMaxRow(matr{cells,mRows,mCols}) = mRows

	fun getMaxCol(matr{cells,mRows,mCols}) = mCols

end;

(*datatype player = Black | White
datatype move = Skip | Move of int (* 0 ... 63 
*)*)

datatype aidata = data of{board:matrix,pl:player,pl_number:int,en_number:int};

structure Reversi_AI = 
struct
	
	type T = aidata

	val author = "alex"


	fun str_ppp(v) = 
		let
			val n= Int.toString(v)
			val n1=print n
			val n2=print "\n"
		in
			v
		end;

	val nickname="tierex"

	fun init player =
		let
			val initMatrix=Matrix_Impl.makeMatrix(8,8)

			fun makeInitState(matrix,[]) = matrix
			|   makeInitState(matrix ,value::row::col::lst)= makeInitState( Matrix_Impl.update(matrix,value,row,col),lst)
			
			fun init_pl_number(Black)= 1
			|   init_pl_number(White)= 2

			fun init_en_number(Black) = 2
			|   init_en_number(White)= 1


		in
			data{board=makeInitState(initMatrix,[1,4,3,1,3,4,2,3,3,2,4,4]),
			pl=player,pl_number=init_pl_number(player),en_number=init_en_number(player)}
		end


	fun changeBoard(matrix,row,col,to) = 
				let
					fun updateBoard(matrix,[],row,col) = matrix
					  | updateBoard(matrix,frow::fcol::lst,row,col)=
						let
							fun isAllowable(frow,fcol,rowit,~1) = false
							|	isAllowable(frow,fcol,~1,colit) = false
							|   isAllowable(frow,fcol,rowit,colit)= if rowit>=Matrix_Impl.getMaxRow(matrix) then false
									else if colit>=Matrix_Impl.getMaxCol(matrix) then false
									else if Matrix_Impl.get(matrix,rowit,colit) = to then true
									else if Matrix_Impl.get(matrix,rowit,colit) = 0 then false
									else if frow(rowit)>=Matrix_Impl.getMaxRow(matrix) orelse fcol(colit)>=Matrix_Impl.getMaxCol(matrix) then false
									else isAllowable(frow,fcol,frow(rowit),fcol(colit))

							fun changeSide(matrix,row,col) = 
								if Matrix_Impl.get(matrix,row,col) = to then matrix
								else changeSide(Matrix_Impl.update(matrix,to,row,col),frow(row),fcol(col))
						
						in
							if isAllowable(frow,fcol,frow(row),fcol(col)) then
								 updateBoard(changeSide(Matrix_Impl.update(matrix,0,row,col),row,col),lst,row,col)
							else updateBoard(matrix,lst,row,col)

						end	

				in
					updateBoard(matrix,[fn x=>x,fn x=>x+1,fn x=>x,fn x=>x-1,fn x=>x+1,fn x=>x,
										fn x=>x-1,fn x=>x,fn x=>x+1,fn x=>x+1,fn x=>x-1,fn x=>x+1,
										fn x=>x+1,fn x=>x-1,fn x=>x-1,fn x=>x-1],row,col)
				end	

	fun moveToCol(value) = value mod 8

	fun moveToRow(value) = value div 8
		
	fun greedyStrategy(data{board,pl,pl_number,en_number}) = 
					let
					
						fun gatherBt(opRow,opCol,row,~1,counter) = 0
						  | gatherBt(opRow,opCol,~1,col,counter) = 0
						  | gatherBt(opRow,opCol,row,col,counter) = 
						  		if row >= Matrix_Impl.getMaxRow(board) orelse col >= Matrix_Impl.getMaxCol(board) then 0
						  		else if Matrix_Impl.get(board,row,col) = en_number then gatherBt(opRow,opCol,opRow(row),opCol(col),counter+1)
						  		else if Matrix_Impl.get(board,row,col) = pl_number then counter
						  		else 0

		     			fun findBtCount(opRow,opCol,0,0) =
							if Matrix_Impl.get(board,0,0) <> 0 then 0
							else if opCol(0) = ~1 orelse(opRow(0)) = ~1 then 0
							else if Matrix_Impl.get(board,opRow(0),opCol(0)) <> en_number then 0
							else gatherBt(opRow,opCol,opRow(0),opCol(0),0)

		     			|   findBtCount(opRow,opCol,row,0) =
							if row>=Matrix_Impl.getMaxRow(board) orelse opRow(row)>=Matrix_Impl.getMaxRow(board) then 0
							else if Matrix_Impl.get(board,row,0) <> 0 then 0
							else if opCol(0) = ~1 then 0
							else if Matrix_Impl.get(board,opRow(row),opCol(0)) <> en_number then 0
							else gatherBt(opRow,opCol,opRow(row),opCol(0),0)

						|   findBtCount(opRow,opCol,0,col) =
						    if col>=Matrix_Impl.getMaxCol(board) orelse opCol(col)>=Matrix_Impl.getMaxCol(board) then 0
							else if Matrix_Impl.get(board,0,col) <> 0 then 0
							else if opRow(0) = ~1 then 0
							else if Matrix_Impl.get(board,opRow(0),opCol(col)) <> en_number then 0
							else gatherBt(opRow,opCol,opRow(0),opCol(col),0)
				
						|   findBtCount(opRow,opCol,row,col) =
						  	if row >= Matrix_Impl.getMaxRow(board) orelse col >= Matrix_Impl.getMaxCol(board) then 0
							else if opRow(row) >=Matrix_Impl.getMaxRow(board) then 0
							else if opCol(col) >=Matrix_Impl.getMaxCol(board) then 0
							else if Matrix_Impl.get(board,row,col) <> 0 then 0
							else if Matrix_Impl.get(board,opRow(row),opCol(col)) <> en_number then 0
							else gatherBt(opRow,opCol,opRow(row),opCol(col),0)
			
						fun findBtCountAll(row,col) = 
							findBtCount(fn x=>x,fn x=>x+1,row,col) +
							findBtCount(fn x=>x,fn x=>x-1,row,col) +
							findBtCount(fn x=>x+1,fn x=>x,row,col) + 
							findBtCount(fn x=>x-1,fn x=>x,row,col) + 
							findBtCount(fn x=>x+1,fn x=>x+1,row,col) + 
							findBtCount(fn x=>x-1,fn x=>x-1,row,col) + 
							findBtCount(fn x=>x+1,fn x=>x-1,row,col) + 
							findBtCount(fn x=>x-1,fn x=>x+1,row,col) 
			
						fun iterateMatrix(row,col,lst) = if row=Matrix_Impl.getMaxRow(board) then lst
														 else if col=Matrix_Impl.getMaxCol(board) then iterateMatrix(row+1,0,lst)
														 else if Matrix_Impl.get(board,row,col) <> 0 then iterateMatrix(row,col+1,lst)
														 else iterateMatrix(row,col+1,findBtCountAll(row,col)::row::col::lst)
					
						fun findMaxColRow([],row,col,maxValue) = (row,col)
						  | findMaxColRow(value::row::col::lst,mrow,mcol,maxValue) =
									   if value>maxValue then findMaxColRow(lst,row,col,value)
									   else findMaxColRow(lst,mrow,mcol,maxValue)
					
						fun genReturn(pair) = (Move(str_ppp((#1 pair)*8+(#2 pair))),
												 data{board=changeBoard(board,(#1 pair),(#2 pair),pl_number),
												  	   pl=pl,pl_number=pl_number,en_number=en_number})

					in
						genReturn(findMaxColRow(iterateMatrix(0,0,[]),0,0,~43))
						(*iterateMatrix(0,0,[])*)
						(*findBtCountAll(4,5)*)
					end


	fun think(data{board,pl,pl_number,en_number},Move m,time) = 
			greedyStrategy(data{board=changeBoard(board,moveToRow(m),moveToCol(m),en_number),
								pl=pl,pl_number=pl_number,en_number=en_number}
						  )

	  | think(data{board,pl,pl_number,en_number},Skip,time) = 
			greedyStrategy(data{board=board,pl=pl,pl_number=pl_number,en_number=en_number})

			(*greedyStrategy(changeBoard(aidata,moveToRow(m),moveToCol(m)))*)

end;


val pl=White;
val ai=Reversi_AI.init pl;

open Matrix_Impl;
open Reversi_AI;
val m=Move(44);

val a1=init Black;
val a2=init White;
val mb=Skip;
val mw=Skip;

val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
(*val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);
val (mb,a1)=think(a1,mw,30);
val (mw,a2)=think(a2,mb,30);*)

(*val ai=updateBoard(ai,1,5,0);
val ai=updateBoard(ai,1,5,1);
val ai=updateBoard(ai,1,5,2);
val ai=updateBoard(ai,1,5,3);
val ai=updateBoard(ai,1,5,4);
val ai=updateBoard(ai,1,5,5);
val ai=updateBoard(ai,1,5,6);
val ai=updateBoard(ai,1,4,3);
val ai=updateBoard(ai,1,4,4);
val ai=updateBoard(ai,1,4,5);
val ai=updateBoard(ai,1,3,4);
val ai=updateBoard(ai,1,3,5);
val ai=updateBoard(ai,1,2,5);
val ai=updateBoard(ai,1,7,6);
val ai=updateBoard(ai,2,6,6);
val ai=updateBoard(ai,2,7,7);
*)

(*val ai=updateBoard(ai,2,4,2);*)


(*val move=Move(30);*)
(*Reversi_AI.think(ai,move,30)*)


				(*fun findAllowableMovements(data{board,pl,pl_number,en_number}) =


						fun getDirectionFunctions(srow,scol,erow,ecol) =
					if srow<>erow andalso scol<>ecol then
						if srow>erow andalso scol>ecol then
							(fn x=>x-1,fn x=>x-1)
						else if srow>erow andalso scol<ecol then 
							(fn x=>x-1,fn x=>x+1)
						else if srow<erow andalso scol<ecol then
							(fn x=> x+1,fn x=>x+1)
						else
							(fn x=>x+1,fn x=>x-1)
					else if srow=erow then
						if scol>ecol then 
							(fn x=>x,fn x=>x-1)
						else if scol<ecol then (fn x=>x,fn x=>x+1)
						else (fn x=>x,fn x=>x)
					else if srow>erow then
							(fn x=>x-1,fn x=>x)
					else if srow<erow then (fn x=>x+1,fn x=>x)
					else (fn x=>x,fn x=>x)
					let

						fun checkAllowance(matrix,pl_number,en_number,opRow,opCol,row,col) = 
							let
								fun checkLstCell(~1,~1) = false
								  | checkLstCell(srow,~1)=false
								  | checkLstCell(~1,scol)=false
								  | checkLstCell(0,scol) =false
								  | checkLstCell(srow,0) = false
								  | checkLstCell(srow,scol) = if  row>= Matrix_Impl.getMaxRow(matrix) then false
								  	  							else if col >=Matrix_Impl.getMaxCol(matrix) then false
								  	  							else if Matrix_Impl.get(matrix,srow,scol) = pl_number then true
																else if Matrix_Impl.get(matrix,srow,scol)=0 then false
																else checkLstCell(opRow(srow),opCol(scol))
							in
								if Matrix_Impl.get(matrix,opRow(row),opCol(col)) = en_number then checkLstCell(opRow(row),opCol(col))
								else false
							end


						fun isAllowableMovement(matrix,pl_number,en_number,row,col) = 
							if checkAllowance(matrix,pl_number,en_number,fn x=>x,fn x=>x+1,row,col) orelse
							   checkAllowance(matrix,pl_number,en_number,fn x=>x,fn x=>x-1,row,col) orelse
							   checkAllowance(matrix,pl_number,en_number,fn x=>x+1,fn x=>x,row,col) orelse
							   checkAllowance(matrix,pl_number,en_number,fn x=>x-1,fn x=>x,row,col) orelse
							   checkAllowance(matrix,pl_number,en_number,fn x=>x+1,fn x=>x+1,row,col) orelse
							   checkAllowance(matrix,pl_number,en_number,fn x=>x-1,fn x=>x+1,row,col) orelse
							   checkAllowance(matrix,pl_number,en_number,fn x=>x+1,fn x=>x-1,row,col) orelse
							   checkAllowance(matrix,pl_number,en_number,fn x=>x-1,fn x=>x-1,row,col) then true
							else false

						fun iterate(row,col,lst) = if(row>=Matrix_Impl.getMaxRow(board)) then lst
													else if col>=Matrix_Impl.getMaxCol(board)then iterate(row+1,0,lst)
													else if isAllowableMovement(board,pl_number,en_number,row,col) then iterate(row,col+1,row::col::lst)
													else iterate(row,col+1,lst)
					in
						iterate(0,0,[])
					end*)

