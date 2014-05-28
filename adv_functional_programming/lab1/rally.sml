
fun get_acs_values(0,0,vals) = 0::vals
		  | get_acs_values(0,minus,vals) = get_acs_values(0,minus-10,~1 * minus::vals) 
		  | get_acs_values(plus,minus,vals) = get_acs_values(plus-10,minus,plus::vals) 
;

fun rally max_acs min_acs rd =
	let
		val road=List.take(rd,length(rd)-1)

		fun get_speed_limit(cell,counter,[]) = 0
		  | get_speed_limit(cell,counter,(tracks,speed)::lst) = if cell<=tracks+counter then speed
		  															else get_speed_limit(cell,counter+tracks,lst)

		fun get_max_speed_limit(scell,ecell) =
			let
				fun	gmsl(curr_speed,counter,[]) = curr_speed
		  		  | gmsl(curr_speed,counter,(tracks,speed)::lst) =
													if counter>ecell then curr_speed
													else if counter>=scell then 
														if curr_speed>speed then gmsl(speed,counter+tracks,lst)
														else gmsl(curr_speed,counter+tracks,lst)
													else gmsl(curr_speed,counter+tracks,lst)
			in
				gmsl(100400,0,road)
			end

		fun findMaxCell([],mc) = mc
		  |	findMaxCell((tracks,speed)::lst,mc) = findMaxCell(lst,mc+tracks)

		val maxCell=findMaxCell(road,0)

		val pspeedList=get_acs_values(max_acs,min_acs,[])

		fun min(v1,v2) = if v1 = ~1 then v2
						 else if v2= ~1 then v1
						 else if v1>v2 then v2 else v1

		fun genEmptyList(0,lst) = lst
		  | genEmptyList(len,lst) = genEmptyList(len-1,~1::lst)

		val mem=Array.fromList(genEmptyList(10001,[]))

		fun makeMove(curr_cell,curr_speed,[],moves) =  if curr_cell> maxCell then moves
														  else ~1

		  |	makeMove(curr_cell,curr_speed,ps::pspeed_lst,moves) =
		  		if curr_cell> maxCell then moves
  				else if (curr_speed+ps) <= 0 then makeMove(curr_cell,curr_speed,pspeed_lst,moves)
			  	else if Array.sub(mem,curr_cell) <> ~1 then Array.sub(mem,curr_cell)
		 	 	else if get_max_speed_limit(curr_cell+1,curr_cell + ((curr_speed+ps) div 10)) < (curr_speed+ps)
			  		then makeMove(curr_cell,curr_speed,pspeed_lst,moves)
			  	else update_array(curr_cell,curr_speed,ps,pspeed_lst,moves)

		 and update_array(curr_cell,curr_speed,ps,pspeed_lst,moves) =

		 	let
		 		val dummy=Array.update(mem,curr_cell,min(makeMove(curr_cell,curr_speed,pspeed_lst,moves),
					  			makeMove(curr_cell+(curr_speed+ps)div 10,curr_speed+ps,pspeedList,moves+1)))
		 	in
		 		Array.sub(mem,curr_cell)
		 	end
		  	

	in
		makeMove(0,0,pspeedList,0)
		(*get_max_speed_limit(0,1111)*)
	end;

rally 30 10 [(10,100),(5,70),(3,40),(6,100),(0,0)];

rally 40 20 [(1,50),(1,40),(1,30),(1,20),(1,10),(1,20),(1,30),(1,40),(1,50),(0,0)];

rally 40 50 [(15,100),(0,0)];

rally 240 240 [(10,100),(50,70),(30,40),(60,100),(2,10),(6,30),(1,20),(9,50),(8,80),(12,10),(3,90),(0,0)];

rally 200 200 [(10,60),(50,130),(3,40),(15,80),(30,90),(0,0)];

rally 200 10 [(243,160),(448,200),(296,220),(285,200),(305,140),(14,30),(548,200),(238,180),(593,160),(218,180),
				(501,100),(475,60),(105,100),(485,240),(460,30),(107,40),(0,0)];


(*get_acs_values(200,200,[])*)
