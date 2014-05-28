-module(rally).

-export([get_acs/3,get_max_speed/5,rally/3,roadLength/1]).
-compile({no_auto_import,[min/2]}).

min(-1,P) -> P;
min(P,-1) -> P;
min(P1,P2) when P1<P2 -> P1;
min(P1,P2) when P1>= P2-> P2. 

get_acs(0,0,List) -> lists:append(List,[0]);
get_acs(0,Minus,List) -> get_acs(0,Minus-10,lists:append(List,[-1*Minus]));
get_acs(Plus,Minus,List) -> get_acs(Plus-10,Minus,lists:append(List,[Plus])).

get_max_speed(_,_,[],_,MinSpeed) -> MinSpeed;
get_max_speed(Scell,Ecell,[{Tracks,Speed}|Rest],Counter,MinSpeed) ->
		case Counter >=Scell  andalso Counter =< Ecell andalso MinSpeed>Speed  of	
			true -> get_max_speed(Scell,Ecell,Rest,Counter+Tracks,Speed);
			false -> get_max_speed(Scell,Ecell,Rest,Counter+Tracks,MinSpeed)
		end.

roadLength([])-> 0;
roadLength([{Tracks,_}|Rest]) -> Tracks+roadLength(Rest).

rallyMove(CurrCell,_,[],Moves,_,RoadLength,_MaxSpeed,_MinSpeed) when CurrCell >= RoadLength -> Moves;
rallyMove(CurrCell,_,[],_,_,RoadLength,_MaxSpeed,_MinSpeed) when CurrCell < RoadLength -> -1;

rallyMove(CurrCell,_,[_Acs|_Rest],Moves,_,RoadLength,_MaxSpeed,_MinSpeed) when CurrCell >= RoadLength -> Moves;
rallyMove(CurrCell,CurrSpeed,[Acs|Rest],Moves,Road,RoadLength,MaxSpeed,MinSpeed) when (CurrSpeed+Acs) =<0 ->
					 rallyMove(CurrCell,CurrSpeed,Rest,Moves,Road,RoadLength,MaxSpeed,MinSpeed);

rallyMove(CurrCell,CurrSpeed,[Acs|Rest],Moves,Road,RoadLength,MaxSpeed,MinSpeed) ->
		case get_max_speed(CurrCell+1,CurrCell+(CurrSpeed+Acs)/10,Road,0,100000) < (CurrSpeed+Acs) of
			true -> rallyMove(CurrCell,CurrSpeed,Rest,Moves,Road,RoadLength,MaxSpeed,MinSpeed);
			false -> min(rallyMove(CurrCell,CurrSpeed,Rest,Moves,Road,RoadLength,MaxSpeed,MinSpeed),
					     rallyMove(CurrCell+(CurrSpeed+Acs)/10,CurrSpeed+Acs,
					     		   get_acs(MaxSpeed,MinSpeed,[]),Moves+1,Road,RoadLength,MaxSpeed,MinSpeed)
					 )
		end.


rally(MaxSpeed,MinSpeed,Road) -> rallyMove(0,0,get_acs(MaxSpeed,MinSpeed,[]),0,lists:delete({0,0},Road),
											 roadLength(Road)-1,MaxSpeed,MinSpeed).

