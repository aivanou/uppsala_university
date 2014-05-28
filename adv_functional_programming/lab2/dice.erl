-module (dice).

-export ([graph_new/0,init_vertices/2,init_edges/2,dice/3]).

graph_new() -> digraph:new().


init_vertices(Graph,0) -> Graph;
init_vertices(Graph, N) -> digraph:add_vertex(Graph,N),
						   init_vertices(Graph,N-1).
init_edges(G,[]) -> G;
init_edges(G,[{V_from,V_to}|Rest]) -> digraph:add_edge(G,V_from,V_to),
									  init_edges(G,Rest).

init_graph(N,Edges) -> init_edges(init_vertices(graph_new(),N),Edges).

make_step(_G,[], Out_Vertices) -> Out_Vertices;
make_step(G,[V|Rest],Out_Vertices) -> 	
	make_step(G,Rest,lists:append(digraph:out_neighbours(G,V),Out_Vertices)).

make_move(_,_,[]) -> [];
make_move(_,0,Current_Vertices) -> Current_Vertices;
make_move(G, Dice_Value,Current_Vertices)-> make_move(G,Dice_Value-1,lists:usort(make_step(G,Current_Vertices,[]))).

%% S_Node - start node
%% E_Node - end node
%% List of [Current DICE | Rest Dice]
%% Dicle_List - initial Dice list
%% Depth - depth
%% Current_Vertices - our current set of vertices on step
%% Border_List - list of list of vertices that appear after the last dice 
dice_process(_,_,_,_,_,_,[],_) -> -1;
dice_process(S_Node,E_Node,[],Dice_List,G,Depth,Current_Vertices,Border_Lists) -> 
	case has_list(Border_Lists,Current_Vertices) of
		true -> -1;
		false ->  dice_process(S_Node,E_Node,Dice_List,Dice_List,G,Depth,Current_Vertices,[Current_Vertices|Border_Lists])
	end;

dice_process(S_Node,E_Node,[Dice_Value|Rest], Dice_List,G,Depth,Current_Vertices,Border_Lists) ->
	case lists:member(E_Node,Current_Vertices) of
		true -> Depth;
		false -> dice_process(S_Node,E_Node,Rest,Dice_List,G,Depth+1,make_move(G,Dice_Value,Current_Vertices),Border_Lists)
	end.


dice(N,Edges,Dice_List)->
	dice_process(1,N,Dice_List,Dice_List,init_graph(N,Edges),0,[1],[]).


equal_lists([],[]) -> true;
equal_lists([],[_El|_Rest])->false;
equal_lists([_El|_Rest],[])->false;
equal_lists([El1|Rest1],[El2|Rest2]) when El1 == El2 -> equal_lists(Rest1,Rest2);
equal_lists([_El1|_Rest1],[_El2|_Rest2]) -> false.

has_list([],_) -> false;
has_list([El|Rest], Ch_List) -> 
	case equal_lists(lists:usort(El),lists:usort(Ch_List)) of
		true -> true;
		false -> has_list(Rest,Ch_List)
	end.


