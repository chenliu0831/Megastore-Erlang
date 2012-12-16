-module(mega_node).
-author('chenyang-liu').

-export([tx_read/2,start_link/3,stop/0]).

-record(coord_state, {is_valid,server,db}).

start_link(NodeId, Others,CoordValid) ->
    application:start(sasl),          
    application:start(ibrowse),       
    application:start(crypto),
    application:start(public_key),   
    application:start(ssl),
    application:start(couchbeam),  
    io:format( "starting ~p agent...", [?MODULE] ),
    Pongs=lists:filter(fun(X)->
                               case X of pong -> true; _-> false end
                       end ,
                       lists:map(
                         fun(Other)-> net_adm:ping(Other) end,
                         Others )),
    io:format( "~p nodes ponged.~n", [length( Pongs )] ),
    CouchServer = couchbeam:server_connection("localhost",5984,"",[]),	    
    State = #coord_state{server=CouchServer},
    Pid = spawn_link( fun() -> coordinator(NodeId,Others,
				State#coord_state{is_valid=CoordValid}) end),
    
    ok=io:format( "starting coordinator: ~p (~p).~n", [get_coordinator(), Pid] ),
    true=register( get_coordinator(), Pid ),
    [Pid].


get_coordinator()-> 
    list_to_atom( "coordinator").

tx_read(DB,DocId) ->
	CoordProc = get_coordinator(),
	CoordProc ! {self(), tx_read, {DB,DocId}},
	receive
		{_From, result, {Doc}} ->
			Doc
	end.

stop() -> 
	CoordProc = get_coordinator(),
	CoordProc ! {self(), stop, normal}.

	
%%Coordinator is just a lightweight service running on top of each node

coordinator(NodeId,OtherPs,State) ->
	receive
	{From, tx_read, {DB,DocId} } ->
 	   {ok,CouchDB} = couchbeam:open_db(State#coord_state.server,DB),	
	    case State#coord_state.is_valid of
		1 -> %%coord is valid, just local read :) 
		io:format("Doing local read!~n"),
		{ok, Doc} = api_db:read(CouchDB,DocId),
		io:format("Finishing Local read!~n"),
		From ! {self(), result, {Doc}};		

	        0 -> %% Doing current read + catchup
		io:format("Coordinator is invalid, doing catchup!~n")	
	    end;
	{_From, stop, normal} ->
		exit(stop)
	end,
	coordinator(NodeId,OtherPs,State).
