-module(mega_node).
-author('chenyang-liu').

-export([tx_read/2,start_link/3,stop/0]).

-record(coord_state, {is_valid,server,db, wal_pos}).

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
		io:format("Coordinator is invalid, starting active paxos for catching up!~n"),
	       	WAL_pos=5, 	
		lists:map( fun(Node)->
                                       io:format("Query Maxium log position message to: ~p ! ~p~n",
                                                 [{get_coordinator(), Node},
                                                  {self(), maxLogPosQuery, {DocId, WAL_pos}}]),
                                       {get_coordinator(), Node} ! {self(), maxLogPosQuery, {DocId,WAL_pos} }
                               end,
                               OtherPs),
               paxos_fsm:start( DocId, NodeId, WAL_pos, OtherPs, [self(), From])		
		%%get max log position	
	    end;
	{_From, maxLogPosQuery, {DocId,WAL_pos}} ->
	    io:format("starting passive paxos: ~p~n", [{_From, subject,{DocId,WAL_pos}}]),
	    paxos_fsm:start(DocId, NodeId, WAL_pos, OtherPs,[self()]);
	{_From, result, {DocId, WAL_pos}} ->
		State#coord_state{wal_pos = WAL_pos};
	{_From, stop, normal} ->
		exit(stop)
	end,
	coordinator(NodeId,OtherPs,State).
