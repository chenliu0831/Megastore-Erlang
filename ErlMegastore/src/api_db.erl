-module(api_db).
-author('chenyang@stonybrook.edu').

-export([init_couch/1,read/2,create/2,write/3]).

%%-record(state,{db}).

init_couch([Server,Port,DB]) ->
	CouchServer = couchbeam:server_connection(Server,Port,"",[]),
	{ok,CouchDB} = couchbeam:open_db(CouchServer,DB).


read(DB,ID) ->
	{ok, Doc} = couchbeam:open_doc(DB,ID).
create(DB,Doc) ->
	{ok,Doc1} = couchbeam:save_doc(DB,Doc),
	{NewDoc}  = couchbeam:set_value(<<"id">>, couchbeam_doc:get_id(Doc1), Doc1).

write(DB,ID,NewDoc) ->
	{ok, Doc} = couchbeam:open_doc(DB,ID),
	NewDoc2 = couchbeam:set_value(<<"_id">>,ID,{NewDoc}),
	NewDoc3 = couchbeam:set_value(<<"_rev">>,
				     couchbeam_doc:get_rev(Doc),NewDoc2),
	{ok,Doc1} = couchbeam:save_doc(DB,NewDoc3).

