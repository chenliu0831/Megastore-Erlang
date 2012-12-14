-module(tx_walog).
-author('chenyangliu').


%%operations on write-ahead log(wal)
-export([empty_tx_log/0]).
-export([add_entry/2]).
-export([update_entry/2]).
-export([find_entry_by_key/2]).
%%operations on wal entry
-export([new_entry/5]).
-export([get_entry_op/1, set_entry_op/2]).
-export([get_entry_key/1,set_entry_key/2]).
-export([get_entry_version/1,set_entry_version/2]).
-export([get_entry_state/1, set_entry_state/2]).
-export([get_entry_val/1,set_entry_val/2]).

-export_type([wal/0,wal_entry/0]).
-export_type([tx_state/0]).
-export_type([tx_op/0]).
%% struct for tx log

-type tx_state() :: val | {fail, abort | not_found}.
-type tx_op()    :: tx_read | tx_write.

-type wal_key() :: string().

%% struct for wal entry, op, key,version,state,value

-type wal_entry() ::
	{
		tx_op(),	
		wal_key(),
		integer(),
		tx_state(),
		any()
	}.
%%list of log for a node
-type wal() :: [wal_entry()].

-spec empty_tx_log() -> wal().
empty_tx_log() -> [].

-spec add_entry(wal(), wal_entry()) -> wal().
add_entry(Tx_log, Entry) -> [ Entry | Tx_log ].

%%update entry
-spec update_entry(wal(),wal_entry()) -> wal().
update_entry(Wal,Entry) -> 
   lists:keyreplace(get_entry_key(Entry),2,Wal,Entry).

%%find entry by key
-spec find_entry_by_key(wal(),wal_key()) -> wal_entry() | false.
find_entry_by_key(Wal,Key) -> 
	lists:keyfind(Key,2,Wal).


%% Get and set member in wal entry
-spec new_entry(tx_op(), string(), integer(), tx_state,any()) -> 
	wal_entry().
new_entry(Op,Key, Ver, State, Val)->
	{Op,Key,Ver,State,Val}.

get_entry_op(Entry) -> element(1,Entry).
set_entry_op(Entry,Op) -> setelement(1,Entry,Op).
get_entry_key(Entry) -> element(2,Entry).
set_entry_key(Entry,Key) -> setelement(2,Entry,Key).

get_entry_version(Entry) -> element(3,Entry).
set_entry_version(Entry,Ver) -> setelement(3,Entry,Ver).
get_entry_state(Entry) -> element(4,Entry).
set_entry_state(Entry,State) -> setelement(4,Entry,State).

get_entry_val(Entry) -> element(5,Entry).
set_entry_val(Entry,Val) -> setelement(5,Entry,Val).












