-module(sup_paxos).

-behaviour(supervisor).

-export([start_link/1,init/1]).

start_link(Options) ->
	supervisor:start_link(?MODULE,{Options}).

init({Options}) ->
	Paxos_fsm = util:sup_worker_desc(paxos_fsm, paxos_fsm,start,[Options]),
	{ok, {{one_for_one, 10, 1}, [Paxos_fsm]}}.

	
