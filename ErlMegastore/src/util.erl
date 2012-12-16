-module(util).

-export([sup_worker_desc/3,
         sup_worker_desc/4,
         sup_supervisor_desc/3,
         sup_supervisor_desc/4]).


sup_worker_desc(Name, Module, Function) ->
    sup_worker_desc(Name, Module, Function, []).

sup_worker_desc(Name, Module, Function, Options) ->
    {Name, {Module, Function, Options}, permanent, brutal_kill, worker, []}.

sup_supervisor_desc(Name, Module, Function) ->
    sup_supervisor_desc(Name, Module, Function, []).

sup_supervisor_desc(Name, Module, Function, Args) ->
    {Name, {Module, Function, Args}, permanent, brutal_kill, supervisor, []}.

