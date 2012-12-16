-module(api_tx).

-export([new_wal/0,req_list/1]). 
-type request_on_key() ::
          {read, string()}
        | {write, string(), any()}.


-type request() :: request_on_key() | {commit}.
-type read_result() :: {ok, any()} | {fail, timeout | not_found}.
-type write_result() :: {ok} | {fail, timeout}.
-type commit_result() :: {ok} | {fail, abort | timeout}.

-type result() ::read_result() | write_result() | commit_result().
new_wal() -> tx_walog:empty_tx_log().

-spec req_list([request()]) -> {tx_walog:wal(), [result()]}.
req_list(ReqList) ->
	tx_walong:empty_tx_log().
	%%req_list(new_wal(), ReqList).



