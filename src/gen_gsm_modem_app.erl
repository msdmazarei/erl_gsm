%%%-------------------------------------------------------------------
%% @doc gen_gsm_modem public API
%% @end
%%%-------------------------------------------------------------------

-module(gen_gsm_modem_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([connect_to_gsm/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    gen_gsm_modem_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
continue_reading(Sock,R) when is_binary(R)->
  case gen_tcp:recv(Sock,1,10) of
    {ok,D} when is_binary(D)->
      R1= <<R/binary,D/binary>>,
    continue_reading(Sock,R1);
    {error,timeout}-> R
  end.

connect_to_gsm()->
  {ok,Sock}  = gen_tcp:connect("127.0.0.1",3285,[binary,{active, false}]),
  gen_tcp:send(Sock,<<"AT",13>>),
  R=continue_reading(Sock,<<>>),
  gen_tcp:close(Sock),
  R.
