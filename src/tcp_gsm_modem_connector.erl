%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2018 2:08 PM
%%%-------------------------------------------------------------------
-module(tcp_gsm_modem_connector).
-author("msd").
-include_lib("logger.hrl").
-behaviour(gen_server).
-behaviour(gsm_modem_connector).
%% API
-export([start_link/3,start_link/2]).

%%gsm_modem_connector callbacks
-export([send_to_modem/2,receive_from_modem/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  server_ip :: list(),
  server_port :: number(),
  cell_no :: list(),
  name :: list(),
  socket::inet:socket()
}).

%%%===================================================================
%%% API
%%%===================================================================
send_to_modem(Pid,Data)->
  R=gen_server:call(Pid,{send_data,Data}),
  ?MLOG(?LOG_LEVEL_DEBUG,"TCP_MODEM_CONNECTOR:send_to_modem - Sent Data: ~p~n",[Data]),
  R.
receive_from_modem(Pid)->
  {read_data,D } = gen_server:call(Pid,read_data),
  D.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(string(),number(),atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ServerIp,ServerPort) ->
  gen_server:start_link(?MODULE, [ServerIp,ServerPort,"",""], []).

start_link(ServerIp,ServerPort,CellNo) ->
  gen_server:start_link({local, CellNo}, ?MODULE, [ServerIp,ServerPort,CellNo,CellNo], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ServerIp,ServerPort,CellNo,Name]) ->
  case  gen_tcp:connect(ServerIp,ServerPort,[binary,{active, false}]) of
    {ok,Sock}->{ok, #state{
      server_ip = ServerIp,
      server_port = ServerPort,
      cell_no = CellNo,
      name = Name,
      socket = Sock
    }};
    {error,Reason}-> {stop,Reason}
  end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({send_data, Data},_,State=#state{socket = Sock})->
  ok = gen_tcp:send(Sock,Data), %%this will crashed if some problem happen in sending
  {reply,ok,State};
handle_call(read_data,_,State=#state{socket = Sock})->
  ReadData = continue_reading(Sock),
  {reply,{read_data,ReadData},State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
continue_reading(Sock)-> continue_reading(Sock,<<>>).

continue_reading(Sock,R) when is_binary(R)->
  case gen_tcp:recv(Sock,1,10) of
    {ok,D} when is_binary(D)->
      R1= <<R/binary,D/binary>>,
      continue_reading(Sock,R1);
    {error,timeout}-> R
  end.
