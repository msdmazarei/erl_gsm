%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Oct 2018 11:16 AM
%%%-------------------------------------------------------------------
-module(gsm_modem_server).
-author("msd").

-behaviour(gen_server).

%% API
-export([send_AT_command/2,read_from_modem/1]).
-export([start_link/0]).
-export([start_link/2]).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  network_tcp_socket::gen_tcp:socket()

}).

%%%===================================================================
%%% API
%%%===================================================================
send_AT_command(ModemServer,Data)->
  gen_server:call(ModemServer,{send_and_wait_to_reply,Data}).
read_from_modem(ModemServer)->
  gen_server:call(ModemServer,read_modem).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(IpAddress,Port) ->
  gen_server:start_link({local,?SERVER},?MODULE,{IpAddress,Port},[]).

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
-spec(init({TargetIp::binary(),TargetPort::integer()}) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({TargetIp,TargetPort})->
   case  gen_tcp:connect(TargetIp,TargetPort,[binary,{active, false}]) of
     {ok,Sock}->{ok,#state{network_tcp_socket = Sock}};
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
handle_call({send_and_wait_to_reply, Data},_,State=#state{network_tcp_socket = Sock})->
  ok = gen_tcp:send(Sock,Data), %%this will crashed if some problem happen in sending
  ReadData =  continue_reading(Sock),
  {reply,{modem_reply, ReadData},State};
handle_call(read_modem,_,State=#state{network_tcp_socket = Sock})->
  ReadData = continue_reading(Sock),
  {reply,{modem_reply,ReadData},State}.
%%
%%handle_call(_Request, _From, State) ->
%%  {reply, ok, State}.

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
terminate(_Reason, #state{network_tcp_socket = Sock}) ->
  gen_tcp:close(Sock),
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
