%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2018 2:04 PM
%%%-------------------------------------------------------------------
-module(simple_gsm_modem_over_tcp).
-author("msd").

-behaviour(gen_server).
-export([get_antenna_status/1]).
%% API
-export([start_link/2, send_at_command/1,send_sms/3]).
-define(MODEM_COMMAND_TIMEOUT,5000).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  modem_connector_pid :: atom(),
  gsm_state_machine_pid :: atom(),
  modem_ip_address :: string(),
  modem_port_no :: integer(),
  last_send_data_epoch :: integer(),
  time_ref_interval :: term(),
  pdu_mode::boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================
get_antenna_status(Pid)->
  gen_server:call(Pid,get_antenna_status,?MODEM_COMMAND_TIMEOUT).
send_sms(Pid,TargetNo,UTF16Bin)->
  gen_server:call(Pid,{send_sms,TargetNo,UTF16Bin},20000).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(string(),integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(IPAdderess, Port) ->
  {ok, ModemPID} = tcp_gsm_modem_connector:start_link(IPAdderess, Port),
  {ok, GSMStateMachine} = gsm_state_machine:start_link(tcp_gsm_modem_connector, ModemPID),
  gen_server:start_link(?MODULE, [GSMStateMachine, ModemPID, IPAdderess, Port], []).

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
init([GSMStateMachine, ModemPID, IPAdderess, Port]) ->
  {ok,TRef} = timer:apply_interval(30000, ?MODULE, send_at_command, GSMStateMachine),
  R = {ok, #state{
    modem_connector_pid = ModemPID,
    modem_ip_address = IPAdderess,
    modem_port_no = Port,
    gsm_state_machine_pid = GSMStateMachine,
    time_ref_interval = TRef,
    pdu_mode = false
  }},

  R.

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
handle_call({send_sms,TargetNo,UTF16Bin},_,State= #state{gsm_state_machine_pid = S,pdu_mode = PDUMode}) ->
  case PDUMode of
    false ->
      gen_statem:call(S,{send,'CMGF',0},?MODEM_COMMAND_TIMEOUT);
    _ ->
      ok
  end,
  R = case byte_size(UTF16Bin) of
    N when N<134 ->
      PDU = gsm_pdu:simple_pdu(international,TargetNo,a16bit,UTF16Bin),
      gen_statem:call(S,{send,'CMGS',PDU});
    _->
      MsgIdentifier= rand:uniform(250),
      PDUS = gsm_pdu:multipart_pdu(international,TargetNo,a16bit,UTF16Bin,MsgIdentifier),
      lists:map(fun(X) ->
                  gen_statem:call(S,{send,'CMGS',X})
                    end, PDUS)
  end,
  NewState = State#state{last_send_data_epoch = get_timestamp(),pdu_mode = true},
  {reply,R,NewState};
handle_call(get_antenna_status,_,State= #state{gsm_state_machine_pid = S}) ->
  R=gen_statem:call(S,{send,'AT'},?MODEM_COMMAND_TIMEOUT),
  NewState = State#state{
    last_send_data_epoch = get_timestamp()
  },
  {reply,R,NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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
terminate(_Reason, _State=#state{time_ref_interval = TRef}) ->
  timer:cancel(TRef),
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
-spec get_timestamp() -> integer().
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).


send_at_command(GSMStateMachine) ->
  gen_statem:call(GSMStateMachine, {send, 'AT'}, 5000).