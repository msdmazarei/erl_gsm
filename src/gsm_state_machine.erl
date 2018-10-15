%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2018 11:09 AM
%%%-------------------------------------------------------------------
-module(gsm_state_machine).
-author("msd").

-behaviour(gen_statem).


-include_lib("gsm_pdu.hrl").
-include_lib("logger.hrl").
%% API
-export([start_link/3]).
-export([read_from_modem_device/3]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

-define(SERVER, ?MODULE).
-type send_status() :: sending | wait_to_modem_response | ready.
-type receive_status() :: receiving | wait_to_modem | ready.

-define(SEND_STATUS_SENDING,sending).
-define(SEND_STATUS_WAIT,wait_to_modem_response).
-define(SEND_STATUS_READY,ready).


-record(cmti_event,{
  storage::list(),
  index::integer()
}).
-type cmti_event()::#cmti_event{}.

-record(state, {
  send_status :: send_status(),
  receive_status :: receive_status(),
  received_chars :: list(),
  inbox_messages :: [tpdu()],
  gsm_modem_connector_module_name :: string(),
  gsm_modem_connector_identifier::term(),
  last_sent_command::at_command(),
  respond_back_to :: term(),
  pdu_enable::boolean(),
  pdu_to_send::pdu(),

  cmti_events :: [cmti_event()],

  command_result :: any()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name,GsmModemConnectorModuleName,GsmModemConnectorIdentifier) ->
  R = gen_statem:start_link({local, Name}, ?MODULE, [GsmModemConnectorModuleName,GsmModemConnectorIdentifier], []),
  {ok,_} = timer:apply_interval(100,?MODULE,read_from_modem_device,[Name,GsmModemConnectorModuleName,GsmModemConnectorIdentifier]),
  R.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
-type at_command()::'AT'.

init([GsmModemConnectorModuleName,GsmModemConnectorIdentifier]) ->
  %%simple interval to read from device
  {ok, state_name, #state{
    gsm_modem_connector_identifier = GsmModemConnectorIdentifier,
    gsm_modem_connector_module_name = GsmModemConnectorModuleName,
    send_status = ready,
    receive_status = ready,
    inbox_messages = [],
    received_chars = <<>>,
    pdu_enable = false,
    cmti_events = []

  }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
  handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------


state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

until_next_happen(Pattern,Binary,Scanned)->
  L = byte_size(Pattern),
  <<I:L/binary,Rest/binary>> = Binary,
  case I of
    Pattern -> {Scanned,Rest};
    _ -> <<S:1/binary,R/binary>>=Binary,
      until_next_happen(Pattern,R,<<Scanned/binary,S/binary>>)
  end.
-spec until_next_happen(binary(),binary())-> {binary(),binary()}.
until_next_happen(Pattern,Binary)->
  L = byte_size(Pattern),
  <<I:L/binary,Rest/binary>> = Binary,
  case I of
    Pattern -> {<<>>,Rest};
    _ -> <<S:1/binary,R/binary>>=Binary,
      until_next_happen(Pattern,R,S)
  end.


process_parts(<<>>,State)-> {State,[]};

process_parts(<<"\r\n+CMTI: ",Remain/binary>>, State = #state{ cmti_events = EVENTS} )->
  ?MLOG(?LOG_LEVEL_DEBUG,"CMTI: CALLED WITH REMAIN: ~p~n",[Remain]),
  <<"\"",STORAGE:2/binary,"\",",MessageIndexStr/binary>> =Remain,
  {MessageIndex,<<"\r\n",Remain1/binary>>} = string:to_integer(MessageIndexStr),
  ?MLOG(?LOG_LEVEL_DEBUG,"CMTI: MegIndex:~p ~n",[MessageIndex]),
  NewState = State#state{
    cmti_events = [#cmti_event{ index = MessageIndex, storage = STORAGE} | EVENTS ]
  },

  process_parts(Remain1,NewState);

process_parts(<<"\r\n+CMGS: ",Rest/binary>>,State)->
  ?MLOG(?LOG_LEVEL_DEBUG,"CMGS PROCESSOR CALLED~n"),
  {MsgNo,<<"\r\n",R/binary>>} = string:to_integer(Rest),
  ?MLOG(?LOG_LEVEL_DEBUG,"CMGS: MSG NO IS ~p~n",[MsgNo]),
  NewState = State#state{
    command_result  = MsgNo
  },
  ?MLOG(?LOG_LEVEL_DEBUG,"UPDATE LAST SENT MESSAGE ID IN STATE~n"),
  process_parts(R,NewState);
process_parts(<<"\r\n> ",Remain/binary>>,State=#state{last_sent_command = 'CMGS',pdu_to_send = PDU,gsm_modem_connector_module_name = M,gsm_modem_connector_identifier = I})->
  io:fwrite("CMGS PART > CALLED.~n"),
  SPDU = hex:bin_to_hex( gsm_pdu_serializers:pdu(PDU)),
  COMMAND = io_lib:format("~s",[SPDU]),
  ok=apply(M,send_to_modem,[I,<< (list_to_binary(COMMAND))/binary,26>>] ),
  process_parts(Remain,State);
process_parts(<<"\r\nERROR\r\n",Remain/binary>>,State=#state{respond_back_to = RES_BACK})->
  ?MLOG(?LOG_LEVEL_DEBUG,"ERROR PART CALLED. RESPOND BACK TO ~p ~n",[RES_BACK]),
  NewState = State#state{send_status = ready , received_chars = Remain},
  {NState,Acts} = process_parts(Remain,NewState),
  {NState,[{reply,RES_BACK,error}|Acts]};

%%process_parts(<<"\r\nOK\r\n",Remain/binary>>,State=#state{respond_back_to = RES_BACK, last_sent_command = 'CSQ', csq_cme = CSQCME})->
%%  ?MLOG(?LOG_LEVEL_DEBUG,"OK PART CALLED FOR CSQ COMMAND. RESPOND BACK TO ~p ~n",[RES_BACK]),
%%  NewState = State#state{send_status = ready,received_chars = Remain},
%%  {NState,Acts} = process_parts(Remain,NewState),
%%  {NState,[{reply,RES_BACK,{ok,CSQCME}}|Acts]};

process_parts(<<"\r\nOK\r\n",Remain/binary>>,State=#state{respond_back_to = RES_BACK, last_sent_command = LAST_COMMAND, command_result =  CMD_RESULT})->
  ?MLOG(?LOG_LEVEL_DEBUG,"OK PART CALLED FOR ~p COMMAND. RESPOND BACK TO ~p ~n",[LAST_COMMAND,RES_BACK]),
  NewState = State#state{send_status = ready,received_chars = Remain},
  {NState,Acts} = process_parts(Remain,NewState),
  case CMD_RESULT of
    undefined ->
      {NState,[{reply,RES_BACK,{ok,ok}}|Acts]};
    _->
      {NState,[{reply,RES_BACK,{ok,CMD_RESULT}}|Acts]}
  end.
%%
%%
%%process_parts(<<"\r\nOK\r\n",Remain/binary>>,State=#state{respond_back_to = RES_BACK})->
%%  ?MLOG(?LOG_LEVEL_DEBUG,"OK PART CALLED FOR RESPOND BACK TO ~p ~n",[RES_BACK]),
%%  NewState = State#state{send_status = ready,received_chars = Remain},
%%  {NState,Acts} = process_parts(Remain,NewState),
%%  {NState,[{reply,RES_BACK,ok}|Acts]}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------

handle_event(
    {call,From},{send,'CMGS',PDU=#pdu{tpdu = TPDU}},StateName,
    State=#state{gsm_modem_connector_module_name = M,gsm_modem_connector_identifier = I,send_status = ?SEND_STATUS_READY,pdu_enable = true})->
  ?MLOG(?LOG_LEVEL_DEBUG,"CMGS: CALLED FROM ~p~n",[From]),
  Len=gsm_pdu:tpdu_length(TPDU),
  COMMAND = io_lib:format("AT+CMGS=~p\r",[Len]),
  ?MLOG(?LOG_LEVEL_DEBUG,"SENDING CMD:~p~n",[COMMAND]),
  ok=apply(M,send_to_modem,[I,<<(list_to_binary(COMMAND))/binary>>]),
  ?MLOG(?LOG_LEVEL_DEBUG,"SENT CMD:~p~n",[COMMAND]),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT,last_sent_command ='CMGS', respond_back_to = From , pdu_to_send = PDU, command_result = undefined},
  {next_state,StateName,NewState};

handle_event(
    {call,From},{send,'CMGF',Value},StateName,
    State=#state{gsm_modem_connector_module_name = M,gsm_modem_connector_identifier = I,send_status = ?SEND_STATUS_READY})->
  CMD =  list_to_binary( io_lib:format( "AT+CMGF=~p",[Value])),
  ok=apply(M,send_to_modem,[I,<<CMD/binary,"\r">>]),

  NewState =case Value of
              0 -> State#state{send_status = ?SEND_STATUS_WAIT,last_sent_command ='CMGF', respond_back_to = From,pdu_enable = true, command_result = undefined};
              _ -> State#state{send_status = ?SEND_STATUS_WAIT,last_sent_command ='CMGF', respond_back_to = From,pdu_enable = false, command_result = undefined}
            end,
  {next_state,StateName,NewState};

handle_event({call,From},{send,'AT'},StateName,State=#state{gsm_modem_connector_module_name = GSMModule,gsm_modem_connector_identifier = GSMIdenifier,send_status  = ?SEND_STATUS_READY})->
  ?MLOG(?LOG_LEVEL_DEBUG,"AT: CALLED FROM ~p~n",[From]),
  ?MLOG(?LOG_LEVEL_DEBUG,"AT: SENDING AT COMMAND"),
  ok = apply(GSMModule,send_to_modem,[GSMIdenifier,<<"AT\r">>]),
  ?MLOG(?LOG_LEVEL_DEBUG,"AT: AT COMMAND SENT"),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT,last_sent_command ='AT', respond_back_to = From, command_result = undefined},
  {next_state,StateName,NewState,[]};

handle_event({call,From},{send,_},StateName,State)->
  {next_state,StateName,State,[{reply,From,busy}]};

handle_event({call,From},{recieved_from_modem, Data},StateName,State=#state{received_chars = CHARS}) ->
  ?MLOG(?LOG_LEVEL_DEBUG,"recieved_from_modem: called with data:~p~n",[Data]),
  NewState = State#state{
    received_chars = <<CHARS/binary,Data/binary>>
  },
  RCBS = byte_size(NewState#state.received_chars)-2,
  << _:RCBS/binary,LASTPART:2/binary>> = NewState#state.received_chars,
  ?MLOG(?LOG_LEVEL_DEBUG,"recieved_from_modem: LASTPARTS:~p~n",[LASTPART]),
  {NewNewState,Actions}  =
    case LASTPART of
    <<"\r\n">> ->
      process_parts(NewState#state.received_chars,NewState);
      <<"> ">> ->
        process_parts(NewState#state.received_chars,NewState);
    _->
      ?MLOG(?LOG_LEVEL_DEBUG,"recieved_from_modem: Nothing to processs, wait to more chars to come"),
      {NewState,[]}
  end ,
  Actions1=[{reply,From,ok}|Actions],
  %%I DONT KNOW WHY WE NEED THIS BUT MODEMS ARE NOT SO FAST.
  %% use send_after
  timer:sleep(100),
%%  Actions1=Actions,
  R=
  {next_state,StateName,NewNewState,Actions1},
  R;

handle_event(E= {call,From} ,EC,SN,S)->
  io:fwrite(
    "E:~p , EC:~p, SN:~p,S:~p ~n",[E,EC,SN,S]
  ),
  {next_state,E,S,[{reply,From,ok}]};
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
read_from_modem_device(Name,Module,Identifier)->
  case apply(Module,receive_from_modem,[Identifier]) of
    <<>> ->
      ok;
    V when is_binary(V) ->
      ?MLOG(?LOG_LEVEL_DEBUG,"read_from_modem_device: data received by modem ~p~n",[V]),
      gen_statem:call(Name,{recieved_from_modem,V}),
      ?MLOG(?LOG_LEVEL_DEBUG,"read_from_modem_device: received data sent to process,~n")
  end.
