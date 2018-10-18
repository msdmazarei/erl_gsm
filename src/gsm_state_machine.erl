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
-export([start_link/3, start_link/2]).
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

-define(SEND_STATUS_SENDING, sending).
-define(SEND_STATUS_WAIT, wait_to_modem_response).
-define(SEND_STATUS_READY, ready).


-record(cmti_event, {
  storage :: list(),
  index :: integer()
}).
-type cmti_event() :: #cmti_event{}.

-record(state, {
  send_status :: send_status(),
  receive_status :: receive_status(),
  received_chars :: list(),
  inbox_messages :: [tpdu()],
  gsm_modem_connector_module_name :: string(),
  gsm_modem_connector_identifier :: term(),
  last_sent_command :: at_command(),
  respond_back_to :: term(),
  pdu_enable :: boolean(),
  pdu_to_send :: pdu(),

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
start_link(GsmModemConnectorModuleName, GsmModemConnectorIdentifier) ->
  {ok, PID} = gen_statem:start_link(?MODULE, [GsmModemConnectorModuleName, GsmModemConnectorIdentifier], []),
  {ok, _} = timer:apply_interval(100, ?MODULE, read_from_modem_device, [PID, GsmModemConnectorModuleName, GsmModemConnectorIdentifier]),
  {ok, PID}.
start_link(Name, GsmModemConnectorModuleName, GsmModemConnectorIdentifier) ->
  R = gen_statem:start_link({local, Name}, ?MODULE, [GsmModemConnectorModuleName, GsmModemConnectorIdentifier], []),
  {ok, _} = timer:apply_interval(100, ?MODULE, read_from_modem_device, [Name, GsmModemConnectorModuleName, GsmModemConnectorIdentifier]),
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
-type at_command() :: 'AT'.

init([GsmModemConnectorModuleName, GsmModemConnectorIdentifier]) ->
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

until_next_happen(Pattern, Binary, Scanned) ->
  L = byte_size(Pattern),
  <<I:L/binary, Rest/binary>> = Binary,
  case I of
    Pattern -> {Scanned, Rest};
    _ -> <<S:1/binary, R/binary>> = Binary,
      until_next_happen(Pattern, R, <<Scanned/binary, S/binary>>)
  end.
-spec until_next_happen(binary(), binary()) -> {binary(), binary()}.
until_next_happen(Pattern, Binary) ->
  L = byte_size(Pattern),
  <<I:L/binary, Rest/binary>> = Binary,
  case I of
    Pattern -> {<<>>, Rest};
    _ -> <<S:1/binary, R/binary>> = Binary,
      until_next_happen(Pattern, R, S)
  end.


append_to_inbox_message(MsgIndex, Message, Inbox) ->
  case lists:keyfind(MsgIndex, 1, Inbox) of
    false -> lists:append(Inbox, [{MsgIndex, Message}]);
    _ -> Inbox
  end.


process_parts(<<>>, State) ->
  {State, []};
process_parts(<<"\r\n> \r\nERROR\r\n">>, State) -> {State, []};

process_parts(<<"\r\n+CUSD: ", Remain/binary>>, State = #state{last_sent_command = 'CUSD', respond_back_to = RES_BACK}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CUSD: Processing called. REAMIN:~p~n", [Remain]),
  {CODE, R1} = string:to_integer(Remain),
  {C,S,D,NS,A} =
    case R1 of
    <<",", R2/binary>> ->
      [STR, R3] = string:split(R2, <<",">>),
      {DCSN, <<"\r\n", R4/binary>>} = string:to_integer(R3),
      RESULT =
        case STR of
          <<"\"", PSTR/binary>> ->
            [PPSTR,_]= string:split(PSTR, <<"\"">>),
            NState_ = State#state{received_chars = R4},
            {NState, Acts} = process_parts(R4, NState_),
            ?MLOG(?LOG_LEVEL_DEBUG,"PPSTR:~p~n",[PPSTR]),
            {CODE, hex:hexstr_to_bin(binary_to_list( PPSTR)), DCSN, NState, Acts};
          _ ->
            {NState, Acts} = process_parts(R4, State#state{received_chars = R4}),
            {CODE, STR, DCSN, NState, Acts}
        end,
      RESULT;
    <<"\r\n",R2/binary>>->
      {NState, Acts} = process_parts(R2, State#state{received_chars = R2}),
      {CODE, "", -1, NState, Acts}
  end,
  NNS = NS # state {
    send_status = ?SEND_STATUS_READY
  },
  case RES_BACK of
    undefined ->
      {NNS , A};
    {P, _} when is_pid(P) ->
      {NNS , [{reply, RES_BACK, {ok,{C,S,D}}} | A]};
    {P, _} when is_atom(P) ->
      {NNS , [{reply, RES_BACK, {ok,{C,S,D}}} | A]}
  end;
%%when CUSD TIMOUT or other notification arrived that we are not waiting for.
process_parts(<<"\r\n+CUSD: ", Remain/binary>>, State ) ->
  [_,R1]=string:split(Remain,<<"\r\n">>),
  process_parts(R1,State#state{received_chars = R1});

process_parts(<<"\r\n+CMGL: ", Remain/binary>>, State = #state{last_sent_command = 'CMGL', inbox_messages = Inbox}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGL: process CALLED WITH REMAIN: ~p~n", [Remain]),
  {MSGINDEX, R1} = string:to_integer(Remain),
  [_, R2] = string:split(R1, <<"\r\n">>),
  [PDU, R3] = string:split(R2, <<"\r\n">>),
  NewInbox = append_to_inbox_message(MSGINDEX, gsm_pdu_deserializers:pdu(hex:hexstr_to_bin(binary_to_list(PDU))), Inbox),
  NewState = State#state{
    inbox_messages = NewInbox,
    command_result = NewInbox
  },
  case R3 of
    <<"+CMGL", _/binary>> ->
      process_parts(<<"\r\n", R3/binary>>, NewState);
    _ ->
      process_parts(R3, NewState)

  end;

process_parts(<<"\r\n+CPMS: ", Remain/binary>>, State = #state{last_sent_command = 'CPMS?'}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CPMS: CALLED WITH REMAIN: ~p~n", [Remain]),
  [RES, Remain1] = string:split(Remain, <<"\r\n">>),

  NewState = State#state{
    command_result = RES
  },
  process_parts(<<Remain1/binary>>, NewState);

process_parts(<<"\r\n+CMGR: ", Remain/binary>>, State = #state{last_sent_command = 'CMGR'}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGR: CALLED WITH REMAIN: ~p~n", [Remain]),
  [Header, Remain1] = string:split(Remain, <<"\r\n">>),
  [Body, Remain2] = string:split(Remain1, <<"\r\n">>),
  NewState = State#state{
    command_result = {Header, gsm_pdu_deserializers:pdu(hex:hexstr_to_bin(binary_to_list(Body)))}
  },
  process_parts(<<Remain2/binary>>, NewState);


process_parts(<<"\r\n+CSQ: ", Remain/binary>>, State = #state{last_sent_command = 'CSQ'}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CSQ: CALLED WITH REMAIN: ~p~n", [Remain]),
  {CSQ, <<",", Remain2/binary>>} = string:to_integer(Remain),
  {CME, <<"\r\n", Remain3/binary>>} = string:to_integer(Remain2),
  NewState = State#state{
    command_result = [CSQ, CME]
  },
  process_parts(Remain3, NewState);

process_parts(<<"\r\n+CMTI: ", Remain/binary>>, State = #state{cmti_events = EVENTS}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMTI: CALLED WITH REMAIN: ~p~n", [Remain]),
  <<"\"", STORAGE:2/binary, "\",", MessageIndexStr/binary>> = Remain,
  {MessageIndex, <<"\r\n", Remain1/binary>>} = string:to_integer(MessageIndexStr),
  ?MLOG(?LOG_LEVEL_DEBUG, "CMTI: MegIndex:~p ~n", [MessageIndex]),
  NewState = State#state{
    cmti_events = [#cmti_event{index = MessageIndex, storage = STORAGE} | EVENTS]
  },

  process_parts(Remain1, NewState);

process_parts(<<"\r\n+CMGS: ", Rest/binary>>, State) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGS PROCESSOR CALLED~n"),
  {MsgNo, <<"\r\n", R/binary>>} = string:to_integer(Rest),
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGS: MSG NO IS ~p~n", [MsgNo]),
  NewState = State#state{
    command_result = MsgNo
  },
  ?MLOG(?LOG_LEVEL_DEBUG, "UPDATE LAST SENT MESSAGE ID IN STATE~n"),
  process_parts(R, NewState);
process_parts(<<"\r\n> ", Remain/binary>>, State = #state{last_sent_command = 'CMGS', pdu_to_send = PDU, gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I}) ->
  io:fwrite("CMGS PART > CALLED.~n"),
  SPDU = hex:bin_to_hex(gsm_pdu_serializers:pdu(PDU)),
  COMMAND = io_lib:format("~s", [SPDU]),
  ok = apply(M, send_to_modem, [I, <<(list_to_binary(COMMAND))/binary, 26>>]),
  process_parts(Remain, State);
process_parts(<<"\r\nERROR\r\n", Remain/binary>>, State = #state{respond_back_to = RES_BACK}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "ERROR PART CALLED. RESPOND BACK TO ~p ~n", [RES_BACK]),
  NewState = State#state{send_status = ready, received_chars = Remain},
  {NState, Acts} = process_parts(Remain, NewState),
  case RES_BACK of
    undefined ->
      {NState, Acts};
    {P, _} when is_pid(P) ->
      {NState, [{reply, RES_BACK, error} | Acts]};
    {P, _} when is_atom(P) ->
      {NState, [{reply, RES_BACK, error} | Acts]}
  end;


%%process_parts(<<"\r\nOK\r\n",Remain/binary>>,State=#state{respond_back_to = RES_BACK, last_sent_command = 'CSQ', csq_cme = CSQCME})->
%%  ?MLOG(?LOG_LEVEL_DEBUG,"OK PART CALLED FOR CSQ COMMAND. RESPOND BACK TO ~p ~n",[RES_BACK]),
%%  NewState = State#state{send_status = ready,received_chars = Remain},
%%  {NState,Acts} = process_parts(Remain,NewState),
%%  {NState,[{reply,RES_BACK,{ok,CSQCME}}|Acts]};
process_parts(<<"\r\nOK\r\n", Remain/binary>>, State = #state{last_sent_command = 'CUSD',command_result = undefined}) ->
  process_parts(Remain, State);
process_parts(<<"\r\nOK\r\n", Remain/binary>>, State = #state{respond_back_to = RES_BACK, last_sent_command = LAST_COMMAND, command_result = CMD_RESULT}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "OK PART CALLED FOR ~p COMMAND. RESPOND BACK TO ~p ~n", [LAST_COMMAND, RES_BACK]),
  NewState = State#state{send_status = ready, received_chars = Remain},
  {NState, Acts} = process_parts(Remain, NewState),
  R = case CMD_RESULT of
        undefined ->
          {NState, [{reply, RES_BACK, {ok, ok}} | Acts]};
        _ ->
          {NState, [{reply, RES_BACK, {ok, CMD_RESULT}} | Acts]}
      end,
  io:fwrite("RES_BACK IS:~p~n", [RES_BACK]),
  case RES_BACK of
    undefined ->
      {NState, Acts};
    {P, _} when is_pid(P) ->
      R;
    {P, _} when is_atom(P) ->
      R
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
handle_event({call, From}, {send, 'CMGD', MSGINX}, StateName, State = #state{gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I, send_status = ?SEND_STATUS_READY}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGD: CALLED FROM ~p~n", [From]),
  COMMAND_L = io_lib:format("AT+CMGD=~p\r", [MSGINX]),
  COMMAND = list_to_binary(COMMAND_L),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENDING CMD:~p~n", [COMMAND]),
  ok = apply(M, send_to_modem, [I, COMMAND]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENT CMD:~p~n", [COMMAND]),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CMGD', respond_back_to = From, command_result = undefined},
  {next_state, StateName, NewState};

handle_event({call, From}, {send, 'CPMS?'}, StateName, State = #state{gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I, send_status = ?SEND_STATUS_READY}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CPMS?: CALLED FROM ~p~n", [From]),
  COMMAND = <<"AT+CPMS?\r">>,
  ?MLOG(?LOG_LEVEL_DEBUG, "SENDING CMD:~p~n", [COMMAND]),
  ok = apply(M, send_to_modem, [I, COMMAND]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENT CMD:~p~n", [COMMAND]),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CPMS?', respond_back_to = From, command_result = undefined},
  {next_state, StateName, NewState};

handle_event({call, From}, {send, 'CSQ'}, StateName,
    State = #state{gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I, send_status = ?SEND_STATUS_READY}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CSQ: CALLED FROM ~p~n", [From]),
  COMMAND = <<"AT+CSQ\r">>,
  ?MLOG(?LOG_LEVEL_DEBUG, "SENDING CMD:~p~n", [COMMAND]),
  ok = apply(M, send_to_modem, [I, COMMAND]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENT CMD:~p~n", [COMMAND]),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CSQ', respond_back_to = From, command_result = undefined},
  {next_state, StateName, NewState};
handle_event({call, From}, {send, 'CTRL_Z'}, StateName, State = #state{send_status = ?SEND_STATUS_READY, gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CTRL_C: CALLED FROM: ~p~n", [From]),
  ok = apply(M, send_to_modem, [I, <<16#1A, 16#D, 16#A>>]),
  {next_state, StateName, State, [{reply, From, ok}]};
handle_event({call, From}, {send, 'CMGR', _}, StateName, State = #state{send_status = ?SEND_STATUS_READY, pdu_enable = false}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGR: CALLED FROM ~p (NO PDU ENABLED)~n", [From]),
  {next_state, StateName, State, [{reply, From, {error, not_in_pdu_mode}}]};

handle_event({call, From}, {send, 'CMGR', MsgIndex}, StateName, State = #state{send_status = ?SEND_STATUS_READY, gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I, pdu_enable = true}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGR: CALLED FROM ~p~n", [From]),
  COMMAND = io_lib:format("AT+CMGR=~p\r", [MsgIndex]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENDING CMD:~p~n", [COMMAND]),
  ok = apply(M, send_to_modem, [I, <<(list_to_binary(COMMAND))/binary>>]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENT CMD:~p~n", [COMMAND]),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CMGR', respond_back_to = From, command_result = undefined},
  {next_state, StateName, NewState};


%%This AT command selects the character set of the mobile equipment. Some possible values are "GSM", "HEX"."IRA", "PCDN", "UCS2","UTF-8" etc.
handle_event({call, From}, {send, 'CSCS', CHARSET}, StateName, State = #state{send_status = ?SEND_STATUS_READY, gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CSCS: CALLED FROM ~p~n", [From]),
  COMMAND = io_lib:format("AT+CSCS=~p\r", [CHARSET]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENDING CMD:~p~n", [COMMAND]),
  ok = apply(M, send_to_modem, [I, <<(list_to_binary(COMMAND))/binary>>]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENT CMD:~p~n", [COMMAND]),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CSCS', respond_back_to = From, command_result = undefined},
  {next_state, StateName, NewState};

handle_event({call, From}, {send, 'CUSD', N, STR, DCS}, StateName, State = #state{send_status = ?SEND_STATUS_READY, gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CUSD: CALLED FROM ~p~n", [From]),
  COMMAND = io_lib:format("AT+CUSD=~p,~s,~p\r", [N, STR, DCS]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENDING CMD:~p~n", [COMMAND]),
  ok = apply(M, send_to_modem, [I, <<(list_to_binary(COMMAND))/binary>>]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENT CMD:~p~n", [COMMAND]),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CUSD', respond_back_to = From, command_result = undefined},
  {next_state, StateName, NewState};

handle_event(
    {call, From}, {send, 'CMGS', _}, StateName,
    State = #state{send_status = ?SEND_STATUS_READY, pdu_enable = false}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGS: CALLED FROM ~p (NO PDU ENABLED)~n", [From]),
  {next_state, StateName, State, [{reply, From, {error, not_in_pdu_mode}}]};

handle_event(
    {call, From}, {send, 'CMGS', PDU = #pdu{tpdu = TPDU}}, StateName,
    State = #state{gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I, send_status = ?SEND_STATUS_READY, pdu_enable = true}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGS: CALLED FROM ~p~n", [From]),
  Len = gsm_pdu:tpdu_length(TPDU),
  COMMAND = io_lib:format("AT+CMGS=~p\r", [Len]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENDING CMD:~p~n", [COMMAND]),
  ok = apply(M, send_to_modem, [I, <<(list_to_binary(COMMAND))/binary>>]),
  ?MLOG(?LOG_LEVEL_DEBUG, "SENT CMD:~p~n", [COMMAND]),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CMGS', respond_back_to = From, pdu_to_send = PDU, command_result = undefined},
  {next_state, StateName, NewState};

handle_event(
    {call, From}, {send, 'CMGF', Value}, StateName,
    State = #state{gsm_modem_connector_module_name = M, gsm_modem_connector_identifier = I, send_status = ?SEND_STATUS_READY}) ->
  CMD = list_to_binary(io_lib:format("AT+CMGF=~p", [Value])),
  ok = apply(M, send_to_modem, [I, <<CMD/binary, "\r">>]),

  NewState = case Value of
               0 ->
                 State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CMGF', respond_back_to = From, pdu_enable = true, command_result = undefined};
               _ ->
                 State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CMGF', respond_back_to = From, pdu_enable = false, command_result = undefined}
             end,
  {next_state, StateName, NewState};

handle_event({call, From}, {send, 'AT'}, StateName, State = #state{gsm_modem_connector_module_name = GSMModule, gsm_modem_connector_identifier = GSMIdenifier, send_status = ?SEND_STATUS_READY}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "AT: CALLED FROM ~p~n", [From]),
  ?MLOG(?LOG_LEVEL_DEBUG, "AT: SENDING AT COMMAND"),
  ok = apply(GSMModule, send_to_modem, [GSMIdenifier, <<"AT\r">>]),
  ?MLOG(?LOG_LEVEL_DEBUG, "AT: AT COMMAND SENT"),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'AT', respond_back_to = From, command_result = undefined},
  {next_state, StateName, NewState, []};
handle_event({call, From}, {send, 'CMGL', Value}, StateName, State = #state{gsm_modem_connector_module_name = GSMModule, gsm_modem_connector_identifier = GSMIdenifier, send_status = ?SEND_STATUS_READY, pdu_enable = true}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGL: CALLED FROM ~p~n", [From]),
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGL: SENDING CMGL COMMAND"),
  COMMAND = io_lib:format("AT+CMGL=~p\r", [Value]),
  ok = apply(GSMModule, send_to_modem, [GSMIdenifier, list_to_binary(COMMAND)]),
  ?MLOG(?LOG_LEVEL_DEBUG, "CMGL: CMGL COMMAND SENT"),
  NewState = State#state{send_status = ?SEND_STATUS_WAIT, last_sent_command = 'CMGL', respond_back_to = From, command_result = undefined},
  {next_state, StateName, NewState, []};
handle_event({call, From}, cmti_events, StateName, State = #state{cmti_events = Events}) ->
  {next_state, StateName, State, [{reply, From, Events}]};
handle_event({call, From}, {remove_cmti_event, CMTI_EVENT}, StateName, State = #state{cmti_events = Events}) ->
  NewEvents = lists:filter(
    fun(E) -> case E of
                CMTI_EVENT -> false;
                _ -> true
              end
    end, Events),
  NewState = State#state{cmti_events = NewEvents},
  {next_state, StateName, NewState, [{reply, From, ok}]};

handle_event({call, From}, {send, _}, StateName, State) ->
  {next_state, StateName, State, [{reply, From, busy}]};

handle_event({call, From}, {recieved_from_modem, Data}, StateName, State = #state{received_chars = CHARS}) ->
  ?MLOG(?LOG_LEVEL_DEBUG, "recieved_from_modem: called with data:~p~n", [Data]),
  NewState = State#state{
    received_chars = <<CHARS/binary, Data/binary>>
  },
  RCBS = byte_size(NewState#state.received_chars) - 2,
  <<_:RCBS/binary, LASTPART:2/binary>> = NewState#state.received_chars,
  ?MLOG(?LOG_LEVEL_DEBUG, "recieved_from_modem: LASTPARTS:~p~n", [LASTPART]),
  {NewNewState, Actions} =
    case LASTPART of
      <<"\r\n">> ->
        process_parts(NewState#state.received_chars, NewState);
      <<"> ">> ->
        process_parts(NewState#state.received_chars, NewState);
      _ ->
        ?MLOG(?LOG_LEVEL_DEBUG, "recieved_from_modem: Nothing to processs, wait to more chars to come"),
        {NewState, []}
    end,
  Actions1 = [{reply, From, ok} | Actions],
  %%I DONT KNOW WHY WE NEED THIS BUT MODEMS ARE NOT SO FAST.
  %% use send_after
  timer:sleep(200),
%%  Actions1=Actions,
  R =
    {next_state, StateName, NewNewState, Actions1},
  R;

handle_event(E = {call, From}, _, _, S) ->
  {next_state, E, S, [{reply, From, ok}]};
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
read_from_modem_device(Name, Module, Identifier) ->
  case apply(Module, receive_from_modem, [Identifier]) of
    <<>> ->
      ok;
    V when is_binary(V) ->
      ?MLOG(?LOG_LEVEL_DEBUG, "read_from_modem_device: data received by modem ~p~n", [V]),
      gen_statem:call(Name, {recieved_from_modem, V}),
      ?MLOG(?LOG_LEVEL_DEBUG, "read_from_modem_device: received data sent to process,~n")
  end.
