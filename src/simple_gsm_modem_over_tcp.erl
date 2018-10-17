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
-export([start_link/2, send_at_command/1, send_sms/3]).
-define(MODEM_COMMAND_TIMEOUT, 5000).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).



-include("gsm_pdu.hrl").
-include("logger.hrl").
-define(SERVER, ?MODULE).

-record(normal_message, {
  sender :: string(),
  message_utf_16 :: string(),
  message_timeepch :: integer(),
  modem_message_index :: list()
}).
-type normal_message() :: #normal_message{}.

-record(state, {
  modem_connector_pid :: atom(),
  gsm_state_machine_pid :: atom(),
  modem_ip_address :: string(),
  modem_port_no :: integer(),
  last_send_data_epoch :: integer(),
  time_ref_interval :: term(),
  pdu_mode :: boolean(),
  low_level_inbox :: [{number(), pdu()}],
  inbox :: [normal_message()],
  time_ref_newsms :: term(),
  time_ref_process_llinbox :: term(),
  sms_handlers_pid :: [pid()]
}).

%%%===================================================================
%%% API
%%%===================================================================
get_antenna_status(Pid) ->
  gen_server:call(Pid, get_antenna_status, ?MODEM_COMMAND_TIMEOUT).
send_sms(Pid, TargetNo, UTF16Bin) ->
  gen_server:call(Pid, {send_sms, TargetNo, UTF16Bin}, 20000).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(string(), integer()) ->
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
  {ok, TRef} = timer:apply_interval(30000, ?MODULE, send_at_command, [GSMStateMachine]),
  {ok, TRef_NEWSMS} = timer:apply_interval(1000, gen_server, call, [self(), check_for_new_sms]),
  {ok, TREF_PROCESS_LLINBOX} = timer:apply_interval(2000, gen_server, call, [self(), process_low_level_inbox]),

  {ok, _} = gen_statem:call(GSMStateMachine, {send, 'CMGF', 0}, ?MODEM_COMMAND_TIMEOUT),
  %%READ OLD INBOX
  {ok, LLM} = gen_statem:call(GSMStateMachine, {send, 'CMGL', 4}, 20000),
  R = {ok, #state{
    modem_connector_pid = ModemPID,
    modem_ip_address = IPAdderess,
    modem_port_no = Port,
    gsm_state_machine_pid = GSMStateMachine,
    time_ref_interval = TRef,
    pdu_mode = false,
    time_ref_newsms = TRef_NEWSMS,
    inbox = [],
    low_level_inbox = LLM,
    time_ref_process_llinbox = TREF_PROCESS_LLINBOX
  }},

  R.
stick_parts([], _, COMPLETED_MESSAGES) ->
  COMPLETED_MESSAGES;
stick_parts([IDENTIFER | IDENTIFER_LIST], ECHList, COMPLETED_MESSAGES) ->
  {SENDERNO, TIMEPOCH, UNIQ, LEN} = IDENTIFER,
  ECL = lists:filter(
    fun(X) ->
      case X of
        {_, SENDERNO, T2, #udh{data = <<UNIQ, _, _>>}, _} ->
          case abs(T2 - TIMEPOCH) of
            P when P < 3600 -> true;
            _ -> false
          end;
        _ -> false
      end
    end,
    ECHList),
  MSG_PARTS = lists:map(
    fun(Y) ->
      Z = lists:filter(fun(W) -> case W of
                                   {_, _, _, #udh{data = <<_, _, Y>>}, _} -> true;
                                   _ -> false
                                 end
                       end, ECL),
      case Z of
        [H | _] -> H;
        _ -> null
      end
    end,
    lists:seq(1, LEN)),
  MSG_PARTS1 = lists:filter(fun(X) -> case X of
                                        null -> false;
                                        _ -> true
                                      end end, MSG_PARTS),
  case length(MSG_PARTS1) of
    LEN ->
      MSG = #normal_message{
        sender = SENDERNO,
        message_timeepch = TIMEPOCH,
        message_utf_16 = lists:map(fun({_, _, _, _, #pdu{tpdu = #tpdu_deliver{tp_ud = UD}}}) -> UD end, MSG_PARTS1),
        modem_message_index = lists:map(fun({MSGINX, _, _, _, _}) -> MSGINX end, MSG_PARTS1)
      },
      CPMSG = lists:append(COMPLETED_MESSAGES, [MSG]),
      stick_parts(IDENTIFER_LIST, ECHList--ECL, CPMSG);
    _ ->
      stick_parts(IDENTIFER_LIST, ECHList--ECL, COMPLETED_MESSAGES)
  end.

process_multipart_messages(State = #state{low_level_inbox = [_]}) ->
  State;
process_multipart_messages(State = #state{low_level_inbox = []}) ->
  State;
process_multipart_messages(State = #state{low_level_inbox = LLInbox, inbox = Inbox}) ->
  Extracted_ConcatHeader = lists:map(
    fun({MSGINX, PDU = #pdu{tpdu = #tpdu_deliver{tp_oa = #address_field{address_digits = SENDERNO}, tp_scts = SCTS}}}) ->
      ConcatHeaders = pdu_utils:get_headers_with_identifier(PDU, 0),
      case ConcatHeaders of
        [] -> {};
        [UDHC | _] ->
          {MSGINX, SENDERNO, pdu_utils:tp_scts_to_epoch(SCTS), UDHC, PDU}
      end
    end, LLInbox),

  ECH1 = lists:filter(fun(X) -> case X of
                                  {} -> false;
                                  _ -> true
                                end
                      end, Extracted_ConcatHeader),
  IDENTIFIER_LIST = lists:map(
    fun({_, SENDERNO, TIMEPOCH, #udh{data = <<UNIQ, LEN, _>>}, _}) ->
      {SENDERNO, TIMEPOCH, UNIQ, LEN}
    end, ECH1),
  IDENTIFIER_SET = sets:from_list(IDENTIFIER_LIST),
  NORMAL_MESSAGES = stick_parts(sets:to_list(IDENTIFIER_SET), ECH1, []),
  MSGIDS_TOREMOVE = lists:flatten(lists:map(fun(#normal_message{modem_message_index = MSGINDXs}) ->
    MSGINDXs end, NORMAL_MESSAGES)),
  NewLLINBOX = lists:filter(fun({MSGINX, _}) -> not lists:member(MSGINX, MSGIDS_TOREMOVE) end, LLInbox),
  NewInbox = lists:append(Inbox, NORMAL_MESSAGES),
  NewState = State#state{
    inbox = NewInbox,
    low_level_inbox = NewLLINBOX
  },
  NewState.

inform_handlers(State = #state{inbox = Inbox, sms_handlers_pid = PIDS, gsm_state_machine_pid = GSMModem}) ->
  lists:foreach(
    fun(X) ->
      lists:foreach(
        fun(Y) ->
          Y ! X,
          #normal_message{modem_message_index = I} = X,
          lists:foreach(
            fun(Z) ->
              gen_statem:call(GSMModem, {send, 'CMGD', Z}, ?MODEM_COMMAND_TIMEOUT)
            end, I)
        end,
        PIDS)
    end, Inbox),
  State#state{inbox = []}.


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
handle_call(process_low_level_inbox, _, State = #state{low_level_inbox = LLInbox, inbox = Inbox}) ->
  NewItems = lists:filter(
    fun({_, PDU}) ->
      case pdu_utils:is_partial_sms(PDU) of
        false -> true;
        _ -> false
      end
    end,
    LLInbox),
  New_LLInbox = LLInbox -- NewItems,
  SMSs = lists:map(
    fun({MSGINX, #pdu{tpdu = #tpdu_deliver{
      tp_oa = #address_field{address_digits = SENDER},
      tp_ud = BODY,
      tp_scts = TPSCTS}}}) ->
      #normal_message{
        sender = SENDER,
        message_utf_16 = BODY,
        message_timeepch = pdu_utils:tp_scts_to_epoch(TPSCTS),
        modem_message_index = [MSGINX]
      }
    end, NewItems),
  New_Inbox = lists:append(Inbox, SMSs),
  NewState = State#state{
    inbox = New_Inbox,
    low_level_inbox = New_LLInbox
  },
  NNewState = process_multipart_messages(NewState),
  NNNewState = inform_handlers(NNewState),

  {reply, ok, NNNewState};

handle_call(check_for_new_sms, _, State = #state{gsm_state_machine_pid = GSMModem, low_level_inbox = LLInbox}) ->
  case gen_statem:call(GSMModem, cmti_events) of
    [] ->
      {reply, ok, State};
    CMTI when is_list(CMTI) ->
      ?MLOG(?LOG_LEVEL_DEBUG, "THERE ARE SOME NEW EVENTS ~p~n", [CMTI]),
      Msgs = lists:map(
        fun(CMTIEVENT = {_, _, MsgIndex}) ->
          {ok, {_, MSG}} = gen_statem:call(GSMModem, {send, 'CMGR', MsgIndex}, 15000),
          gen_statem:call(GSMModem, {remove_cmti_event, CMTIEVENT}, 15000),
          {MsgIndex, MSG}
        end, CMTI),
      NewState = State#state{
        low_level_inbox = lists:append(LLInbox, Msgs)
      },
      ?MLOG(?LOG_LEVEL_DEBUG, "State:~p~n", [NewState]),
      {reply, ok, NewState}
  end;
handle_call({send_sms, TargetNo, UTF16Bin}, _, State = #state{gsm_state_machine_pid = S, pdu_mode = PDUMode}) ->
  case PDUMode of
    false ->
      gen_statem:call(S, {send, 'CMGF', 0}, ?MODEM_COMMAND_TIMEOUT);
    _ ->
      ok
  end,
  R = case byte_size(UTF16Bin) of
        N when N < 134 ->
          PDU = gsm_pdu:simple_pdu(international, TargetNo, a16bit, UTF16Bin),
          gen_statem:call(S, {send, 'CMGS', PDU});
        _ ->
          MsgIdentifier = rand:uniform(250),
          PDUS = gsm_pdu:multipart_pdu(international, TargetNo, a16bit, UTF16Bin, MsgIdentifier),
          lists:map(fun(X) ->
            gen_statem:call(S, {send, 'CMGS', X})
                    end, PDUS)
      end,
  NewState = State#state{last_send_data_epoch = get_timestamp(), pdu_mode = true},
  {reply, R, NewState};
handle_call(get_antenna_status, _, State = #state{gsm_state_machine_pid = S}) ->
  R = gen_statem:call(S, {send, 'AT'}, ?MODEM_COMMAND_TIMEOUT),
  NewState = State#state{
    last_send_data_epoch = get_timestamp()
  },
  {reply, R, NewState};
handle_call({remove_handler, PID}, _, State = #state{sms_handlers_pid = PIDS}) ->
  NewState = State#state{
    sms_handlers_pid = lists:filter(
      fun
        (X) when X=:=PID -> false;
        (_) -> true
      end, PIDS)

  },
  {reply, ok, NewState};
handle_call({add_handler, SENDER_Regex, TXTRegex, SUCCESSMODULE, SUCCESSFUNC, SUCCESSARGS, FAILMODULE, FAILFUNC, FAILARGS, TIMEOUT}, _, State = #state{sms_handlers_pid = PIDS}) ->
  GENSERVERPID = self(),
  PID = spawn(
    fun() ->
      receive
        MSG = #normal_message{sender = SENDER, message_utf_16 = MSG} ->
          ?MLOG(?LOG_LEVEL_DEBUG, "HANDLERCALLED WITH MSG:~p~n", [MSG]),
          case re:run(SENDER, SENDER_Regex) of
            nomatch -> ok;
            _ -> case re:run(MSG, TXTRegex) of
                   nomatch -> ok;
                   _ ->
                     apply(SUCCESSMODULE, SUCCESSFUNC, lists:append(SUCCESSARGS, [MSG]))
                 end
          end

      after
        TIMEOUT ->
          ?MLOG(?LOG_LEVEL_DEBUG, "HANDLERCALLED TIMEOUTED ~n", []),
          gen_server:call(GENSERVERPID, {remove_handler, GENSERVERPID}, 1000),
          apply(FAILMODULE, FAILFUNC, FAILARGS)
      end

    end),
  NewPIDS = [PID | PIDS],
  NewState = State#state{sms_handlers_pid = NewPIDS},
  {reply, ok, NewState};
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
terminate(_Reason, _State = #state{time_ref_interval = TRef, time_ref_newsms = TRefNEWSMS, time_ref_process_llinbox = TREF_LLINBOX}) ->
  timer:cancel(TRef),
  timer:cancel(TRefNEWSMS),
  timer:cancel(TREF_LLINBOX),
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




