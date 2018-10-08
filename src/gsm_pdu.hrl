%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2018 3:38 PM
%%%-------------------------------------------------------------------
-author("msd").



%%conveying a short message from the SC to the MS
-define(PDU_TYPE_SMS_DELIVER,pdu_type_sms_deliver ).

%%conveying a failure cause (if necessary)
-define(PDU_TYPE_SMS_DELIVER_REPORT,pdu_type_sms_deliver_report).

%%conveying a short message from the MS to the SC.
-define(PDU_TYPE_SMS_SUBMIT,pdu_type_sms_submit).

%%conveying a failure cause (if necessary)
-define(PDU_TYPE_SMS_SUBMIT_REPORT,pdu_type_sms_submit_report).

%%conveying a status report from the SC to the MS
-define(PDU_TYPE_SMS_STATUS_REPORT,pdu_type_sms_status_report).

%%onveying a command from the MS to the SC.
-define(PDU_TYPE_SMS_COMMAND,pdu_type_sms_command).


-record(first_octet, {
  %%Parameter describing the message type.
  tp_mti :: integer(),

  %%Parameter indicating whether or not the
  %%SC shall accept an SMS-SUBMIT for an
  %%SM still held in the SC which has the sam
  %%TP-MR and the same TP-DA as a
  %%previously submitted SM from the same
  %%OA
  tp_rd  :: boolean(),

  %%Parameter indicating whether or not the
  %%TP-VP field is present.
  tp_vpf :: integer(),

  %%Parameter indicating the request for Reply
  %%Path.
  tp_ssr :: boolean(),

  %%Parameter indicating that the TP-UD field
  %%contains a Header.
  tp_udhi:: boolean(),

  %%Parameter indicating if the MS is
  %%requesting a status report.
  tp_rp  :: boolean()
}).
-type first_octet() :: first_octet().

-record(type_of_address,{
%%0 0 0	Unknown
%%0 0 1	International number
%%0 1 0	National number
%%0 1 1	Network specific number
%%1 0 0	Subscriber number
%%1 0 1	Alphanumeric, (coded according to 3GPP TS 23.038 [9] GSM 7-bit default alphabet)
%%1 1 0	Abbreviated number
%%1 1 1	Reserved for extension
  ton :: integer(),

%%0 0 0 0	Unknown
%%0 0 0 1	ISDN/telephone numbering plan (E.164/E.163)
%%0 0 1 1	Data numbering plan (X.121)
%%0 1 0 0	Telex numbering plan
%%0 1 0 1	Service Centre Specific plan 1)
%%0 1 1 0	Service Centre Specific plan 1)
%%1 0 0 0	National numbering plan
%%1 0 0 1	Private numbering plan
%%1 0 1 0	ERMES numbering plan (ETSI DE/PS 3 01 3)
%%1 1 1 1	Reserved for extension
  npi :: integer()

}).
-type type_of_address() :: #type_of_address{}.


-record(address_field, {
  address_length :: byte(),
  type_of_address:: type_of_address(),
  %%!! SMSC
  address_digits :: list()
}).

-type address_field() :: #address_field{}.

-export_type([type_of_address/0, address_field/0]).

-record(tp_pid,{
  two_high_bits::byte(),
  telemetric_interwoking::boolean(),
  telemetric_devices_type::byte()
}).
-type tp_pid() :: #tp_pid{}.
-record(tp_dcs,{
  compress :: boolean(),
  is_bit_0_1_class_meaning :: boolean(),
  %% 0-> Default alphabet
  %% 1-> 8bit
  %% 2-> 16bit
  %% 3-> reserved
  alphabet_indication::byte(),
  %% 0 -> class 0
  %% 1 -> Class 1 Default meaning: ME-specific.
  %% 2 -> Class 2 SIM specific message
  %% 3 -> Class 3 Default meaning: TE specific (see GSM TS 07.05)
  message_class ::byte()
}).

-type tp_dcs() :: #tp_dcs{}.
-record(tp_scts,{
  year::byte(),
  month::byte(),
  day::byte(),
  hour::byte(),
  minute::byte(),
  second::byte(),
  timezone::byte()
}).
-type tp_scts() :: #tp_scts{}.

%%user data header
-record(udh , {
  %% 0 -> Concatenated short messages
  %% 1 -> Special SMS Message Indication
  %% 2 -> Reserved
  %% 3 -> Value not used to avoid misinterpretation as <LF> character
  %% 04 - 7F -> Reserved for future use
  %% 80 - 9F -> SME to SME specific use
  %% A0 - BF -> Reserved for future use
  %% C0 - DF -> SC specific use
  %% E0 - FF -> Reserved for future use
  identifier :: byte(),
  length :: byte(),
  data :: binary()
}).
-type udh():: #udh{}.

-record(dh,{
  length_of_user_data_header :: byte(),
  headers :: [udh()]
}).
-type dh():: #dh{}.

-record(tpdu,{
  first_octet :: first_octet(),

  %%Message Reference
  tp_mr::byte(),

  %%Destination Address
  tp_da:: address_field(),

  %%Protocol Identifier
  tp_pid:: tp_pid(),

  %%Data Coding Scheme
  tp_dcs:: tp_dcs(),

  %%Validity Period
  tp_vp:: byte()|tp_scts(),

  %%User Data Length
  tp_udl::byte(),

  %%TP-User Data
  tp_ud::string(),

  %%Data Header
  dh::dh()
}).

-type tpdu()::#tpdu{}.

-record(pdu, {
  smsc_address:: address_field(),
  tpdu ::tpdu()
}).
-type pdu()::pdu().

-export_type([tpdu/0,first_octet/0,address_field/0,tp_pid/0,tp_dcs/1,tp_scts/1]).
-export_type([pdu/0]).