%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2018 3:02 PM
%%%-------------------------------------------------------------------
-module(gsm_pdu_deserializers).
-author("msd").
-include_lib("gsm_pdu.hrl").
%% API
-export([first_octet/1, type_of_address/1, address_field/1]).
-export([address_field_decimal_len/1, tp_pid/1, tp_dcs/1]).
-export([tp_scts/1, udh/1, dh/1, tpdu/1, pdu/1]).

-spec first_octet(binary()) -> {first_octet()|first_octet_deliver(), binary()}.
first_octet(<<FirstByte, Remain/binary>>) ->
  <<_:6, MTI:2>> = <<FirstByte>>,
  case MTI of
    0 ->
      %% SMS-DELIVER PACKET
      << SRI:1, UDHI:1,RP:1,_:2 , MMS:1, MTI:2>> = <<FirstByte>>,
      {#first_octet_deliver{
        tp_mti = MTI,
        tp_mms = bit_to_boolean(MMS),
        tp_rp = bit_to_boolean(RP),
        tp_udhi = bit_to_boolean(UDHI),
        tp_sri = bit_to_boolean(SRI)
      }, Remain};
    1 ->
      %% SMS-SUBMIT
      <<RP:1, UDHI:1, SSR:1, VPF:2, RD:1, MTI:2>> = <<FirstByte>>,
      {
        #first_octet{
          tp_mti = MTI,
          tp_rd = bit_to_boolean(RD),
          tp_vpf = VPF,
          tp_ssr = bit_to_boolean(SSR),
          tp_udhi = bit_to_boolean(UDHI),
          tp_rp = bit_to_boolean(RP)
        }, Remain};
    2 ->
      %% SMS-COMMAND
      {#first_octet{}, Remain};
    3 ->
      %%reserved
      {#first_octet{}, Remain}


  end.

-spec type_of_address(binary()) -> {type_of_address(), binary()}.
type_of_address(<<_:1, TON:3, NPI:4, Remain/binary>>) ->
  {#type_of_address{ton = TON, npi = NPI}, Remain}.

-spec address_field(binary()) -> {address_field(), binary()}.
address_field(<<0, BTOA:1/binary, Remain/binary>>) ->
  TOA = case BTOA of
          <<>> -> undefined;
          _ -> type_of_address(BTOA)
        end,
  {#address_field{
    address_length = 0,
    type_of_address = TOA
  }, Remain};
address_field(<<L, Remain/binary>>) when L > 0 ->
  L1 = (L + 1) div 2,

  <<BTOA:1/binary, BNumber:L1/binary,Remain2/binary>> = Remain,
  Digits = BNumber,
  {TOA,_} = type_of_address(BTOA),
  {
    #address_field{
      address_length = L1,
      type_of_address = TOA,
      address_digits = case TOA of
                         #type_of_address{ ton=5} ->
                           sms_7bit_encoding:from_7bit(Digits);
                         _->binary_to_list( Digits)
                       end
    }, Remain2}.


-spec address_field_decimal_len(binary()) -> {address_field(), binary()}.
address_field_decimal_len(<<0, Remain/binary>>) ->
  {#address_field{address_length = 0}, Remain};
address_field_decimal_len(
    <<L1:1/binary, BTOA:1/binary, Remain/binary>>
) ->
  L = octet_to_deciaml(L1) - 1,
  <<BNumber:L/binary, Remain1/binary>> = Remain,
  TOA = type_of_address(BTOA),
  Digits = decode_binlist_to_number(BNumber),
  {#address_field{
    address_length = L,
    type_of_address = TOA,
    address_digits = Digits
  }, Remain1}.


-spec tp_pid(binary()) -> {tp_pid(), binary()}.
tp_pid(<<THB:2, TI:1, TD:5, Remain/binary>>) ->
  {
    #tp_pid{
      two_high_bits = THB,
      telemetric_interwoking = bit_to_boolean(TI),
      telemetric_devices_type = TD
    }, Remain
  }.


-spec tp_dcs(binary()) -> {tp_dcs(), binary()}.
tp_dcs(<<
  _:2, %% two high bits should be zero accordings to Standard GSM 3.4
  C:1, I:1, A:2, M:2, Remain/binary>>)
  ->
  {
    #tp_dcs{
      compress = bit_to_boolean(C),
      is_bit_0_1_class_meaning = bit_to_boolean(I),
      alphabet_indication = A,
      message_class = M
    }, Remain}.

-spec tp_scts(binary()) -> {tp_scts(), binary()}.
tp_scts(<<Y, M, D, H, Minu, S, T, Remain/binary>>) ->
  {
    #tp_scts{
      year = semi_octet_to_decimal( Y),
      month =semi_octet_to_decimal( M),
      day = semi_octet_to_decimal(D),
      hour = semi_octet_to_decimal(H),
      minute = semi_octet_to_decimal(Minu),
      second = semi_octet_to_decimal(S),
      timezone = semi_octet_to_decimal(T)
    }, Remain}.

-spec udh(binary()) -> {udh(), binary()}.
udh(<<I, L, D/binary>>) ->
  io:fwrite("I:~p L:~p D:~p~n", [I, L, D]),
  #udh{};
udh(<<I, L, D:L/binary, Remain/binary>>) ->
  {#udh{
    identifier = I,
    length = L,
    data = D
  }, Remain}.

-spec dh_helper(integer(), binary(), list()) -> {list(), binary()}.
dh_helper(0, R, Headers) ->
  {Headers, R};
dh_helper(Count, HeadersInBinary, Headers) ->
  {H, Remain} = udh(HeadersInBinary),
  dh_helper(Count - 1, Remain, [H | Headers]).


-spec dh(binary()) -> {dh(), binary()}.
dh(<<L, Headers/binary>>)
  ->
  {Hs, R} = dh_helper(L, Headers, []),
  {#dh{
    length_of_user_data_header = L,
    headers = Hs
  }, R}.

tpdu(FirstOctet,B) when is_record(FirstOctet,first_octet_deliver)->
  {OA,R1} = address_field(B),
  {PID,R2}= tp_pid(R1),
  {DCS,R3} = tp_dcs(R2),
  {SCTS,R4}=tp_scts(R3),
  <<UDL,R5/binary>> =R4,
  UDL1=UDL-1,
  <<D:UDL1/binary,_/binary>>=R5,
  #tpdu_deliver{
    first_octet = FirstOctet,
    tp_oa = OA,
    tp_pid = PID,
    tp_dcs = DCS,
    tp_scts = SCTS,
    tp_udl = UDL,
    tp_ud = case DCS of
              #tp_dcs{ alphabet_indication = 0 } -> sms_7bit_encoding:from_7bit(D);
              _-> D
            end
  };
tpdu(FirstOctet,B) when is_record(FirstOctet,first_octet)->
  <<MR, R2/binary>> = B,
  {DA, R3} = address_field(R2),
  {PID, R4} = tp_pid(R3),
  {DCS, R5} = tp_dcs(R4),
  {VPV, R6} = case FirstOctet#first_octet.tp_vpf of
                0 -> {undefined, R5};
                1 -> {undefined, R5};
                2 -> <<K, R_/binary>> = R5, {K, R_};
                3 -> tp_scts(R5)
              end,
  <<UDL, R7/binary>> = R6,
  <<Headers, R8>> = dh(R7),
  #tpdu{
    first_octet = FirstOctet,
    tp_mr = MR,
    tp_da = DA,
    tp_pid = PID,
    tp_dcs = DCS,
    tp_vp = VPV,
    tp_udl = UDL,
    dh = Headers,
    tp_ud = R8
  }.

-spec tpdu(binary()) -> tpdu().
tpdu(B) ->
  {FirstOctet, R1} = first_octet(B),
  tpdu(FirstOctet,R1).

-spec pdu(binary()) -> pdu().
pdu(B) ->
  {SA, Remain} = address_field_decimal_len(B),
  TPDU = tpdu(Remain),
  #pdu{
    smsc_address = SA,
    tpdu = TPDU
  }.

-spec semi_octet_to_decimal(byte())-> integer().
semi_octet_to_decimal(Byte)->
  <<R:4,C:4>> = <<Byte>>,
  R+C*10.

bit_to_boolean(0) -> false;
bit_to_boolean(1) -> true.

-spec octet_to_deciaml(binary()) -> byte().
octet_to_deciaml(<<C:4, R:4>>) ->
  C * 10 + R.

-spec decode_binlist_to_number(binary()) -> list().
decode_binlist_to_number(Binary) -> decode_binlist_to_number(Binary, "").

-spec decode_binlist_to_number(binary(), list()) -> list().
decode_binlist_to_number(<<>>, Acc) -> Acc;
decode_binlist_to_number(<<A:4, B:4, Rest/binary>>, Acc) ->
  BNUMB = $0 + B,
  ACC1 = Acc ++ [BNUMB],
  ACC2 = case A of
           16#F -> ACC1;
           _ -> ACC1 ++ [$0 + A]
         end,
  decode_binlist_to_number(Rest, ACC2).