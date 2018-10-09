%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2018 4:32 PM
%%%-------------------------------------------------------------------
-module(gsm_pdu).
-author("msd").

%% API
-export([address_field/3, smsc_address_field/2, protocol_identifier/2]).
-export([data_coding_schema/2, simple_pdu/5, simple_pdu/4, multipart_pdu/5, multipart_pdu/6]).
-export([tpdu_length/1]).
-export([simple_first_octet/1,flash_pdu/5]).
-include_lib("gsm_pdu.hrl").

-spec type_of_address(international|national) -> type_of_address().
type_of_address(international) ->
  #type_of_address{ton = 1, npi = 1};
type_of_address(national) ->
  #type_of_address{ton = 1, npi = 2}.

-spec address_field(international|national, digit_length|half_of_digit_length, list()) -> address_field().
address_field(AddressType, _, "") -> #address_field{address_length = 0, type_of_address = type_of_address(AddressType)};
address_field(AddressType, digit_length, Digits) ->
  #address_field{
    address_length = length(Digits),
    type_of_address = type_of_address(AddressType),
    address_digits = Digits
  };
address_field(AddressType, half_of_digit_length, Digits) ->
  #address_field{
    address_length = round(length(Digits) / 2) + 1,
    type_of_address = type_of_address(AddressType),
    address_digits = Digits
  }.

-spec smsc_address_field(international|national, list()) -> type_of_address().
smsc_address_field(AddressType, Digits) ->
  address_field(AddressType, half_of_digit_length, Digits).

-spec protocol_identifier(internetwroking|no_internetworking,
    implicit|telex|telefax|voice_tele|europian_radio_msg_sys|national_paging
    |x_400_base_msg|email) -> tp_pid().
protocol_identifier(INTER_NETWORK, DEVICE) ->
  BIT5 = case INTER_NETWORK of
           internetworking -> true;
           no_internetworking -> false
         end,
  DEV = case DEVICE of
          implicit -> 0;
          telex -> 1;
          telefax -> 2;
          voice_tele -> 4;
          europian_radio_msg_sys -> 5;
          national_paging -> 6;
          x_400_base_msg -> 17;
          email -> 18
        end,
  #tp_pid{
    two_high_bits = 0,
    telemetric_interwoking = BIT5,
    telemetric_devices_type = DEV
  }.

-spec data_coding_schema(default|a8bit|a16bit|reserved, byte()) -> tp_dcs().
data_coding_schema(AlphabetIndication, ClassNo) ->
  Alphabets = case AlphabetIndication of
                default -> 0;
                a8bit -> 1;
                a16bit -> 2;
                reserved -> 3
              end,
  #tp_dcs{compress = false, is_bit_0_1_class_meaning = false,
    alphabet_indication = Alphabets, message_class = ClassNo}.

-spec user_data_header(csms, byte(), byte(), byte()) -> udh().
user_data_header(csms, Identifier, TotalPartsCount, PartIndex) ->
  #udh{
    length = 3,
    identifier = 0,
    data = <<Identifier, TotalPartsCount, PartIndex>>
  }.

data_header(HeaderList) ->
  #dh{
    length_of_user_data_header = 2 * length(HeaderList) +
      lists:foldr(
        fun(#udh{length = L}, A) ->
          A + L
        end,
        0, HeaderList),
    headers = HeaderList
  }.

-spec simple_first_octet(?PDU_TYPE_SMS_SUBMIT) -> first_octet().
simple_first_octet(COMMAND) ->
  CMD = case COMMAND of
          ?PDU_TYPE_SMS_SUBMIT -> 1
        end,
  #first_octet{
    tp_mti = CMD,
    tp_rd = false,
    tp_vpf = 2,
    tp_ssr = false,
    tp_udhi = false,
    tp_rp = false
  }.

flash_pdu(international, SMS_CENTER_NO, TARGET_NO, MESSAGE_ENCODING, MSG_BODY) ->
  PDU=#pdu{tpdu = TPDU=#tpdu{tp_dcs = TPDCS}} = simple_pdu(international,SMS_CENTER_NO,TARGET_NO,MESSAGE_ENCODING,MSG_BODY),
  TPDCS1 = TPDCS#tp_dcs{
    is_bit_0_1_class_meaning = true,
    message_class = 0
  },
  PDU1 = PDU #pdu{tpdu = TPDU#tpdu{tp_dcs = TPDCS1}},
  PDU1.


-spec simple_pdu(international, list(), list(), a8bit|a16bit, binary()) -> pdu().
simple_pdu(international, SMS_CENTER_NO, TARGET_NO, MESSAGE_ENCODING, MSG_BODY) ->
  #pdu{
    smsc_address = smsc_address_field(international, SMS_CENTER_NO),
    tpdu = #tpdu{
      first_octet = simple_first_octet(?PDU_TYPE_SMS_SUBMIT),
      tp_mr = 0,
      tp_da = address_field(international, digit_length, TARGET_NO),
      tp_pid = protocol_identifier(no_internetworking, implicit),
      tp_dcs = data_coding_schema(MESSAGE_ENCODING, 0),
      tp_vp = 16#AA,
      tp_udl = byte_size(MSG_BODY),
      tp_ud = MSG_BODY

    }
  }.
simple_pdu(international, TARGET_NO, MESSAGE_ENCODING, MSG_BODY) ->

  #pdu{
    smsc_address = smsc_address_field(international, ""),
    tpdu = #tpdu{
      first_octet = simple_first_octet(?PDU_TYPE_SMS_SUBMIT),
      tp_mr = 0,
      tp_da = address_field(international, digit_length, TARGET_NO),
      tp_pid = protocol_identifier(no_internetworking, implicit),
      tp_dcs = data_coding_schema(MESSAGE_ENCODING, 0),
      tp_vp = 16#AA,
      tp_udl = byte_size(MSG_BODY),
      tp_ud = MSG_BODY

    }
  }.

split_packet(Size, P) when byte_size(P) >= Size ->
  {Chunk, Rest} = split_binary(P, Size),
  [Chunk | split_packet(Size, Rest)];
split_packet(_Size, <<>>) ->
  [];
split_packet(_Size, P) ->
  [P].
-spec multipart_pdu(international, list(), list(), a16bit, binary(), byte) -> [pdu()].
multipart_pdu(international, SMS_CENTER, TARGET_NO, a16bit, MSG_BODY, MESSAGE_ID) ->
  List_of_MSG_BODY = split_packet(140 - 6, MSG_BODY),
  Count = length(List_of_MSG_BODY),
  {Result, _} = lists:mapfoldl(
    fun(MSG, AIn) ->

      PDU = #pdu{
        tpdu = TPDU = #tpdu{
          first_octet = FisrtOctet,
          tp_udl = ORG_USER_DATA
        }
      } = simple_pdu(international, SMS_CENTER, TARGET_NO, a16bit, MSG),

      CSMSH = user_data_header(csms, MESSAGE_ID, Count, AIn),
      Hs = #dh{length_of_user_data_header = Header_DATA_LENGTH} = data_header([CSMSH]),
      PDU1 = PDU#pdu{
        tpdu = TPDU#tpdu{
          first_octet = FisrtOctet#first_octet{tp_udhi = true},
          dh = Hs,
          tp_udl = ORG_USER_DATA +
            Header_DATA_LENGTH +
            1 %% cause of HDL field itsown!
        }
      },
      {PDU1, AIn + 1}
    end,
    1,
    List_of_MSG_BODY),
  Result.

-spec multipart_pdu(international, list(), a16bit, binary(), byte) -> [pdu()].
multipart_pdu(international, TARGET_NO, a16bit, MSG_BODY, MESSAGE_ID) ->
  multipart_pdu(international, "", TARGET_NO, a16bit, MSG_BODY, MESSAGE_ID).

tpdu_length(#tpdu{
  first_octet = #first_octet{
    tp_vpf = VPF
  },
  tp_da = #address_field{address_length = AD_LENGTH},
  tp_udl = DATA_LEN}) ->
  TARGET_LEN = round(AD_LENGTH / 2) + 2 + 1,
  R = 1 + 1 + TARGET_LEN + 1 + 1 + DATA_LEN,
  case VPF of
    0 -> R;
    2 -> R + 1;
    1 -> R;
    3 -> R + 7
  end.
