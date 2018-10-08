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
-export([address_field/3,smsc_address_field/2,protocol_identifier/2]).
-export([data_coding_schema/2,simple_pdu/5]).
-export([simple_first_octect/1]).
-include_lib("gsm_pdu.hrl").

-spec type_of_address(international|national)->type_of_address().
type_of_address(international)->
  #type_of_address{ton=1,npi=1};
type_of_address(national)->
  #type_of_address{ton = 1,npi = 2  }.

-spec address_field(international|national,digit_length|half_of_digit_length,list())->address_field().
address_field(AddressType,digit_length,Digits) ->
  #address_field{
    address_length = length(Digits),
    type_of_address = type_of_address(AddressType),
    address_digits = Digits
  };
address_field(AddressType,half_of_digit_length,Digits)->
  #address_field{
    address_length = round(length(Digits) /  2)+1 ,
    type_of_address = type_of_address(AddressType),
    address_digits = Digits
  }.

-spec smsc_address_field(international|national,list())->type_of_address().
smsc_address_field(AddressType,Digits)->
  address_field(AddressType,half_of_digit_length,Digits).

-spec protocol_identifier(internetwroking|no_internetworking,
    implicit|telex|telefax|voice_tele|europian_radio_msg_sys|national_paging
    |x_400_base_msg|email)-> tp_pid().
protocol_identifier(INTER_NETWORK,DEVICE)->
  BIT5 = case INTER_NETWORK of
    internetworking -> true;
    no_internetworking->false
  end,
  DEV = case DEVICE of
          implicit -> 0;
          telex ->  1;
          telefax-> 2;
          voice_tele->4;
          europian_radio_msg_sys->5;
          national_paging->6;
          x_400_base_msg->17;
          email -> 18
        end,
  #tp_pid{
    two_high_bits = 0,
    telemetric_interwoking = BIT5,
    telemetric_devices_type = DEV
  }.

-spec data_coding_schema(default|a8bit|a16bit|reserved,byte())->tp_dcs().
data_coding_schema(AlphabetIndication,ClassNo)->
  Alphabets = case AlphabetIndication of
                default -> 0 ;
                a8bit -> 1;
                a16bit->2;
                reserved->3
              end,
  #tp_dcs{compress = false,is_bit_0_1_class_meaning = false,
    alphabet_indication = Alphabets,message_class = ClassNo}.

simple_first_octect(COMMAND)->
  CMD = case COMMAND of
    ?PDU_TYPE_SMS_SUBMIT -> 1
  end,
  #first_octet{
    tp_mti = CMD,
    tp_rd = false,
    tp_vpf = 2,
    tp_ssr = false,
    tp_udhi = false,
    tp_rp=false
  }.

-spec simple_pdu(international,list(),list(),a8bit|a16bit,binary())->pdu().
simple_pdu(international,SMS_CENTER_NO,TARGET_NO,MESSAGE_ENCODING,MSG_BODY)->
  #pdu{
    smsc_address = smsc_address_field(international,SMS_CENTER_NO),
    tpdu = #tpdu{
      first_octet = simple_first_octect(?PDU_TYPE_SMS_SUBMIT),
      tp_mr = 0,
      tp_da = address_field(international,digit_length,TARGET_NO),
      tp_pid = protocol_identifier(no_internetworking,implicit),
      tp_dcs = data_coding_schema(MESSAGE_ENCODING,0),
      tp_vp = 16#AA,
      tp_udl = byte_size(MSG_BODY),
      tp_ud = MSG_BODY

      }
    }.
