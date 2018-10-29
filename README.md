gen_gsm_modem
=====

An OTP application

Build
-----

    $ rebar3 compile

USAGE
-----
{ok,P} = simple_gsm_modem_over_tcp:start_link("127.0.0.1",3285).
simple_gsm_modem_over_tcp:register_handler(P,"989360076133","salam",simple_gsm_modem_over_tcp,success_message,[],io,fwrite,["Timeouted!!!!!~n"],120000).
simple_gsm_modem_over_tcp:read_inbox_sms(P).
simple_gsm_modem_over_tcp:send_sms(P,"989360076133",<<"salam"/utf16>>).


simple_gsm_modem_over_tcp:register_handler(P,"98800","1",simple_gsm_modem_over_tcp,success_message,[],io,fwrite,["Timeouted!!!!!~n"],180000).


 F = fun(X) -> io:fwrite("~ts",[unicode:characters_to_binary(unicode:characters_to_binary(X,utf16,utf8))]) end.
