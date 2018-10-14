%%%-------------------------------------------------------------------
%%% @author msd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2018 10:43 AM
%%%-------------------------------------------------------------------
-author("msd").
-define(LOG_LEVEL_DEBUG,debug).
-define(MLOG(LEVEL,MSG),io:fwrite(MSG)).
-define(MLOG(LEVEL,FORMAT,DATA),io:fwrite(FORMAT,DATA)).