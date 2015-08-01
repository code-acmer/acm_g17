-module(g17_util).
-export([unixtime/0, urlencode/1, md5_hex/1]).
-include("define_http_server.hrl").
-include("g17.hrl").
-export([game_id_param/1,
         guild_id_param/1,
         user_id_param/1,
         server_id_param/1,
         key_param/1,
         time_param/0,
         sign_param/1,
         query_string/1,
         simple_ret/1,
         fix_guild_id/1,
         http_reply/3,
         sign/1]).

-export([to_string/1,
         to_integer/1]).

-export([escape_uri/1]).

fix_guild_id("0") ->
   "";
fix_guild_id(Value) ->
  Value.

to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_string(Value) ->
    Value.

to_integer(Value) when is_binary(Value) ->
    list_to_integer(binary_to_list(Value));
to_integer(Value) when is_list(Value) ->
    list_to_integer(Value);
to_integer(Value) ->
    Value.

user_id_param(UserId) ->
    "user_id=" ++ UserId.

guild_id_param(GuildId) ->
    "guild_id=" ++ GuildId.

game_id_param(GameId) ->
  "game_id=" ++ integer_to_list(GameId).

server_id_param(ServerId) ->
    "server_id=" ++ integer_to_list(ServerId).

key_param(Key) ->
   "key=" ++ urlencode(Key).

time_param() ->
    "time=" ++ integer_to_list(unixtime()).



sign_param(ParamList) ->
    Value = sign(ParamList),
    "sign=" ++ Value.

sign(ParamList) ->
    String = query_string(ParamList),
    string:to_lower(md5_hex(String)).

query_string(ParamList) ->
    string:join(ParamList, "&").


% reomve_query_string_prefix(ParamList) ->
%     String = lists:concat(ParamList),
%     case String of
%       [] -> String;
%       "&" ++ Rest -> Rest
%     end.


unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

urlencode(Value) ->
    escape_uri(Value).
    % http_uri:encode(String).


md5_hex(S) ->
    Md5_bin = erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).


simple_ret(Body) ->
    {ok, {obj, ObjAttr}, []} = rfc4627:decode(Body),
    #simple_ret{
        code = proplists:get_value("code", ObjAttr),
        msg = proplists:get_value("msg", ObjAttr)
    }.


escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ string:to_upper(hex_octet(C)).
    % "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].






http_reply(Code, Req, Content) ->
    cowboy_req:reply(Code, [content_type(json)], Content, Req).
http_200(Req, Content) ->
    http_reply(?HTTP_OK, Req, Content).
http_400(Req, Content) ->
    http_reply(?HTTP_BAD_REQUEST, Req, Content).


%% reply_error(Req, Content) ->
%%     cowboy_req:reply(?HTTP_BAD_REQUEST, [content_type(json)], Content, Req).
%% reply_ok(Req, Content) ->
%%     cowboy_req:reply(?HTTP_OK, [content_type(json)], Content, Req).
    
content_type(html) ->
    {<<"content-type">>, <<"text/html; charset=utf-8">>};
content_type(json) ->
    {<<"content-type">>, <<"application/json; charset=utf-8">>};
content_type(plain) ->
    {<<"content-type">>, <<"text/plain; charset=utf-8">>}.

require_ip_address(Req) ->
    {ClientIp, _Port} = cowboy_req:peer(Req),
    lists:foldr(fun
                    (IPBlock, false) ->
                       case string:tokens(IPBlock, "/") of
                           [IPAddress] ->
                               IPAddress =:= string:join(lists:map(fun erlang:integer_to_list/1, tuple_to_list(ClientIp)), ".");
                           [IPAddress, Mask] ->
                               MaskInt = list_to_integer(Mask),
                               IPAddressTuple = list_to_tuple(lists:map(fun erlang:list_to_integer/1, string:tokens(IPAddress, "."))),
                               mask_ipv4_address(ClientIp, MaskInt) =:= mask_ipv4_address(IPAddressTuple, MaskInt)
                       end;
                    (_, true) ->
                       true
                end, false, ["192.168.0.0/16", "127.0.0.1", "10.0.0.0/16"]),
    true.

mask_ipv4_address({I1, I2, I3, I4}, MaskInt) ->
    ((I1 bsl 24 + I2 bsl 16 + I3 bsl 8 + I4) band ((1 bsl 32) - (1 bsl (32 - MaskInt)))).
