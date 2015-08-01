-module(g17_quit_guild_notify).
-export([url/2, decode/1]).
-include("g17.hrl").


% http://api.zwj.17g.com/quit_guild_notify.php?data=$data&time=$time&sign=$sign

url(#state{
       address = Address,
       simple_key = Key
    }, {quit_guild_notify, QuitMsgList}) ->
    QuitData = gen_data(QuitMsgList),
    DataParam = "data=" ++ QuitData,
    EncodeDataParam = "data=" ++ g17_util:urlencode(QuitData),
    TimeParam = g17_util:time_param(),
    EncodeKeyParam = g17_util:key_param(Key),
    SignParam = g17_util:sign([EncodeDataParam, TimeParam, EncodeKeyParam]),
    QueryString = g17_util:query_string([DataParam, TimeParam, SignParam]),
    lists:concat([Address, "quit_guild_notify.php?", QueryString]).



gen_data(MsgList) ->
    StrMsgList = [UserId ++ "-" ++ GuildId || {UserId, GuildId} <- MsgList],
    string:join(StrMsgList, "_").




decode(Body) ->
    g17_util:simple_ret(Body).

