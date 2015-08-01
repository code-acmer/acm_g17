-module(g17_guild_apply_reject_notify).
-export([url/2, decode/1]).
-include("g17.hrl").



% http://api.zwj.17g.com/guild_apply_reject_notify.php?data=$data&time=$time&sign=$sign
% md5(data=$data&time=$time&key=$key)


url(#state{
       address = Address,
       simple_key = Key
    }, {guild_apply_reject_notify, RefuseMsgList}) ->
    RefuseData = gen_data(RefuseMsgList),
    DataParam = "data=" ++ RefuseData,
    EncodeDataParam = "data=" ++ g17_util:urlencode(RefuseData),
    TimeParam = g17_util:time_param(),
    EncodeKeyParam = g17_util:key_param(Key),
    SignParam = g17_util:sign([EncodeDataParam, TimeParam, EncodeKeyParam]),
    QueryString = g17_util:query_string([DataParam, TimeParam, SignParam]),
    lists:concat([Address, "guild_apply_reject_notify.php?", QueryString]).



% 举例:
% 消息体内容为 1001-10_1002-11_1003-10
% 这个消息体一共有3条拒绝信息
% (1001-10 和 1002-11 及 1003-10).

% 1001-10 的意思为:
% 用户(user_id:1001) 向 公会(guild_id:10) 发起的入会申请被拒绝了.
gen_data(MsgList) ->
    StrMsgList = [UserId ++ "-" ++ GuildId || {UserId, GuildId} <- MsgList],
    string:join(StrMsgList, "_").



decode(Body) ->
   g17_util:simple_ret(Body).
