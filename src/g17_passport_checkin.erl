-module(g17_passport_checkin).

-export([url/2, decode/1]).
-include("g17.hrl").


% 参数key   是否必须    类型范围    说明
% appkey  true    string  当前游戏的key
% deviceid    true    string  设备id
% time    true    string  当前时间
% sign    true    string  签名串
% token   true    string  用户身份标识
% user_id true    string  用户的编号


url(#state{
        address = Address,
        app_key = Key,
        app_secret = Secret
    }, {passport_checkin, DeviceId, Token, UserId}) ->
    AppKeyParam = "appkey=" ++ Key,
    DeviceIdParam = "deviceid=" ++ DeviceId,
    CurrentTimeStr = integer_to_list(hmisctime:unixtime()),
    TimeParam = "time=" ++ CurrentTimeStr,
    TokenParam = "token=" ++ Token,
    UserIdParam = "user_id=" ++ UserId,
    Sign = sign([AppKeyParam, DeviceIdParam, TimeParam, TokenParam, UserIdParam], Key, Secret),
    SignParam = "sign=" ++ Sign,
    QueryString = g17_util:query_string([AppKeyParam, DeviceIdParam, TimeParam, SignParam, TokenParam, UserIdParam]),
    RequestUrl = lists:concat([Address, "open/passport/checkin?", QueryString]),
    RequestUrl.


% url(#state{
%         address = Address,
%         game_id = GameId,
%         app_key = Key,
%         app_secret = Secret
%     }, {passport_checkin, DeviceId, Token, UserId}) ->
%     AppKeyParam = "appkey=" ++ Key,
%     DeviceIdParam = "deviceid=" ++ DeviceId,
%     CurrentTimeStr = integer_to_list(hmisctime:unixtime()),
%     TimeParam = "time=" ++ CurrentTimeStr,
%     TokenParam = "token=" ++ Token,
%     UserIdParam = "user_id=" ++ UserId,
%     Sign = sign([AppKeyParam, DeviceIdParam, TimeParam, TokenParam, UserIdParam], Key, Secret),
%     SignParam = "sign=" ++ Sign,
%     RequestUrl = lists:concat([Address, "open/passport/checkin"]),
%     BodyObj = {obj, [{appkey, Key},
%                    {deviceid, DeviceId},
%                    {time, CurrentTimeStr},
%                    {sign, g17_util:urlencode(Sign)},
%                    {token, Token},
%                    {user_id, UserId}]
%                 },
%     Body = rfc4627:encode(BodyObj),
%     {RequestUrl, Body}.




sign(List, Key, Secret) ->
    A = g17_util:query_string(List),
    B = Key ++ Secret,
    C = g17_util:md5_hex(B),
    D = A ++ "|" ++ C,
    E = g17_util:md5_hex(D),
    E.

decode(Body) ->
    {ok, {obj, ObjAttr}, []} = rfc4627:decode(Body),
    ExistUserId = proplists:get_value("user_id", ObjAttr, undefined),
    case ExistUserId of
        undefined ->
            #passport_checkin_ret{
                 code = list_to_integer(binary_to_list(proplists:get_value("code", ObjAttr))),
                 error = binary_to_list(proplists:get_value("error", ObjAttr))
            };
        _ ->
            %% 正常返回
            #passport_checkin_ret{
                code = ?G17_SIMPLE_RET_SUCCESS,
                error = "",
                data = #user_info_ret{
                    user_id = g17_util:to_string(ExistUserId),
                    guild_id = g17_util:fix_guild_id(g17_util:to_string(proplists:get_value("guild_id", ObjAttr))),
                    mb_title_id = g17_util:to_integer(proplists:get_value("mb_title_id", ObjAttr)),
                    mb_number_id = g17_util:to_integer(proplists:get_value("mb_number_id", ObjAttr))
                }
            }
    end.
