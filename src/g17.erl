-module(g17).
-export([start/0, stop/0,query_guild_info/2]).
-compile(export_all).
-define(ERROR_MSG, "本次请求失败，请稍后重试！").
start() ->
    application:start(g17).
stop() ->
   application:stop(g17).

query_guild_info(undefined, undefined) ->
    {error, both_ids_undefined};
query_guild_info(GuildIds, undefined) ->
    query_guild_info(GuildIds, []);
query_guild_info(undefined, UserIds) ->
    query_guild_info([], UserIds);
query_guild_info(GuildIds, UserIds) ->
    gen_server:call(g17_server, {query_guild_info, GuildIds, UserIds}).


% 查询平台上指定公会编号内的公会成员的活跃信息。
% 用于游戏内需要判断用户活跃信息的功能（如：游戏内的借将功能）。用户公龄值就是用户的活跃天数

query_guild_ageinfo(GuildId, UserId) ->
    gen_server:call(g17_server, {query_guild_ageinfo, GuildId, UserId}).


% 接口描述

% 查询平台上用户的相关信息。
% 接口提供方



query_user_info(UserId) ->
    try
        gen_server:call(g17_server, {query_user_info, UserId})
    catch
        _:Any ->
            {error, Any}
    end.


% 接口描述

% 未加入公会的军团长在游戏内创建平台公会时调用。
% 当一个散人军团的军团长加入或创建公会之后48小时内，军团内散人成员会在登录游戏任一区服时收到一条确认消息：
% 是否跟随军团长一同加入此公会？如果散人成员同意跟随则自动加入军团长所在公会，不需要公会会长审核；
% 如果散人成员不同意跟随则自动退出军团。确认消息只会对同一用户提示一次。
% 接口提供方



guild_create(UserId, GuildName) ->
    try
        gen_server:call(g17_server, {guild_create, UserId, GuildName})
    catch
        _:Any ->
            io:format("error : ~p~n", [Any]),
            {error, Any}
    end.


% 接口描述

% 有加入过公会的玩家在游戏内退出所在公会时调用。
% 玩家退出公会(例如：在游戏中主动退出、在平台中主动退出、在平台中被会长踢出公会、被系统踢出公会等)后，
% 平台会通过接口通知各游戏中心服务器，游戏中心服务器要将消息下发到各游戏区服，
% 游戏区服内要解除此玩家的军团关系，如果此玩家是军团长还要进行军团长转让逻辑处理。
% 公会会长不允许退出公会。
% 接口提供方


guild_quit(UserId, GuildId) ->
    try
        gen_server:call(g17_server, {guild_quit, UserId, GuildId})
    catch
        _:Any ->
            {error, Any}
    end.



% 接口描述

% 在游戏内,当一个既未加入过军团也未加入过公会的玩家成功加入到一个有公会的军团时,
% 自动成为此公会成员,无需公会会长审核且不受其它限制条件(如:公会人数上限)限制。
% 当散人军团的团长成功加入公会之后，全体团员都可以在登录游戏的时候收到一条消息：
% 是否跟随团长一同加入公会？如果团员同意，则直接加入公会成为公会成员，无需公会会长审核且不受其它限制条件限制。
% 接口提供方


guild_join(UserId, GuildId) ->
    try
        gen_server:call(g17_server, {guild_join, UserId, GuildId})
    catch
        _:Any ->
            {error, Any}
    end.



% 接口描述

% 游戏内无公会的军团长向平台公会发起入会申请时调用。

guild_applyjoin(UserId, GroupId, GroupName, MemberCnt, GuildId) ->
    try
        gen_server:call(g17_server, {guild_applyjoin, UserId, GroupId, GroupName, MemberCnt, GuildId})
    catch
        _:Any ->
            {error, Any}
    end.



query_role_info(UserId) ->
    gen_server:call(g17_server, {query_role_info, UserId}).


% order_submit.php?order_id=$order_id&order_type=$order_type
% &user_id=$user_id&server_id=$server_id&coin=$coin&extra=$extra
% &money=$money&time=$time&sign=$sign&role_id=$role_id
order_submit(OrderId, OrderType, UserId, Coin, Extra, Money, RoleId) ->
    gen_server:call(g17_server, {order_submit, OrderId, OrderType, UserId, Coin, Extra, Money, RoleId}).

guild_apply_reject_notify(RefuseMsgList) ->
    gen_server:call(g17_server, {guild_apply_reject_notify, RefuseMsgList}).

guild_newbie_notify(JoinMsgList) ->
    try
        gen_server:call(g17_server, {guild_newbie_notify, JoinMsgList})
    catch
        _:Any ->
            {error, Any}
    end.


quit_guild_notify(QuitMsgList) ->
    gen_server:call(g17_server, {quit_guild_notify, QuitMsgList}).

user_info_notify(UserIds) ->
    gen_server:call(g17_server, {user_info_notify, UserIds}).

passport_checkin(DeviceId, Token, UserId) ->
    try
        gen_server:call(g17_server, {passport_checkin, DeviceId, Token, UserId})
    catch
        _:Any ->
            io:format("passprot_checkin failed Any : ~p~n", [Any]),
            {error, Any}
    end.
