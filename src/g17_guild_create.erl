-module(g17_guild_create).
-export([url/2, decode/1]).
-include("g17.hrl").



% 接口描述

% 未加入公会的军团长在游戏内创建平台公会时调用。
% 当一个散人军团的军团长加入或创建公会之后48小时内，军团内散人成员会在登录游戏任一区服时收到一条确认消息：是否跟随军团长一同加入此公会？如果散人成员同意跟随则自动加入军团长所在公会，不需要公会会长审核；如果散人成员不同意跟随则自动退出军团。确认消息只会对同一用户提示一次。
% 接口提供方

% 联运平台提供接口, 游戏厂商调用
% 接口地址示例

% http://www.17g.com/api/guild/create?user_id=$user_id&guild_name=$guild_name&game_id=$game_id&server_id=$server_id&time=$time&sign=$sign

% 接口参数说明
% 参数  类型  必传  说明
% http://www.17g.com/api/guild/create             军团长创建平台公会接口
% user_id     bigint  是   平台用户编号(新创建的公会会长编号)
% guild_name  string(21)  是   平台公会名称
% 公会名称最短为6个字符, 最长长度为21个字符(一个汉字占3个字符)
% 注意: 在接口调用以及参数md5计算签名时,公会名称要经过urlencode编码.
% game_id     int     是   游戏编号,由联运平台提供,每款游戏编号固定不变
% server_id   int     是   区服编号,一服是1,二服是2,以此类推
% time    int     是   平台服务器的时间戳(为长整数,单位秒)
% sign    string(32)  是   参数签名, 计算方法见表后”Sign签名算法”说明
% Sign签名算法

%             md5(user_id=$user_id&guild_name=$guild_name&game_id=$game_id&server_id=$server_id&time=$time&key=$key)


url(#state{
        address = Address,
        game_id = GameId,
        server_id = ServerId,
        simple_key = Key
    }, {_, UserId, GuildName}) ->

   UserIdParam = g17_util:user_id_param(UserId),
   GuildNameParam = "guild_name=" ++ g17_util:urlencode(GuildName),
   EncodeGuildNameParam = "guild_name=" ++ g17_util:urlencode(GuildName),
   GameIdParam = g17_util:game_id_param(GameId),
   ServerIdParam = g17_util:server_id_param(ServerId),
   TimeParam = g17_util:time_param(),
   EncodeKeyParam = g17_util:key_param(Key),
   SignParam = g17_util:sign_param([UserIdParam, EncodeGuildNameParam, GameIdParam, ServerIdParam, TimeParam, EncodeKeyParam]),
   QueryString = g17_util:query_string([UserIdParam, GuildNameParam, GameIdParam, ServerIdParam, TimeParam, SignParam]),
   lists:concat([Address, "api/guild/create?", QueryString]).



% 返回值格式

%             {"code":1,"msg":"SUCCESS","data":{"guild_id":10000,"guild_name":"\u516c\u4f1a\u540d\u79f0","owner_user_id":123456}}


% 属性  说明
% code

% 接口返回代码
%     1 创建成功
%     -1 参数错误
%     -2 查询时间过期
%     -3 签名错误
%     -4 系统错误
%     -5 用户不存在
%     -6 用户已加入其它公会不能创建公会
%     -7 公会名称不能为空
%     -8 公会名称长度不正确
%     -9 公会名中含有非法字
%     -10 公会名称已被占用
%     -11 创建失败

% msg     提示信息
% data    公会信息, 当公会创建失败时为空
% guild_id    公会编号
% guild_name  公会名称
% owner_user_id   公会会长用户编号

decode(Body) ->
    {ok, {obj, ObjAttr}, []} = rfc4627:decode(Body),
    Code = proplists:get_value("code", ObjAttr),
    Msg = proplists:get_value("msg", ObjAttr),
    Result = #guild_create_ret{
       code = Code,
       msg = Msg
    },
    case Code of
        1 ->
            {obj, Attr} =proplists:get_value("data", ObjAttr),
            GuildRet = #guild_ret{
                guild_id = g17_util:fix_guild_id(g17_util:to_string(proplists:get_value("guild_id", Attr))),
                guild_name = proplists:get_value("guild_name", Attr),
                owner_user_id = g17_util:to_string(proplists:get_value("owner_user_id", Attr))
            },
            Result#guild_create_ret{
               data = GuildRet
            };
        _ ->
            Result
    end.
