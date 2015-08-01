-module(g17_guild_quit).
-export([url/2, decode/1]).
-include("g17.hrl").

% 接口描述

% 有加入过公会的玩家在游戏内退出所在公会时调用。
% 玩家退出公会(例如：在游戏中主动退出、在平台中主动退出、在平台中被会长踢出公会、被系统踢出公会等)后，平台会通过接口通知各游戏中心服务器，游戏中心服务器要将消息下发到各游戏区服，游戏区服内要解除此玩家的军团关系，如果此玩家是军团长还要进行军团长转让逻辑处理。
% 公会会长不允许退出公会。
% 接口提供方

% 联运平台提供接口, 游戏厂商调用
% 接口地址示例

% http://www.17g.com/api/guild/quit?user_id=$user_id&guild_id=$guild_id&game_id=$game_id&server_id=$server_id&time=$time&sign=$sign

% 接口参数说明
% 参数  类型  必传  说明
% http://www.17g.com/api/guild/quit           公会成员退出平台公会接口
% user_id     bigint  是   平台用户编号
% guild_id    int     是   平台公会编号
% game_id     int     是   游戏编号,由联运平台提供,每款游戏编号固定不变
% server_id   int     是   区服编号,一服是1,二服是2,以此类推
% time    int     是   平台服务器的时间戳(为长整数,单位秒)
% sign    string(32)  是   参数签名, 计算方法见表后”Sign签名算法”说明
% Sign签名算法

%             md5(user_id=$user_id&guild_id=$guild_id&game_id=$game_id&server_id=$server_id&time=$time&key=$key)

url(#state{
        address = Address,
        game_id = GameId,
        server_id = ServerId,
        simple_key = Key
    }, {_, UserId, GuildId}) ->

   UserIdParam = g17_util:user_id_param(UserId),
   GuildIdParam = g17_util:guild_id_param(GuildId),
   GameIdParam = g17_util:game_id_param(GameId),
   ServerIdParam = g17_util:server_id_param(ServerId),
   TimeParam = g17_util:time_param(),
   EncodeKeyParam = g17_util:key_param(Key),
   SignParam = g17_util:sign_param([UserIdParam, GuildIdParam, GameIdParam, ServerIdParam, TimeParam, EncodeKeyParam]),
   QueryString = g17_util:query_string([UserIdParam, GuildIdParam, GameIdParam, ServerIdParam, TimeParam, SignParam]),
   lists:concat([Address, "api/guild/quit?", QueryString]).



% 返回值格式

%             {"code":1,"msg":"SUCCESS"}


% 属性  说明
% code

% 接口返回代码
%     1 成功退出平台公会
%     -1 参数错误
%     -2 查询时间过期
%     -3 签名错误
%     -4 系统错误
%     -5 用户非此公会成员
%     -6 会长不能退出公会
%     -7 退出失败

% msg     提示信息

decode(Body) ->
    g17_util:simple_ret(Body).

