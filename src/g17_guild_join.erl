-module(g17_guild_join).
-export([url/2, decode/1]).
-include("g17.hrl").




% 接口描述

% 在游戏内,当一个既未加入过军团也未加入过公会的玩家成功加入到一个有公会的军团时,自动成为此公会成员,无需公会会长审核且不受其它限制条件(如:公会人数上限)限制。
% 当散人军团的团长成功加入公会之后，全体团员都可以在登录游戏的时候收到一条消息：是否跟随团长一同加入公会？如果团员同意，则直接加入公会成为公会成员，无需公会会长审核且不受其它限制条件限制。
% 接口提供方

% 联运平台提供接口, 游戏厂商调用
% 接口地址示例

% http://www.17g.com/api/guild/join?user_id=$user_id&guild_id=$guild_id&game_id=$game_id&server_id=$server_id&time=$time&sign=$sign

% 接口参数说明
% 参数  类型  必传  说明
% http://www.17g.com/api/guild/join           自动加入平台公会接口
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
   lists:concat([Address, "api/guild/join?", QueryString]).


% 返回值格式

%             {"code":1,"msg":"SUCCESS"}


% 属性  说明
% code

% 接口返回代码
%     1 成功加入平台公会
%     -1 参数错误
%     -2 查询时间过期
%     -3 签名错误
%     -4 系统错误
%     -5 公会在封禁期间内无法接收新成员
%     -6 用户已经加过其它公会了
%     -7 加入失败

% msg     提示信息

decode(Body) ->
    g17_util:simple_ret(Body).
