-module(g17_guild_applyjoin).
-export([url/2, decode/1]).
-include("g17.hrl").



% 接口描述

% 游戏内无公会的军团长向平台公会发起入会申请时调用。
% 接口提供方

% 联运平台提供接口, 游戏厂商调用
% 接口地址示例

% http://www.17g.com/api/guild/applyJoin?user_id=$user_id&group_id=$group_id
% &group_name=$group_name&member_cnt=$member_cnt&guild_id=$guild_id
% &game_id=$game_id&server_id=$server_id&time=$time&sign=$sign

% 接口参数说明
% 参数  类型  必传  说明
% http://www.17g.com/api/guild/applyJoin          用户提交入会申请接口
% user_id     bigint  是   军团长用户编号
% group_id    String  是   游戏内军团编号
% group_name  String  是   游戏内军团名称
% 注意: 在接口调用以及参数md5计算签名时,军团名称要经过urlencode编码.
% member_cnt  int     是   游戏内军团人数
% guild_id    int     是   平台公会编号, 申请要加入的公会编号
% game_id     int     是   游戏编号,由联运平台提供,每款游戏编号固定不变
% server_id   int     是   区服编号,一服是1,二服是2,以此类推
% time    int     是   平台服务器的时间戳(为长整数,单位秒)
% sign    string(32)  是   参数签名, 计算方法见表后”Sign签名算法”说明
% Sign签名算法

% md5(user_id=$user_id&group_id=$group_id&group_name=$group_name
% &member_cnt=$member_cnt&guild_id=$guild_id&game_id=$game_id&server_id=$server_id&time=$time&key=$key)


url(#state{
        address = Address,
        game_id = GameId,
        server_id = ServerId,
        simple_key = Key
    }, {guild_applyjoin, UserId, GroupId, GroupName, MemberCnt, GuildId}) ->
   UserIdParam = g17_util:user_id_param(UserId),
   GroupIdParam = "group_id=" ++ integer_to_list(GroupId),
   GroupNameParam = "group_name=" ++ g17_util:urlencode(GroupName),
   EncodeGroupNameParam = "group_name=" ++ g17_util:urlencode(GroupName),
   MemberCntParam = "member_cnt=" ++ integer_to_list(MemberCnt),
   GuildIdParam = g17_util:guild_id_param(GuildId),
   GameIdParam = g17_util:game_id_param(GameId),
   ServerIdParam = g17_util:server_id_param(ServerId),
   TimeParam = g17_util:time_param(),
   EncodeKeyParam = g17_util:key_param(Key),
   SignParam = g17_util:sign_param([UserIdParam, GroupIdParam, EncodeGroupNameParam,
                                    MemberCntParam, GuildIdParam, GameIdParam,
                                    ServerIdParam, TimeParam, EncodeKeyParam]),
   QueryString = g17_util:query_string([UserIdParam, GroupIdParam, GroupNameParam,
                                    MemberCntParam, GuildIdParam, GameIdParam,
                                    ServerIdParam, TimeParam, SignParam]),
   lists:concat([Address, "api/guild/applyJoin?", QueryString]).



% {"code":1,"msg":"SUCCESS"}
% 属性  说明
% code

% 接口返回代码
%     1 申请提交成功
%     -1 参数错误
%     -2 查询时间过期
%     -3 签名错误
%     -4 系统错误
%     -5 公会不存在
%     -6 已经加入其它公会,不能再申请加入公会
%     -7 已经提交过申请了,请不要重复提交
%     -8 申请加入失败

% msg     提示信息
decode(Body) ->
    g17_util:simple_ret(Body).
