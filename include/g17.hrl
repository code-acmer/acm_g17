-ifndef(DEFINE_G17_HRL).
-define(DEFINE_G17_HRL, true).

-define(G17_SIMPLE_RET_SUCCESS, 1).

-define(G17_GUILD_QUERY_SUCCESS, 1).  %%    1 公会信息查询成功(当查询条件的公会都存在时)
-define(G17_GUILD_QUERY_SUCCESS_PART, 2). %%    2 公会信息查询成功,信息部分存在(当查询条件的公会部分存在时)
-define(G17_GUILD_NOT_EXIST,         -1). %%    -1 公会信息不存在(当查询条件的公会都不存在时)
-define(G17_PARAM_ERROR,           -2). %% -2 参数错误
-define(G17_PARAM_NUM_LIMIT,       -3). %%    -3 查询数量超过限制
-define(G17_QUERY_TIME_OUT,        -4). %%    -4 查询时间过期
-define(G17_SIGN_ERROR,            -5). %%    -5 签名错误

-record(state, {
    address,
    game_id,
    server_id,
    money_key,
    simple_key,
    app_key,
    app_secret
    }).

%%============= 查询平台公会返回结构=================
-record(query_guild_info_ret,{
     code,     %% 处理结果编码
     data = [],     %% guild_info_ret 列表
     msg       %% 处理结果消息
  }).

-record(guild_info_ret, {
      query_id,                  %% 查询用的id，平台公会id或者平台用户id
      guild_id,                  %% 平台公会id
      guild_name,                %% 平台公会名称
      guild_logo,                %% 平台公会logo
      owner_user_id              %% 平台公会拥有者id
  }).


%%=========== 查询指定平台公会和用户的活跃信息返回=======================
-record(query_user_ageinfo_ret,{
      code,      %% 处理结果编码
      msg,       %% 处理结果消息
      data       %% 如果成功，则是user_ageinfo_ret
  }).

-record(user_ageinfo_ret,{
      guild_id,      %% 平台公会
      user_id,       %% 平台用户Id
      series_age_cnt,  %%连续公龄(活跃天)值 (连续多少天获得公龄,只要有一天没有获得公龄就从0开始重新累计. 最长只记录7天的连续公龄,即此值最大为7)
      last_add_dt     %% 字符串类型 "2015-03-29"， 最后一次获得公龄(活跃天)的时间,
  }).

%%==============查询平台用户信息返回=============
-record(query_user_info_ret, {
     code,
     msg,
     data     %% 有值的话为user_info_ret
  }).

-record(user_info_ret, {
    user_id,     %%  平台用户编号
    guild_id,    %% 所属平台公会编号
    mb_title_id = 0,  %%   公会内职位编号（如果没有加入平台上公会或没有设置此值时,值为 0）
    mb_number_id = 0  %%   公会内第几号人物编号（如果没有加入平台上公会或没有设置此值时,值为 0）
  }).

%%==============创建平台公会返回===================
-record(guild_create_ret, {
    code,
    msg,
    data           %% 如果成功，则未guild_ret
  }).

-record(guild_ret, {
    guild_id,       %% 生成的平台公会ID
    guild_name,     %% 生成的平台公会名称
    owner_user_id   %% 平台公会拥有者
  }).


%%===============退出公会操作返回 ===========
-record(simple_ret,{
     code,
     msg
  }).


%%================ 我们提供的API使用的数据结构，测试用 ==========
-record(query_role_info_ret, {
         code,
         msg,
         data       %% role_info_ret
  }).

-record(role_info_ret, {
      role_id,
      role_name,
      role_level
  }).

-record(passport_checkin_ret, {
      code,
      error,
      data                     %% 正常的话是user_info_ret 结构
  }).

-endif.

