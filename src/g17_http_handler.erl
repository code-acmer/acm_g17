-module(g17_http_handler).

-export([init/2]).
-export([terminate/3]).

%% -include("define_logger.hrl").
-include("define_http_server.hrl").
-define(OUT(Format, Args),
        io:format("~w:~w "++ Format, [?MODULE, ?LINE]++Args)).

init(Req, Opts) ->
    Req1 = execute(Req),
    {ok, Req1, Opts}.

terminate(_Reason, _Req, _State) ->
    ok.

execute(Req) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            %% ?OUT("excute Req: ~p~n",[Req]),
            handle_get_req(Req);
        %% <<"POST">> ->
        %%     execute_post(Req);
        _ ->
            ?REPLY_ERROR(Req, <<"only get method allow">>)
    end.

handle_get_req(Req) ->
    Path = cowboy_req:path(Req),
    %?QPRINT(Path),
    KeyValues = cowboy_req:parse_qs(Req),
    case route(Path) of
        notfound ->
            g17_util:http_reply(?HTTP_NOT_FOUND, Req, []);
        Action ->
            ?OUT("Action : ~w, KeyValues: ~p~n",[Action, KeyValues]),
            case handle(Action, KeyValues) of                
                {ok, Reply} ->
                    g17_util:http_reply(?HTTP_OK, Req, Reply);
                {error, ErrorMsg} ->
                    g17_util:http_reply(?HTTP_BAD_REQUEST, Req, hmisc:to_binary(ErrorMsg));
                _ ->
                    g17_util:http_reply(?HTTP_INTERNAL_SERVER_ERROR, Req, [])
            end
    end.

get_config(Key) ->
    {ok, Config} = application:get_env(g17, config),
    proplists:get_value(Key, Config).

%% execute_post(Req) ->
%%     Path = cowboy_req:path(Req),
%%                                                 %?QPRINT(Path),
%%                                                 %?QPRINT(Req),
%%     {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
%%                                                 %?QPRINT(PostVals),
%%     case route(Path, PostVals) of
%%         {fail, Reason} ->
%%             ?REPLY_ERROR(Req, Reason);
%%         {Mod, FunName, Args} ->
%%             %% ?PRINT("read apply~n", []),
%%             case apply(Mod, FunName, Args) of
%%                 {fail, R} ->
%%                     ?REPLY_ERROR(Req2, R);
%%                 Other ->
%%                     ?REPLY_ERROR(Req2, Other)
%%             end
%%     end.

route(<<"/role_info.php">>) ->
    get_role_info;
route(<<"/pay_callback.php">>) ->
    pay_callback;
route(<<"/active_user.php">>) ->
    get_active_user;
route(<<"/guild_apply_reject_notify.php">>) ->
    guild_apply_reject_notify;
route(<<"/guild_newbie_notify.php">>) ->
    guild_newbie_notify;
route(<<"/quit_guild_notify.php">>) ->
    quit_guild_notify;
route(_) ->
    notfound.
    
%% route(<<"/state_get_player">>, KeyValues) ->
%%     case check_value_valid(<<"player_id">>, KeyValues) of
%%         fail ->
%%             {fail, <<"输入错误"/utf8>>};
%%         {ok, Val} ->
%%             {server_state_handler, state_get_player, [Val]}
%%     end;
%% route(<<"/state_get_bag">>, KeyValues) ->
%%     case check_value_valid(<<"player_id">>, KeyValues) of
%%         fail ->
%%             {fail, <<"输入错误"/utf8>>};
%%         {ok, Val} ->
%%             {server_state_handler, state_get_bag, [Val]}
%%     end;
%% route(Other, _) ->
%%     ?WARNING_MSG("Other ~p~n", [Other]),
%%     {fail, <<"error api">>}.

check_value_valid(Value, KeyValues) ->
    case proplists:get_value(Value, KeyValues) of
        undefined ->
                                                %?WARNING_MSG("http not recv value ~s~n", [binary_to_list(Value)]),
            fail;
        Bin ->
            case catch binary_to_integer(Bin) of
                Val when is_integer(Val) ->
                    {ok, Val};
                _ ->
                    ?OUT("invalid value ~p~n", [Bin]),
                    fail
            end
    end.



%% http://s1.zwj.17g.com/role_info.php?user_id=$user_id&server_id=$server_id&time=$time&sign=$sign
%% md5(user_id=$user_id&server_id=$server_id&time=$time&key=$key)
handle(get_role_info, KeyValues) ->
    MsgDef = [
              {user_id,   require, int}, 
              {server_id, require, int},
              {time,      require, int},
              {sign,      require, string}
             ],
    SignDef = [user_id, server_id, time, key],
    ReplyDef = [{code, int}, 
                {data,[
                       {role_id, int}, 
                       {role_name, string}, 
                       {role_level, int}
                      ]},
                {msg, string}],
    case check_http_req(MsgDef, KeyValues) of
        {ok, ConvertedKeyValue} ->
            ?OUT("ConvertedKeyValue : ~p~n",[ConvertedKeyValue]),
            case check_http_sign(SignDef, KeyValues, get_config(simple_key)) of
                ok ->
                    case lib_g17_api:get_role_info(get_value(user_id, ConvertedKeyValue),
                                                   get_value(server_id, ConvertedKeyValue)) of
                        [] ->
                            ?OUT("RoleNotfound~n",[]),
                            Reply = build_reply(ReplyDef, [-1, [], "ERROR"]),
                            {ok, Reply};
                        Roles ->
                            Reply = build_reply(ReplyDef, [0, Roles, "SUCCESS"]),
                            {ok, Reply}
                    end;
                {error, error_sign} ->
                    %% {error, hmisc:to_binary(ErrorMsg)}
                    Reply = build_reply(ReplyDef, [-3, [], "ERROR"]),
                    {ok, Reply}
            end;
        {error, timeout} ->
            Reply = build_reply(ReplyDef, [-4, [], "TIMEOUT"]),
            {ok, Reply};
        {error, error_param} ->
            Reply = build_reply(ReplyDef, [-2, [], "ERROR"]),
            {ok, Reply}
    end;
%% http://s1.zwj.17g.com/order_submit.php?order_id=$order_id&order_type=$order_type&user_id=$user_id&server_id=$server_id&coin=$coin&extra=$extra&money=$money&time=$time&sign=$sign&role_id=$role_idhttp://s1.zwj.17g.com/order_submit.php?order_id=$order_id&order_type=$order_type&user_id=$user_id&server_id=$server_id&coin=$coin&extra=$extra&money=$money&time=$time&sign=$sign&role_id=$role_id
%% md5(order_id=$order_id&order_type=$order_type&user_id=$user_id&server_id=$server_id&coin=$coin&extra=$extra&money=$money&time=$time&key=$key)
handle(pay_callback, KeyValues) ->
    MsgDef = [
              {order_id,       require,     string},
              {order_type,     require,     int},
              {user_id,        require,     int},
              {server_id,      require,     int},
              {coin,           require,     int},
              {extra,          require,     int},
              {money,          require,     float},
              {time,           require,     int},
              {sign,           require,     string},
              {role_id,        require,     int}
             ],
    SignDef = [order_id, order_type, user_id, server_id, coin, extra, money, time, key],
    ReplyDef = [{code, int}, {msg, string}],
    case check_http_req(MsgDef, KeyValues) of
        {ok, ConvertedKeyValue} ->
            ?OUT("ConvertedKeyValue : ~p~n",[ConvertedKeyValue]),
            case check_http_sign(SignDef, KeyValues, get_config(money_key)) of
                ok ->
                    case lib_g17_api:pay_callback(get_value(order_id, ConvertedKeyValue),
                                                  get_value(order_type, ConvertedKeyValue),
                                                  get_value(user_id, ConvertedKeyValue),
                                                  get_value(server_id, ConvertedKeyValue),
                                                  get_value(coin, ConvertedKeyValue),
                                                  get_value(extra, ConvertedKeyValue),
                                                  get_value(money, ConvertedKeyValue),
                                                  get_value(role_id, ConvertedKeyValue)
                                                 ) of
                        {error, role_notfound} ->
                            ?OUT("RoleNotfound~n",[]),
                            Reply = build_reply(ReplyDef, [-2, "USER_NOTFOUNT"]),
                            {ok, Reply};
                        {error, error_aleady_pay} ->
                            Reply = build_reply(ReplyDef, [-5, "ALEADY_PAY"]),
                            {ok, Reply};
                        ok ->
                            Reply = build_reply(ReplyDef, [1, "SUCCESS"]),
                            {ok, Reply}
                    end;
                {error, error_sign} ->
                    %% {error, hmisc:to_binary(ErrorMsg)}
                    Reply = build_reply(ReplyDef, [-4, "ERROR"]),
                    {ok, Reply}
            end;
        {error, timeout} ->
            Reply = build_reply(ReplyDef, [-6, "TIMEOUT"]),
            {ok, Reply};
        {error, error_param} ->
            Reply = build_reply(ReplyDef, [-8, "ERROR"]),
            {ok, Reply}
    end;
%% http://api.zwj.17g.com/active_user.php?game_id=$game_id&score=$score&page_rows_num=$page_rows_num&page_id=$page_id&time=$time&sign=$sign
%% md5(game_id=$game_id&score=$score&page_rows_num=$page_rows_num&page_id=$page_id&time=$time&key=$key)
handle(get_active_user, KeyValues) ->
    MsgDef = [
              {game_id,        require,     int},
              {score,          require,     int},
              {page_rows_num,  require,     int},
              {page_id,        require,     int},
              {time,           require,     int},
              {sign,           require,     string}              
             ],
    SignDef = [game_id, score, page_rows_num, page_id, time, key],
    ReplyDef = [
                {code, int}, {msg, string}, {data, {{score, int}, {page_rows_num, int}, {page_id, int}, {total, int}, {list, [int]}}}
               ],
    case check_http_req(MsgDef, KeyValues) of
        {ok, ConvertedKeyValue} ->
            case check_http_sign(SignDef, KeyValues, get_config(simple_key)) of
                ok ->
                    Score       = get_value(score, ConvertedKeyValue),
                    PageRowsNum = get_value(page_rows_num, ConvertedKeyValue),
                    PageId      = get_value(page_id, ConvertedKeyValue),
                    case lib_g17_api:get_active_users(Score, PageRowsNum, PageId) of
                        {error, xx}->
                            {ok, role_nofound};
                        {ok, Count, UserIds} ->
                            Reply = build_reply(ReplyDef, [1, "SUCCESS", {Score, PageRowsNum, PageId, Count, UserIds}]),
                            {ok, Reply}
                    end;
                {error, error_sign} ->
                    %% {error, hmisc:to_binary(ErrorMsg)}
                    Reply = build_reply(ReplyDef, [-4, "ERROR"]),
                    {ok, Reply}
            end;
        {error, timeout} ->
            Reply = build_reply(ReplyDef, [-6, "TIMEOUT", {0, 0, 0, 0, []}]),
            {ok, Reply};
        {error, error_param} ->
            Reply = build_reply(ReplyDef, [-8, "ERROR", {0, 0, 0, 0, []}]),
            {ok, Reply}
    end;
%% http://api.zwj.17g.com/guild_apply_reject_notify.php?data=$data&time=$time&sign=$sign
%% md5(data=$data&time=$time&key=$key)
handle(guild_apply_reject_notify, KeyValues) ->
    MsgDef = [
              {data,           require,     string},
              {time,           require,     int},
              {sign,           require,     string}              
             ],
    SignDef = [data, time, key],
    ReplyDef = [{code, int}, {msg, string}],
    case check_http_req(MsgDef, KeyValues) of
        {ok, ConvertedKeyValue} ->
            case check_http_sign(SignDef, KeyValues, get_config(simple_key)) of
                ok ->
                    Rejects = convert_2_tuples(get_value(data, ConvertedKeyValue)),
                    case lib_g17_api:guild_apply_reject_notify(Rejects) of
                        [] ->
                            {ok, role_nofound};
                        ok ->
                            Reply = build_reply(ReplyDef, [1, "SUCCESS"]),
                            {ok, Reply}
                    end;
                {error, error_sign} ->
                    %% {error, hmisc:to_binary(ErrorMsg)}
                    Reply = build_reply(ReplyDef, [-4, "ERROR"]),
                    {ok, Reply}
            end;
        {error, timeout} ->
            Reply = build_reply(ReplyDef, [-6, "TIMEOUT"]),
            {ok, Reply};
        {error, error_param} ->
            Reply = build_reply(ReplyDef, [-8, "ERROR"]),
            {ok, Reply}
    end;
%% http://api.zwj.17g.com/guild_newbie_notify.php?data=$data&time=$time&sign=$sign
handle(guild_newbie_notify, KeyValues) ->
    MsgDef = [
              {data,           require,     string},
              {time,           require,     int},
              {sign,           require,     string}              
             ],
    SignDef = [data, time, key],
    ReplyDef = [{code, int}, {msg, string}],
    case check_http_req(MsgDef, KeyValues) of
        {ok, ConvertedKeyValue} ->
            case check_http_sign(SignDef, KeyValues, get_config(simple_key)) of
                ok ->
                    Tuples = convert_2_tuples(get_value(data, ConvertedKeyValue)),
                    case lib_g17_api:guild_newbie_notify(Tuples) of
                        [] ->
                            {ok, role_nofound};
                        ok ->
                            Reply = build_reply(ReplyDef, [1, "SUCCESS"]),
                            {ok, Reply}
                    end;
                {error, error_sign} ->
                    %% {error, hmisc:to_binary(ErrorMsg)}
                    Reply = build_reply(ReplyDef, [-4, "ERROR"]),
                    {ok, Reply}
            end;
        {error, timeout} ->
            Reply = build_reply(ReplyDef, [-6, "TIMEOUT"]),
            {ok, Reply};
        {error, error_param} ->
            Reply = build_reply(ReplyDef, [-8, "ERROR"]),
            {ok, Reply}
    end;
%% http://api.zwj.17g.com/quit_guild_notify.php?data=$data&time=$time&sign=$sign
handle(quit_guild_notify, KeyValues) ->
    MsgDef = [
              {data,           require,     string},
              {time,           require,     int},
              {sign,           require,     string}              
             ],
    SignDef = [data, time, key],
    ReplyDef = [{code, int}, {msg, string}],
    case check_http_req(MsgDef, KeyValues) of
        {ok, ConvertedKeyValue} ->
            case check_http_sign(SignDef, KeyValues, get_config(simple_key)) of
                ok ->
                    Tuples = convert_2_tuples(get_value(data, ConvertedKeyValue)),
                    case lib_g17_api:quit_guild_notify(Tuples) of
                        [] ->
                            {ok, role_nofound};
                        ok ->
                            Reply = build_reply(ReplyDef, [1, "SUCCESS"]),
                            {ok, Reply}
                    end;
                {error, error_sign} ->
                    %% {error, hmisc:to_binary(ErrorMsg)}
                    Reply = build_reply(ReplyDef, [-4, "ERROR"]),
                    {ok, Reply}
            end;
        {error, timeout} ->
            Reply = build_reply(ReplyDef, [-6, "TIMEOUT"]),
            {ok, Reply};
        {error, error_param} ->
            Reply = build_reply(ReplyDef, [-8, "ERROR"]),
            {ok, Reply}
    end;
%% http://api.zwj.17g.com/user_info_notify.php?data=$data&time=$time&sign=$sign
handle(user_info_notify, KeyValues) ->
    MsgDef = [
              {data,           require,     string},
              {time,           require,     int},
              {sign,           require,     string}              
             ],
    SignDef = [data, time, key],
    ReplyDef = [],
    case check_http_req(MsgDef, KeyValues) of
        {ok, ConvertedKeyValue} ->
            case check_http_sign(SignDef, KeyValues, get_config(simple_key)) of
                ok ->
                    case mod_g17_srv:user_info_notify(get_value(data, ConvertedKeyValue)
                                                     ) of
                        [] ->
                            {ok, role_nofound};
                        Roles ->
                            {ok, []}
                    end;
                {error, ErrorMsg} ->
                    {error, hmisc:to_binary(ErrorMsg)}
            end;
        {error, ErrorMsg} ->
            {error, hmisc:to_binary(ErrorMsg)}
    end;
%% http://api.zwj.17g.com/get_cdkey.php?server_id=$server_id&user_id=$user_id&type=$type&time=$time&sign=$sign
handle(get_cdkey, KeyValues) ->
    MsgDef = [
              {server_id,      require,     int},
              {user_id,        require,     int},
              {type,           require,     int},
              {time,           require,     int},
              {sign,           require,     string}              
             ],
    SignDef = [server_id, user_id, type, time, key],
    ReplyDef = [],
    case check_http_req(MsgDef, KeyValues) of
        {ok, ConvertedKeyValue} ->
            case check_http_sign(SignDef, KeyValues, get_config(simple_key)) of
                ok ->
                    case mod_g17_srv:get_cdkey(get_value(server_id, ConvertedKeyValue),
                                               get_value(user_id, ConvertedKeyValue),
                                               get_value(type, ConvertedKeyValue)
                                              ) of
                        [] ->
                            {ok, role_nofound};
                        Roles ->
                            {ok, []}
                    end;
                {error, ErrorMsg} ->
                    {error, hmisc:to_binary(ErrorMsg)}
            end;
        {error, ErrorMsg} ->
            {error, hmisc:to_binary(ErrorMsg)}
    end;
handle(Action, Req) ->
    ?OUT("http_handler ignored Action:~w, Req : ~p~n", [Action, Req]),
    {error, ?HTTP_INTERNAL_SERVER_ERROR}.

%% 检查参数 必须字段与字段转换成功与否
check_http_req(MsgDef, Req) ->
    check_http_req(MsgDef, Req, []).

check_http_req([], _Req, ConvertedKeyValue) ->
    {ok, ConvertedKeyValue};
check_http_req([Param|Tail], Req, ConvertedKeyValue) ->
    case convert_param(Param, Req) of
        {error, _} ->
            {error, error_param};
        {ok, NewKeyValue} ->
            case NewKeyValue of
                {time, Timestamp} when is_integer(Timestamp) ->
                    Now = hmisctime:unixtime(),
                    ?OUT("Now:~w, Timestamp:~w~n",[Now, Timestamp]),
                    if
                        Timestamp < Now + 300 andalso
                        Timestamp > Now - 300 ->
                            check_http_req(Tail, Req, [NewKeyValue|ConvertedKeyValue]);
                        true -> 
                            {error, timeout}
                    end;
                _  ->
                    check_http_req(Tail, Req, [NewKeyValue|ConvertedKeyValue])
            end
    end.

convert_param({Param, require, Type}, KeyValues) ->
    BinaryParam = list_to_binary(atom_to_list(Param)),
    case lists:keyfind(BinaryParam, 1, KeyValues) of
        false ->
            {error, error_require_not_found};
        {_, Value} ->
            ?OUT("conver_param OriValue :~p~n ", [Value]),
            TermValue = binary_to_list(Value),
            case decode_value(Type, TermValue) of
                error ->
                    {error, error_param};
                NewValue->
                    {ok, {Param, NewValue}}
            end
    end;
convert_param({Param, optional, Type}, KeyValues) ->
    BinaryParam = list_to_binary(atom_to_list(Param)),
    case lists:keyfind(BinaryParam, 1, KeyValues) of
        false ->
            {ok, {Param, undefined}};
        {_, Value}->
            TermValue = hmisc:bitstring_to_term(Value),
            case decode_value(Type, TermValue) of
                error ->
                    {ok, {Param, undefined}};
                NewValue ->
                    {ok, {Param, NewValue}}
            end
    end.

check_http_sign(SignDef, KeyValues, MyKey) ->
    check_http_sign(SignDef, KeyValues, MyKey, "").

check_http_sign([], KeyValues, _MyKey, JoinedStr) ->
    SignKey = list_to_binary("sign"),
    case lists:keyfind(SignKey, 1, KeyValues) of
        false ->
            {error, error_sign};
        {_, Value}->
            TermValue = binary_to_list(Value),
            Md5 = hmisc:md5(JoinedStr),
            ?OUT("Sign : ~w,  OrgStr:~s, Md5 : ~w~n",[TermValue, JoinedStr, Md5]),
            if
                TermValue =:= Md5 ->
                    ok;
                true ->
                    {error, error_sign}
            end
    end;
check_http_sign([Next|REST], KeyValues, MyKey, JoinedStr) ->
    case Next of
        key ->
            AddedString = "key="++MyKey,
            check_http_sign(REST, KeyValues, MyKey, JoinedStr ++ AddedString);
        Key ->
            BinaryKey = list_to_binary(atom_to_list(Key)),
            case lists:keyfind(BinaryKey, 1, KeyValues) of
                false ->
                    check_http_sign(REST, KeyValues, MyKey, JoinedStr);
                {_, Value} ->     
                    ?OUT("Value: ~p~n",[ Value]),
                    StringValue = case binary_to_list(Value) of
                                      undefined ->
                                          "";
                                      V when is_list(V) ->
                                          ?OUT("binary_to_list V :~p~n", [V]),
                                          %% g17_util:urlencode(V)
                                          V
                                  end,
                    AddedString = atom_to_list(Key) ++ "=" ++ StringValue ++ "&",
                    check_http_sign(REST, KeyValues, MyKey, JoinedStr ++ AddedString)
            end
    end.

get_value(Key, KeyValues) ->
    case lists:keyfind(Key, 1, KeyValues) of
        {_, Value} ->
            Value;
        _ ->
            undefined
    end.


decode_value(Type, Value) when is_atom(Type) ->
    ?OUT("convert_value Type : ~p, Value : ~p ~n",[Type, Value]),
    case Type of
        int ->
            hmisc:to_integer(Value);
        string ->
            hmisc:to_string(Value);
        _ ->
            hmisc:to_string(Value)
    end.

encode_value(Type, Value) when is_atom(Type) ->
    %% ?OUT("convert_value Type : ~w, Value : ~w ~n",[Type, Value]),
    case Type of
        int ->
            hmisc:to_integer(Value);
        string ->
            hmisc:to_binary(Value);
        _ ->
            hmisc:to_string(Value)
    end;
encode_value([Type], Values) when is_atom(Type) andalso is_list(Values) ->
    %% ?OUT("Type : ~w, Values : ~w~n",[Type, Values]),
    lists:map(fun(Value) ->
                      encode_value(Type, Value)
              end, Values);
encode_value(ListType, Values) when is_list(ListType) andalso is_list(Values) ->
    %% ?OUT("ListType : ~w, Values : ~w~n",[ListType, Values]),
    lists:map(fun(Value) ->
                      {obj, lists:zipwith(fun encode_reply/2, ListType, tuple_to_list(Value))}
              end, Values);
encode_value(ObjType, Value) when is_tuple(ObjType) andalso is_tuple(Value) ->
    %% ?OUT("ObjType : ~w, Value : ~w~n",[ObjType, Value]),
    {obj, lists:zipwith(fun encode_reply/2, tuple_to_list(ObjType), tuple_to_list(Value))}.


encode_reply({FieldName, FieldType}, Value) ->
    NewValue = encode_value(FieldType, Value),
    {FieldName, NewValue}.

build_reply(ReplyDef, Reply) ->
    ?OUT("ReplyDef : ~w, Reply : ~p~n",[ReplyDef, Reply]),
    rfc4627:encode({obj, lists:zipwith(fun encode_reply/2, ReplyDef, Reply)}).


%% ($user_id-$guild_id) 1001-10_1002-11_1003-10
convert_2_tuples(String) ->
    Tuples = string:tokens(String, "_"),
    ?OUT("Tuples : ~w~n",[Tuples]),
    lists:foldl(fun(Tuple, Rets) ->
                        case string:tokens(Tuple, "-") of
                            Values when length(Values) >=2 ->
                                [{encode_value(int, lists:nth(1, Values)), 
                                  encode_value(int, lists:nth(2, Values))
                                 }|Rets];
                            _ ->
                                Rets
                        end
                end, [], Tuples).
