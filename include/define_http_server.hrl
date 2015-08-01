-ifndef(DEFINE_HTTP_SERVER_HRL).
-define(DEFINE_HTTP_SERVER_HRL, ture).

-define(HTTP_OK, 200).
-define(HTTP_FOUND, 302).

-define(HTTP_BAD_REQUEST, 400).
-define(HTTP_NOT_FOUND, 404).
-define(HTTP_METHOD_NOT_ALLOWED, 405).

-define(HTTP_INTERNAL_SERVER_ERROR, 500).

-define(REPLY_ERROR(REQ, CONTENT),
        cowboy_req:reply(?HTTP_BAD_REQUEST, [?CONTENT_JSON], CONTENT, REQ)).
-define(REPLY_OK(REQ, CONTENT),
        cowboy_req:reply(?HTTP_OK, [?CONTENT_JSON], CONTENT, REQ)).

-define(REPLY_JSON(CODE, REQ, CONTENT),        
        cowboy_req:reply(CODE, [?CONTENT_JSON], CONTENT, REQ)).



-define(CONTENT_HTML, {<<"content-type">>, <<"text/html; charset=utf-8">>}).
-define(CONTENT_JSON, {<<"content-type">>, <<"application/json; charset=utf-8">>}).
-define(CONTENT_PLAIN,{<<"content-type">>, <<"text/plain; charset=utf-8">>}).

-endif.
