-module(lf_http).

-include_lib("modlib/include/webapp.hrl").

-export([request/3, page_title/1]).

request(Method, "/signup" ++ Path, Info) ->
    lf_signup_http:request(Method, Path, Info);
request(_, _, _) ->
    not_handled.

page_title(Page) -> Page ++ " - Listfever".
