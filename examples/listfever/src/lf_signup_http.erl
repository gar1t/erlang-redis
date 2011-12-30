-module(lf_signup_http).

-export([request/3]).

request(_, _, _) ->
    {ok, {html, render_page()}}.

render_page() ->
    lf_dtl:render("signup", [{title, lf_http:page_title("Signup")}]).
