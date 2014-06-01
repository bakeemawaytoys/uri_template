-module(uri_template).

%% uri_template: uri_template library's entry point.

-export([sub/2]).

-type alpha() :: 16#41..16#5A | 16#61..16#7A.
-type digit() :: 16#30..16#39.
-type hexdig() :: digit() | 65..70 | 97..102.
-type pct_encoded() :: 37 | hexdig().
%% - . _ ~
-type unreserved() :: alpha() | digit() | 45 | 46 | 95 | 126.
%% : / ? # [ ] @
-type gen_delims() :: 58 | 47 | 63 | 35 | 91 | 93 | 64.
%% ! $ & ' ( ) * + , ; =
-type sub_delims() :: 33 | 61 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 59 | 61.
-type reserved() :: gen_delims() | sub_delims().
-type ucschar() :: 16#A0..16#D7FF | 16#F900..16#FDCF | 16#FDF0..16#FFEF 
	| 16#10000..16#1FFFD | 16#20000..16#2FFFD | 16#30000..16#3FFFD
	| 16#40000..16#4FFFD | 16#50000..16#5FFFD | 16#60000..16#6FFFD
	| 16#70000..16#7FFFD | 16#80000..16#8FFFD | 16#90000..16#9FFFD
	| 16#A0000..16#AFFFD | 16#B0000..16#BFFFD | 16#C0000..16#CFFFD
	| 16#D0000..16#DFFFD | 16#E1000..16#EFFFD.
-type iprivate() :: 16#E000..16#F8FF | 16#F0000..16#FFFFD | 16#100000..16#10FFFD.

%% API

-spec sub(list({atom(),string()}|{atom(),list(string())}|{atom()}),string()) -> string().
sub(_Args,[]) -> "";
sub(Args,[${|Rem]) -> extract_expression(Args,Rem, []);
sub(Args,[C|Rem]) -> [C|sub(Args,Rem)].

%% Internals
extract_expression(_, [], _) -> erlang:error(malformed_template);
extract_expression(Args,[$}|Rem], Exp) -> parse_expression(Args,lists:reverse(Exp)) ++ sub(Args,Rem);
extract_expression(Args,[H|T],Exp) -> extract_expression(Args,T,[H|Exp]).

parse_expression(_Args,[]) -> "";
parse_expression(Args, [$+|Exp]) -> process_expression({reserved, Args, variable_list(Exp)});
parse_expression(Args, [$#|Exp]) -> process_expression({fragment, Args, variable_list(Exp) });
parse_expression(Args, [$.|Exp]) -> process_expression({named, Args, variable_list(Exp)});
parse_expression(Args, [$/|Exp]) -> process_expression({path, Args, variable_list(Exp)});
parse_expression(Args, [$;|Exp]) -> process_expression({pairs, Args, variable_list(Exp)});
parse_expression(Args, [$?|Exp]) -> process_expression({query_component, Args, variable_list(Exp)});
parse_expression(Args, [$&|Exp]) -> process_expression({query_continuation, Args, variable_list(Exp)});
parse_expression(_Args, [$=|_]) -> erlang:error(reserved_operator);
parse_expression(_Args, [$,|_]) -> erlang:error(reserved_operator);
parse_expression(_Args, [$!|_]) -> erlang:error(reserved_operator);
parse_expression(_Args, [$@|_]) -> erlang:error(reserved_operator);
parse_expression(_Args, [$||_]) -> erlang:error(reserved_operator);
parse_expression(Args, Exp) -> process_expression({expansion, Args, variable_list(Exp)}). 

-type expression_variable() :: {nonempty_string()} | {explode, nonempty_string()} | {prefix, nonempty_string(),integer()}.

-spec variable_list(string()) -> [expression_variable()].
variable_list(Exp) -> [varspec(Var)|| Var  <- string:tokens(Exp,",")].

-spec varspec(string()) -> expression_variable().
varspec(Var) -> modifier_level4(string:tokens(Var,":")).

-spec modifier_level4(string()) -> expression_variable().
modifier_level4([Var,Len]) -> case string:to_integer(Len) of {Int, []} when Int >= 0 ->  { prefix, Var, Int } end;
modifier_level4([Var]) -> case lists:reverse(Var) of 
				[$*|V] -> {explode, V};
				_ -> {Var}
			end.

process_expression({expansion, Args, Vars}) -> string:join([ encode(process_variable(Var,Args)) || Var <- Vars  ],",");
process_expression({reserved, Args, Vars}) -> string:join([reserved_encode(process_variable(Var,Args)) || Var <- Vars], ",");
process_expression({fragment, Args, Vars}) -> "#" ++ process_expression({reserved, Args, Vars});
process_expression(Exp) -> "".

process_variable({Var},Args) ->proplists:get_value(Var,Args,"");
process_variable({explode,Var},Args) -> proplists:get_value(Var,Args,"");
process_variable({prefix,Var,Len},Args) -> string:substr(proplists:get_value(Var,Args,""), 0, Len).

-spec is_alpha(char()) -> boolean(). 
is_alpha(C) -> 
	(16#41 =< C andalso C =< 16#5A) orelse (16#61 =< C andalso C =< 16#7A).

-spec is_digit(char()) -> boolean().
is_digit(C) ->
	16#30 =< C andalso C =< 16#39.

-spec is_unreserved(char()) -> boolean().
is_unreserved($-) -> true;
is_unreserved($.) -> true;
is_unreserved($_) -> true;
is_unreserved($~) -> true;
is_unreserved(C) -> is_alpha(C) orelse is_digit(C).

-spec is_gen_delim(char()) -> boolean().
is_gen_delim($:) -> true;
is_gen_delim($/) -> true;
is_gen_delim($?) -> true;
is_gen_delim($#) -> true;
is_gen_delim($[) -> true;
is_gen_delim($]) -> true;
is_gen_delim($@) -> true;
is_gen_delim(_) -> false.

-spec is_sub_delim(char()) -> boolean().
is_sub_delim($!) -> true;
is_sub_delim($$) -> true;
is_sub_delim($&) -> true;
is_sub_delim($') -> true;
is_sub_delim($() -> true;
is_sub_delim($)) -> true;
is_sub_delim($*) -> true;
is_sub_delim($+) -> true;
is_sub_delim($,) -> true;
is_sub_delim($;) -> true;
is_sub_delim($=) -> true;
is_sub_delim(_) -> false.

-spec is_reserved(char()) -> boolean().
is_reserved(C) ->
	is_gen_delim(C) orelse is_sub_delim(C).

-spec percent_encode(char()) -> string().
percent_encode(C) -> case is_unreserved(C) of 
		       	false -> binary:bin_to_list(erlang:iolist_to_binary(io_lib:format("%~2.16.0B", [C])));
			true -> [C]
			end.

-spec encode(string()) -> string().
encode([]) -> "";
encode([H|T]) -> percent_encode(H) ++ encode(T).

reserved_encode([]) -> "";
reserved_encode([$%,H,L|T]) when 16#30 =< H andalso H =< 16#39 andalso 16#65 =< H andalso H =< 16#70 andalso 16#97 =< H andalso H =< 16#102 
		andalso 16#30 =< L andalso L =< 16#39 andalso 16#65 =< L andalso L =< 16#70 andalso 16#97 =< L andalso L =< 16#102 -> 
	[$%,H,L] ++ reserved_encode(T);
reserved_encode([H|T]) -> 
	case is_unreserved(H) orelse is_reserved(H) of 
		true -> [H] ++ reserved_encode(T);
		false -> percent_encode(H) ++ reserved_encode(T)
	end.
