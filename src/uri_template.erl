-module(uri_template).

%% uri_template: uri_template library's entry point.

-export([sub/2,sub/1,new/0,add/3]).

-export_type([expression_arguments/0]).

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

-type expression_argument() :: 	{nonempty_string(), {string, string()}} | {nonempty_string(), {list, [string()]}} | {nonempty_string(), {assoc_array, [{nonempty_string(),string()}|{nonempty_string()}]}} | {string()}. 
-opaque expression_arguments() :: [expression_argument()].

-type encoder_function() :: fun((string()) -> string()).
%% API

-spec new() -> expression_arguments().
new() -> [].

-spec add(expression_arguments(), string(),[]) -> expression_arguments()
	; (expression_arguments(), string(), nonempty_string()) -> expression_arguments()
	; (expression_arguments(), nonempty_string(),[nonempty_string()]) -> expression_arguments()
	; (expression_arguments(), nonempty_string(), [{nonempty_string(),nonempty_string()}|{nonempty_string}]) -> expression_arguments().
add(Args, Key, []) -> [{Key,[]}| Args];
add(Args, Key, [H|_] = Value) when is_integer(H) andalso H >= 0 -> [{Key, {string, Value}}|Args];
add(Args, Key, [H|_] = ValueList) when is_list(H) -> [{Key, {list, ValueList}}|Args];
add(Args, Key, [{K,V}|_] = AssocList) when is_list(K) andalso is_list(V) -> [{Key, {assoc_array, AssocList}}|Args].

-spec sub(string(), expression_arguments()) -> string().
sub([],_Args) -> "";
sub([${|Rem],Args) -> extract_expression(Args,Rem, []);
sub([C|Rem],Args) -> [C|sub(Rem, Args)].

-spec sub(string()) -> fun((expression_arguments()) -> string()).
sub(Template) -> fun(Args) -> sub(Template,Args) end.

%% Internals
-spec extract_expression(expression_arguments(),string(),string()) -> string().
extract_expression(_, [], _) -> erlang:error(malformed_template);
extract_expression(Args,[$}|Rem], Exp) -> parse_expression(Args, lists:reverse(Exp)) ++ sub(Rem, Args);
extract_expression(Args,[H|T],Exp) -> extract_expression(Args,T,[H|Exp]).

-spec parse_expression(expression_arguments(),string()) -> string().
parse_expression(_Args,[]) -> "";
parse_expression(Args, [$+|Exp]) -> process_expression({reserved, Args, variable_list(Exp)});
parse_expression(Args, [$#|Exp]) -> process_expression({fragment, Args, variable_list(Exp) });
parse_expression(Args, [$.|Exp]) -> process_expression({dot, Args, variable_list(Exp)});
parse_expression(Args, [$/|Exp]) -> process_expression({path, Args, variable_list(Exp)});
parse_expression(Args, [$;|Exp]) -> process_expression({path_param, Args, variable_list(Exp)});
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

-spec modifier_level4([string(), ...]) -> expression_variable().
modifier_level4([Var,Len]) -> case string:to_integer(Len) of {Int, []} when Int >= 0 ->  { prefix, Var, Int } end;
modifier_level4([Var]) -> case lists:reverse(Var) of 
				[$*|V] -> {explode, lists:reverse(V)};
				_ -> {Var}
			end.

-spec process_expression({atom(),expression_arguments(),[string()]}) -> string().
process_expression({expansion, Args, Vars}) -> string:join([process_variable(Var, Args) || Var <- Vars  ],",");
process_expression({reserved, Args, Vars}) -> string:join([process_variable(Var, Args, fun reserved_encode/1, ",") || Var <- Vars], ",");
process_expression({fragment, Args, Vars}) -> "#" ++ process_expression({reserved, Args, Vars});
process_expression({dot, Args, Vars}) -> "." ++ string:join([process_variable(Var, Args, ".") || Var <- Vars  ],".");
process_expression({path, Args, Vars}) -> "/" ++ string:join([process_variable(Var, Args, "/") || Var <- Vars  ],"/");
process_expression({path_param, Args, Vars}) -> 
	Separator = ";",
       	Separator ++ string:join([ process_param_variable(Var,Args,Separator, fun(Value) -> Value end) || Var <- Vars  ],Separator);
process_expression({query_component, Args, Vars}) ->
       	Separator = "&",
	"?" ++ string:join([ process_param_variable(Var,Args,Separator, fun(Value) -> Value ++ "=" end) || Var <- Vars  ],Separator);
process_expression({query_continuation, Args, Vars}) -> 
	Separator = "&",
       	Separator ++ string:join([ process_param_variable(Var,Args,Separator, fun(Value) -> Value ++ "=" end) || Var <- Vars  ],Separator).


-spec process_variable(expression_variable(),expression_arguments()) -> string().
process_variable(Var, Args) -> process_variable(Var, Args, fun encode/1,",").

process_variable(Var, Args, Separator) -> process_variable(Var, Args, fun encode/1, Separator).

-spec process_variable(expression_variable(), expression_arguments(), encoder_function(), string()) -> string().
process_variable({Var}, Args, Encoder,_Separator) -> process_value(proplists:get_value(Var,Args,{string,""}),Encoder);
%% TODO Handle lists of strings and assoc arrays
process_variable({explode, Var}, Args, Encoder, Separator) ->
	case proplists:lookup(Var,Args) of
		{_, {list, ValueList}} -> string:join([ Encoder(Value) || Value <- ValueList],Separator);
		%% TODO: Handle tuple with no value
		{_, {assoc_array, List}} -> string:join([ Encoder(Key) ++ "=" ++ Encoder(Value)  || {Key,Value} <- List  ],Separator);
		{_, {string, Value}} -> Encoder(Value);
		{_} -> "";
		none -> ""
	end;
process_variable({prefix, Var, Len}, Args, Encoder, _Separator) -> 
	{string, Value} = proplists:get_value(Var,Args,{string,""}),
	Encoder(string:substr(Value, 1, Len)).

-spec process_value(expression_argument(), encoder_function()) -> string().
process_value({string, Val},Encoder) -> Encoder(Val);
process_value({list, List},Encoder) -> string:join([Encoder(V) || V <- List],",");
process_value({assoc_array, Array},Encoder) -> string:join([process_assoc_array_value(Value, Encoder) || Value <- Array],","). 

-spec process_assoc_array_value({string()}, encoder_function()) -> string();
	({string(),string()}, encoder_function()) -> string().
process_assoc_array_value({_},_Encoder) -> "";
process_assoc_array_value({Key,Value}, Encoder) -> Encoder(Key) ++ "," ++ Encoder(Value).

%% Variable processing for parameter style expressions

-spec process_param_variable(expression_variable(),expression_arguments(),string(),encoder_function()) -> string().
process_param_variable({Var},Args,_Separator,HandleEmpty) ->
	Encoded = encode(Var),
       	case proplists:lookup(Var,Args) of 
		{_, {list, ValueList}} -> Encoded ++ "=" ++ string:join([encode(Value) || Value <- ValueList],",");
		{_,[]} -> HandleEmpty(Encoded);
		{_, {string, Value}} -> Encoded ++ "=" ++ encode(Value);
		%% TODO Handle single value tuples
		{_, {assoc_array, TupleList}} -> Encoded ++ "=" ++ string:join([encode(Key) ++ "," ++ encode(Value) || {Key, Value} <- TupleList],",");
		{_}  -> HandleEmpty(Encoded);
		none -> ""
	end;
process_param_variable({explode,Var},Args,Separator,HandleEmpty) -> 
	Encoded = encode(Var),
	case proplists:lookup(Var,Args) of
		{_, {list, ValueList}} -> string:join([ Encoded ++ "=" ++ encode(Value)  || Value <- ValueList ],Separator);
		{_, []} -> HandleEmpty(Encoded);
		{_, {string, Value}} -> Encoded ++ "=" ++ encode(Value);
		%% TODO Handle single value tuples
		{_, {assoc_array, TupleList}} -> string:join([ encode(Key) ++ "=" ++ encode(Value)  || {Key,Value} <- TupleList ],Separator);
		{_} -> HandleEmpty(Encoded);
		none -> ""
	end;
process_param_variable({prefix,Var, Len},Args,_Separator,HandleEmpty) -> 
	Encoded = encode(Var),
	case proplists:lookup(Var,Args) of
		{_, {list,_}} -> ""; %% Prefix is not applicable for lists
		{_, []} -> HandleEmpty(Encoded);
		{_, {string, Value}} -> Encoded ++ "=" ++ encode(string:substr(Value,1,Len));
		{_, {assoc_array,_}} -> ""; %% Prefix is not valid for assoc arrays
		{_} -> HandleEmpty(Encoded);
		none -> ""
	end.



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

-spec reserved_encode(string()) -> string().
reserved_encode([]) -> "";
reserved_encode([$%,H,L|T]) when 16#30 =< H andalso H =< 16#39 andalso 16#65 =< H andalso H =< 16#70 andalso 16#97 =< H andalso H =< 16#102 
		andalso 16#30 =< L andalso L =< 16#39 andalso 16#65 =< L andalso L =< 16#70 andalso 16#97 =< L andalso L =< 16#102 -> 
	[$%,H,L] ++ reserved_encode(T);
reserved_encode([H|T]) -> 
	case is_unreserved(H) orelse is_reserved(H) of 
		true -> [H] ++ reserved_encode(T);
		false -> percent_encode(H) ++ reserved_encode(T)
	end.

