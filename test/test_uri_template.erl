-module(test_uri_template).
-include_lib("eunit/include/eunit.hrl").

level_1_test_() -> run_tests_for_level(1).
level_2_test_() -> run_tests_for_level(2).
level_3_test_() -> run_tests_for_level(3).
level_4_test_() -> run_tests_for_level(4).
%%additional_examples_1_test_() -> run_additional_examples(1).

run_tests_for_level(Level) ->
	{Variables, Tests} = load_tests_for_level(Level),
	[ create_test_fun(Template, Variables, Expected) || {Template, Expected} <- Tests ].

run_additional_examples(ExampleSetNumber) ->
	{Variables, Tests} =  load_tests_for_additional_examples(ExampleSetNumber),
	[ create_test_fun(Template, Variables, Expected) || {Template, Expected} <- Tests ]. 

create_test_fun(Template, Variables, Expected) ->
	{"Template: " ++ Template,fun() -> 
		Result = uri_template:sub(Template,Variables),
		?debugVal(Template),
		?debugVal(Variables),
		?debugVal(Result),
		?debugVal(sets:to_list(Expected)),
		?assertEqual(true, sets:is_element(Result,Expected)) 
	end}.


%% Functions for loading and converting the tests data.
convert_test_cases(Tests) -> [convert_test_case(Test) || Test <- Tests].
convert_test_case([Template,Data]) -> {binary_to_list(Template), convert_expected_data(Data)}.

convert_expected_data(Data) when is_binary(Data) -> sets:from_list([binary_to_list(Data)]);
convert_expected_data(Data) when is_list(Data) -> sets:from_list([ binary_to_list(D) || D <- Data]).

create_template_arguments(Args, []) -> Args;
create_template_arguments(Args, [{Key, Value}|T]) -> create_template_arguments(uri_template:add(Args, Key, Value),T).

convert_variables(Variables) -> [convert_variable(V) || V <- Variables].
convert_variable({Name, Value}) when is_integer(Value) -> {binary_to_list(Name), integer_to_list(Value)};
convert_variable({Name, Value}) when is_float(Value) -> {binary_to_list(Name), float_to_list(Value,[{decimals,3},compact])};
convert_variable({Name, Value}) when is_binary(Value)  -> {binary_to_list(Name),binary_to_list(Value)};
convert_variable({Name, Values}) when is_list(Values) -> {binary_to_list(Name), [convert_variable_list_entry(V) || V <- Values]}.
convert_variable_list_entry({Key,Value}) when is_binary(Value) -> {binary_to_list(Key),binary_to_list(Value)};
convert_variable_list_entry({Key,Value}) when is_list(Value) -> {binary_to_list(Key), [{binary_to_list(K), binary_to_list(V)} || {K,V} <- Value]};
convert_variable_list_entry(Value) when is_binary(Value) -> binary_to_list(Value).

load_tests_for_level(1) -> load_tests_for_name(<<"Level 1 Examples">>);
load_tests_for_level(2) -> load_tests_for_name(<<"Level 2 Examples">>);
load_tests_for_level(3) -> load_tests_for_name(<<"Level 3 Examples">>);
load_tests_for_level(4) -> load_tests_for_name(<<"Level 4 Examples">>).
load_tests_for_additional_examples(1) -> load_tests_for_name(<<"Additional Examples 1">>, fun load_extended_test_data/0);
load_tests_for_additional_examples(2) -> load_tests_for_name(<<"Additional Examples 2">>, fun load_extended_test_data/0);
load_tests_for_additional_examples(3) -> load_tests_for_name(<<"Additional Examples 3: Empty Variables">>, fun load_extended_test_data/0);
load_tests_for_additional_examples(4) -> load_tests_for_name(<<"Additional Examples 4: Numeric Keys">>, fun load_extended_test_data/0).
load_tests_for_name(Name) -> load_tests_for_name(Name, fun load_spec_test_data/0).
load_tests_for_name(Name, LoadFunction) ->
	LoadedData = LoadFunction(),
	{_,LevelData}  = proplists:lookup(Name,LoadedData),
	{_,VariablesData} = proplists:lookup(<<"variables">>,LevelData),
	{_,TestcasesData} = proplists:lookup(<<"testcases">>,LevelData), 
	{create_template_arguments(uri_template:new(), convert_variables(VariablesData)),convert_test_cases(TestcasesData)}.

load_extended_test_data() -> load_data("extended-tests.data").
load_negative_test_data() -> load_data("negative-tests.data"). 
load_spec_test_data() -> load_data("spec-examples.data").
data_file(FileName) -> "../test/" ++ FileName.
load_data(FileName) -> 	
	{ok, Bin} = file:read_file(data_file(FileName)),
	binary_to_term(Bin).		
