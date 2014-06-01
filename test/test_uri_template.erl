-module(test_uri_template).
-include_lib("eunit/include/eunit.hrl").

level_1_test_() -> run_tests(1).
level_2_test_() -> run_tests(2).

run_tests(Level) ->
	{Variables, Tests} = load_tests_for_level(Level),
	[ create_test_fun(Template, Variables, Expected) || {Template, Expected} <- Tests ].

create_test_fun(Template, Variables, Expected) ->
	fun() -> 
		Result = uri_template:sub(Variables,Template),
		?debugVal(Result),
		?debugVal(sets:to_list(Expected)),
		?assertEqual(true, sets:is_element(Result,Expected)) 
	end.


%% Functions for loading and converting the tests data.
convert_test_cases(Tests) -> [convert_test_case(Test) || Test <- Tests].
convert_test_case([Template,Data]) -> {binary_to_list(Template), convert_expected_data(Data)}.

convert_expected_data(Data) when is_binary(Data) -> sets:from_list([binary_to_list(Data)]);
convert_expected_data(Data) when is_list(Data) -> sets:from_list([ binary_to_list(D) || D <- Data]).

convert_variables(Variables) -> [convert_variable(V) || V <- Variables].
convert_variable({Name, Value}) when is_binary(Value)  -> {binary_to_list(Name),binary_to_list(Value)};
convert_variable({Name, Values}) when is_list(Values) -> {binary_to_list(Name), [convert_variable_list_entry(V) || V <- Values]}.
convert_variable_list_entry({Key,Value}) -> {binary_to_list(Key),binary_to_list(Value)};
convert_variable_list_entry(Value) when is_list(Value) -> binary_to_list(Value).

load_tests_for_level(1) -> load_tests_for_name(<<"Level 1 Examples">>);
load_tests_for_level(2) -> load_tests_for_name(<<"Level 2 Examples">>);
load_tests_for_level(3) -> load_tests_for_name(<<"Level 3 Examples">>);
load_tests_for_level(4) -> load_tests_for_name(<<"Level 4 Examples">>).
load_tests_for_name(Level) ->
	LoadedData = load_spec_test_data(),
	{_,LevelData}  = proplists:lookup(Level,LoadedData),
	{_,VariablesData} = proplists:lookup(<<"variables">>,LevelData),
	{_,TestcasesData} = proplists:lookup(<<"testcases">>,LevelData), 
	{convert_variables(VariablesData),convert_test_cases(TestcasesData)}.

load_extended_test_data() -> load_data("extended-tests.data").
load_negative_test_data() -> load_data("negative-tests.data"). 
load_spec_test_data() -> load_data("spec-examples.data").
data_file(FileName) -> "../test/" ++ FileName.
load_data(FileName) -> 	
	{ok, Bin} = file:read_file(data_file(FileName)),
	binary_to_term(Bin).		
