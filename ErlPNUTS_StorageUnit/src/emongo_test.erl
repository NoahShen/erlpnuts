-module(emongo_test).

-compile(export_all).

start() ->
	application:start(emongo),
	emongo:add_pool(pool1, "localhost", 27017, "testdb", 1),
	
%% 	emongo:update(pool1, "table1", 
%% 				  [{"field1", "value1"}], 
%% 				  [{"field1", "value1Changed"}, {"newFiled", [{"newSubField1", "newSubFieldValue1"}, {"newSubField2", "newSubFieldValue2"}]}]).
	
%% 	emongo:update(pool1, "table1", 
%% 		[{"field1", 1}], 
%% 		[{"$set", [{"newFiled", "justNewFiled"}]}]).

	emongo:find_all(pool1, "table1", []).
%%  emongo:insert(pool1, "table1", [{"field1", "value1"}, {"field2", "value2"}]).