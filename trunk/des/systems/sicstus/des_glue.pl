/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.2.0                 */
/*                                                       */
/*    SYSTEM DEPENDENT PREDICATES                        */
/*    Tested for Sicstus 4.1.2                           */
/*                                                       */
/*                                                       */
/*                    Fernando Sáenz-Pérez (c) 2004-2010 */
/*                                             DISIA UCM */
/*             Please send comments, questions, etc. to: */
/*                                     fernan@sip.ucm.es */
/*                                Visit the Web site at: */
/*                           http://des.sourceforge.net/ */
/*                                                       */
/*********************************************************/


% Prolog system identification
prolog_system(sicstus).

% System library
:- use_module(library(system)).

% File systems library
:- use_module(library(file_systems)).

% FD constraint library
:- use_module(library(clpfd)).

% ODBC library
:- use_module(library(odbc)).

% Sorting library
:- use_module(library(samsort)).


%%%%%%% ODBC Connections %%%%%%%%

% Open an ODBC connection
my_open_odbc(Connection,UOptions) :-
  (odbc_env_open(EnvHandle),
   translate_open_odbc_options(UOptions,Options),
   odbc_db_open(Connection,EnvHandle,Options,ConnectionHandle) ->
    set_flag(current_db,Connection,(EnvHandle, ConnectionHandle)),
    write_verb(['Info: ODBC connection ''',Connection,''' successfully opened.',nl])
   ;
    write_log_list(['Error: Opening connection ''',Connection,'''.',nl])
  ).

% Close an ODBC connection
my_close_odbc(Connection) :-
  (retract(current_db(Connection,(EnvHandle, ConnectionHandle))) ->
    set_default_db,
    odbc_db_close(ConnectionHandle),
    odbc_env_close(EnvHandle),
    write_verb(['Info: ODBC connection ''',Connection,''' successfully closed.',nl])
   ;
   write_log_list(['Error: Connection ''',Connection,''' not found.',nl])
  ).
  
translate_open_odbc_options([],[]).
translate_open_odbc_options([user(UserName)|UOptions],[username(UserName)|Options]) :-  
  !,
  translate_open_odbc_options(UOptions,Options).
translate_open_odbc_options([Option|UOptions],[Option|Options]) :-  
  translate_open_odbc_options(UOptions,Options).

  
%%%%%%% ODBC Statement Executions %%%%%%%%

% Execute a SELECT statement returning all the answer tuples
my_odbc_dql_query(SQLstr,Schema,Rows) :-  
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  get_typed_arguments(ResultSet,ColNameTypes),
  get_rows(ResultSet,Rows),
  odbc_query_close(ResultSet),
  Schema=..[answer|ColNameTypes].

% Execute a SELECT statement returning one answer tuple at a time via backtracking
my_odbc_dql_query_fetch_row(SQLstr,Row) :-
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  (get_row(ResultSet,Row)
   ;
   odbc_query_close(ResultSet),
   !,
   fail).

% Execute a DML statement returning the number of tuples affected
my_odbc_dml_query(SQLstr,NumberOfRows) :-  
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  ResultSet=odbc_result_set(_,_,NumberOfRows,_,_),
  odbc_query_close(ResultSet).

% Execute a DDL statement returning nothing
my_odbc_ddl_query(SQLstr) :-  
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  odbc_query_close(ResultSet).

  
% Get schema from result set description
get_typed_arguments(ResultSet,ColNameTypes) :-
 ResultSet=odbc_result_set(_,_,_,Columns,_),
 get_columns_colname_typename(Columns,ColNameTypes).

get_columns_colname_typename([],[]).
get_columns_colname_typename([cd(_,ColName,DataType,_,_Table)|Columns],[ColName:TypeName|ColumnsSchema]) :-
  odbc_datatype_typename(DataType,TypeName),
  get_columns_colname_typename(Columns,ColumnsSchema).

get_colnames(ResultSet,ColNames) :-
% my_view('$des',ite(ResultSet),nl,
 ResultSet=odbc_result_set(_,_,_,Columns,_),
 get_columns_schema(Columns,ColNames).
%odbc_result_set(statement_handle(1),2,-1,[cd(1,a,4,4,odbc_column_binding(1,odbc_buffer(4,21501992),odbc_buffer(4,21502024))),cd(2,b,4,4,odbc_column_binding(2,odbc_buffer(4,21501928),odbc_buffer(4,21501960)))],[])

get_columns_schema([],[]).
get_columns_schema([cd(_,Column,_Type,_,_Table)|Columns],[Column|ColumnsSchema]) :-
  get_columns_schema(Columns,ColumnsSchema).
  

% Get schema from result set description
get_coltypes(ResultSet,ColTypes) :-
% my_view('$des',ite(ResultSet),nl,
 ResultSet=odbc_result_set(_,_,_,Columns,_),
 get_column_types(Columns,ColTypes).

get_column_types([],[]).
get_column_types([cd(_,_Column,Type,_,_Table)|Columns],[Type|ColumnTypes]) :-
  get_column_types(Columns,ColumnTypes).
  

% Get all rows fetching one by one
get_rows(ResultSet,Rows) :-
  odbc_sql_fetch(ResultSet,Row),
  get_rows1(Row,ResultSet,Rows).
  
get_rows1([],_ResultSet,[]) :-
  !.
get_rows1(URow,ResultSet,[Row|Rows]) :-
  format_answer(URow,Row),
  odbc_sql_fetch(ResultSet,URow1),
  get_rows1(URow1,ResultSet,Rows).
  
% Get a single row. Backtracking allows to return the complete answer set
get_row(ResultSet,Row) :-
  repeat,
  odbc_sql_fetch(ResultSet,URow),
  (URow==[],
   !,
   fail
  ;
   format_answer(URow,Row)).
  
 
% get_row1([],_ResultSet,_) :-
%   !,
%   fail.
% get_row1(URow,ResultSet,Row) :-
%   format_answer(URow,Row),
%   odbc_sql_fetch(ResultSet,URow1),
%   get_row1(URow1,ResultSet,Row).
  
format_answer(UR,FR) :-
  format_answer(UR,[],UCL),
  format_columns(UCL,CL),
  FR=..[answer|CL].

format_answer([_CN-Value|Y],Z,W) :- 
  format_answer(Y,[Value|Z],W).
format_answer([],X,X).

% Columns of type string returned by ODBC interface use the Prolog string type, instead atom type (as SWI and DES).
% The 'null' value is translated into its internal representation '$NULL'(_)
format_columns([],[]).
format_columns([C|Cs],[A|As]) :-
  ((C=[_|_];C==[]) ->
    atom_codes(A,C)
   ;
    (C==null ->
      A='$NULL'(_)
     ;
      A=C)
  ),
  format_columns(Cs,As).
  
  
%%%%%%% ODBC Metadata %%%%%%%%

% Get table names
my_odbc_get_tablenames(TableNames) :-
  my_odbc_get_dbms(DBMS),
  my_odbc_get_tablenames(DBMS,TableNames).
  
my_odbc_get_tablenames(access,TableNames) :-
% WARNING: The following makes crash the application when calling MS Access ODBC
%  SQLstr="SELECT Name FROM MSysObjects WHERE Type=1 AND Flags=0",
% WARNING: Instead, before using this interface, copy table MSysObjects (both data and structure) to a new table "db_schema"
% WARNING: This means that DES will not be aware of schema changes after this
  SQLstr="SELECT Name FROM db_schema WHERE Type=1 AND Flags=0 AND Name<>'db_schema'",
  get_single_value_list(SQLstr,TableNames).

% WARNING: This other method does not work since access to MSysObjects ends in a crash
%   current_db(_Connection,(_EnvHandle,ConnectionHandle)),
%   odbc_query_open(ConnectionHandle,StatementHandle),
%   catch(
%     odbc_query_execute_sql(StatementHandle,
%         "create table db_schema(name varchar(64))",
%         _),
%     _,
%     true),
%   odbc_query_execute_sql(StatementHandle,
%       "delete from db_schema",
%       _),
%   odbc_query_execute_sql(StatementHandle,
%       "insert into db_schema SELECT Name FROM [MSysObjects] WHERE Type=1 AND Flags=0 and Name<>'db_schema'",
%       ResultSet),
%   odbc_query_close(ResultSet),
%   get_single_value_list("select Name from db_schema",TableNames).
 
my_odbc_get_tablenames(mysql,TableNames) :-
  current_db(Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,
      "CREATE TABLE IF NOT EXISTS db_schema(table_name VARCHAR(64))",
      _),
  odbc_query_execute_sql(StatementHandle,
      "DELETE FROM db_schema",
      _),
  name(Connection,ConnectionStr),
  concat_lists(["INSERT INTO db_schema SELECT table_name FROM information_schema.tables WHERE table_type='BASE TABLE' AND table_name<>'db_schema' AND table_schema='",ConnectionStr,"'"],SQLstr),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  odbc_query_close(ResultSet),
  get_single_value_list("SELECT table_name FROM db_schema",TableNames).

my_odbc_get_tablenames(oracle,TableNames) :-
  SQLstr="SELECT table_name FROM user_tables",
  get_single_value_list(SQLstr,TableNames).

% Ask whether a given table does exist
my_odbc_exists_table(TableName) :-
  my_odbc_get_dbms(DBMS),
  my_odbc_exists_table(DBMS,TableName).
  
my_odbc_exists_table(access,TableName) :-
  to_uppercase(TableName,UTableName),
  name(UTableName,UTableNameStr),
  concat_lists(["SELECT UCASE(Name) FROM db_schema WHERE Type=1 AND Flags=0 AND UCASE(Name)='",UTableNameStr,"'"],SQLstr),
  get_single_value_list(SQLstr,[UTableName]).

my_odbc_exists_table(mysql,TableName) :-
  current_db(Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,
      "CREATE TABLE IF NOT EXISTS db_schema(table_name VARCHAR(64))",
      _),
  odbc_query_execute_sql(StatementHandle,
      "DELETE FROM db_schema",
      _),
  name(Connection,ConnectionStr),
  concat_lists(["INSERT INTO db_schema SELECT table_name FROM information_schema.tables WHERE table_type='BASE TABLE' AND table_name<>'db_schema' AND table_schema='",ConnectionStr,"'"],SQLstr),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  odbc_query_close(ResultSet),
  to_uppercase(TableName,UTableName),
  name(UTableName,UTableNameStr),
  concat_lists(["SELECT UPPER(table_name) FROM db_schema WHERE UPPER(table_name)='",UTableNameStr,"'"],SQLStr),
  get_single_value_list(SQLStr,[UTableName]).

my_odbc_exists_table(oracle,TableName) :-
  to_uppercase(TableName,UTableName),
  name(UTableName,UTableNameStr),
  concat_lists(["SELECT UPPER(table_name) FROM user_tables WHERE UPPER(table_name)='",UTableNameStr,"'"],SQLstr),
  get_single_value_list(SQLstr,[UTableName]).

% Get view names
my_odbc_get_viewnames(ViewNames) :-
  my_odbc_get_dbms(DBMS),
  my_odbc_get_viewnames(DBMS,ViewNames).
  
my_odbc_get_viewnames(access,ViewNames) :-
% WARNING: The following makes crash the application when calling MS Access ODBC
%    SQLstr="SELECT Name FROM MSysObjects WHERE Type=5 AND Flags=0",
% WARNING: Instead, before using this interface, copy table MSysObjects (both data and structure) to a new table "db_schema"
  SQLstr="SELECT Name FROM db_schema WHERE Type=5 AND Flags=0",
  get_single_value_list(SQLstr,ViewNames).
 
my_odbc_get_viewnames(mysql,ViewNames) :-
  current_db(Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,
      "CREATE TABLE IF NOT EXISTS db_schema(table_name VARCHAR(64))",
      _),
  odbc_query_execute_sql(StatementHandle,
      "DELETE FROM db_schema",
      _),
  name(Connection,ConnectionStr),
  concat_lists(["INSERT INTO db_schema SELECT table_name FROM information_schema.tables WHERE table_type='VIEW' AND table_name<>'db_schema' AND table_schema='",ConnectionStr,"'"],SQLstr),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  odbc_query_close(ResultSet),
  get_single_value_list("SELECT table_name FROM db_schema",ViewNames).
 
my_odbc_get_viewnames(oracle,ViewNames) :-
  SQLstr="SELECT view_name FROM user_views",
  get_single_value_list(SQLstr,ViewNames).
 
% Ask whether a given view does exist
my_odbc_exists_view(ViewName) :-
  my_odbc_get_dbms(DBMS),
  my_odbc_exists_view(DBMS,ViewName).
  
my_odbc_exists_view(access,ViewName) :-
  to_uppercase(ViewName,UViewName),
  name(UViewName,UViewNameStr),
  concat_lists(["SELECT UCASE(Name) FROM db_schema WHERE Type=5 AND Flags=0 AND UCASE(Name)='",UViewNameStr,"'"],SQLstr),
  get_single_value_list(SQLstr,[UViewName]).

my_odbc_exists_view(mysql,ViewName) :-
  current_db(Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,
      "CREATE TABLE IF NOT EXISTS db_schema(table_name VARCHAR(64))",
      _),
  odbc_query_execute_sql(StatementHandle,
      "DELETE FROM db_schema",
      _),
  name(Connection,ConnectionStr),
  concat_lists(["INSERT INTO db_schema SELECT table_name FROM information_schema.tables WHERE table_type='VIEW' AND table_name<>'db_schema' AND table_schema='",ConnectionStr,"'"],SQLstr),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  odbc_query_close(ResultSet),
  to_uppercase(ViewName,UViewName),
  name(UViewName,UViewNameStr),
  concat_lists(["SELECT UPPER(table_name) FROM db_schema WHERE UPPER(table_name)='",UViewNameStr,"'"],SQLStr),
  get_single_value_list(SQLStr,[UViewName]).

my_odbc_exists_view(oracle,ViewName) :-
  to_uppercase(ViewName,UViewName),
  name(UViewName,UViewNameStr),
  concat_lists(["SELECT UPPER(view_name) FROM user_views where UPPER(view_name)='",UViewNameStr,"'"],SQLstr),
  get_single_value_list(SQLstr,[UViewName]).

% Get both table and view names
my_odbc_get_table_and_view_names(Names) :-
  my_odbc_get_tablenames(TableNames),
  my_odbc_get_viewnames(ViewNames),
  my_append(TableNames,ViewNames,Names).
  
my_odbc_get_table_arity(TableName,Arity) :-
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  name(TableName,TableNameStr),
  (DBMS==access -> 
    OD="[", CD="]"
    ;
    (DBMS==mysql ->
     OD="`", CD=OD
     ;
     DBMS==oracle ->
      OD="""", CD=OD
     ;
      !,
      fail
    )
  ),
  concat_lists(["SELECT * FROM ",OD,TableNameStr,CD],SQLstr),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  ResultSet=odbc_result_set(_,Arity,_,_Columns,_),
  odbc_query_close(ResultSet).
%odbc_result_set(statement_handle(1),2,-1,[cd(1,a,4,4,odbc_column_binding(1,odbc_buffer(4,21501992),odbc_buffer(4,21502024))),cd(2,b,4,4,odbc_column_binding(2,odbc_buffer(4,21501928),odbc_buffer(4,21501960)))],[])

% Get table column names. It is expected to get them in the same order they were defined via the create SQL statement
my_odbc_get_colnames(TableName,ColNames) :-
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  name(TableName,TableNameStr),
  (DBMS==access -> 
    OD="[", CD="]"
    ;
    (DBMS==mysql ->
     OD="`", CD=OD
     ;
     DBMS==oracle ->
      OD="""", CD=OD
     ;
      !,
      fail
    )
  ),
  concat_lists(["SELECT * FROM ",OD,TableNameStr,CD],SQLstr),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  get_colnames(ResultSet,ColNames),
  odbc_query_close(ResultSet).

% Get the list of ColumnName:TypeName for a table/view  
my_odbc_get_table_typed_arguments(TableName,ColNameTypes) :-
  my_odbc_get_dbms(DBMS),
  my_odbc_get_table_typed_arguments(DBMS,TableName,ColNameTypes).

my_odbc_get_table_typed_arguments(DBMS,TableName,ColNameTypes) :-
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  name(TableName,TableNameStr),
  (DBMS==access -> 
    OD="[", CD="]"
    ;
    (DBMS==mysql ->
     OD="`", CD=OD
     ;
     DBMS==oracle ->
      OD="""", CD=OD
     ;
      !,
      fail
    )
  ),
  concat_lists(["SELECT * FROM ",OD,TableNameStr,CD],SQLstr),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  get_typed_arguments(ResultSet,ColNameTypes),
  odbc_query_close(ResultSet).

  
get_single_value_list(SQLstr,Values)  :-
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_query_execute_sql(StatementHandle,SQLstr,ResultSet),
  my_nf_bagof(Value,get_row(ResultSet,answer(Value)),Values),
  odbc_query_close(ResultSet),
  !.
  
% Get the list of type names for a table/view  
my_odbc_get_table_typenames(TableName,TypeNames) :-
  current_db(_Connection,(_EnvHandle,ConnectionHandle)),
  odbc_query_open(ConnectionHandle,StatementHandle),
  odbc_list_data_types(StatementHandle,TableName,DataTypes),
  odbc_datatype_typename_list(DataTypes,TypeNames).

odbc_datatype_typename_list([],[]).
odbc_datatype_typename_list([DataType|DataTypes],[TypeName|TypeNames]) :-
  odbc_datatype_typename(DataType,TypeName),
  odbc_datatype_typename_list(DataTypes,TypeNames).

odbc_datatype_typename(3,number) :- !.
odbc_datatype_typename(4,integer) :- !.
odbc_datatype_typename(8,float) :- !.
odbc_datatype_typename(12,varchar) :- !.
odbc_datatype_typename(X,unknown(X)) :- !.

%%%%%%% ODBC. Others %%%%%%%%

% Get the DBMS for the current connection
my_odbc_get_dbms(DBMS) :-
  current_db(Connection,(EnvHandle,_ConnectionHandle)),
  get_dbms(Connection,EnvHandle,DBMS).
  
get_dbms(Connection,EnvHandle,DBMS) :-
  odbc_list_DSN(EnvHandle,DSNs),
  get_dbms_from_DSNs(DSNs,Connection,DBMS).
  
get_dbms_from_DSNs([[Connection-DSN]|_DSNs],Connection,access) :-
  is_DBMS(DSN,'ACCESS'),
  !.
get_dbms_from_DSNs([[Connection-DSN]|_DSNs],Connection,mysql) :-
  is_DBMS(DSN,'MYSQL'),
  !.
get_dbms_from_DSNs([[Connection-DSN]|_DSNs],Connection,oracle) :-
  is_DBMS(DSN,'ORACLE'),
  !.
get_dbms_from_DSNs([[Connection-DSN]|_DSNs],Connection,unknown
) :-
  !,
  my_raise_exception(DSN,unsupported,['Unknown DBMS']),
  fail.
get_dbms_from_DSNs([_DSN|DSNs],Connection,DBMS) :-
  get_dbms_from_DSNs(DSNs,Connection,DBMS).
  
is_DBMS(DSN,Text) :-
  to_uppercase(DSN,UDSN),
  to_uppercase(Text,UText),
  sub_atom(UDSN,_,_,_,UText).
  
%%%%%%% End ODBC  %%%%%%%%
  

%%%%%%%% FD Constraint Solving %%%%%%%% 

% FD operators.
my_fd_equiv(X,Y) :-
  '#<=>'(X,Y).

% FD constraints
my_fd_domain(List,Min,Max) :-
  domain(List,Min,Max).

% FD enumeration
my_fd_labeling(Vars) :-
  labeling([ff],Vars).
    
%%%%%%%% END FD Constraint Solving %%%%%%%% 

% Executing operating system commands
my_shell(C,S) :-
  shell(C,S).

% Current opened streams
my_current_stream(St) :-
  current_stream(_,_,St).
  
% Date and time
my_datetime((Y,M,D,H,Mi,S)) :-
  datime(datime(Y,M,D,H,Mi,S)).
  
% Sorting a list, keeping duplicates
my_sort(List, Orderedlist) :-
  samsort(List, Orderedlist). 

% Changing the current directory
my_change_directory(Path) :-
  current_directory(_OldPath, Path).

% Testing whether exists the given directory
my_directory_exists(Path) :-
  current_directory(CWD),
  absolute_file_name(Path, AbsPath, [relative_to(CWD)]),
  directory_exists(AbsPath).

% Testing whether exists the given file
my_file_exists(File) :-
  current_directory(CWD),
  absolute_file_name(File, AbsFile, [relative_to(CWD)]),
  file_exists(AbsFile).

% Getting the current directory
my_working_directory(Path) :-
  current_directory(Path).

% Getting the ordered list of files from the given path
my_directory_files(Path, Files) :-
%  TODO: Wildcards (possible in SP4 and SWI)
%  The following works for SP4:
%  my_absolute_filename(Path,AFN),
%  my_dir_file(AFN,AP,BFN),
%  setof(BN, FN^file_member_of_directory(AP,BFN,BN,FN), Files).
  file_members_of_directory(Path, BN_FNs),
  setof(BN, FN^my_member(BN-FN,BN_FNs), Files),
  !.
my_directory_files(_Path, []).

% Getting the ordered list of directories from the given path
my_directory_directories(Path, Directories) :-
%  TODO: Wildcards (possible in SP4 and SWI)
%  The following should work for SP4:
%  my_absolute_filename(Path,AFN),
%  my_dir_file(AFN,AP,BFN),
%  setof(BN, FN^directory_member_of_directory(AP,BFN,BN,FN), Directories).
  directory_members_of_directory(Path,BN_FNs),
  setof(BN, FN^my_member(BN-FN,BN_FNs), Directories),
  !.
my_directory_directories(_Path, []).

% Getting the absolute filename for a file
my_absolute_filename(F,AFN) :-
  my_absolute_file_name(F,AFN).

my_absolute_file_name(Path,AbsolutePath) :-
  current_directory(CWD),
  absolute_file_name(Path, AbsolutePath, [relative_to(CWD)]).

% Testing whether the input is a file
my_is_file(_File).

% Testing whether the input is a directory
my_is_directory(_Path).

% Extracting the absolute path and file from an absolute file name
my_dir_file(AFN,AP,FN) :-
  name(AFN,SF),
  (([S]="/", reverse_find(S,SF,SP,SFN), name(AP,SP), name(FN,SFN), !); 
   ([S]="\\", reverse_find(S,SF,SP,SFN), name(AP,SP), name(FN,SFN), !); 
   (FN=AFN, AP='')).

reverse_find(X,S,P,F) :-
  my_reverse(S,RS),
  my_append(RF,[X|RP],RS), 
  my_reverse(RP,P),
  my_reverse(RF,F).

my_reverse(L,RL) :-
  my_reverse(L,[],RL).
my_reverse([],R,R).
my_reverse([A|As],Bs,R) :-
  my_reverse(As,[A|Bs],R).

% Gets a byte from the handle
my_get0(HIn, C) :- get_code(HIn, C).

% Gets a byte from the current handle
my_get0(C) :- get_code(C).

% Negation by failure
not(X) :-
  my_not(X).

% Reading terms along with variable names and its scope in line numbers
my_read(Term, VariableNames, (Start,End)) :-
  read_term(Term, [variable_names(VariableNames),layout(LineNumbers)]),
  find_min_max(LineNumbers,Start,End).

find_min_max(Ls,Start,End) :-
  find_min_max(Ls,_Min,_Max,Start,End).
  
find_min_max([],Min,Max,Min,Max).
find_min_max(N,Min,Max,Start,End) :-
  number(N), !,
  (var(Min) -> Min=N,Start=N; true),
  (var(Max) -> Max=N,End=N; true),
  (N<Min -> Start=N; Start=Min),
  (N>Max -> End=N; End=Max).
find_min_max([L|Ls],Min,Max,Start,End) :-
  find_min_max(L,Min,Max,Min1,Max1),
  find_min_max(Ls,Min1,Max1,Start,End).

% Timing: Resetting and displaying the elapsed time
reset_elapsed_time :-
  retractall(time(_,_,_)),
  assertz(time(_,_,_)),
  statistics(walltime,_).

get_elapsed_time(Elapsed) :-
   statistics(walltime,[_,Elapsed]).
      
