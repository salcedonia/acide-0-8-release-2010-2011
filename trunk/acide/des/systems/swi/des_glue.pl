/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.2.0                 */
/*                                                       */
/*    SYSTEM DEPENDENT PREDICATES 2                      */
/*    Tested for SWI-Prolog 5.10.0                       */
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
prolog_system(swi).

% FD constraint library
:- use_module(library(clpfd)).


%%%%%%% ODBC Connections %%%%%%%%

% Open an ODBC connection
my_open_odbc(Connection,Options) :-
  (odbc_connect(Connection,Handle,[open(once)|Options]) ->
    set_flag(current_db,Connection,Handle),
    write_verb(['Info: ODBC connection ''',Connection,''' successfully opened.',nl])
   ;
    write_log_list(['Error: Opening connection ''',Connection,'''.',nl])
  ).

% Close an ODBC connection
my_close_odbc(Connection) :-
  (retract(current_db(Connection,Handle)) ->
    set_default_db,
    odbc_disconnect(Handle),
    write_verb(['Info: ODBC connection ''',Connection,''' successfully closed.',nl])
   ;
   write_log_list(['Error: Connection ''',Connection,''' not found.',nl])
  ).
  
  
%%%%%%% ODBC Statement Executions %%%%%%%%

% Execute a SELECT statement returning all the answer tuples
my_odbc_dql_query(SQLstr,Schema,Rows) :-  
  current_db(_Connection,Handle),
  name(SQL,SQLstr),
  findall(Answer,Args^(odbc_query(Handle,SQL,Row,[null('$NULL'(_))]),Row=..[row|Args],Answer=..[answer|Args]),Rows),
  concrete_nulls(Rows),
  my_odbc_get_query_typed_arguments(SQLstr,ColNameTypes),
  Schema=..[answer|ColNameTypes].
% Second (worser) method to get column schema:
%   (odbc_query(Handle,SQL,Row,[null('$NULL'(_)),source(true)]) ->
%     Row=..[_Row|Columns],
%     get_columns_schema(Columns,ColumnsSchema),
%     Schema=..[answer|ColumnsSchema]
%    ;
%     Schema=answer).

% Execute a SELECT statement returning one answer tuple at a time via backtracking
my_odbc_dql_query_fetch_row(SQLstr,Row) :-
  current_db(_Connection,Handle),
  name(SQL,SQLstr),
%  catch(odbc_query(Handle,SQL,Row,[null('$NULL'(_))]),_,true).
  odbc_query(Handle,SQL,Row,[null('$NULL'(_))]).
  
% Execute a DML statement returning the number of tuples affected
my_odbc_dml_query(SQLstr,NumberOfRows) :-  
  current_db(_Connection,Handle),
  name(SQL,SQLstr),
%  catch(odbc_query(Handle,SQL,affected(NumberOfRows)),_,true).
  odbc_query(Handle,SQL,affected(NumberOfRows)).
    
% Execute a DDL statement returning nothing
my_odbc_ddl_query(SQLstr) :-  
  current_db(_Connection,Handle),
  name(SQL,SQLstr),
%  catch(odbc_query(Handle,SQL),M,(write_log(M),!,fail)).
  odbc_query(Handle,SQL).
    
  
%%%%%%% ODBC Metadata %%%%%%%%

% Get table names
my_odbc_get_tablenames(TableNames) :-
  current_db(_Connection,Handle),
  findall(TableName,odbc_current_table(Handle,TableName),TableNames).

% Ask whether a given table does exist
my_odbc_exists_table(TableName) :-
  current_db(_Connection,Handle),
  odbc_current_table(Handle,TableName).

% Get view names
my_odbc_get_viewnames(ViewNames) :-
  current_db(_Connection,Handle),
  findall(ViewName,odbc:odbc_tables(Handle,row(_ConnectionDetails,_X,ViewName,'VIEW',_Y)),ViewNames).
% T = row('C:\\Documents and Settings\\Fernando\\Escritorio\\bd1', '$null$', 'Consulta6', 'VIEW', '$null$').

% Ask whether a given view does exist
my_odbc_exists_view(ViewName) :-
  current_db(_Connection,Handle),
  odbc:odbc_tables(Handle,row(_ConnectionDetails,_X,ViewName,'VIEW',_Y)).
  
% Get both table and view names
my_odbc_get_table_and_view_names(TableNames) :-
  current_db(_Connection,Handle),
  findall(TableName,X^Y^(odbc:odbc_tables(Handle,row(_ConnectionDetails,X,TableName,Type,Y)),(Type='TABLE';Type='VIEW')),TableNames).
  
% Get table arity
my_odbc_get_table_arity(TableName,Arity) :-
  current_db(_Connection,Handle),
%   current_output(CurrentOut),
%   open_null_stream(Out),
  % Avoid annoying ODBC useless messages. Does not work!
%  with_output_to(Out,odbc_current_table(Handle,TableName,arity(Arity))),
  odbc_current_table(Handle,TableName,arity(Arity)).
%   close(Out),
%   set_output(CurrentOut).
  
% Get table column names. It is expected to get them in the same order they were defined via the create SQL statement
my_odbc_get_colnames(TableName,ColNames) :-
  current_db(_Connection,Handle),
  findall(ColName,odbc_table_column(Handle,TableName,ColName),ColNames).

% The following allows SWI to detect predicate current_stream as needed for building the executable  
my_void_odbc_table_column(Handle,TableName,ColName) :-
  odbc_table_column(Handle,TableName,ColName).  
  
% Get the list of ColumnName:TypeName for a table/view  
my_odbc_get_table_typed_arguments(TableName,ColNameTypes) :-
  current_db(_Connection,Handle),
  findall(ColName:TypeName,odbc_table_column(Handle,TableName,ColName,type_name(TypeName)),ColNameTypes).

% The following allows SWI to detect predicate current_stream as needed for building the executable  
my_void_odbc_table_column(Handle,TableName,ColName,type_name(TypeName)) :-
  odbc_table_column(Handle,TableName,ColName,type_name(TypeName)).  

% Get the list of type names for a table/view  
my_odbc_get_table_typenames(TableName,TypeNames) :-
  current_db(_Connection,Handle),
  findall(TypeName,ColName^odbc_table_column(Handle,TableName,ColName,type_name(TypeName)),TypeNames).

% Get the query schema (a list of ColName:TypeName). First method: Create a view, get its schema, and drop the view.
my_odbc_get_query_typed_arguments(SQLstr,ColNameTypes) :-  
  TableName='des__temporary',
  name(TableName,TableNameStr),
  concat_lists(["CREATE VIEW ",TableNameStr," AS ",SQLstr],CreateStr),
  catch(
   (
	  my_odbc_ddl_query(CreateStr),
	  my_odbc_get_table_typed_arguments(TableName,ColNameTypes),
	  my_odbc_get_dbms(DBMS),
	  (DBMS=='access' ; DBMS=='ACCESS' ->
	    concat_lists(["DROP TABLE ",TableNameStr],DropStr)
	   ;
	    concat_lists(["DROP VIEW ",TableNameStr],DropStr)
	  ),
	  my_odbc_ddl_query(DropStr)
	 ),
	 _,
	 ColNameTypes=[]
	    ).

% Get the query schema
get_columns_schema([],[]).
get_columns_schema([column(Table,Column,_Value)|Columns],[TableDotColumn|ColumnsSchema]) :-
  atom_concat(Table,'.',TableDot),
  atom_concat(TableDot,Column,TableDotColumn),
  get_columns_schema(Columns,ColumnsSchema).
    
  
%%%%%%% ODBC. Others %%%%%%%%

% Get the DBMS for the current connection
my_odbc_get_dbms(DBMS) :-
  current_db(_Connection,Handle),
  odbc_get_connection(Handle,dbms_name(DBMS)).
  
%%%%%%% End ODBC  %%%%%%%%

  
%%%%%%%% FD Constraint Solving %%%%%%%% 

% FD constraints
my_fd_domain(List,Min,Max) :-
  List ins Min..Max.

% FD enumeration
my_fd_labeling(Vars) :-
  labeling([ff],Vars).
    
% FD equivalence operator
% SICStus operator defined with the same priority and association as SWI
% for allowing a goal expansion
:- op(760, yfx, #<=>). 

goal_expansion(A #<=> B, A #<==> B) :-
  !. 
goal_expansion(G,G). 
      
%%%%%%%% END FD Constraint Solving %%%%%%%% 

% false predicate: Always fails
false :-
  fail.

% Current opened streams
my_current_stream(St) :-
  my_nf_bagof(OSt,X^Y^current_stream(X,Y,OSt),OSts),
  my_member_chk(St,OSts), !.

% The following allows SWI to detect predicate current_stream as needed for building the executable  
my_void_current_stream(X,Y,OSt) :- 
  current_stream(X,Y,OSt).

% Executing operating system commands
my_shell(C,S) :-
  name(C,SC),
  ((shell(SC,0), S=0);                                           %Unix
   (concatLsts(["cmd.exe /C ",SC],SCC), shell(SCC,0), S=0);      %Windows NT
   (concatLsts(["command.exe /C ",SC],SCC), shell(SCC,0), S=0);  %Windows 98
   S=1),
  !.

% Date and time
my_datetime((Y,M,D,H,Mi,S)) :-
  get_time(TS),
  stamp_date_time(TS,date(Y,M,D,H,Mi,Se,_,_,_),local),
  S is integer(Se).
  
% Sorting a list, keeping duplicates
my_sort(List, Orderedlist) :-
  msort(List, Orderedlist).

% Changing the current directory
my_change_directory(Path) :-
  chdir(Path).

% Testing whether exists the given directory
my_directory_exists(Path) :-
  exists_directory(Path).

% Testing whether exists the given file
my_file_exists(File) :-
  exists_file(File).

% Getting the current directory
my_working_directory(Path) :-
  absolute_file_name(., Path).

% Getting the ordered list of files from the given path
my_directory_files(Path, Files) :-
  absolute_file_name(., WorkingPath),
  absolute_file_name(Path, AbsolutePath),
  chdir(AbsolutePath),
  expand_file_name('*', FNs),
  (setof(FN, AFN^(my_member(FN,FNs),my_absolute_filename(FN,AFN),my_is_file(AFN)), Files) ->
   true ; Files = []),
  chdir(WorkingPath).

% Getting the ordered list of directories from the given path
my_directory_directories(Path, Directories) :-
  absolute_file_name(., WorkingPath),
  absolute_file_name(Path, AbsolutePath),
  chdir(AbsolutePath),
  expand_file_name('*', FNs),
  (setof(FN, AFN^(my_member(FN,FNs),my_absolute_filename(FN,AFN),my_is_directory(AFN)), Directories) ->
   true ; Directories = []),
  chdir(WorkingPath).

% Testing whether the input is a file
my_is_file(File) :-
  exists_file(File).

% Testing whether the input is a directory
my_is_directory(Path) :-
  exists_directory(Path).

% Getting the absolute filename for a file
my_absolute_filename(Path, AbsolutePath) :-
  absolute_file_name(Path, AbsolutePath).

% Extracting the absolute path and file from an absolute file name
my_dir_file(AFN,AP,FN) :-
  file_directory_name(AFN,AP),
  atom_concat(AP,'/',PS),
  atom_concat(PS,FN,AFN).

% Gets a byte from the handle (Edinburgh version)
my_get0(HIn,C) :-
  get0(HIn,C).

% Gets a byte from the current handle (Edinburgh version)
my_get0(C) :-
  get0(C).

% Reading terms along with variable names and its scope in line numbers
% TODO: Get max line numbers
my_read(Term, VariableNames, (Min,Max)) :-
  current_input(CurrentStream),
  stream_property(CurrentStream, position(CurrentPos)),
  stream_position_data(line_count,CurrentPos,Min),
  read_term(Term, [variable_names(VariableNames),term_position(NewPos)]),
  stream_position_data(line_count,NewPos,Max).
  
% Timing: Resetting and displaying the elapsed time
:- dynamic(time/1).

reset_elapsed_time :-
  get_time(T),
  retractall(time(_)),
  retractall(time(_,_,_)),
  assertz(time(_,_,_)),
  assertz(time(T)).

get_elapsed_time(Elapsed) :-
   time(T1),
   get_time(T2),
   Elapsed is integer(1000*(T2-T1)).
