/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.2.0                 */
/*                                                       */
/*    SYSTEM DEPENDENT PREDICATES                        */
/*    Tested for CIAO Prolog 1.10p5                      */
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
prolog_system(ciao).

% Needed libraries
:- use_module(library(system)).
:- use_module(library(sort)).

%%%%%%% ODBC Connections %%%%%%%%

% Open an ODBC connection
my_open_odbc(Connection,Options) :-
  my_raise_exception(my_open_odbc(Connection,Options),odbc_unsupported,[]).

% Close an ODBC connection
my_close_odbc(Connection) :-
  my_raise_exception(my_close_odbc(Connection),odbc_unsupported,[]).
  
%%%%%%% ODBC Statement Executions %%%%%%%%

% Execute a SELECT statement returning all the answer tuples
my_odbc_dql_query(SQLstr,Schema,Rows) :-  
  my_raise_exception(my_odbc_dql_query(SQLstr,Schema,Rows),odbc_unsupported,[]).
  
% Execute a SELECT statement returning one answer tuple at a time via backtracking
my_odbc_dql_query_fetch_row(SQLstr,Row) :-
  my_raise_exception(my_odbc_dql_query_fetch_row(SQLstr,Row),odbc_unsupported,[]).
  
% Execute a DML statement returning the number of tuples affected
my_odbc_dml_query(SQLstr,NumberOfRows) :-  
  my_raise_exception(my_odbc_dml_query(SQLstr,NumberOfRows),odbc_unsupported,[]).

% Execute a DDL statement returning nothing
my_odbc_ddl_query(SQLstr) :-  
  my_raise_exception(my_odbc_ddl_query(SQLstr),odbc_unsupported,[]).

%%%%%%% ODBC Metadata %%%%%%%%

% Get table names
my_odbc_get_tablenames(TableNames) :-
  my_raise_exception(my_odbc_get_tablenames(TableNames),odbc_unsupported,[]).
  
% Ask whether a given table does exist
my_odbc_exists_table(TableName) :-
  my_raise_exception(my_odbc_exists_table(TableName),odbc_unsupported,[]).
  
% Get view names
my_odbc_get_viewnames(ViewNames) :-
  my_raise_exception(my_odbc_get_viewnames(ViewNames),odbc_unsupported,[]).

% Ask whether a given view does exist
my_odbc_exists_view(ViewName) :-
  my_raise_exception(my_odbc_exists_view(ViewName),odbc_unsupported,[]).
  
% Get both table and view names
my_odbc_get_table_and_view_names(TableNames) :-
  my_raise_exception(my_odbc_get_table_and_view_names(TableNames),odbc_unsupported,[]).
  
% Get table arity
my_odbc_get_table_arity(TableName,Arity) :-
  my_raise_exception(my_odbc_get_table_arity(TableName,Arity),odbc_unsupported,[]).

% Get table column names. It is expected to get them in the same order they were defined via the create SQL statement
my_odbc_get_colnames(TableName,ColNames) :-
  my_raise_exception(my_odbc_get_colnames(TableName,ColNames),odbc_unsupported,[]).
  
% Get the list of ColumnName:TypeName for a table/view  
my_odbc_get_table_typed_arguments(TableName,ColNameTypes) :-
  my_raise_exception(my_odbc_get_table_typed_arguments(TableName,ColNameTypes),odbc_unsupported,[]).
  
% Get the list of type names for a table/view  
my_odbc_get_table_typenames(TableName,TypeNames) :-
  my_raise_exception(my_odbc_get_table_typenames(TableName,TypeNames),odbc_unsupported,[]).

% Get the query schema (a list of ColName:TypeName). First method: Create a view, get its schema, and drop the view.
my_odbc_get_query_typed_arguments(SQLstr,ColNameTypes) :-  
  my_raise_exception(my_odbc_get_query_typed_arguments(SQLstr,ColNameTypes),odbc_unsupported,[]).
    
%%%%%%% ODBC. Others %%%%%%%%

% Get the DBMS for the current connection
my_odbc_get_dbms(DBMS) :-
  my_raise_exception(my_odbc_get_dbms(DBMS),odbc_unsupported,[]).
  
%%%%%%% End ODBC  %%%%%%%%


%%%%%%%% FD Constraint Solving %%%%%%%% 
  
% FD constraint library
%:- use_module(library(clpfd)).

% FD operators
:- op(760, yfx, #<=>). 
:- op(740, yfx, #\/). 
:- op(730, yfx, #\). 
:- op(720, yfx, #/\). 
:- op(700, xfx, [(#=),(#\=),(#<),(#=<),(#>),(#>=)]).

% FD constraints
my_fd_domain(List,_Min,_Max) :-
  my_raise_exception(my_fd_domain(List),fd_unsupported,[]).

% FD enumeration
my_fd_labeling(Vars) :-
  my_raise_exception(my_fd_labeling(Vars),fd_unsupported,[]).
  
%%%%%%%% END FD Constraint Solving %%%%%%%% 

% false predicate: Always fails
false :-
  fail.

% Current opened streams
my_current_stream(St) :-
  current_stream(_,_,St).
  
% Executing operating system commands
my_shell(C,S) :-
  shell(C,S).

% Date and time
my_datetime((Y,M,D,H,Mi,S)) :-
  datime(datime(Y,M,D,H,Mi,S)).
  
% Sorting a list, keeping duplicates
my_sort(List, Orderedlist) :-
  my_quicksort(List, Orderedlist).

% Changing the current directory
my_change_directory(Path) :-
  working_directory(_OldPath,Path).

% Testing whether exists the given directory
my_directory_exists(Path) :-
  file_exists(Path),
  file_property(Path,type(directory)).

% Testing whether exists the given file
my_file_exists(File) :-
  file_exists(File),
  file_property(File, type(regular)).

% Getting the current directory
my_working_directory(Path) :-
  working_directory(Path, Path).

% Getting the ordered list of files from the given path
% Getting the ordered list of files from the given path
my_directory_files(Path, Files) :-
  directory_files(Path, FNs),
  name(Path,SPath),
  setof(FN, AFN^SFN^SAFN^
        (
         my_member(FN,FNs),   
         name(FN,SFN), 
         concat_lists([SPath,"/",SFN],SAFN), 
         name(AFN,SAFN),
         my_is_file(AFN)
        ), 
        Files),
  !.
my_directory_files(_Path, []).

% Getting the ordered list of directories from the given path
my_directory_directories(Path, Directories) :-
  directory_files(Path, FNs),
  name(Path,SPath),
  setof(FN, AFN^SFN^SAFN^
        (
         my_member(FN,FNs),   
         name(FN,SFN), 
         concat_lists([SPath,"/",SFN],SAFN), 
         name(AFN,SAFN),
         my_is_directory(AFN)
        ), 
        Directories),
  !.
my_directory_directories(_Path, []).

% Getting the absolute filename for a file
my_absolute_filename(Path, AbsolutePath) :-
  absolute_file_name(Path, AbsolutePath).

% Testing whether the input is a file
my_is_file(File) :-
  file_property(File, type(regular)).

% Testing whether the input is a directory
my_is_directory(Path) :-
  file_property(Path, type(directory)).

% Giving an absolute file name
% my_absolute_filename(F,AFN) :-
%   my_working_directory(WD),
%   (name(F,SF), [SC,Sl,BSl] = ":/\\", SF = [L,SC,X|_Xs], X \== Sl, X \== BSl -> % TODO
%    nl, write_log('Cannot access '), write_string_log([L,SC]), write('.'), !, fail; true),
%   my_dir_file(F,P,FN),
%   (my_directory_exists(P) ->
%     my_change_directory(P),
%     my_working_directory(AP),
%     atom_concat(AP,'/',ASP),  
%     atom_concat(ASP,FN,AFN),
%     my_change_directory(WD);
%     F=AFN).

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
my_get0(HIn,C) :- get_code(HIn,C).

% Gets a byte from the current handle
my_get0(C) :- get_code(C).

% Negation by failure
not(X) :-
  my_not(X).

% Reading terms along with variable names and its scope in line numbers
my_read(Term, VariableNames, (Min,Max)) :-
  read_term(Term, [variable_names(VariableNames),lines(Min,Max)]).

% Timing: Resetting and displaying the elapsed time
reset_elapsed_time :-
  retractall(time(_,_,_)),
  assertz(time(_,_,_)),
  statistics(walltime,_).

get_elapsed_time(Elapsed) :-
   statistics(walltime,[_,Elapsed]).
   
