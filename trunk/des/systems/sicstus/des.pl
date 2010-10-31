/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.2.0                 */
/*                                                       */
/*    MAIN PROGRAM                                       */
/*                                                       */
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

/*********************************************************/
/* Features:                                             */
/* - Arithmetic expressions                              */
/* - Stratified negation                                 */
/* - Null value support                                  */
/* - Outer join builtin relations (lj,rj,fj)             */
/* - Aggregate builtin relations (count,sum,min,...)     */
/* - GROUP BY and HAVING support (see User Manual)       */
/* - Disjunctive bodies                                  */
/* - Type inferring/checking system                      */
/* - Datalog Declarative debugger                        */
/* - Multi-language support: Datalog, SQL, Prolog        */
/* - Memoization                                         */
/* - Terminating computations modulo arithmetics (is/2)  */
/* - RDB connections via ODBC                            */
/* Limitations:                                          */
/* - No database updates via rules                       */
/* - No compound terms as arguments in user relations    */
/* - No multiset answers                                 */
/* TODO:                                                 */
/* - Number of consulted rules                           */
/* - Do not delete et when submitting autoviews          */
/* - Delete only added entries from et after executing an*/
/*   autoview                                            */
/* - Move type inferring from my_assertz to preprocess   */
/* - Extended Boolean expressions in join conditions     */
/* - Query language autoswitch                           */
/* - Database updates                                    */
/* - Display Prolog answers with the answer substitution */
/* - Multiline input                                     */
/* - Constraints                                         */
/* - Disjunctive heads                                   */
/* - Mobile/PDA version                                  */
/* - A more efficient parser for arithmetic expressions  */
/*********************************************************/


/*********************************************************************/
/* Notes about the implementation                                    */
/*********************************************************************/

% 1. Rule representation:
%    * Rule:    ':-'(Head,Body) or simply Head
%    * ruleNVs: (Rule,NVs), where NVs=['Varname1'=Var1,...]
%    * dlrule:  datalog(Rule,NVs,RuleId,Lines,FileId,source)
%               datalog(Rule,NVs,RuleId,Lines,FileId,compilation(SH,SB,[dlrules]))
%               datalog(Rule,NVs,RuleId,Lines,FileId,compiled)
%      source means that the rule has not been compiled
%      compilation means that the source rule SH:-SB has been compiled into a list of dlrules
%      RuleId is the rule identifier (an integer)
%      Lines are the lines in the source text program where the rule occurs: (FromLine-ToLine)
%      FileID is the file identifier (an integer) where the rule is defined


/*********************************************************************/
/* DES Version number                                                */
/*********************************************************************/

des_version('2.0').

/*********************************************************************/
/* Predicate for debugging when developing                           */
/*********************************************************************/

deb.

/*********************************************************************/
/* Dynamic Predicates                                                */
/*********************************************************************/

:- dynamic(log/2).            % Log file information (filename and associated stream)
:- dynamic(verbose/1).        % Verbose mode flag 
:- dynamic(pretty_print/1).   % Pretty print for listings (takes more lines to print)
:- dynamic(batch/3).          % Batch mode flag 
:- dynamic(consult/2).        % Consult mode flag 
:- dynamic(et/2).             % Extension Table 
:- dynamic(called/1).         % Call Patterns
:- dynamic(et_flag/1).        % Extension Table flag 
:- dynamic(complete_flag/2).  % Complete computation flag 
:- dynamic(datalog/6).        % Datalog Rules Database 
:- dynamic(strata/1).         % Result from a stratification
:- dynamic(pdg/1).            % Predicate Dependency Graph
:- dynamic(rule_id/1).        % Integer identifier for rules
:- dynamic(neg/1).            % Flag indicating the negation algorithm: et_not [SD91] or strata (optimized)
:- dynamic(duplicates/1).     % Flag indicating whether duplicates are enabled
:- dynamic(timing/1).         % Flag indicating elapsed time display: on, off or detailed
:- dynamic(time/3).           % Elapsed times for parsing, computation and display
:- dynamic(def_preds/1).      % List of defined predicates in the current program
:- dynamic(error/0).          % Flag indicating whether there was an error
:- dynamic(safe/1).           % Flag indicating whether program transformation for safe rules is allowed
:- dynamic(simplification/1). % Flag indicating whether program transformation for safe rules is allowed
:- dynamic(language/1).       % Flag indicating the current default query language
:- dynamic(start_path/1).     % Path on first initialization
:- dynamic(development/1).    % Flag indicating a development session. Listings and consultings show source and compiled rules
:- dynamic(safety_warnings/1).% Flag indicating whether safety warnings are enabled
:- dynamic(last_autoview/1).  % Flag indicating the last autoview executed. This autoview should be retracted upon exceptions
:- dynamic(current_db/2).     % Flag indicating the current opened DB
:- dynamic(rdb_id/2).         % Identifier for RDB tuples (coming from tables or views)
:- dynamic(trusting/1).       % Flag indicating whether a trust file is being processed
:- dynamic(trusted_views/1).  % Predicate containing trusted view names
:- dynamic(output/1).         % Flag indicating whether output is enabled (on or off)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RDB external data sources via ODBC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% datalog(Rule,NVs,RuleId,Lines,FileId,source)
enable_rdb_datasource :-
  current_db(Connection,_Handle),
  !,
  RDBDS = ':-'(    datalog(Rule,[],RuleId,[],rdb(Connection),source),
               datalog_rdb(Rule,[],RuleId,[],rdb(Connection),source)),
  (Connection\=='$des' ->
    (retract(RDBDS) -> true ; true),
    assertz(RDBDS)
   ;
    true).
enable_rdb_datasource.

% Data source for a single table/view
datalog_rdb(R,[],RuleId,[],rdb(Connection),source) :-
  nonvar(R),
  !,
  R \= ':-'(_H,_T),
  current_db(Connection,_Handle),
  Connection\=='$des',
  R=..[TableName|Columns],
  length(Columns,Arity),
  my_odbc_get_table_arity(TableName,Arity),
  prepare_rdb_ruleid(TableName),
  build_sql_rdb_datasource(TableName,Columns,SQLstr),
  my_odbc_dql_query_fetch_row(SQLstr,Row),
  Row=..[_AnswerRel|Columns],
  get_rdb_ruleid(TableName,RuleId).
% Data source for all the tables in the RDB
datalog_rdb(R,[],RuleId,[],rdb(Connection),source) :-
  current_db(Connection,_Handle),
  Connection\=='$des',
  my_odbc_get_table_and_view_names(TableNames),
  my_member(TableName,TableNames),
  my_odbc_get_table_arity(TableName,Arity),
  length(Columns,Arity),
  R=..[TableName|Columns],
  length(Columns,Arity),
  datalog_rdb(R,[],RuleId,[],rdb(Connection),source).

prepare_rdb_ruleid(TableName) :-
  retractall(rdb_id(_,_)),
  assertz(rdb_id(TableName,0)).
  
get_rdb_ruleid(TableName,RuleId) :-
  retract(rdb_id(TableName,OID)),
  ID is OID+1,
  RuleId=rdb_id(TableName,ID),
  assertz(RuleId).
  
build_sql_rdb_datasource(TableName,Columns,SQLstr) :-
  my_odbc_get_colnames(TableName,ColNames),
  build_where_cond(Columns,ColNames,[],CondStr),
  name(TableName,StrTableName),
  my_odbc_get_dbms(DBMS),
  my_sql_left_quotation_mark(LQstr,DBMS),
  my_sql_right_quotation_mark(RQstr,DBMS),
  concat_lists(["SELECT * FROM ",LQstr,StrTableName,RQstr," WHERE ",CondStr],SQLstr).

build_where_cond([],[],[],"TRUE").  
build_where_cond([Col],[Name],ICondStr,OCondStr) :-
  nonvar(Col),
  !,
  name(Col,ColStr),
  (atom(Col)-> concat_lists(["'",ColStr,"'"],ValStr) ; ValStr=ColStr),
  name(Name,NameStr),
  concat_lists([ICondStr,NameStr,"=",ValStr],OCondStr).  
build_where_cond([Col,Col1|Cols],[Name,Name1|Names],ICondStr,OCondStr) :-
  nonvar(Col),
  !,
  build_where_cond([Col],[Name],ICondStr,T1CondStr),
  my_append(T1CondStr," AND ",T2CondStr),
  build_where_cond([Col1|Cols],[Name1|Names],T2CondStr,OCondStr).
build_where_cond([_Col|Cols],[_Name|Names],ICondStr,OCondStr) :-
  build_where_cond(Cols,Names,ICondStr,OCondStr).

  
/*********************************************************************/
/* Initial Status                                                    */
/*********************************************************************/

set_initial_status :-
  assertz(verbose(off)),        % Verbose output disabled
  assertz(pretty_print(on)),    % Pretty print activated
  assertz(duplicates(off)),     % Duplicates disabled
  assertz(neg(strata)),         % Default algorithm for solving negation
  assertz(timing(off)),         % Elapsed time display disabled
  assertz(safe(off)),           % Program transformation disabled
  assertz(simplification(off)), % Program simplification disabled
  assertz(language(datalog)) ,  % Default interpreter language (possible values are: datalog, prolog, sql)
  assertz(development(off)),    % Development session on/off
  assertz(safety_warnings(on)), % Safety warnings on/off, no command for changing it
  assertz(trusting(off)),       % No trust file processing
  assertz(output(on)),          % Output enabled
  set_default_db.

% Datalog DB ($des) is the default DB. No connection data ($void)
set_default_db :-
  assertz(current_db('$des','$void')).
    
/*********************************************************************/
/* Autorun Information (only for reference)                          */
/*********************************************************************/

%Autorun:
% Sicstus:
%  -l des.pl
% SWI-Prolog:
%  -g "[des]"
% GNU-Prolog:
%  --entry-goal ['des.pl']
% CIAO Prolog:
%  -l ciaorc

/*********************************************************************/
/* Specific Files                                                    */
/*********************************************************************/

% Loading Prolog system-dependent predicates
:- initialization(consult('des_glue.pl')).
% Loading declarative debugger
:- initialization(consult('des_debug.pl')).
% Loading sql query processor
:- initialization(consult('des_sql.pl')).
% Loading test case generator
:- initialization(consult('des_tc.pl')).

/*********************************************************************/
/* Auto-start after loading                                          */
/*********************************************************************/

:- initialization((start;true)).
%:- initialization(start).


/*********************************************************************/
/* Starting the System from scratch: start                           */
/*********************************************************************/

start :- 
  init_des, 
%  display_status,
  des.

% System initialization on start
init_des :- 
  (start_path(D) -> cd_path(D) ; my_working_directory(D), assertz(start_path(D))),
  retractall(log(_,_)),
  retractall(batch(_,_,_)),
  retractall(consult(_,_)),
  retractall(rule_id(_,_)),
  processC(abolish,[],_NVs,_Continue),
  set_flag(et_flag,no),
  reset_id,
  set_initial_status,
  banner, 
  process_batch. %If des.ini exists, their entries are processed as command prompt inputs

% Entry execution point
des :-
  repeat, 
  catch(exec_des(Continue), M, (my_message(M), complete_pending_tasks)), 
  ((M == '$aborted'
    ;
    M == 'control_c' 
    ;
    Continue==no
   )
   ->
    true
   ;
    fail).

% Completing pending tasks upon exceptions
%::WARNING: Other views/rules might be removed upon exceptions. Hint: Use answer as a view and follow the dependencies
complete_pending_tasks :-
%  get_object_dlrules(name,answer,ODLs),
%  retract_dlrule_list(ODLs,_Error1),
  (retract(last_autoview(V)) ->
    get_object_dlrules(rule,V,OVDLs),
    retract_dlrule_list(OVDLs,_Error2)
   ;
    true),
  abolishET,
  compute_stratification,
  !.
  

% Exception handling
my_message(M) :- 
   (consult(_,CSt) ->    % Closes the program file currently loading, if any
    close(CSt),
    retractall(consult(_,_))
    ; 
    true), 
  ((M = instantiation_error(_,_)
    ;
    M = error(instantiation_error,_)  % New SICStus 4.x format
   ) ->
   true
  ;
   nl,
   my_print_message(error,M),
   nl
  ),
  (batch(_,_,_) -> 
   repoint_batch_file;
   true).

% Print exception messages
my_print_message(_,M) :-
  write_log_list([nl,'Exception: ',M,nl]).

% Relocating the pointer to the batch file
repoint_batch_file :- 
   batch(L,F,S),
  (my_current_stream(S) -> close(S);true),
  open(F,read,S1),
  set_input(S1),
  retractall(batch(_,_,_)),
  assertz(batch(L,F,S1)), 
  my_skip_line(L).       % Skip the offending line

my_skip_line(0) :- 
  !.
my_skip_line(N) :- 
  readln(_,_),
  N1 is N-1,
  my_skip_line(N1).
  
% Processing the batch file des.ini at the distribution directory
process_batch :-
  F = 'des.ini',
  my_file_exists(F),
  process_batch(F,'initialization batch').
process_batch.

process_batch(F) :-
  my_file_exists_with_default_extensions(F,['.sql','.ini'],FP),
  process_batch(FP,'file'),
  !.
process_batch(F) :-
  write_log_list(['Error: When processing file ''',F,'''',nl]).

my_file_exists_with_default_extensions(F,_Exts,CF) :-
  (my_file_exists(F) ->
    CF=F
   ;
    my_working_directory(D),
    atom_concat(D,F,CF),
    my_file_exists(CF)
  ),
  !.
my_file_exists_with_default_extensions(F,Exts,FP) :- 
  my_file_exists_with_default_extension_list(F,Exts,FP).

my_file_exists_with_default_extension_list(F,[],_FP) :-  
  write_log_list(['Error: File ''',F,''' not found.',nl]), 
  !,
  fail.
my_file_exists_with_default_extension_list(F,[Ext|_Exts],FP) :-  
  atom_concat(F,Ext,FExt),
  (my_file_exists(FExt) ->
    FP=FExt
   ;
    my_working_directory(D),
    atom_concat(D,FExt,FP),
    my_file_exists(FP)
  ),
  !.
my_file_exists_with_default_extension_list(F,[_Ext|Exts],FP) :-  
  my_file_exists_with_default_extension_list(F,Exts,FP).
  
process_batch(F,M) :-
  my_absolute_filename(F,CFN),
  seeing(OldInput),   % Current input stream
  open(CFN,read,BSt), % Open initialization batch
  set_input(BSt),
  assertz(batch(0,CFN,BSt)),
  write_log_list(['Info: Processing ',M,' ''',F,''' ...',nl,nl]), 
  process_batch_file_lines(OldInput).

process_batch_file_lines(OldInput) :-
  repeat,
    readln(S,E),          % Read a line
    (E == end_of_file ->
      true
     ;
      inc_line,
      write_prompt,
      write_string_log(S), 
      nl_log, 
      nl_log, 
      catch(process_input(S,_), M, my_message(M)), 
      (nonvar(M) -> complete_pending_tasks ; true),      
      nl_log, 
      fail),
  batch(_,_,BSt),
  close(BSt),             % Close the input batch file
  see(OldInput),          % Restore current input stream
  retractall(batch(_,_,_)),
  write_log('Info: Batch file processed.'), nl.
      
inc_line :-
  retract(batch(L,F,S)),
  L1 is L+1,
  assertz(batch(L1,F,S)).

/*********************************************************************/
/* Informative banner                                                */
/*********************************************************************/

banner :-
  write_log('*********************************************************'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('*        DES: Datalog Educational System v.2.0          *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('* Type "/help" for help about commands                  *'), nl_log,
  write_log('* Type "des." to continue if you get out of DES         *'), nl_log,
  write_log('*   from a Prolog interpreter                           *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('*                    Fernando Sáenz-Pérez (c) 2004-2010 *'), nl_log,
  write_log('*                                             DISIA UCM *'), nl_log,
  write_log('*             Please send comments, questions, etc. to: *'), nl_log,
  write_log('*                                     fernan@sip.ucm.es *'), nl_log,
  write_log('*                                             Web site: *'), nl_log,
  write_log('*                           http://des.sourceforge.net/ *'), nl_log,
  write_log('*********************************************************'), nl_log.


/*********************************************************************/
/* Datalog Prompt                                                    */
/*********************************************************************/

exec_des(Continue) :-
  nl_log, 
  write_prompt,
  flush_output, 
% WARNING: Remove "remove_start" when ACIDE gets fixed
  %  readln(S,_E), 
  readln(S0,_E), 
  remove_start(S0,S),
  write_only_to_log(S),
  nl_only_to_log, 
  nl_log, 
  process_input(S,Continue), 
  !.
exec_des(yes) :-
  write_log_list(['Error: Input processing error.', nl]).

remove_start(S,RS) :-
  my_append("start ",RS,S),
  !.
remove_start(S,S).

write_prompt :- 
  language(L),
  (L==datalog -> write_log('DES-Datalog> ') ; true),
  (L==prolog  -> write_log('DES-Prolog> ')  ; true),
  (L==sql     -> write_log('DES-SQL> ')     ; true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% readln(-String,-EOF). Read a line from the current input stream
% (console, by default). Informs whether the line ends with a
% 'end of file' or 'end of line' character.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readln(S,E) :-
        current_input(HIn),
        readln(HIn,S,E).

readln(HIn,Cs,E) :-
        my_get0(HIn,C),
        read_chars(HIn,C,Cs,E).

read_chars(_HIn,C,[],end_of_line) :-
        end_of_line(C), !.
read_chars(_HIn,C,[],end_of_file) :-
        end_of_file(C), !.
read_chars(HIn,C,[C|Cs],E) :-
        readln(HIn,Cs,E).

end_of_line(C) :- (C = 13; C = 10).

end_of_file(C) :- (C = 26; C = -1).


/*********************************************************************/
/* Preprocessing                                                     */
/* preprocess(+Rule,-SimplifiedRules,-TransformedRules,              */
/*            +GroundVars     % Vars known to be ground at run-time  */
/*            +NameVariables, +CopiedNameVariables,                  */
/*            +Action,+Object,-Causes,?Simplify,-Error)              */
/*********************************************************************/

% Preprocessing consists of source to source translations for:
% - not(is_null(G)) -> is_not_null(G)
% - Grouped aggregates
% - Disjunctive bodies
% - Outer joins. May deliver conditions which can be further simplified
% - Simplification of equality (=), true, not(true) and not(false) goals

preprocess(Rule,SRules,Rules,IArgs,NVs,SNVs,Action,Object,Causes,Simplify,Error) :-
  copy_term((Rule,NVs),(CRule,CNVs)),
  replace_not_is_null(CRule,RNRule),
  translate_aggregates_rule_list([RNRule],CNVs,Simplified0,Exploded0,TARules),
  disjunctive_to_conjunctive_rule_list(TARules,CNVs,DNVs,Exploded1,CRules),
  simplify_rules(CRules,SCRules,Simplify,Simplified1),
  (Exploded1==true -> Object1 = view ; Object1 = Object),
  make_safe_list(SCRules,SRules,IArgs,DNVs,Action,Object1,Safed,Error),
  translate_outer_joins_list(SRules,TRules),
  disjunctive_to_conjunctive_rule_list(TRules,DNVs,SNVs,Exploded2,CRules2),
  simplify_rules(CRules2,Rules,Simplify,Simplified2),
  ((Simplified0==true ; Simplified1==true ; Simplified2==true) -> SCauses=[simplification] ; SCauses=[]),
  (Safed==true -> SfCauses = [safety|SCauses] ; SfCauses=SCauses),
  ((Exploded0==true ; Exploded1==true ; Exploded2==true) -> Causes = [exploded|SfCauses] ; Causes=SfCauses).


% Replaces all occurrences of not(is_null(S)) by is_not_null(S) in a term T
% not(null(T)) has to be avoided because of computation by strata
replace_not_is_null(T,T) :- 
  (number(T) ; var(T) ; atom(T)),
  !.
replace_not_is_null(not(is_null(T)),is_not_null(T)) :- 
  !.
replace_not_is_null(C,RC) :- 
  C =.. [F|As],
  !, 
  replace_not_is_null_list(As,RAs),
  RC =.. [F|RAs].

replace_not_is_null_list([],[]) :-
  !.
replace_not_is_null_list([T|Ts],[RT|RTs]) :-
  !, 
  replace_not_is_null(T,RT), 
  replace_not_is_null_list(Ts,RTs).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating aggregates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% Translate aggregate predicates with compound goals, as in min((p(X,Y),X>Y),X,M) and group_by((p(X,Y),q(Y,Z)),[X],R=max(X))
translate_aggregates_rule_list([],_,_,_,[]).
translate_aggregates_rule_list([Rule|Rules],NVs,Simplified,Exploded,TRules) :-
  translate_aggregates(Rule,NVs,Simplified,Exploded,TRules1),
  translate_aggregates_rule_list(Rules,NVs,Simplified,Exploded,TRules2),
  my_append(TRules1,TRules2,TRules).

translate_aggregates(':-'(H,B),NVs,Simplified,Exploded,[':-'(H,TB)|DLs]) :-
  !,
  my_term_variables(H,HVs),
  translate_body_aggregates(B,TB,HVs,NVs,Simplified,Exploded,[],TDLs),
  reorder_goals_by_efficiency_rule_list(TDLs,DLs).
translate_aggregates(H,_NVs,_Simplified,_Exploded,[H]).


translate_body_aggregates(T,T,_HVs,_NVs,_Simplified,_Exploded,DLs,DLs) :-
  (atomic(T) ; var(T)),
  !.
translate_body_aggregates(group_by(P,GBVs,C),group_by(TP,GBVs,TC),HVs,NVs,Simplified,Exploded,DLsi,DLso) :-
  !,
  translate_aggregate_goal(P,TP,HVs,NVs,Exploded,DLsi,DLsi1),
  translate_aggregate_cond(C,TC,HVs,NVs,Simplified,Exploded,DLsi1,DLso).
translate_body_aggregates(G,TG,HVs,NVs,_Simplified,Exploded,DLsi,DLso) :-
  G=..[AF,P|Args],
  length([P|Args],Arity),
%  (my_aggregate_relation(AF,Arity) ; (AF,Arity)==(group_by,3)),
  my_aggregate_relation(AF,Arity),
  !,
  translate_aggregate_goal(P,TP,HVs,NVs,Exploded,DLsi,DLso),
  my_term_variables(P,PVs),
  my_intersect_var(HVs,PVs,GBVs),
  insert_into_last_but_one_pos(Args,GBVs,RArgs),
  TG=..[AF,TP|RArgs].
translate_body_aggregates(G,TG,HVs,NVs,Simplified,Exploded,DLsi,DLso) :- 
  G =.. [F|As],
  !, 
  translate_body_aggregates_list(As,RAs,HVs,NVs,Simplified,Exploded,DLsi,DLso),
  TG =.. [F|RAs].
      
translate_body_aggregates_list([],[],_HVs,_NVs,_Simplified,_Exploded,DLs,DLs).
translate_body_aggregates_list([T|Ts],[TT|TTs],HVs,NVs,Simplified,Exploded,DLsi,DLso) :-
  !, 
  translate_body_aggregates(T,TT,HVs,NVs,Simplified,Exploded,DLsi,DLsi1), 
  translate_body_aggregates_list(Ts,TTs,HVs,NVs,Simplified,Exploded,DLsi1,DLso).

translate_aggregate_goal(G,G,_HVs,_NVs,_Exploded,DLs,DLs) :-
  my_literal(G),
  !.
translate_aggregate_goal(G,TP,HVs,NVs,true,DLsi,[':-'(CTP,CTG)|DLso]) :-
  translate_body_aggregates(G,TG,HVs,NVs,_Simplified,_Exploded,DLsi,DLso),
  my_term_variables(G,Vs),
  'Vs2NVs'(Vs,NVs,GNVs),
  remove_non_relevant_vars(TG,GNVs,RNVs),
  get_new_predicate_name(p,P),
  build_head(P,RNVs,TP),
  copy_term((TP,TG),(CTP,CTG)).

translate_aggregate_cond(C,TC,_HVs,NVs,Simplified,Exploded,DLsi,DLso) :-
  replace_and_get_aggregates_equalities(C,[],LEQs,NSBody),
  (LEQs == [] ->
    EQs=true
   ;
    my_list_to_tuple(LEQs,EQs)),
  simplify_body(NSBody,Body),
%  NSBody=Body,
  my_term_variables(Body,BVs),
%  'Vs2NVs'(BVs,NVs,BNVs),
  my_var_name_list(BVs,NVs,BNVs),
  remove_non_relevant_vars(Body,BNVs,RNVs), % Non-relevant vars are set vars in aggregates, as X in avg(p(X),X,Y)
  get_new_predicate_name(p,P),
  build_head(P,RNVs,Head),
  my_term_variables(RNVs,HeadVars),
  get_arg_position_list(HeadVars,HeadVars,IArgs),
  Rule = ':-'(Head,Body),
  preprocess(Rule,SRules,Rules,IArgs,NVs,_SNVs,exec,view,Causes,_Simplify,_Error),
  (my_member_chk(exploded,Causes) ->
    goals_append(EQs,Head,TC),
    my_append(DLsi,Rules,DLso),
    Exploded=true
   ;
%     SRules=[':-'(_TH,TB)],
%     goals_append(EQs,TB,TC),
%     DLso=DLsi,
%     Simplified=true).
   (my_member_chk(simplified,Causes) ->
    SRules=[':-'(_TH,TB)],
    goals_append(EQs,TB,TC),
    DLso=DLsi,
    Simplified=true
    ;
    goals_append(EQs,Body,TC),
    DLso=DLsi)).
  
% Gets equalities aggregate(Variable)=Result from a term including aggregates. It also replaces aggregate(Variable) by Result in the term
replace_and_get_aggregates_equalities(T,Eqs,Eqs,T) :- 
  var(T),
  !.
replace_and_get_aggregates_equalities('$NULL'(ID),Eqs,Eqs,'$NULL'(ID)) :- 
  !.
% replace_and_get_aggregates_equalities(V1=V2,Eqs,Eqs,V1=V2) :- 
%   var(V1),
%   var(V2),
%   !.
replace_and_get_aggregates_equalities(R=C,Eqsi,Eqso,NR=R) :- 
  var(R),
  C =.. [F,_V],
  arithmetic_function(F,_,_,aggregate,_,1),
  !,
  (my_member_var(C,R=C,Eqsi) ->
    Eqso=Eqsi
   ;
    Eqso=[NR=C|Eqsi]).
replace_and_get_aggregates_equalities(C=R,Eqsi,Eqso,NR=R) :- 
  var(R),
  C =.. [F,_V],
  arithmetic_function(F,_,_,aggregate,_,1),
  !,
  (my_member_var(C,R=C,Eqsi) ->
    Eqso=Eqsi
   ;
    Eqso=[NR=C|Eqsi]).
replace_and_get_aggregates_equalities(R=C,Eqsi,Eqso,NR=R) :- 
  var(R),
  atom(C),
  arithmetic_function(C,_,_,aggregate,_,0),
  !,
  (my_member_var(C,R=C,Eqsi) ->
    Eqso=Eqsi
   ;
    Eqso=[NR=C|Eqsi]).
replace_and_get_aggregates_equalities(C=R,Eqsi,Eqso,NR=R) :- 
  var(R),
  atom(C),
  arithmetic_function(C,_,_,aggregate,_,0),
  !,
  (my_member_var(C,R=C,Eqsi) ->
    Eqso=Eqsi
   ;
    Eqso=[NR=C|Eqsi]).
replace_and_get_aggregates_equalities(C,Eqsi,Eqso,R) :- 
  C =.. [F|_Vs],
  arithmetic_function(F,_,_,aggregate,_,_Arity),
  !,
  (my_member_var(C,R=C,Eqsi) ->
    Eqso=Eqsi
   ;
    Eqso=[R=C|Eqsi]).
replace_and_get_aggregates_equalities(T,Eqs,Eqs,T) :- 
  atomic(T),
  !.
replace_and_get_aggregates_equalities(C,Eqsi,Eqso,RC) :- 
  C =.. [F|As],
  replace_and_get_aggregates_equalities_list(As,Eqsi,Eqso,RAs),
  RC =.. [F|RAs].

replace_and_get_aggregates_equalities_list([],Eqs,Eqs,[]) :-
  !.
replace_and_get_aggregates_equalities_list([T|Ts],Eqsi,Eqso,[RT|RTs]) :-
  replace_and_get_aggregates_equalities(T,Eqsi,Eqsi1,RT), 
  replace_and_get_aggregates_equalities_list(Ts,Eqsi1,Eqso,RTs).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translating outer joins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translate_outer_joins_list
translate_outer_joins_list([],[]).
translate_outer_joins_list([R|Rs],TRs) :-
  translate_outer_joins(R,TRs1),  
  translate_outer_joins_list(Rs,TRs2),
  my_append(TRs1,TRs2,TRs).

translate_outer_joins(':-'(Head,Body),[':-'(Head,TBody)|DLs]) :-
  !,
  translate_body_outer_joins(Body,TBody,[],TDLs),
  reorder_goals_by_efficiency_rule_list(TDLs,DLs).
translate_outer_joins(Head,[Head]).

translate_body_outer_joins(OJG,TG,DLsi,DLso) :-
  OJG =.. [OJ,_L,_R,_C],
  my_outer_join_relation(OJ/_Arity),
  !,
  translate_outer_join_argument(OJG,TG,DLsi,DLso).
translate_body_outer_joins((G,Gs),(TG,TGs),DLsi,DLso) :-
  !,
  translate_body_outer_joins(G,TG,DLsi,DLs1),
  translate_body_outer_joins(Gs,TGs,DLs1,DLso).
translate_body_outer_joins(G,G,DLs,DLs).
  
my_outer_join_relation(lj/3).
my_outer_join_relation(rj/3).
my_outer_join_relation(fj/3).

translate_outer_join_argument(OJG,TOJG,DLsi,DLso) :-
  OJG =.. [OJ,L,R,C],
  my_outer_join_relation(OJ/3),
  OJ \== fj,
  !,
  translate_outer_join_argument(L,TL,DLsi,DLTLs),
  translate_outer_join_argument(R,TR,DLTLs,DLTRs),
  get_new_predicate_name(p,P),
  TL =.. [LF|LFs],
  TR =.. [RF|RFs],
  (my_outer_join_relation(LF/_LFAr) -> (LFs=[XL], XL=..[_XLF|LArgs]) ; TL =.. [_LFT|LArgs]),
  (my_outer_join_relation(RF/_RFAr) -> (RFs=[XR], XR=..[_XRF|RArgs]) ; TR =.. [_RFT|RArgs]),
  my_append(LArgs,RArgs,Args),
  TG =.. [P|Args],
  TOJG =.. [OJ,TG],
  build_outer_join_tuple(OJ,LArgs,RArgs,NArgs),
  TNG =.. [P|NArgs],
  build_datalog_rules_outer_join(OJ,TG,TL,TR,C,TNG,DLTRs,DLso).
translate_outer_join_argument(fj(L,R,C),TOJG,DLsi,DLso) :-
  !,
  translate_outer_join_argument(L,TL,DLsi,DLTLs),
  translate_outer_join_argument(R,TR,DLTLs,DLTRs),
  get_new_predicate_name(p,P),
  TL =.. [LF|LFs],
  TR =.. [RF|RFs],
  (my_outer_join_relation(LF/_LFAr) -> (LFs=[XL], XL=..[_LFX|LArgs]) ; TL =.. [_LFT|LArgs]),
  (my_outer_join_relation(RF/_RFAr) -> (RFs=[XR], XR=..[_RFX|RArgs]) ; TR =.. [_RFT|RArgs]),
  my_append(LArgs,RArgs,Args),
  TG =.. [P|Args],
  TOJG =.. [fj,TG],
  build_outer_join_tuple(lj,LArgs,RArgs,LNArgs),
  build_outer_join_tuple(rj,LArgs,RArgs,RNArgs),
  LTNG =.. [P|LNArgs],
  RTNG =.. [P|RNArgs],
  build_datalog_rules_outer_join(fj,TG,TL,TR,C,LTNG,RTNG,DLTRs,DLso).
translate_outer_join_argument(L,L,DLs,DLs) :-
  my_atom(L),
  !.
translate_outer_join_argument(L,L,DLs,DLs) :-
  my_raise_exception(L,basic_goal,[]).

build_datalog_rules_outer_join(lj,TG,TL,TR,C,TNG,DLsi,DLso) :-
  TG =.. [_G|Args],
  get_new_predicate_name(p,P),
  TG1 =.. [P|Args],
  (C == true ->
   copy_term_list([':-'(TG,TG1),':-'(TNG,(TL,not(TG1))),':-'(TG1,(TL,TR))],DLs)
   ;
   copy_term_list([':-'(TG,TG1),':-'(TNG,(TL,not(TG1))),':-'(TG1,(TL,TR,C))],DLs)),
   my_append(DLs,DLsi,DLso).
build_datalog_rules_outer_join(rj,TG,TL,TR,C,TNG,DLsi,DLso) :-
  build_datalog_rules_outer_join(lj,TG,TR,TL,C,TNG,DLsi,DLso).
build_datalog_rules_outer_join(fj,TG,TL,TR,C,LTNG,RTNG,DLsi,DLso) :-
  TG =.. [_G|Args],
  get_new_predicate_name(p,P),
  TG1 =.. [P|Args],
  (C == true ->
   copy_term_list([':-'(TG,TG1),':-'(LTNG,(TL,not(TG1))),':-'(RTNG,(TR,not(TG1))),':-'(TG1,(TL,TR))],DLs)
   ;
   copy_term_list([':-'(TG,TG1),':-'(LTNG,(TL,not(TG1))),':-'(RTNG,(TR,not(TG1))),':-'(TG1,(TL,TR,C))],DLs)),
   my_append(DLs,DLsi,DLso).

build_outer_join_tuple(lj,LArgs,RArgs,Args) :-
  length(RArgs,N),
  build_null_list(N,NULLs),
  my_append(LArgs,NULLs,Args).
build_outer_join_tuple(rj,LArgs,RArgs,Args) :-
  length(LArgs,N),
  build_null_list(N,NULLs),
  my_append(NULLs,RArgs,Args).

build_null_list(0,[]) :-
  !.
build_null_list(N,['$NULL'(_ID)|NULLs]) :-
  N1 is N-1,
  build_null_list(N1,NULLs).
  
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simplify_rules(+DDLsts,-DLsts,-Simplified) 
% Simplifies a list of Datalog rules with equalities
% Equality goals are removed and their arguments unified
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_rules(Rs,SRs,Simplify,Simplified) :-
  ((simplification(on) ; Simplify==simplify) ->
    force_simplify_rules(Rs,SRs,Simplified)
   ;
    SRs=Rs).

force_simplify_rules([],[],_S).
force_simplify_rules([R|Rs],[SR|SRs],S) :-
  R = ':-'(H,B),
  !,
  simplify_body(B,SB,S),
  (SB==true -> SR=H ; SR = ':-'(H,SB)),
  force_simplify_rules(Rs,SRs,S).
force_simplify_rules([R|Rs],[R|SRs],S) :-
  force_simplify_rules(Rs,SRs,S).

simplify_body(Body,SBody,Simplified) :-
  simplify_body(Body,SBody),
  (Body==SBody ->
   true
   ;
   Simplified=true).

simplify_body((true;Bs),SBs) :-
  !,
  simplify_body(Bs,SBs).
simplify_body((Bs;true),SBs) :-
  !,
  simplify_body(Bs,SBs).
% simplify_body((B1s;B2s),SBs) :-
%   !,
%   simplify_body(B1s,SB1s),
%   simplify_body(B2s,SB2s),
%   (SB1s == true ->
%    SBs = SB2s
%    ;
%    (SB2s == true ->
%     SBs = SB1s
%     ;
%     SBs = (SB1s;SB2s))).
simplify_body((B,Bs),SBody) :-
  !,
  simplify_goal(B,SB),
  simplify_body(Bs,SBs),
  goals_append(SB,SBs,SBody).
simplify_body(B,SB) :-
  !,
  simplify_goal(B,SB).
    
% Simplify goal
simplify_goal(A=B,G) :-
  !,
  (my_ground(A) -> eval_expr(A,EA,_) ; EA = A),
  (my_ground(B) -> eval_expr(B,EB,_) ; EB = B),
  (my_noncompoundterm(EA),
   my_noncompoundterm(EB)
   ->
    (EA=EB -> G=true ; G=false)
   ;
    G=(EA=EB)).
simplify_goal(not(true),false) :-
  !.
simplify_goal(not(false),true) :-
  !.
simplify_goal(not(false),true) :-
  !.
simplify_goal(G,G).
  
% append_goal(true,true,true) :-
%   !.
% append_goal(true,LBs,LBs) :-
%   !.
% append_goal(G,true,G) :-
%   !.
% append_goal(B,LBs,NLBs) :-
%   tuple_append(B,LBs,NLBs).

    
/*********************************************************************/
/* Processing                                                        */
/*********************************************************************/

process_input(end_of_file,yes) :- !.
process_input(Input,Continue) :-
  reset_elapsed_time,
  % cd and ls commands can end with a mandatory dot (e.g., "cd ." or "cd ..")
  (((Command=cd;Command=ls;Command=dir), 
    (parse_command(Command,[..],_NVsT,Input,[]); 
     parse_command(Command,[.],_NVsF,Input,[]))) -> 
   (Input=CInput, !);
   (my_append(CInput,".",Input), !; % Inputs are allowed to end with an optional dot
    CInput=Input)),
   (command(CInput) -> 
      process_command(CInput,Continue), !
    ; 
	    (blank_input(CInput),
       store_elapsed_time(parsing),
       store_elapsed_time(computation),
	     !
	    ; 
	     process_remark(CInput), 
	     !
	    ;
	     language(datalog), 
	     process_datalog(CInput), 
	     !
	    ; 
	     language(prolog),  
	     process_prolog(CInput), 
	     !
	    ; 
	     language(sql),     
	     process_sql(CInput), 
	     !
	    ; 
	     invalid_input)
	 ).

process_command(SCommand,yes) :- 
  lang_interpreter_cmd(Language,SCommand,CInput),
  my_not(my_blanks_star(CInput,[])),
  % CInput\==[],
  !,
  atom_concat('process_',Language,F),
  P=..[F,CInput],
  call(P).
process_command(SCommand,Continue) :- 
  ((parse_command(Command,Arguments,NVs,SCommand,CInput),
    my_blanks_star(CInput,[])) -> 
    store_elapsed_time(parsing),
    processC(Command,Arguments,NVs,Continue)
   ; 
    syntax_error('command and/or its argument. Type /help for help on commands')).

process_datalog(CInput) :-
  reset_id,
  (my_blanks_star(CInput,[]) ->  % Switch to Datalog command prompt, empty argument
    true
    ;
    write_verb_list(['Info: Parsing query...',nl])),
  (process_datalog_query(CInput), 
   !
  ; 
   process_view(CInput), 
   !
  ; 
   process_autoview(CInput)
  ).

% Processing a Prolog goal  
process_prolog(CInput) :-
  write_verb_list(['Info: Parsing goal...',nl]),
  parse_body(Goal,[],NVs,CInput,[]), 
  write_verb_list(['Info: Goal successfully parsed.',nl]),
  store_elapsed_time(parsing),
  solve_prolog(Goal,NVs).

% Processing a SQL query
process_sql(QueryStr) :-
  write_verb_list(['Info: Parsing query...',nl]),
  parse_sql_query(Query,QueryStr,[]), 
  !,
  write_verb_list(['Info: Query successfully parsed.',nl]),
  store_elapsed_time(parsing),
  reset_id,
  solve_sql_query(QueryStr,Query).
% Failing DES SQL parsing of a statement to an ODBC connection does not definitely mean that the statement is wrong
% It will be submitted to the ODBC controller, which will raise an exception if the statement is actually wrong
process_sql(QueryStr) :-
  current_db(Connection,_Handle),
  Connection\=='$des',
  write_verb_list(['Info: Giving up parsing to ODBC controller.',nl]),
  store_elapsed_time(parsing),
  solve_sql_query(QueryStr,unknown).

% Processing a Datalog query
process_datalog_query(SBody) :-  
  parse_datalog_query(Body,NVs,SBody,[]),
  write_verb_list(['Info: Query successfully parsed.',nl]),
  store_elapsed_time(parsing),
  process_datalog_query(Body,NVs).
  
% Aggregates at the system prompt
process_datalog_query(Body,NVs) :-
  functor(Body,AF,Arity),
  (my_aggregate_relation(AF,Arity)
   ;
   (AF,Arity)==(group_by,3)
  ),
  !,
  remove_non_relevant_vars(Body,NVs,RNVs), % Non-relevant vars are set vars in aggregate predicates, as X in avg(p(X),X,Y)
  build_head(answer,RNVs,Head),
  Rule = ':-'(Head,(Body,0=0)),
  retract(simplification(S)),
  assertz(simplification(on)),
  retractall(last_autoview(_)),
  process_rule(Rule,NVs,autoview),
%  process_rule(Rule,NVs,query),
  retract(simplification(on)), % WARNING: If Query fails, simplification mode is lost
  assertz(simplification(S)).
% A non-aggregate query 
process_datalog_query(Body,NVs) :- 
  remove_non_relevant_vars(Body,NVs,RNVs), % Non-relevant vars are set vars in aggregates, as X in avg(p(X),X,Y)
  build_head(answer,RNVs,Head),
  retractall(last_autoview(_)),
  Rule = ':-'(Head,Body),
  process_rule(Rule,NVs,query).
  
process_view(SRule) :-
  name(':-',[S,D]),
  my_appendfind(SHead,[S,D|SBody],SRule), 
  (parse_head(Head,[],V,SHead,[]) -> true; syntax_error('rule head'), fail),
  (parse_body(Body,V,NVs,SBody,[]) -> true; syntax_error('rule body'), fail),
  write_verb_list(['Info: View successfully parsed.',nl]),
  store_elapsed_time(parsing),
  Rule = ':-'(Head,Body),
  retractall(last_autoview(_)),
  assertz(last_autoview(Rule)),
  process_rule(Rule,NVs,view).

process_autoview(SBody) :-
  parse_body(Body,[],NVs,SBody,[]),
  remove_non_relevant_vars(Body,NVs,RNVs), % Non-relevant vars are set vars in aggregates, as X in avg(p(X),X,Y)
  build_head(answer,RNVs,Head),
  write_verb_list(['Info: Conjunctive query successfully parsed.',nl]),
  store_elapsed_time(parsing),
  Rule = ':-'(Head,Body),
  retractall(last_autoview(_)),
  assertz(last_autoview(Rule)),
  process_rule(Rule,NVs,autoview).

process_rule(Rule,NVs,QueryType) :-
  preprocess(Rule,SRules,Rules,[],NVs,SNVs,exec,QueryType,Causes,_Simplify,Error),
  (Error==true ->
   true
   ;
   process_compiled_rule(Rule,SRules,Rules,NVs,SNVs,QueryType,Causes)
  ).

process_compiled_rule(ORule,SRules,Rules,NVs,SNVs,QueryType,Causes) :-
  ORule =.. [':-',Head,OBody],
  Rules = [_Rule|TRules],
  SRules = [SRule|_SRules],
  (SRule =.. [':-',_SHead,SBody] ; true),
  !,
  ((QueryType==query, TRules==[]) ->
   true
   ;
   (strata(CurrentStrata),                         % Increased program, compute stratification
    pdg(CurrentPDG),
    my_datetime(X),
    my_append(NVs,SNVs,ANVs),
    build_datalog_rules(ORule,Rules,ANVs,[],asserted(X),DLs),
    assert_list(DLs,Error),
    retractall(complete_flag(_G,_CF)))
  ),
  ((QueryType==query, TRules==[]) -> % A basic query
%   Query=OBody
   (nonvar(SBody) -> Query=SBody ; Query = OBody)
   ;
   (Query=Head,
    write_log_list(['Info: Processing:',nl]), % A transformed query and/or conjunctive query
    (my_member(exploded,Causes) ->
      write_log('  '),
      write_with_NVs(Query,NVs),
      write_log_list([nl,'in the program context of the exploded query:',nl]),
      rule_to_ruleNV_list(SRules,SNVs,DRules),
      display_ruleNVs_list(DRules,2)
      ;
      display_ruleNVs_list([(SRule,SNVs)],2)
    ),
    compute_stratification)
  ),
  solve_datalog_query(Query,Undefined),
  store_elapsed_time(computation),
  display_solutions(Query,Undefined),
  display_elapsed_time,
  ((QueryType==query, TRules==[]) ->
   true                                            % Basic query: No rules have been added
   ;
   (retract_list(DLs,Error),                       % Transformed query: Added rules have to be removed
    abolishET,                                     % Decrease program, compute stratification
    load_stratification(CurrentStrata,CurrentPDG)) % TODO: Restore previous state
  ).

build_head(Functor,NVs,Head) :-
  'NVs2Vs'(NVs,Vs),
  Head =.. [Functor|Vs].
  
remove_non_relevant_vars(Body,NVs,RNVs) :-
  find_non_relevant_vars(Body,NVs,[],NRNVs),
  my_subtract_var(NVs,NRNVs,RNVs).

find_non_relevant_vars(T,_NVs,NRNVs,NRNVs) :-
  (var(T) ; atomic(T)),
  !. 
find_non_relevant_vars(T,NVs,INRNVs,ONRNVs) :- 
  T=..[AF,P|Args],
  length([P|Args],Arity),
  my_aggregate_relation(AF,Arity),
  !,
  my_term_variables(P,Vs),
  find_var_name_list(Vs,NVs,NRNVs),
  my_append(NRNVs,INRNVs,ONRNVs).
find_non_relevant_vars(T,NVs,INRNVs,ONRNVs) :- 
  T=..[group_by,P,GBVs|Args],
  length([P,GBVs|Args],3),
  !,
  my_term_variables(P,Vs),
  my_subtract_var(Vs,GBVs,SVs),
  find_var_name_list(SVs,NVs,NRNVs),
  my_append(NRNVs,INRNVs,ONRNVs).
find_non_relevant_vars(C,NVs,INRNVs,ONRNVs) :- 
  C =.. [_F|As],
  !, 
  find_non_relevant_vars_list(As,NVs,INRNVs,ONRNVs).

find_non_relevant_vars_list([],_NVs,NRNVs,NRNVs) :-
  !.
find_non_relevant_vars_list([T|Ts],NVs,INRNVs,ONRNVs) :-
  !, 
  find_non_relevant_vars(T,NVs,INRNVs,TNRNVs), 
  find_non_relevant_vars_list(Ts,NVs,TNRNVs,ONRNVs).

process_remark(SRemark) :-
  parse_line_remark(SRemark,_), !.

blank_input(SBlanks) :-
  my_blanks_star(SBlanks,[]).  

invalid_input :-
  language(datalog),
  !,
  write_log('Error: Input not recognized as a valid Datalog query, view, autoview or command.'), nl_log, 
  write_log('  Queries  : Atom      |'), nl_log, 
  write_log('             not(Atom) |'), nl_log, 
  write_log('             X Infix Y '), nl_log, 
  write_log('  Views    : Head :- Body'), nl_log, 
  write_log('  Autoviews: Body'), nl_log, 
  write_log('  Commands : /Command Argument(s)'), nl_log, 
  write_log('Queries, views and commands can optionally end with a dot.'), nl_log.
invalid_input :-
  language(sql),
  !,
  write_log('Error: Input not recognized as a valid SQL statement or command.'), nl_log, 
  write_log('  Queries : Consult the manual for syntax'), nl_log, 
  write_log('  Commands: /Command Argument(s)'), nl_log, 
  write_log('Queries and commands can optionally end with a semicolon or a dot, resp.'), nl_log.
invalid_input :-
  language(prolog),
  !,
  write_log('Error: Input not recognized as a valid Prolog goal or command.'), nl_log, 
  write_log('  Goals   : Atom      |'), nl_log, 
  write_log('            not(Atom) |'), nl_log, 
  write_log('            X Infix Y '), nl_log, 
  write_log('  Commands: /Command Argument(s)'), nl_log, 
  write_log('Queries and commands can optionally end with a dot or a semicolon.'), nl_log.

processC(SQuit,[],_NVs,no) :- 
  (SQuit=q; SQuit=quit; SQuit=e; SQuit=exit; SQuit=halt), 
  !, 
  (log(_F,_S) -> processC(nolog,[],_,no) ; true),
  current_db(C,_H),
  (C\=='$des', processC(close_db,[],_,_), fail
   ;
   true),
  halt.
processC(STerminate,[],_NVs,no) :- 
  STerminate=t, 
  !.
processC(SSpy,[Predicate],_NVs,yes) :- 
  SSpy=spy, 
  !,
  spy(Predicate).
processC(SNoSpy,[],_NVs,yes) :- 
  SNoSpy=nospyall, 
  !,
  nospyall.
processC(SCommand,[C],_NVs,yes) :- 
  (SCommand=shell; SCommand=s), 
  !, 
  my_shell(C,S),
  (S=0 -> 
    (nl_log, 
     write_verb(['Info: Operating system command executed.']))
   ;
    (write_log('Error: Operating system command failed.'), nl_log)).
processC(SHelp,[],_NVs,yes) :- 
  (SHelp=h; SHelp=help), 
  !,
  write_log('Available Commands:'), nl_log,
  write_log(' * Rule Database Commands:'), nl_log,
  write_log('   - /[Filenames]         Consult Datalog files, abolishing previous rules'), nl_log,
  write_log('   - /[+Filenames]        Consult Datalog files, keeping previous rules'), nl_log,
  write_log('   - /abolish             Abolish all Datalog rules'), nl_log,
  write_log('   - /abolish Name        Abolish all Datalog rules matching a predicate name'), nl_log,
  write_log('   - /abolish Name/Arity  Abolish all Datalog rules matching the pattern'), nl_log,
  write_log('   - /assert Head:-Body   Assert a rule. :-Body is optional (for facts)'), nl_log,
  write_log('   - /consult Filename    Consult a Datalog file, abolishing previous rules'), nl_log,
  write_log('   - /c Filename          Shorthand for /consult Filename'), nl_log,
  write_log('   - /listing             List Datalog rules'), nl_log,
  write_log('   - /listing Name        List Datalog rules matching a name'), nl_log,
  write_log('   - /listing Name/Arity  List Datalog rules matching the pattern'), nl_log,
  write_log('   - /listing Head        List Datalog rules whose head is subsumed by Head'), nl_log,
  write_log('   - /listing Head:-Body  List Datalog rules that are subsumed by Head:-Body'), nl_log,
  write_log('   - /reconsult Filename  Consult a Datalog file, keeping previous rules'), nl_log,
  write_log('   - /r Filename          Shorthand for /reconsult Filename '), nl_log,
  write_log('   - /restore_ddb Filename Restore the Datalog database in file (same as consult)'), nl_log,
  write_log('   - /retract Head:-Body  Retract a rule. :-Body is optional (for facts)'), nl_log,
  write_log('   - /retractall Head     Retract all rules matching the given head'), nl_log,
  write_log('   - /save_ddb Filename   Save the current Datalog database to a file'), nl_log,
  write_log(' * Relational Database Commands:'), nl_log,
  write_log('   - /open_db Conn [Opts] Open and set the current ODBC connection,'), nl_log,
  write_log('                          where Opts=[user(Name)] [password(Pwd)].'), nl_log,
% TODO: Enable the following when multiple connections are allowed
%  write_log('   - /close_db Connection Close the specified ODBC connection'), nl_log,
  write_log('   - /close_db            Close the current ODBC connection'), nl_log,
  write_log('   - /current_db          Display the current ODBC connection name and DSN provider'), nl_log,
  write_log(' * Debugging and Test Case Generation:'), nl_log,
  write_log('   - /debug_datalog Goal [Level]'), nl_log,
  write_log('                          Debug a Datalog basic goal at clause (c) or the default predicate (p) level'), nl_log,
%  write_log('   - /debug_sql View [Opts]'), nl_log,
%  write_log('                          Debug a SQL view where Opts=[trust_tables([yes|no])] [trust_file(FileName)]'), nl_log,
  write_log('                          Defaults are trust tables (postorder traversing) and no trust file'), nl_log,
  write_log('   - /trace_datalog Goal [Order]'), nl_log,
  write_log('                          Trace a Datalog basic goal in preorder (default) or postorder'), nl_log,
  write_log('   - /trace_sql View [Order]'), nl_log,
  write_log('                          Trace a SQL view in preorder (default) or postorder'), nl_log,
  write_log('   - /test_case View [Opts]'), nl_log,
  write_log('                          Generate test case classes for the given view,'), nl_log,
  write_log('                          where Opts=[all|positive|negative] [display|add|replace]'), nl_log,
  write_log('   - /tc_size Min Max     Set the minimum and maximum number of tuples generated for a test case'), nl_log,
  write_log('   - /tc_size             Display the minimum and maximum number of tuples generated for a test case'), nl_log,
  write_log('   - /tc_domain Min Max   Set the domain of values for test cases between Min and Max'), nl_log,
  write_log('   - /tc_domain           Display the domain of values for test cases'), nl_log,
  write_log(' * Extension Table Commands:'), nl_log,
  write_log('   - /clear_et            Clear the extension table'), nl_log,
  write_log('   - /list_et             List contents of the extension table'), nl_log,
  write_log('   - /list_et Name        List contents of the extension table matching a name'), nl_log,
  write_log('   - /list_et Name/Arity  List contents of the extension table matching the pattern'), nl_log,
  write_log(' * Operating System Commands:'), nl_log,
  write_log('   - /cd Path             Set the current directory to path'), nl_log,
  write_log('   - /cd                  Set the current directory to the one DES was started from'), nl_log,
  write_log('   - /dir                 Synonym for /ls'), nl_log,
  write_log('   - /dir Path            Synonym for /ls path'), nl_log,
  write_log('   - /ls                  Display the contents of the current directory'), nl_log,
  write_log('   - /ls Path             Display the contents of the given directory'), nl_log,
  write_log('   - /pwd                 Display the current directory'), nl_log,
  write_log('   - /shell Command       Submit Command to the OS shell'), nl_log,
  write_log('   - /s Command           Shorthand for /shell'), nl_log,
  write_log(' * Log Commands:'), nl_log,
  write_log('   - /log                 Display the current log file, if any'), nl_log,
  write_log('   - /log Filename        Set the current log to the given filename'), nl_log,
  write_log('   - /nolog               Disable logging'), nl_log,
  write_log(' * Informative Commands:'), nl_log,
  write_log('   - /builtins            List builtin operators, functions and predicates'), nl_log,
  write_log('   - /dbschema            Display the database schema'), nl_log,
  write_log('   - /dbschema Name       Display the database schema for the given view or table name'), nl_log,
  write_log('   - /development         Display whether development listings are enabled'), nl_log,
  write_log('   - /development Switch  Enable or disable development listings (on or off, resp.)'), nl_log,
  write_log('   - /duplicates          Display whether duplicates are enabled'), nl_log,
  write_log('   - /help                Display this help'), nl_log,
  write_log('   - /h                   Shorthand for /help'), nl_log,
  write_log('   - /negation            Display the selected algorithm for solving negation'), nl_log,
  write_log('   - /pdg                 Display the predicate dependency graph for the loaded program'), nl_log,
  write_log('   - /pretty_print        Display whether pretty print listings is enabled'), nl_log,
  write_log('   - /pretty_print Switch Enable or disable pretty print for listings (on or off, resp.)'), nl_log,
  write_log('   - /safe                Display whether program transformation is enabled'), nl_log,
  write_log('   - /simplification      Display whether program simplification is enabled'), nl_log,
  write_log('   - /status              Display the current status of the system'), nl_log,
  write_log('   - /strata              Display the stratification for the loaded program'), nl_log,
  write_log('   - /timing              Display whether elapsed time display is enabled'), nl_log,
  write_log('   - /timing Switch       Disable or enable either a basic or detailed elapsed time display (off, on, detailed, resp.)'), nl_log,
  write_log('   - /verbose             Display whether verbose output is enabled'), nl_log,
  write_log('   - /verbose Switch      Enable or disable output messages (on or off, resp.)'), nl_log,
  write_log('   - /version             Display the current system version'), nl_log,
  write_log(' * Languages:'), nl_log,
  write_log('   - /datalog             Switch to Datalog interpreter'), nl_log,
  write_log('   - /datalog Query       Trigger Datalog evaluation for query'), nl_log,
  write_log('   - /prolog              Switch to Prolog interpreter'), nl_log,
  write_log('   - /prolog Goal         Trigger Prolog evaluation for goal'), nl_log,
  write_log('   - /sql                 Switch to SQL interpreter'), nl_log,
  write_log('   - /sql SQL_statement   Trigger SQL evaluation for SQL_statement'), nl_log,
  write_log(' * Miscellanea:'), nl_log,
  write_log('   - /duplicates Switch   Enable or disable duplicates (on or off, resp.)'), nl_log,
  write_log('   - /negation Algorithm  Set the required Algorithm for solving negation (strata or et_not)'), nl_log,
  write_log('   - /halt                Quit DES'), nl_log,
  write_log('   - /output Switch       Enable or disable display output (on or off, resp.)'), nl_log,
  write_log('   - /process Filename    Process the contents of Filename as if they were typed at the system prompt'), nl_log,
  write_log('   - /p Filename          Shorthand for /process Filename'), nl_log,
  write_log('   - /quit,/q,/exit,/e    Synonyms/Shorthands for /halt'), nl_log,
  write_log('   - /safe Switch         Enable or disable program transformation (on or off, resp.)'), nl_log,
  write_log('   - /simplification Switch'), nl_log,
  write_log('                          Enable or disable program simplification (on or off, resp.)'), nl_log,
  write_log('Any other input is evaluated as a Prolog goal, SQL or Datalog query,'), nl_log,
  write_log('view or autoview, depending on the prompt.'), nl_log,
  write_log('Type des. if you get out of DES from a Prolog interpreter.'), nl_log.
processC(save_ddb,[File],_NVs,yes) :-
  !, 
  (my_file_exists(File) ->
    write_log('Info: File exists already. Overwrite? (y/n) [n]: '),
 	  my_get0(Char1),
	  %WARNING: System-dependent. Newline character is assumed to be the charcode 10.
	  ((Char1==10 ; (my_get0(_Char2), [Char1]=="y")) ->
	    Continue=yes
	    ;
     Continue=no)
   ;
    Continue=yes
  ),
  (Continue==yes ->
    (log(F,S) ->
      close(S), 
      retract(log(F,S))
     ;
      true
    ),
    processC(log,[write,File],[],yes),
    output(OutputFlag),
    processC(output,[off],[],yes),
    list_rules_wo_number(0,_DLs),
    processC(output,[OutputFlag],[],yes),
    log(FS,SS),
    close(SS), 
    retract(log(FS,SS)),
    (var(F) ->
      true
     ;
      processC(log,[append,F],[],yes)
    )
   ;
    true
  ).
processC(SConsult,Files,_NVs,yes) :-
  (SConsult=consult ; SConsult=c ; SConsult=restore_ddb), 
  !, 
  (Files=[] ->
    write_log('Warning: Nothing consulted.'), nl_log;
    abolishFT,
    abolishDL, 
    abolishET, 
    remove_duplicates_var(Files,UFiles),
    consultDLlist(UFiles,false,Success),
    (Success -> compute_stratification; true)).
processC(SReconsult,Files,_NVs,yes) :-
  (SReconsult=reconsult; SReconsult=r), !, 
  (Files=[] ->
    write_log('Warning: Nothing reconsulted.'), nl_log;
    abolishET, 
    remove_duplicates_var(Files,UFiles),
    consultDLlist(UFiles,false,Success),
    (Success -> compute_stratification; true)).
processC(assert,[T],NVs,yes) :-
  !, 
  assert_rule((T,NVs),_Simplify,Error),
  (Error==true -> 
    true
    ;
    abolishET, 
    drilldown_stratification([T]),
    (verbose(on) -> 
      write_log('Info: Rule asserted.'), 
      nl_log
     ; 
      true)
  ).
processC(retract,[R],_NVs,yes) :-
  !, 
  get_source_dlrules(rule,R,SDLs),
  (SDLs==[] -> 
    write_log_list(['Warning: Nothing retracted.',nl])
    ;
    SDLs = [SDL|_],
    get_object_dlrules(SDL,ODLs),
    retract_dlrule_list(ODLs,Error),
    (Error==true -> 
      true
      ;
      abolishET, 
      rollup_stratification([R]),
      (verbose(on) ->
      	(development(on) -> 
      	  length(ODLs,Nbr),
      	  (Nbr==1 -> S =' ' ; S='s '),
          write_log_list(['Info: ',Nbr,' rule',S,'retracted:',nl]), 
          display_dlrule_list(ODLs,2)
          ; 
          write_log_list(['Info: Rule retracted.',nl]))
  	   ;
  	   true)
     )
   ).
processC(retractall,[H],_NVs,yes) :-
  !, 
  get_object_dlrules(head,H,ODLs),
  (ODLs==[] -> 
    write_log_list(['Warning: Nothing retracted.',nl])
    ;
    get_source_dlrules(head,H,SDLs),
    retract_dlrule_list(ODLs,Error),
    (Error==true -> 
      true
     ;
      abolishET, 
      rollup_stratification(_Rs),
      display_tuples_and_nbr_info(SDLs,ODLs)
    )
   ).
processC(abolish,[],_NVs,yes) :-
  !, 
  abolishFT,
  abolishDL, 
  write_verb(['Info: Program abolished.',nl]), 
  abolishET, 
  reset_stratification.
processC(abolish,[N/A],_NVs,yes) :-
  !,
  get_object_dlrules(namearity,N/A,ODLs),
  (ODLs==[] -> 
    write_log_list(['Warning: Nothing abolished.',nl])
    ;
    get_source_dlrules(namearity,N/A,SDLs),
    retract_dlrule_list(ODLs,_Error),
    abolishET, 
    rollup_stratification(_Rs),
    display_tuples_and_nbr_info(SDLs,ODLs)
   ).
processC(abolish,[N],_NVs,yes) :-
  !,
  get_object_dlrules(name,N,ODLs),
  (ODLs==[] -> 
    write_log_list(['Warning: Nothing abolished.',nl])
   ;
    get_source_dlrules(name,N,SDLs),
    retract_dlrule_list(ODLs,_Error),
    abolishET, 
    rollup_stratification(_Rs),
    display_tuples_and_nbr_info(SDLs,ODLs)
	).
processC(listing,[],_NVs,yes) :-
  !,
  list_rules(0),
  display_elapsed_time.
processC(listing,[N/A],_NVs,yes) :-
  !,
  list_rules(N,A,0),
  display_elapsed_time.
processC(listing,[N],_NVs,yes) :-
  atom(N),
  !,
  list_rules(N,0),
  display_elapsed_time.
processC(listing,[H],_NVs,yes) :-
  H\=':-'(_H,_T),
  !,
  list_rules_from_head(H,0),
  display_elapsed_time.
processC(listing,[R],_NVs,yes) :-
  !,
  list_rules_from_rule(R,0),
  display_elapsed_time.
processC(dbschema,[],_NVs,yes) :-
  !,
  list_schema.
processC(dbschema,[N],_NVs,yes) :-
  !,
  list_schema(N).
processC(list_et,[],_NVs,yes) :-
  !,
  list_et.
processC(list_et,[N/A],_NVs,yes) :-
  !,
  list_et(N/A).
processC(list_et,[N],_NVs,yes) :-
  !,
  list_et(N).
processC(clear_et,[],_NVs,yes) :- 
  !,
  retractall(et(_E,_I)),
  retractall(called(_C)), 
  retractall(complete_flag(_G,_CF)),
  write_verb(['Info: Extension table cleared.', nl]).
processC(builtins,[],_NVs,yes) :-
  !,
  list_builtins.
processC(datalog,[],_NVs,yes) :-
  !,
  retractall(language(_L)),
  assertz(language(datalog)),
  write_verb(['Info: Switched to Datalog interpreter.', nl]).
processC(prolog,[],_NVs,yes) :-
  !,
  retractall(language(_L)),
  assertz(language(prolog)),
  write_verb(['Info: Switched to Prolog interpreter.', nl]).
processC(sql,[],_NVs,yes) :-
  !,
  retractall(language(_L)),
  assertz(language(sql)),
  write_verb(['Info: Switched to SQL interpreter.', nl]).
% processC(sql,[Query],_NVs,yes) :-
%   !,
%   retract(simplification(S)),
%   assertz(simplification(on)),
%   reset_id,
%   solve_sql_query(Query),
%   retract(simplification(on)), % WARNING: If Query fails, simplification mode is lost
%   assertz(simplification(S)).
processC(cd,[],_NVs,yes) :-
  !,
  start_path(Path),
  cd_path(Path).
processC(cd,[Path],_NVs,yes) :-
  !,
  cd_path(Path).
processC(pwd,[],_NVs,yes) :-
  !,
  pwd_path.
processC(LS,[],_NVs,yes) :-
  (LS=ls; LS=dir),
  !,
  ls.
processC(LS,[P],_NVs,yes) :-
  (LS=ls; LS=dir),
  !,
  ls(P).
processC(verbose,[],_NVs,yes) :-
  !, 
  verbose(Switch),
  write_log_list(['Info: Verbose output is ', Switch, '.', nl]).
processC(verbose,[Switch],_NVs,yes) :-
  !, 
  ((Switch == on; Switch == off) ->
    retractall(verbose(_)), 
    assertz(verbose(Switch)),
    write_log_list(['Info: Verbose output is ', Switch, '.', nl])
    ;
    write_log_list(['Error: Incorrect switch. Use ''on'' or ''off''', nl])
    ).
processC(duplicates,[],_NVs,yes) :-
  !, 
  duplicates(Switch),
  write_log_list(['Info: Duplicates are ', Switch, '.', nl]).
processC(duplicates,[Switch],NVs,yes) :-
  !, 
  ((Switch == on; Switch == off) ->
    retractall(duplicates(_)), 
    assertz(duplicates(Switch)),
    write_log_list(['Info: Duplicates are ', Switch, '.', nl]),
    processC(clear_et,[],NVs,yes)
    ;
    write_log_list(['Error: Incorrect switch. Use ''on'' or ''off''', nl])
    ).
processC(negation,[],_NVs,yes) :-
  !, 
  neg(Algorithm),
  write_log_list(['Info: Algorithm ''', Algorithm, ''' selected for solving negation.', nl]).
processC(negation,[Algorithm],_NVs,yes) :-
  !, 
  ((Algorithm == strata; Algorithm == et_not) ->
    retractall(neg(_)), 
    assertz(neg(Algorithm)),
    write_verb(['Info: Algorithm ''', Algorithm, ''' selected for solving negation.',nl])
    ;
    write_log_list(['Error: Algorithm ''', Algorithm, ''' is not supported. Use ''strata'' or ''et_not''.', nl])
    ).
processC(timing,[],_NVs,yes) :-
  !, 
  timing(Switch),
  write_log_list(['Info: Elapsed time display is ', Switch, '.', nl]).
processC(timing,[Switch],_NVs,yes) :-
  !, 
  ((Switch == on ; Switch == off ; Switch == detailed ) ->
    retractall(timing(_)), 
    assertz(timing(Switch)),
    processC(timing,[],_,_)
    ;
    write_log_list(['Error: Incorrect switch. Use ''on'', ''off'' or ''detailed''', nl])
    ).
processC(pretty_print,[],_NVs,yes) :-
  !, 
  pretty_print(Switch),
  write_log_list(['Info: Pretty print is ', Switch, '.', nl]).
processC(pretty_print,[Switch],_NVs,yes) :-
  !, 
  ((Switch == on; Switch == off) ->
    retractall(pretty_print(_)), 
    assertz(pretty_print(Switch)),
    processC(pretty_print,[],_,_)
    ;
    write_log_list(['Error: Incorrect switch. Use ''on'' or ''off''', nl])
    ).
processC(safe,[],_NVs,yes) :-
  !, 
  safe(Switch),
  write_log_list(['Info: Program transformation is ', Switch, '.', nl]).
processC(safe,[Switch],_NVs,yes) :-
  !, 
  ((Switch == on; Switch == off) ->
    retractall(safe(_)), 
    assertz(safe(Switch)),
    processC(safe,[],_,_)
    ;
    write_log_list(['Error: Incorrect switch. Use ''on'' or ''off''', nl])
    ).
processC(simplification,[],_NVs,yes) :-
  !, 
  simplification(Switch),
  write_log_list(['Info: Program simplification is ', Switch, '.', nl]).
processC(simplification,[Switch],_NVs,yes) :-
  !, 
  ((Switch == on; Switch == off) ->
    retractall(simplification(_)), 
    assertz(simplification(Switch)),
    processC(simplification,[],_,_)
    ;
    write_log_list(['Error: Incorrect switch. Use ''on'' or ''off''', nl])
    ).
processC(output,[],_NVs,yes) :-
  !, 
  output(Switch),
  write_log_list(['Info: Output is ', Switch, '.', nl]).
processC(output,[Switch],_NVs,yes) :-
  !, 
  ((Switch == on; Switch == off) ->
    retractall(output(_)), 
    assertz(output(Switch)),
    (verbose(on) ->
      processC(output,[],_,_)
     ;
      true
    )
    ;
   write_log_list(['Error: Incorrect switch. Use ''on'' or ''off''', nl])
  ).
processC(strata,[],_NVs,yes) :-
  !, 
  (strata(S) -> 
   (write_log(S), nl_log); 
   (write_log('Warning: Strata not yet computed.'), nl_log)).
processC(pdg,[],_NVs,yes) :-
  !, 
  (pdg((N,A)) -> 
   (write_log('Nodes: '), write_log(N), nl_log, write_log('Arcs : '), write_log(A), nl_log)
   ; 
   (write_log('Info: Predicate dependency graph not yet computed.'), nl_log)).
processC(log,[],_NVs,yes) :-
  !, 
  (log(F,_S) -> 
   (write_log('Info: Currently logging to '), write_log(F), write_log('.'), nl_log)
   ;
   (write_log('Info: Logging disabled.'), nl_log)).
processC(log,[Mode,File],_NVs,yes) :-
  !, 
  (log(CF,_S) -> 
    write_log_list(['Warning: Currently logging to ''',CF,'''. First, use /nolog for disabling the current log.',nl])
   ;
   (my_absolute_filename(File,AFN),
    my_dir_file(AFN,AP,_FN),
    (my_directory_exists(AP) ->
      open(AFN,Mode,S),
      assertz(log(AFN,S)), 
      write_verb(['Info: Logging enabled to ',AFN,'.',nl])
     ;
      write_log_list(['Warning: Directory ',AP,' does not exist.',nl])
    )
   )
  ). 
processC(nolog,[],_NVs,yes) :-
  !, 
  (log(F,S) -> 
   (close(S), retract(log(F,S)), write_verb(['Info: Logging to ', F, ' disabled.', nl]))
   ;
   (write_log('Warning: Logging already disabled.'), nl_log)).
processC(SProcess,[F],_NVs,yes) :-
  (SProcess=process ; SProcess=p), 
  !,
  process_batch(F).
processC(version,[],_NVs,yes) :-
  !, 
  des_version(V), 
  write_log_list(['Info: DES version ',V,'.',nl]).
processC(status,[],_NVs,yes) :-
  !, 
  display_status.
processC(open_db,[NewConnection|Opts],_NVs,yes) :-
  !, 
  current_db(CurrentConnection,_Handle),
  (CurrentConnection == '$des' ->
    (empty_des_rdb ->
      my_open_odbc(NewConnection,Opts),
      % The following enables an RDB data source for Datalog queries and programs
      enable_rdb_datasource
      ;
      write_log_list(['Error: The current DES relational database is not empty. First, use ''drop database'' for removing the current database.',nl])
    )
   ;
    (CurrentConnection==NewConnection ->
      write_log_list(['Error: The database ''',CurrentConnection,''' is already opened.',nl])
     ;
      write_log_list(['Warning: The database ''',CurrentConnection,''' is still opened.',nl,
                      '         Do you want to close ''',CurrentConnection,''' and open ''',NewConnection,'''? (y/n) ']),
      my_get0(Char),
      %WARNING: System-dependent. Newline character is assumed to be the charcode 10.
      (Char==10 ->
        true
       ;
        my_get0(_NL),
        ([Char] == "y" ->
          processC(close_db,[],[],yes),
          processC(open_db,[NewConnection|Opts],[],yes)
         ;
          true
        )
      )
    )
  ).
processC(close_db,[],NVs,yes) :-
  !, 
  current_db(Connection,_Handle),
  (Connection=='$des' ->
    write_log_list(['Warning: There is no opened ODBC database.',nl])
   ;
    my_close_odbc(Connection),
    processC(clear_et,[],NVs,yes)
  ).
% TODO: Unremark the following when multiple connections are allowed
% processC(close_db,[Connection],_NVs,yes) :-
%   !, 
%   my_close_odbc(Connection).
processC(current_db,[],_NVs,yes) :-
  !, 
  current_db(Connection,_Handle),
  (Connection=='$des' ->
    write_log_list(['Warning: There is no opened ODBC database.',nl])
   ;
    my_odbc_get_dbms(DBMS),
    write_log_list(['Info: The current ODBC database is ''',Connection,'''. DBMS: ',DBMS,nl])).
processC(trace_sql,[ViewName],_NVs,yes) :-
  !, 
  trace_sql(ViewName,preorder).
processC(trace_sql,[ViewName,Ordering],_NVs,yes) :-
  !, 
  trace_sql(ViewName,Ordering).
processC(trace_datalog,[Query],NVs,yes) :-
  !, 
  trace_datalog(Query,NVs,preorder).
processC(trace_datalog,[Query,Ordering],NVs,yes) :-
  !, 
  trace_datalog(Query,NVs,Ordering).
processC(debug_datalog,[Goal],NVs,yes) :-
  !, 
  debugGoalLevel(Goal,'p',NVs).
processC(debug_datalog,[Goal,Level],NVs,yes) :-
  !, 
  debugGoalLevel(Goal,Level,NVs).
% processC(debug_sql,[ViewName|Options],_NVs,yes) :-
%   !, 
%   debug_sql(ViewName,Options).
processC(test_case,[ViewName|Options],_NVs,yes) :-
  !, 
  process_test_case(ViewName,Options).
processC(tc_size,[],_NVs,yes) :-
  !, 
  tc_size(Min,Max),
  write_log_list(['Info: Test case size is set between ', Min, ' and ', Max, '.', nl]).
processC(tc_size,[Min,Max],NVs,yes) :-
  !, 
  (number(Min), number(Max), Min>0, Min=<Max ->
    set_flag(tc_size,Min,Max),
    (verbose(on) -> processC(tc_size,[],NVs,_) ; true)
   ;
    write_log_list(['Error: Incorrect parameter(s).', nl])).
processC(tc_domain,[],_NVs,yes) :-
  !, 
  tc_domain(Min,Max),
  write_log_list(['Info: Test case domain is set between ', Min, ' and ', Max, '.', nl]).
processC(tc_domain,[Min,Max],NVs,yes) :-
  !, 
  (number(Min), number(Max), Min=<Max ->
    set_flag(tc_domain,Min,Max),
    (verbose(on) -> processC(tc_domain,[],NVs,_) ; true)
   ;
    write_log_list(['Error: Incorrect parameter(s).', nl])).
processC(development,[],_NVs,yes) :-
  !, 
  development(Switch),
  write_log_list(['Info: Development listings are ', Switch, '.', nl]).
processC(development,[Switch],_NVs,yes) :-
  !, 
  ((Switch == on; Switch == off) ->
    retractall(development(_)), 
    assertz(development(Switch)),
    processC(development,[],_,_)
    ;
    write_log_list(['Error: Incorrect switch. Use ''on'' or ''off''', nl])
    ).
processC(_Error,_L,_NVs,yes) :-
  !, 
  write_log('Error: Unknown command or incorrect number of arguments. Use /help for help.'), nl_log.

debugGoalLevel(Goal,Level,NVs) :-
  (filet(_F,_Fid) -> 
   functor(Goal,Pred,Arity),
   pdg((Nodes,_Arcs)),
   (my_member(Pred/Arity,Nodes) ->
    ((Level == c ; Level == p) ->
      solve_datalog_query(Goal,_Undefined),
     !, 
     write_verb(['Info: Starting declarative debugger...']), 
     initDebug(Goal, Level, NVs)
    ;
    write_log_list(['Error: Unsupported option ''',Level,'''.',nl]))
   ;
   write_log_list(['Error: Predicate ',Pred/Arity,' does not exist.',nl]))
  ; 
  write_log('Error: Program not yet loaded.'), nl_log).

process_test_case(ViewName,Options) :-
  (my_view('$des',ViewName,_A,_S,_D,_L,_SC) ->
    (get_tc_options(Options,Class,Action) ->
      compute_test_case(ViewName,Class,Action)
     ;
     write_log_list(['Error: Incorrect option(s): ',Options,'.',nl])
    )
   ;
    write_log_list(['Error: View ''',ViewName,''' is not defined.', nl])
   ).

get_tc_options([],all,display).
get_tc_options([Opt],Class,Action) :-
  (Opt=Class,
   tc_class_option(Class),
   Action=display,
   !
  ;
   Opt=Action,
   tc_action_option(Action),
   Class=all).
get_tc_options([Opt1,Opt2],Class,Action) :-
  (Class=Opt1,
   Action=Opt2,
   tc_class_option(Opt1),
   tc_action_option(Opt2),
   !
  ;
   Class=Opt2,
   Action=Opt1,
   tc_class_option(Opt2),
   tc_action_option(Opt1)).
   
tc_class_option(all).
tc_class_option(positive).
tc_class_option(negative).

tc_action_option(display).
tc_action_option(replace).
tc_action_option(add).

/*********************************************************************/
/* Parsing                                                           */
/*********************************************************************/

%
% Parsing commands
%

command(CInput) :-
  command_begin(CInput,_).

command_begin -->
  my_blanks_star,
  "/".
  
parse_command(log,[append,File],[]) -->
  command_begin,
  my_blanks_star, 
  "log",
  my_blanks,
  "append",
  my_blanks,
  my_charsbutcomma(Cs),
  {name(File,Cs)},
  my_blanks_star,
  !.
parse_command(log,[write,File],[]) -->
  command_begin,
  my_blanks_star, 
  "log",
  my_blanks,
  my_charsbutcomma(Cs),
  {name(File,Cs)},
  my_blanks_star,
  !.
parse_command(log,[],[]) -->
  command_begin,
  my_blanks_star, 
  "log",
  my_blanks_star,
  !.
parse_command(reconsult,Files,[]) -->
  command_begin,
  my_blanks_star, 
  "[+",
  my_blanks_star,
  my_arguments(Files),
  my_blanks_star,
  "]",
  my_blanks_star,
  !.
parse_command(consult,Files,[]) -->
  command_begin,
  my_blanks_star,
  "[",
  my_blanks_star,
  my_arguments(Files),
  my_blanks_star,
  "]",
  my_blanks_star,
  !.
% parse_command(datalog,[Goal],NVs) -->
%   command_begin,
%   my_blanks_star,
%   my_command(datalog),
%   " ",
%   !,
%   my_blanks_star,
%   my_body(Goal,[],NVs),
%   my_blanks_star. 
% parse_command(prolog,[Goal],NVs) -->
%   command_begin,
%   my_blanks_star,
%   my_command(prolog),
%   " ",
%   !,
%   my_blanks_star,
%   my_body(Goal,[],NVs),
%   my_blanks_star. 
parse_command(abolish,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_command(abolish),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(list_schema,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_command(list_schema),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_command(list_et,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_command(list_et),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(listing,[R],[]) -->
  command_begin,
  my_blanks_star,
  my_command(listing),
  my_blanks,
  my_rule(R,[],_NVs),
  my_blanks_star.
parse_command(listing,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_command(listing),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(listing,[H],[]) -->
  command_begin,
  my_blanks_star,
  my_command(listing),
  my_blanks,
  my_head(H,[],_NVs),
  my_blanks_star,
  !.
parse_command(listing,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_command(listing),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_command(retract,[Rule],NVs) -->
  command_begin,
  my_blanks_star,
  my_command(retract),
  " ",
  !,
  my_blanks_star,
  my_rule(Rule,[],NVs),
  my_blanks_star.
parse_command(retractall,[Head],[]) -->
  command_begin,
  my_blanks_star,
  my_command(retractall),
  " ",
  !,
  my_blanks_star,
  my_head(Head,[],_NVs),
  my_blanks_star.
parse_command(assert,[Rule],NVs) -->
  command_begin,
  my_blanks_star,
  my_command(assert),
  " ",
  !,
  parse_rule(Rule,[],NVs).
parse_command(open_db,[Connection|Options],_NVs) -->
  command_begin,
  my_blanks_star,
  my_command(open_db),
  my_blanks,
  !,
  my_symbol(Connection),
  my_blanks_star,
  my_atoms_star(Options,[],_).
parse_command(debug_datalog,[Goal,Level],NVs) -->
  command_begin,
  my_blanks_star,
  my_command(debug_datalog),
  my_blanks,
  my_literal(Goal,[],NVs),
  my_blanks,
  my_symbol(Level),
  my_blanks_star,
  !.
parse_command(debug_datalog,[Goal],NVs) -->
  command_begin,
  my_blanks_star,
  my_command(debug_datalog),
  my_blanks,
  !,
  my_literal(Goal,[],NVs),
  my_blanks_star.
parse_command(debug_sql,[ViewName,Opt1,Opt2],[]) -->
  command_begin,
  my_blanks_star,
  my_command(debug_sql),
  my_blanks,
  my_user_identifier(ViewName),
  my_blanks,
  my_atom(Opt1,[],[]),
  my_blanks,
  my_atom(Opt2,[],[]),
  my_blanks_star,
  !.
parse_command(debug_sql,[ViewName,Opt],[]) -->
  command_begin,
  my_blanks_star,
  my_command(debug_sql),
  my_blanks,
  my_user_identifier(ViewName),
  my_blanks,
  my_atom(Opt,[],[]),
  my_blanks_star,
  !.
parse_command(debug_sql,[ViewName],[]) -->
  command_begin,
  my_blanks_star,
  my_command(debug_sql),
  my_blanks,
  my_symbol(ViewName),
  my_blanks_star,
  !.
parse_command(trace_sql,[ViewName,Ordering],[]) -->
  command_begin,
  my_blanks_star,
  my_command(trace_sql),
  my_blanks,
  my_symbol(ViewName),
  my_blanks,
  my_symbol(Ordering),
  my_blanks_star,
  !.
parse_command(trace_sql,[ViewName],[]) -->
  command_begin,
  my_blanks_star,
  my_command(trace_sql),
  my_blanks,
  my_symbol(ViewName),
  my_blanks_star,
  !.
parse_command(trace_datalog,[Query,Ordering],NVs) -->
  command_begin,
  my_blanks_star,
  my_command(trace_datalog),
  my_blanks,
  my_literal(Query,[],NVs),
  my_blanks,
  my_symbol(Ordering),
  my_blanks_star,
  !.
parse_command(trace_datalog,[Query],NVs) -->
  command_begin,
  my_blanks_star,
  my_command(trace_datalog),
  my_blanks,
  my_literal(Query,[],NVs),
  my_blanks_star,
  !.
parse_command(test_case,[ViewName,Opt1,Opt2],[]) -->
  command_begin,
  my_blanks_star,
  my_command(test_case),
  my_blanks,
  my_symbol(ViewName),
  my_blanks,
  my_symbol(Opt1),
  my_blanks,
  my_symbol(Opt2),
  my_blanks_star,
  !.
parse_command(test_case,[ViewName,Opt],[]) -->
  command_begin,
  my_blanks_star,
  my_command(test_case),
  my_blanks,
  my_symbol(ViewName),
  my_blanks,
  my_symbol(Opt),
  my_blanks_star,
  !.
parse_command(test_case,[ViewName],[]) -->
  command_begin,
  my_blanks_star,
  my_command(test_case),
  my_blanks,
  my_symbol(ViewName),
  my_blanks_star,
  !.
parse_command(tc_size,[Min,Max],[]) -->
  command_begin,
  my_blanks_star,
  my_command(tc_size),
  my_blanks,
  !,
  my_integer(Min),
  my_blanks_star,
  my_integer(Max),
  my_blanks_star.
parse_command(tc_domain,[Min,Max],[]) -->
  command_begin,
  my_blanks_star,
  my_command(tc_domain),
  my_blanks,
  !,
  my_integer(Min),
  my_blanks_star,
  my_integer(Max),
  my_blanks_star.
parse_command(Command,[],[]) -->
  command_begin,
  my_blanks_star,
  my_command(Command),
  my_blanks_star
  %,!
  .
parse_command(Command,Arguments,[]) -->
  %WARNING: This is needed to avoid the cut in the clause above. SP4 differently translates cuts in DCGs
  {Arguments\==[]},
  command_begin,
  my_blanks_star,
  my_command(Command),
  my_blanks,
  my_arguments(Arguments),
  my_blanks_star.

my_command(Command) -->
  my_identifier_chars(Cs),
  {to_lowercase_list(Cs,LCs),
   name(Command,LCs)}.

my_arguments([]) -->
  [].
my_arguments([A|As]) -->
  my_charsbutcomma(Cs),
  my_blanks_star,
  ",",
  my_blanks_star,
  {name(A,Cs)},
  my_arguments(As).
my_arguments([A]) -->
  my_charsbutcomma(Cs),
  {name(A,Cs)}.

my_charsbutcomma([C|Cs]) -->
  my_charbutcomma(C),
  my_charsbutcomma(Cs).
my_charsbutcomma([C]) -->
  my_charbutcomma(C).

my_charbutcomma(C) -->
  [C],
  {C\=","}.

my_pattern(N/A) -->
  my_blanks_star,
  my_symbol(N),
  my_blanks_star,
  "/",
  my_blanks_star,
  my_positive_integer(A),
  my_blanks_star.


%
% Parsing queries
%

parse_datalog_query(Term,Vo) -->
  my_blanks_star,
  my_literal(Term,[],Vo),
  my_blanks_star.

  
%
% Parsing rules: Nulls are uniquelly grounded
%

parse_rule(Rule,Vi,Vo) -->
  my_blanks_star,
  my_head(NHead,Vi,Vo1),
  my_blanks_star,
  ":-",
  my_blanks_star,
  !,
  {redef_error(NHead)},
  my_body(NBody,Vo1,Vo),
  my_blanks_star,
  {NRule=':-'(NHead,NBody),
   abstract_nulls(NRule,ARule),
   concrete_nulls(ARule,Rule,_Grounded)}.  

parse_rule(Rule,Vi,Vo) -->
  my_blanks_star,
  my_head(NHead,Vi,Vo),
  my_blanks_star,
  {redef_error(NHead),
   abstract_nulls(NHead,AHead),
   concrete_nulls(AHead,Rule,_Grounded)},
  !.  % This cut should be before abstract_nulls. SWI Prolog incorrectly translates this DCG

   
%
% Parsing rule heads: Nulls are uniquelly grounded
%

parse_head(Head,Vi,Vo) -->
  my_blanks_star,
  my_atom(NHead,Vi,Vo),
  my_blanks_star,
  {redef_error(NHead),
   abstract_nulls(NHead,AHead),
   concrete_nulls(AHead,Head,_Grounded)},
  !.  % This cut should be before abstract_nulls. It seems that SWI-Prolog incorrectly translates this DCG
  
redef_error(Term) :-
   Term =.. [F|As],
   length(As,A),
   (my_infix_relation(F,_), A=2       -> redefinition_error(F,A); true),
   (my_builtin_relation(F,A,_)        -> redefinition_error(F,A); true),
%  (my_infix_comparison(F,_), A=2     -> redefinition_error(F,A); true), % Infix comparison operators are rejected when parsing
%  (my_infix_arithmetic(F,_), A=2     -> redefinition_error(F,A); true), % Names of arithmetic Built-ins can be reused because of their scope
   (F = (','), A=2                    -> redefinition_error(F,A); true),
   (F = 'not', A=1                    -> redefinition_error(F,A); true),
   (F = 'answer'                      -> redefinition_error(F,A); true).

  
   
%
% Parsing rule bodies: Nulls are uniquelly grounded
%

parse_body(Body,Vi,Vo) -->
  my_blanks_star,
  my_body(NBody,Vi,Vo),
  my_blanks_star,
  {abstract_nulls(NBody,ABody),
   concrete_nulls(ABody,Body,_Grounded)}.
   %,
   %!.  % This cut should be before {. It seems that SWI-Prolog incorrectly translates this DCG


%
% Parsing line remarks: lines starting with % or -- are interpreted as remarks
%

parse_line_remark -->
  my_blanks_star,
  "%".
parse_line_remark -->
  my_blanks_star,
  "--".


%
% Rules, heads, bodies and queries
%

my_rule(':-'(H,B),Vi,Vo) -->
  my_blanks_star,
  my_head(H,Vi,Vo1),
  my_blanks_star,
  ":-",
  my_blanks_star,
  my_body(B,Vo1,Vo),
  my_blanks_star.
my_rule(H,Vi,Vo) -->
  my_blanks_star,
  my_head(H,Vi,Vo),
  my_blanks_star.

my_head(H,Vi,Vo) -->
  my_atom(H,Vi,Vo).

my_body(B,Vi,Vo) -->
  my_literals(B,Vi,Vo).

my_query(Q,Vi,Vo) -->
  my_literal(Q,Vi,Vo).


%
% Constants, Variables, Terms, Atoms, Literals
%

my_constant(A) -->
  my_number(A).
my_constant(A) -->
  my_symbol(A),
  {A\=null}.

my_variable(V,Vi,Vo) -->
  my_uppercase(C),
  my_identifier_chars(Cs),
  {name(N,[C|Cs]),
   append_var(N,V,Vi,Vo)}.
my_variable(V,Vi,Vo) -->
  my_uppercase(C),
  {name(N,[C]),
  append_var(N,V,Vi,Vo)}.
my_variable(V,Vi,Vo) -->
  "_",
  my_underscored_variable_chars(Cs),
  {[US]="_",
   name(N,[US|Cs]),
   append_var(N,V,Vi,Vo)}.
my_variable(_,Vi,Vi) -->
  "_".
  
my_variable_or_number(V,Vi,Vo) -->
  my_variable(V,Vi,Vo).
my_variable_or_number(N,Vi,Vi) -->
  my_number(N).

my_variable_or_star_in_aggr(V,_AF,Vi,Vo) -->
  my_variable(V,Vi,Vo).
my_variable_or_star_in_aggr(*,count,Vi,Vi) -->
  "*".

my_noncompoundterms([T|Ts],Vi,Vo) -->
  my_noncompoundterm(T,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_noncompoundterms(Ts,Vo1,Vo).
my_noncompoundterms([T],Vi,Vo) -->
  my_noncompoundterm(T,Vi,Vo).

my_noncompoundterm('$NULL'(_ID),V,V) -->
  "null".
my_noncompoundterm(A,V,V) -->
  my_constant(A).
my_noncompoundterm(A,Vi,Vo) -->
  my_variable(A,Vi,Vo).
my_noncompoundterm('$NULL'(ID),V,V) -->
  "'$NULL'(",
  my_integer(ID),
  ")".
my_noncompoundterm('$NULL'(ID),Vi,Vo) -->
  "'$NULL'(",
  my_variable(ID,Vi,Vo),
  ")".

my_atom(A,Vi,Vo) -->
  my_symbol(F),
  my_blanks_star,
  "(",
  {F\==not},
  my_blanks_star,
  my_noncompoundterms(LT,Vi,Vo),
  my_blanks_star,
  ")",
  {A =.. [F|LT]}.
my_atom(A,Vi,Vo) -->
  my_noncompoundterm(L,Vi,Vo1),
  my_blanks_star,
  my_infix_comparison(P),
  my_blanks_star,
  my_noncompoundterm(R,Vo1,Vo), 
  my_blanks_star,
  {A =.. [P,L,R]}.
my_atom(A,Vi,Vo) -->
  my_noncompoundterm(L,Vi,Vo1),
  my_blanks,
  "is",
  my_blanks,
  my_arithmeticexp(R,Vo1,Vo),
  my_blanks_star,
  {A =.. [is,L,R]}.
my_atom(A,Vi,Vo) -->
  my_expression(L,Vi,Vo1),
  my_blanks_star,
  my_infix_comparison(Op),
  my_blanks_star,
  my_expression(R,Vo1,Vo),
  my_blanks_star,
  {A =.. [Op,L,R]}.
my_atom(A,V,V) -->
  my_symbol(A).
  
my_atoms_star([A,B|As],Vi,Vo) -->
  my_atom(A,Vi,Vo1),
  my_blanks,
  my_atoms_star([B|As],Vo1,Vo).
my_atoms_star([A],Vi,Vo) -->
  my_atom(A,Vi,Vo),
  my_blanks_star.
my_atoms_star([],V,V) -->
  [].  

my_literals(L,Vi,Vo) -->
  my_ub_literals(L,Vi,Vo).
my_literals(L,Vi,Vo) -->
  my_b_literals(L,Vi,Vo).
%my_literals(L,Vi,Vo) -->
%  my_ub_literals(L,Vi,Vo).

my_b_literals(L,Vi,Vo) -->
  "(",
  my_blanks_star,
  my_literals(L,Vi,Vo),
  my_blanks_star,
  ")".
  
my_ub_literals((T,Ts),Vi,Vo) -->
  my_literal(T,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_literals(Ts,Vo1,Vo).
my_ub_literals(T,Vi,Vo) -->
  my_literal(T,Vi,Vo).
%  my_ub_literal(T,Vi,Vo).

my_literal(L,Vi,Vo) -->  
  my_b_literal(L,Vi,Vo).
my_literal(L,Vi,Vo) -->  
  my_ub_literal(L,Vi,Vo).

my_b_literal(L,Vi,Vo) -->
  "(",
  my_blanks_star,
  my_literal(L,Vi,Vo),
  my_blanks_star,
  ")".
  
my_ub_literal(';'(L1,L2),Vi,Vo) -->  
  my_b_literals(L1,Vi,Vo1),
  my_blanks_star,
  ";",
  my_blanks_star,
  my_literals(L2,Vo1,Vo).
my_ub_literal(';'(L1,L2),Vi,Vo) -->  
  my_basic_literal(L1,Vi,Vo1),
  my_blanks_star,
  ";",
  my_blanks_star,
  my_literals(L2,Vo1,Vo).
my_ub_literal(L,Vi,Vo) -->  
  my_basic_literal(L,Vi,Vo).

my_basic_literal(not(A),Vi,Vo) -->
  "not",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_atom(A,Vi,Vo),
  my_blanks_star,
  ")".
my_basic_literal(Aggr,Vi,Vo) -->
  my_aggregate_relation(AF,3),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_body(P,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_variable(V,Vo1,Vo2),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_variable_or_number(T,Vo2,Vo),
  my_blanks_star,
  ")",
  {Aggr =.. [AF,P,V,T]}.
my_basic_literal(Aggr,Vi,Vo) -->
  my_aggregate_relation(AF,2),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_body(P,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_variable_or_number(T,Vo1,Vo),
  my_blanks_star,
  ")",
  {Aggr =.. [AF,P,T]}.
my_basic_literal(group_by(B,Vs,C),Vi,Vo) -->
  "group_by",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_body(B,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_var_list(Vs,Vo1,Vo2),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_body(C,Vo2,Vo),
  my_blanks_star,
  ")".
my_basic_literal(lj(A),Vi,Vo) -->
  "lj",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_atom(A,Vi,Vo),
  my_blanks_star,
  ")".
my_basic_literal(lj(A,B,C),Vi,Vo) -->  
  "lj",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_basic_literal(A,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_basic_literal(B,Vo1,Vo2),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_literals(C,Vo2,Vo),
  my_blanks_star,
  ")".
my_basic_literal(rj(A),Vi,Vo) -->
  "rj",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_atom(A,Vi,Vo),
  my_blanks_star,
  ")".
my_basic_literal(rj(A,B,C),Vi,Vo) -->  
  "rj",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_basic_literal(A,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_basic_literal(B,Vo1,Vo2),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_literals(C,Vo2,Vo),
  my_blanks_star,
  ")".
my_basic_literal(fj(A),Vi,Vo) -->
  "fj",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_atom(A,Vi,Vo),
  my_blanks_star,
  ")".
my_basic_literal(fj(A,B,C),Vi,Vo) -->  
  "fj",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_basic_literal(A,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_basic_literal(B,Vo1,Vo2),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_literals(C,Vo2,Vo),
  my_blanks_star,
  ")".
my_basic_literal(L,Vi,Vo) -->
  my_atom(L,Vi,Vo).

my_aggregate_relation(count,4) -->
  "count".  
my_aggregate_relation(count,3) -->
  "count".  
my_aggregate_relation(count,2) -->
  "count".  
my_aggregate_relation(sum,4) -->
  "sum".  
my_aggregate_relation(sum,3) -->
  "sum".  
my_aggregate_relation(times,4) -->
  "times".  
my_aggregate_relation(times,3) -->
  "times".  
my_aggregate_relation(avg,4) -->
  "avg".  
my_aggregate_relation(avg,3) -->
  "avg".  
my_aggregate_relation(min,4) -->
  "min".  
my_aggregate_relation(min,3) -->
  "min".  
my_aggregate_relation(max,4) -->
  "max".
my_aggregate_relation(max,3) -->
  "max".
 
my_aggregate_relation(AF,Arity) :-
  my_aggregate_relation(AF,Arity,_H,_T).  
  
%
% Ancillary Stuff
%
    
lang_interpreter_cmd(datalog) --> 
  command_begin,
  my_blanks_star,
  my_kw("DATALOG"),
  my_blanks_star.
lang_interpreter_cmd(prolog) --> 
  command_begin,
  my_blanks_star,
  my_kw("PROLOG"),
  my_blanks_star.
lang_interpreter_cmd(sql) --> 
  command_begin,
  my_blanks_star,
  my_kw("SQL"),
  my_blanks_star.

my_var_list([],V,V) -->  
  "[",
  my_blanks_star,
  "]".
my_var_list(Vs,Vi,Vo) -->  
  "[",
  my_blanks_star,
  my_comma_separated_vars(Vs,Vi,Vo),
  my_blanks_star,
  "]".
  
my_comma_separated_vars([V|Vs],Vi,Vo) -->
  my_variable(V,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_comma_separated_vars(Vs,Vo1,Vo).
my_comma_separated_vars([V],Vi,Vo) -->
  my_variable(V,Vi,Vo).
  
my_blank -->         % One blank
  " ".
  
my_blanks_star -->   % Zero or more blanks. Eagerly consumes blanks
  my_blanks.
my_blanks_star -->
  [].

my_blanks -->        % One or more blanks
  my_blank,
  my_blanks_star.


my_letter(C) --> 
  my_lowercase(C).
my_letter(C) --> 
  my_uppercase(C).

my_lowercase(C) --> 
  [C],
  {my_lowercase(C)}.
  
my_uppercase(C) -->
  [C],
  {C>="A",
   C=<"Z"}.
   
my_digit(C) -->
  [C],
  {C>="0",
   C=<"9"}.

my_lowercase(C) :-
  C>="a",
  C=<"z".

my_uppercase(C) :-
  C>="A",
  C=<"Z".

% my_identifier_chars: 
% one or more digits, letters, underscores or dollars
% WARNING: The order of the following two clauses have been changed. SICStus 4 seems to add a cut somewhere (error during parsing any command, as /help)
my_identifier_chars([C|Cs]) -->
  my_identifier_char(C),
  my_identifier_chars(Cs).
my_identifier_chars([C]) -->
  my_identifier_char(C).

% my_underscored_variable_chars: 
% one or more letters, digits or underscores
my_underscored_variable_chars([C]) -->
  my_underscored_variable_char(C).
my_underscored_variable_chars([C|Cs]) -->
  my_underscored_variable_char(C),
  my_underscored_variable_chars(Cs).

my_underscored_variable_char(C) --> 
  my_letter(C).
my_underscored_variable_char(C) -->
  my_digit(C).
my_underscored_variable_char(C) -->
  "_",
  {[C]="_"}.

% my_identifier_chars_star:
% zero or more digits, letters, underscores or dollars
% WARNING: The order of the following two clauses has been changed because SP4
my_identifier_chars_star(Cs) -->
  my_identifier_chars(Cs).
my_identifier_chars_star([]) -->
  [].

% my_identifier_char(C) --> 
%   my_char(C).
my_identifier_char(C) --> 
  my_lowercase(C).
my_identifier_char(C) -->
  my_uppercase(C).
my_identifier_char(C) -->
  my_digit(C).
my_identifier_char(C) -->
  "_",
  {[C]="_"}.
my_identifier_char(C) -->
  "$",
  {[C]="$"}.

% my_chars: one or more characters
my_chars([C]) -->
  my_char(C).
my_chars([C|Cs]) -->
  my_char(C),
  my_chars(Cs).

my_char(C) -->
  [C].

% my_symbol is intended to parse constants, function (functor), and relation (predicate) symbols
my_symbol(A) -->
  my_lowercase(C),
  my_identifier_chars_star(Cs),
 {name(A,[C|Cs])}.
% my_symbol(A) -->
%   my_lowercase(C),
%   {name(A,[C])}.
my_symbol(A) -->
  "'",
%  my_identifier_chars(Cs),
  my_chars(Cs),
  "'",
  {atom_codes(A,Cs)}.
%  {name(A,Cs)}.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
my_expression(E,Vi,Vo) --> 
  my_noncompoundterm(E,Vi,Vo).
my_expression(E,Vi,Vo) --> 
  my_arithmeticexp(E,Vi,Vo).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing arithmetic expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_arithmeticexp(E,Vi,Vo) --> 
  my_aterm(T,Vi,Vo1),
  my_blanks_star, 
  my_arithmeticexp_r(T,E,Vo1,Vo).
my_arithmeticexp_r(E0,E,Vi,Vo) --> 
  {my_priority_operator(low,SOP,OP)}, 
  my_string(SOP),
  my_blanks_star, 
  my_aterm(T,Vi,Vo1),
  my_blanks_star, 
  {TOP =.. [OP,E0,T]},
  my_arithmeticexp_r(TOP,E,Vo1,Vo).
my_arithmeticexp_r(E,E,V,V) -->
  [].

my_aterm(T,Vi,Vo) --> 
  my_power(P,Vi,Vo1),
  my_blanks_star, 
  my_aterm_r(P,T,Vo1,Vo).
my_aterm_r(T0,T,Vi,Vo) --> 
  {my_priority_operator(medium,SOP,OP)}, 
  my_string(SOP),
  my_blanks_star, 
  my_power(P,Vi,Vo1),
  my_blanks_star, 
  {TOP =.. [OP,T0,P]},
  my_aterm_r(TOP,T,Vo1,Vo).
my_aterm_r(T,T,V,V)    --> [].

my_power(P,Vi,Vo) --> 
  my_factor(F,Vi,Vo1),
  my_blanks_star, 
  my_power_r(F,P,Vo1,Vo).
my_power_r(P0,TOP,Vi,Vo) --> 
  {my_priority_operator(high,SOP,OP)}, 
%  SOP,  % Unsupported in GNU Prolog
  my_string(SOP), 
  my_blanks_star, 
  my_factor(P1,Vi,Vo1),
  my_blanks_star,
  {TOP =.. [OP,P0,P]},
  my_power_r(P1,P,Vo1,Vo).
my_power_r(P,P,V,V) -->
  [].

my_factor('$NULL'(ID),Vi,Vi) -->
  my_kw("NULL"),
  {get_null_id(ID)}.
my_factor(N,Vi,Vi) -->
  my_number(N).
my_factor(V,Vi,Vo) -->
  my_variable(V,Vi,Vo).
my_factor(C,Vi,Vi) -->
  my_arithmetic_constant(C).
my_factor(T,Vi,Vo) --> 
  {my_arithmetic_function(SF,F,A)},
  my_string(SF), 
  my_blanks_star,
  "(",
  my_blanks_star,
  my_function_arguments(A,As,Vi,Vo),
  my_blanks_star,
  ")",
  {T=..[F|As]}.
my_factor(E,Vi,Vo) -->
  "(",
  my_blanks_star,
  my_arithmeticexp(E,Vi,Vo),
  my_blanks_star,
  ")".
my_factor(T,Vi,Vo) --> 
  {my_unary_operator(SOP,OP)},
  my_string(SOP),
  my_blanks_star, 
  my_arithmeticexp(E,Vi,Vo),
  {T=..[OP,E]}.

%my_priority_operator(Priority, StringOperator, Operator)
my_priority_operator(P,SOP,POP) :-
  my_infix_arithmetic(OP,POP,_,_,P),
  name(OP,SOP).

my_function_arguments(1,[E],Vi,Vo) -->
  !,
  my_arithmeticexp(E,Vi,Vo).
my_function_arguments(A,[E|Es],Vi,Vo) -->
  {A>1},
  my_arithmeticexp(E,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  {A1 is A-1},
  my_function_arguments(A1,Es,Vo1,Vo).

  
%
% Numbers
%

my_number(N) -->
  my_negative_number(N).
my_number(N) -->
  my_positive_number(N).

my_negative_number(N) -->
  "-",
  my_positive_number(P),
  {N is -P}.

my_positive_number(N) -->
  my_fractional_positive_number(M),
  "E+",
  my_integer(E),
  {N is M*(10**E)}.
my_positive_number(N) -->
  my_fractional_positive_number(M),
  "E",
  my_integer(E),
  {N is M*(10**E)}.
my_positive_number(N) -->
  my_fractional_positive_number(N).
my_positive_number(N) -->
  my_positive_integer(N).

my_fractional_positive_number(N) -->
  my_digits(Is),
  ".",
  my_digits(Ds),
  {concat_lists([Is,".",Ds],Ns),
   name(N,Ns)}.

my_integer(N) -->
  my_negative_integer(N).
my_integer(N) -->
  my_positive_integer(N).

my_negative_integer(N) -->
  "-",
  my_positive_integer(P),
  {N is -P}.

my_positive_integer(N) -->
  my_digits(Ds),
  {name(N,Ds)}.

my_digits([D]) -->
  my_digit(D).
my_digits([D|Ds]) -->
  my_digit(D),
 my_digits(Ds).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Built-ins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Built-in Arithmetic Constants

my_arithmetic_constant(Value) --> 
  {arithmetic_constant(Value,Constant,_), 
   name(Constant, SConstant)}, 
  my_string(SConstant).

arithmetic_constant(4*atan(1),'pi','Archimedes'' constant').
arithmetic_constant(exp(1),'e','Euler''s number').

% Built-in Arithmetic Functions

my_arithmetic_function(SF,PF,A) :- 
  arithmetic_function(F,PF,_,_,number(_N),A),
  name(F,SF).

% arithmetic_function(Name, PrologPredefined, Description, Kind(arithmetic or aggregate), Type, Arity)
arithmetic_function('sqrt','sqrt','Square root',arithmetic,number(float),1).
arithmetic_function('ln','log','Neperian logarithm',arithmetic,number(float),1).
arithmetic_function('log','log','Neperian logarithm',arithmetic,number(float),1).
arithmetic_function('log','log','Logarithm of the second argument in the base of the first one',arithmetic,number(float),2).
arithmetic_function('sin','sin','Sine',arithmetic,number(float),1).
arithmetic_function('cos','cos','Cosine',arithmetic,number(float),1).
arithmetic_function('tan','tan','Tangent',arithmetic,number(float),1).
arithmetic_function('cot','cot','Cotangent',arithmetic,number(float),1).
arithmetic_function('asin','asin','Arc sine',arithmetic,number(float),1).
arithmetic_function('acos','acos','Arc cosine',arithmetic,number(float),1).
arithmetic_function('atan','atan','Arc tangent',arithmetic,number(float),1).
arithmetic_function('acot','acot','Arc cotangent',arithmetic,number(float),1).
arithmetic_function('abs','abs','Absolute value',arithmetic,number(float),1).
arithmetic_function('float','float','Float value of its argument',arithmetic,number(float),1).
arithmetic_function('integer','integer','Closest integer between 0 and its argument',arithmetic,number(integer),1).
arithmetic_function('sign','sign','Returns -1 if its argument is negative, 0 otherwise',arithmetic,number(integer),1).
arithmetic_function('gcd','gcd','Greatest common divisor between two numbers',arithmetic,number(integer),2).
arithmetic_function('min','min','Least of two numbers',arithmetic,_Type,2).
arithmetic_function('max','max','Greatest of two numbers',arithmetic,_Type,2).
arithmetic_function('truncate','truncate','Closest integer between 0 and its argument',arithmetic,number(integer),1).
arithmetic_function('float_integer_part','float_integer_part','Integer part as a float',arithmetic,number(float),1).
arithmetic_function('float_fractional_part','float_fractional_part','Fractional part as a float',arithmetic,number(float),1).
arithmetic_function('round','round','Closest integer',arithmetic,number(integer),1).
arithmetic_function('floor','floor','Greatest integer less or equal to its argument',arithmetic,number(integer),1).
arithmetic_function('ceiling','ceiling','Least integer greater or equal to its argument',arithmetic,number(integer),1).
% aggregate functions
arithmetic_function('avg','avg','Average. Returns a float',aggregate,number(float),1).
arithmetic_function('count','count','Count all (with no argument). Returns an integer',aggregate,number(integer),0).
arithmetic_function('count','count','Count but nulls wrt. its argument. Returns an integer',aggregate,number(integer),1).
arithmetic_function('max','max','Maximum. Returns a value with the same type as its argument',aggregate,_Type,1).
arithmetic_function('min','min','Minimum. Returns a value with the same type as its argument',aggregate,_Type,1).
arithmetic_function('sum','sum','Cumulative sum. Returns a value with the same type as its argument',aggregate,number(_N),1).
arithmetic_function('times','times','Cumulative product. Returns a value with the same type as its argument',aggregate,number(_N),1).


% Built-in Predicates

my_builtin_pred(X) :- 
  my_infix_relation(X,_);
  my_infix_comparison(X,_);
  my_outer_join_relation(X/_);
  my_aggregate_relation(X,_);
  X=group_by.
%  my_infix_comparison(X,_).

% Built-in Functions

my_builtin_function(not/1).
my_builtin_function(group_by/3).
my_builtin_function(count/3).
my_builtin_function(count/2).
my_builtin_function(sum/3).
my_builtin_function(avg/3).
my_builtin_function(min/3).
my_builtin_function(max/3).
%my_builtin_function(is_null/1).
my_builtin_function(lj/1).
my_builtin_function(rj/1).
my_builtin_function(fj/1).

% Built-in Operators

my_infix(X) :- 
  X==(':-'); 
  my_infix_relation(X,_); 
  my_infix_comparison(X,_); 
  my_infix_arithmetic(X,_,_,_,_).

% Built-in Unary Operators
my_unary_operator(SOP,POP) :-
  unary_operator(OP,POP,_),
  name(OP,SOP).
  
unary_operator('\\','\\','Bitwise negation').
unary_operator('-','-','Negative value of its single argument').

% Built-in Binary Operators

% Built-in arithmetic expression evaluation operator
my_infix_relation('is','Evaluation of arithmetic expressions').

% Built-in relations
my_builtin_relation('is_null',1,'Determining whether its single argument is null').
my_builtin_relation('is_not_null',1,'Determining whether its single argument is not null').
my_builtin_relation('lj',3,'Left outer join: first relation, second relation, and join condition').
my_builtin_relation('rj',3,'Right outer join: first relation, second relation, and join condition').
my_builtin_relation('fj',3,'Full outer join: first relation, second relation, and join condition').
my_builtin_relation('avg',3,'Aggregate returning the average of values for an argument in a relation, ignoring nulls').
my_builtin_relation('count',3,'Aggregate returning the number of the tuples in a relation wrt. an argument, ignoring nulls').
my_builtin_relation('count',2,'Aggregate returning the number of the tuples in a relation (cf. SQL''s COUNT(*))').
my_builtin_relation('group_by',3,'Creates groups from a relation wrt. a list of variables, possibly applying aggregated conditions').
my_builtin_relation('max',3,'Aggregate returning the maximum of values for an argument in a relation, ignoring nulls').
my_builtin_relation('min',3,'Aggregate returning the minimum of values for an argument in a relation, ignoring nulls').
my_builtin_relation('sum',3,'Aggregate returning the sum of values for an argument in a relation, ignoring nulls').
my_builtin_relation('times',3,'Aggregate returning the product of values for an argument in a relation, ignoring nulls').

% Built-in Binary Comparison Operators
% my_infix_comparison(Name, Description)
my_infix_comparison('=','Syntactic equality').
my_infix_comparison('\\=','Syntactic disequality').
my_infix_comparison('>','Greater than').
my_infix_comparison('>=','Greater or equal than').
my_infix_comparison('<','Less than').
my_infix_comparison('=<','Less or equal than').

my_infix_comparison(Op) --> {my_infix_comparison(Op,_), name(Op,SOp)}, my_string(SOp).


% Built-in Binary Arithmetic Operators
% my_infix_arithmetic(Name, PrologBuiltin, ReturnType, Description, PriorityGroup)
% The priority of an operator in each priority group follows textual order of clauses (the first one has the higher priority, the last one has the lower priority)
my_infix_arithmetic('^','**',number(_),'Power',high).
my_infix_arithmetic('**','**',number(_),'Power',high).
my_infix_arithmetic('*','*',number(_),'Multiplication',medium).
my_infix_arithmetic('/','/',number(float),'Real division',medium).
my_infix_arithmetic('//','//',number(integer),'Integer quotient',medium).
my_infix_arithmetic('rem','rem',number(_),'Integer remainder',medium).
my_infix_arithmetic('\\/','\\/',number(integer),'Bitwise disjunction between integers',medium).
my_infix_arithmetic('#','#',number(integer),'Bitwise exclusive or between integers',medium).
my_infix_arithmetic('+','+',number(_),'Addition',low).
my_infix_arithmetic('-','-',number(_),'Difference between its arguments',low).
my_infix_arithmetic('/\\','/\\',number(integer),'Bitwise conjuntion between integers',low).
my_infix_arithmetic('<<','<<',number(integer),'Shift left the first argument the number of places indicated by the second one',low).
my_infix_arithmetic('>>','>>',number(integer),'Shift right the first argument the number of places indicated by the second one',low).


/*********************************************************************/
/* Solving Prolog Goals                                              */
/*********************************************************************/

solve_prolog(Goal,NVs) :- 
  solve_prolog_body(Goal,NVs), 
  write_with_NVs(Goal,NVs), 
  nl_log, 
  write_log('? (type ; for more solutions, <Intro> to continue) '), 
  flush_output,
  readln(S,_), 
  (batch(_,_,_) -> write_string(S), nl, inc_line; true),
  write_only_to_log(S), 
  nl_only_to_log, 
  S=="",
  !, 
  write_log_list([yes,nl]).
solve_prolog(_Goal,_R) :- 
  write_log(no), 
  nl_log.

solve_prolog_body(true,_) :- 
  !.
solve_prolog_body((G,Gs),R) :- 
  !, 
  solve_prolog_goal(G,R), 
  solve_prolog_body(Gs,R).
solve_prolog_body(G,R) :- 
  solve_prolog_goal(G,R).

solve_prolog_goal(not(G),R) :- % Negation
  !, 
  my_not(solve_prolog_body(G,R)).
solve_prolog_goal(group_by(A,Vs,C),R) :- % Group by
  !, 
  my_raise_exception(group_by(A,Vs,C),unsupported_in_Prolog,R).
solve_prolog_goal(Aggr,R) :- % Aggregates
  my_aggregate_relation(AF,Arity),
  functor(Aggr,AF,Arity),
  !, 
  my_raise_exception(Aggr,unsupported_in_Prolog,R).
solve_prolog_goal(G,_) :-      % Solves a goal using all of its matching rules
  (datalog((G:-B),NVs,Rid,Ls,Fid,Rs); (datalog(G,NVs,Rid,Ls,Fid,Rs),B=true)),
  R=datalog((G:-B),NVs,Rid,Ls,Fid,Rs),
  solve_prolog_body(B,R).
solve_prolog_goal(G,R) :-      % Prolog Built-ins
  G =.. [P|As],
  (my_builtin_pred(P) ->
   call(G)
   ;
   (user_predicates(Ps),
    length(As,L),
    (my_member_var(P/L,Ps) ->
     fail
     ;
     my_raise_exception(G,undefined,R)
    )
   )
  ).


/*********************************************************************/
/* Solving Datalog Queries                                           */
/*********************************************************************/

solve_datalog_query(Query,Undefined) :-
  strata(S),
  (S==[] ->
    solve_datalog_stratum(Query,1,Undefined)  % No program was loaded; so, no strata computed
    ;
    (S==[non-stratifiable] -> 
      try_solve_stratified(Query,Undefined) % Although in a non-stratifiable program, try to solve for the given query, hopefully finding a stratifiable subprogram
     ;
      solve_stratified(Query,Undefined))).  % Stratifiable program: stratum solving

solve_datalog_stratum(Query,Stratum,_Undefined) :- 
  solve_star(Query,Stratum). % This call is always made to fail
solve_datalog_stratum(_Query,_Stratum,Undefined) :-
  remove_undefined(Undefined),
  ground_nulls,
  set_complete_flags.
  
set_complete_flags :-
  retract(complete_flag(G,no)),
  assertz(complete_flag(G,yes)),
  fail.
set_complete_flags.

try_solve_stratified(Query,Undefined) :-
  (Query=not(Q) -> true; Q=Query),
  pdg(G),
  functor(Q,N,A),
  sub_pdg(N/A,G,SG),
  stratify(NS,SG,B), !,
  (B==false -> 
   (write_log_list(['Warning: Unable to ensure correctness for this query.', nl]),
    solve_datalog_stratum(Query,1,Undefined)); % Non-stratifiable yet
   (write_log_list(['Info: Stratifiable subprogram found for the given query.', nl]),
    strata(S),
    load_stratification(NS,SG),
    solve_stratified(Query,Undefined),
    load_stratification(S,G))).

solve_stratified(Query,Undefined) :- 
  (Query=not(Q) -> true; Query=Q),
  pdg(G),
  functor(Q,N,A),
  sub_pdg(N/A,G,(_Nodes,Arcs)),
  neg_dependencies(Arcs,ND),
  (ND=[] ->
   solve_datalog_stratum(Query,1,Undefined)
   ;
   strata(S),
   sort_by_strata(S,ND,SR),
   build_queries(SR,Queries,NVs),
   (verbose(on) -> 
    write_log('Info: Computing by stratum of ['),
    write_csa_with_NVs(Queries,NVs),
    write_log_list(['].',nl])
    ;
    true),
   solve_datalog_stratum_list(Query,Undefined,Queries)).

solve_datalog_stratum_list(Query,Undefined,[Q|Qs]) :-
  get_stratum(Q,Stratum),
  solve_datalog_stratum(Q,Stratum,_U),
  solve_datalog_stratum_list(Query,Undefined,Qs).
solve_datalog_stratum_list(Query,Undefined,[]) :-
  get_stratum(Query,Stratum),
  solve_datalog_stratum(Query,Stratum,Undefined).

get_stratum(G,St) :-
  G =.. [F,SG],
  my_builtin_function(F/1),
  !,
  get_atom_stratum(SG,St).
get_stratum(G,St) :-
  get_atom_stratum(G,St).

get_atom_stratum(G,St) :-
  G =.. [P|Args],
  length(Args,A),
  strata(S),
  my_member((P/A,St),S),
  !.
  
neg_dependencies([],[]).
neg_dependencies([_T-F|As],[F|Fs]) :-
  neg_dependencies(As,Fs).
neg_dependencies([_T+_F|As],Fs) :-
  neg_dependencies(As,Fs).

sort_by_strata(S,R,SR) :-
  flip_pairs(S,FS),
  my_sort(FS,OFS),
  filterdrop(OFS,R,SR).

flip_pairs([],[]).
flip_pairs([(P,S)|Xs],[(S,P)|Ys]) :-
  flip_pairs(Xs,Ys).

filterdrop([],_,[]).
filterdrop([(_S,P)|Xs],R,[P|Ps]) :-
  my_member(P,R),
  !,
  filterdrop(Xs,R,Ps).
filterdrop([(_S,_P)|Xs],R,Ps) :-
  filterdrop(Xs,R,Ps).

build_queries([],[],[]).
build_queries([N/A|Ps],[Q|Qs],NVs) :-
  length(L,A),
  Q =.. [N|L],
  assign_variable_names(L,[],NV1s),
  build_queries(Ps,Qs,NV2s),
  my_append(NV1s,NV2s,NVs).
  
assign_variable_names_list([],[]).
assign_variable_names_list([T|Ts],[(T,Vs)|RTVs]) :-
  assign_variable_names(T,[],Vs),
  assign_variable_names_list(Ts,RTVs).

assign_variable_names(T,JNVs,NVs) :-
  my_term_variables(T,Vs),
  name_NVs(Vs,JNVs,_,NVs).

% (NVs,AlreadyNamed by other procedure, Last name, Name vars)
name_NVs([],NVs,_,NVs).
name_NVs([V|Vs],JNVs,LN,NVs) :-  
  find_var_name(V,_N,JNVs),
  !,
  name_NVs(Vs,JNVs,LN,NVs).
name_NVs([V|Vs],JNVs,LN,NVs) :-
  name_var(JNVs,LN,N),
  name_NVs(Vs,[N=V|JNVs],N,NVs).

name_var(NVs,LN,N) :-
  (var(LN) -> 
   first(SN),
   name(TN,SN);
   next(LN,TN)),
   (my_member(TN=_,NVs) ->
    name_var(NVs,TN,N)
    ;
    TN=N).

name_var(NVs,N) :-
  name_var(NVs,_LN,N).
  
next(X,Y) :-
  name(X,SX),
  nextS(SX,SY),
  name(Y,SY).

nextS("",SF) :- 
  !, 
  first(SF).
nextS(SX,SY) :-
  my_append(SFX,[LX],SX),
  last([CZ]),
  (LX<CZ ->
   LY is LX+1,
   my_append(SFX,[LY],SY)
   ;
   nextS(SFX,SFY),
   first(SF),
   my_append(SFY,SF,SY)
  ).

first(SF) :-
  name('A',SF).
last(SL) :-
  name('Z',SL).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fixpoint Computation: solve_star
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_star(Q,St) :-
  repeat,
  (remove_calls,
   et_not_changed, % Sets a flag indicating that the extension table has not changed 
   solve(Q,St,_),  % Solves the call to Q using memoization at stratum St
   fail;           % Requests all alternatives
   no_change,      % When no more alternatives, restart the computation if the extension table has changed,
   !,              % otherwise, fail and exit
   fail).    
                      

% Building a Predicate with Fresh Variables and Universal Nulls
build(Q,G) :-
  copy_term(Q,FQ),
  abstract_nulls(FQ,G).
  
% Get the variables of a term
my_term_variables(T,Vs):-
  my_term_variables(T,[],Vs).
  
my_term_variables(V,Vi,Vo) :-
  var(V), !,
  (my_member_var(V,Vi) ->
   Vo=Vi
   ;
   my_append(Vi,[V],Vo)).
my_term_variables(T,V,V) :-
  atomic(T),
  !.  
my_term_variables(T,Vi,Vo) :-
  T=..[_|As], 
  my_terms_variables(As,Vi,Vo).  

my_terms_variables([],Vs,Vs).
my_terms_variables([T|Ts],Vi,Vs) :-
  my_term_variables(T,Vi,Vo1),
  my_terms_variables(Ts,Vo1,Vs).
  
/*********************************************************************/
/* Removing previous calls on a new run of fixpoint computation      */
/*********************************************************************/

remove_calls :-
  retractall(called(_X)).

/*********************************************************************/
/* Testing whether the extension table has not changed               */
/*********************************************************************/

no_change :-
  et_flag(no).

/*********************************************************************/
% Setting Extension Table Flag to 'changed'
/*********************************************************************/

et_changed :-
  set_flag(et_flag,yes).

/*********************************************************************/
% Setting Extension Table Flag to 'not changed'
/*********************************************************************/

et_not_changed :-
  set_flag(et_flag,no).

/*********************************************************************/
% Setting a flag
/*********************************************************************/

set_flag(Flag,Value) :-
  Fact =.. [Flag,_OldValue],
  retractall(Fact),
  NewFact =.. [Flag,Value],
  assertz(NewFact).

set_flag(Flag,Value1,Value2) :-
  Fact =.. [Flag,_OldValue1,_OldValue2],
  retractall(Fact),
  NewFact =.. [Flag,Value1,Value2],
  assertz(NewFact).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solving with Extension Table: solve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(G,St,R) :-
  solve(G,St,R,_Ids).

solve(true,_St,_,[]) :-
  !.
solve((G,Gs),St,R,[]) :-
  duplicates(off),
  !, 
  solve(G,St,R,_Ids1), 
  solve(Gs,St,R,_Ids2).
solve((G,Gs),St,R,Ids) :-
  !, 
  % Duplicates ON
  solve(G,St,R,Ids1), 
  solve(Gs,St,R,Ids2),
  my_append(Ids1,Ids2,Ids).
solve(lj(G),St,R,Ids) :-  % Left outer join; simply execute the goal
  !,
  solve(G,St,R,Ids).
solve(rj(G),St,R,Ids) :-  % Left outer join; simply execute the goal
  !,
  solve(G,St,R,Ids).
solve(fj(G),St,R,Ids) :-  % Left outer join; simply execute the goal
  !,
  solve(G,St,R,Ids).
% solve(G,_St,R) :-     
%   compute_builtin_relation(G,_St,R).
% solve(G,_St,R) :-     
%   compute_primitive(G,R).
solve(G,St,R,[Id]) :- 
  memo(G,St,R,Id).

% Already called. Extension table with an entry for the current call
memo(G,_St,_,IdG) :-
  build(G,Q),           % Builds in Q the same call with fresh variables
  called(Q),            % Tries to find a unifiable call in the extension table for the current call
  my_subsumes(Q,G),     % Tests whether the extension table call subsumes the current call
  !,                    % If so,
  et_lookup(G,IdG).     % use the result in the extension table; otherwise, process the new call with the next memo clause
% New call. Extension table without an entry for the current call
memo(G,St,R,IdG) :-
  assertz(called(G)),   % Asserts the current call in the extension table (because: (1) there is no previous call to G, or (2) G is not subsumed by a previous call to G
  set_complete_flag(G,CF),
  (
   (et_lookup(G,IdG))    % If the et is not empty, the first call have to return all the possible answers computed in a previous pass of the fixpoint computation
   ;
   (
    CF == yes,          % Don't try to recompute a completed computation
    !,
    fail
   )
   ;
   (solve_goal(G,St,R,IdG), % Solves the current call using its matching rules
    build(G,Q),         % Builds in Q the same call with fresh variables
    no_subsumed_by_et(Q,(G,IdG)),
%    term_depth_leq(IdG,4),
%    assertz(et(G)),    % Asserts the new result
    et_assert(G,IdG),
    et_changed)).       % Sets a flag indicating that the extension table has changed

% Tests whether there is not an entry in the extension table subsuming the current result
% If duplicates are enabled, identifier chains has to be used to distinguish data sources
no_subsumed_by_et(Q,(G,IdG)) :-
  duplicates(on),
  !,
  my_not((et_lookup(Q,IdQ),      
          my_subsumes((Q,IdQ),(G,IdG)))),
  nr_id(IdG).
no_subsumed_by_et(Q,(G,_IdG)) :-
  % Duplicates OFF
  my_not((et_lookup(Q,_IdQ),      
          my_subsumes(Q,G))).

% Asserts the input fact whenever it does not contain any variable;
% though, it may contain null
% et_assert(G) :-
%   ((my_no_contains_vars(G)
%     ;
%     functor(G,group_by,3)
%     ;
%     (functor(G,F,A), my_aggregate_relation(F,A))
%     ;
%     % Negated, non-ground facts are allowed to be asserted for outer join computations
%    (G=not(NG), functor(NG,NGF,_), [Dolar]="$", name(NGF,[Dolar|_])))
%    ->
%     assertz(et(G))
%    ;
%     my_raise_exception(G,instantiation,'Extension table')).   % Asserts the new result
% If nonground facts are not allowed, some sql translations cannot be computed as:
% select a from s where b not in ((select a from t where t.a=s.a) union (select a from t where b=1))
% in des.ini
et_assert(G,IdG) :-
  assertz(et(G,IdG)).

    
% et_lookup(G) :-
%   G=..[F,GP|_IArgs],
%   my_aggregate_relation(F,_A),
%   !,
%   et(S),
%   S=..[F,SP|_SArgs],
%   my_equal_up_to_renaming(GP,SP),
%   G=S.
%et_lookup(G) :-
%  et(G).
et_lookup(G,IdG) :-
  et(G,IdG).
  
set_complete_flag(G,CF) :-
  ((complete_flag(SG,CF), my_subsumes(SG,G)) -> 
    true 
   ; 
    ((functor(G,F,_A),my_builtin_pred(F)) ->   % As built-ins are infinite relations, they can not be completely computed
      true
     ;
     (CF = no,
      assertz(complete_flag(G,CF))))).

% Non recursive chain of ids
nr_id((ID,T)) :-
  my_not(my_member_term(ID,T)),
  !.

% Solving a Goal: solve_goal
solve_goal(not(G),St,R,(-1,[])) :- % Negation; follows the et mechanism
  !, 
  solve_not(G,St,R).
solve_goal(G,_St,R,(-1,[])) :-     
  compute_primitive(G,R).
solve_goal(group_by(A,Vs,C),St,R,(-1,[])) :-     
  compute_group_by(group_by(A,Vs,C),St,R).
solve_goal(G,_St,R,(-1,[])) :-     
  compute_pred_aggregate(G,R).
solve_goal(G,St,R,(-1,[])) :-  % Deciding whether a term is null
  compute_builtin_relation(G,St,R).
solve_goal(G,St,_,(RId,AIds)) :-      % Solves a goal using all of its matching rules
  (datalog((G:-B),NVs,RId,Ls,Fid,Rs)
   ;
  (datalog(G,NVs,RId,Ls,Fid,Rs),B=true)),
  R=datalog((G:-B),NVs,RId,Ls,Fid,Rs),
  solve(B,St,R,AIds).

% Solving Negation: solve_et_not for et_not [SD91] and solve_strata (optimized)
solve_not(G,St,R) :-
  neg(A),
  (A=strata -> 
    solve_strata(G,St,R);
    solve_et_not(G,St)).

solve_strata(G,St,R) :-
  solve(G,St,R), 
  !, 
  fail.
solve_strata(_G,_St,_R).

solve_et_not(G,St) :-
  (solve_star(G,St), fail; true),  % ET* evaluation
  (et(G,_), !, fail; true).        % ET lookup

% Computing Primitives: compute_primitive
compute_primitive(A is B,R) :- 
  ((my_ground(B), my_not(contain_null(B))) ->
     A is B
    ;
     (contain_null(B) ->
       A='$NULL'(_ID)
      ;
       my_raise_exception(A is B,instantiation,R))).
compute_primitive(A=B,_R) :- 
  eval_expr(A,EA,R),
  eval_expr(B,EB,R),
  EA=EB.
compute_primitive(A\=B,R) :- 
  ((A='$NULL'(ID) ; B='$NULL'(ID)) -> fail ; true),
  ((var(A); var(B)) ->
   my_raise_exception(A\=B,instantiation,R)
   ;
   eval_expr(A,EA,R),
   eval_expr(B,EB,R),
   EA\=EB
  ).
compute_primitive(A>B,R)  :- 
  ((var(A); var(B)) ->
   my_raise_exception(A>B,instantiation,R)
   ;
   eval_expr(A,EA,R),
   eval_expr(B,EB,R),
   (number(EA),number(EB) -> 
    EA>EB
    ; 
    ((EA='$NULL'(_IDA) ; EB='$NULL'(_IDB)) ->
     fail 
     ; 
     EA@>EB)
   )
  ).
compute_primitive(A>=B,R) :- 
  ((var(A); var(B)) ->
   my_raise_exception(A>=B,instantiation,R)
   ;
   eval_expr(A,EA,R),
   eval_expr(B,EB,R),
   (number(EA),number(EB) -> 
    EA>=EB
    ; 
    ((EA='$NULL'(_IDA) ; EB='$NULL'(_IDB)) ->
     fail 
     ; 
     EA@>=EB)
   )
  ).
compute_primitive(A<B,R)  :- 
  ((var(A); var(B)) ->
   my_raise_exception(A<B,instantiation,R)
   ;
   eval_expr(A,EA,R),
   eval_expr(B,EB,R),
   (number(EA),number(EB) -> 
    EA<EB
    ; 
    ((EA='$NULL'(_IDA) ; EB='$NULL'(_IDB)) ->
     fail 
     ; 
     EA@<EB)
   )
  ).
compute_primitive(A=<B,R) :- 
  ((var(A); var(B)) ->
   my_raise_exception(A=<B,instantiation,R)
   ;
   eval_expr(A,EA,R),
   eval_expr(B,EB,R),
   (number(EA),number(EB) -> 
    EA=<EB
    ; 
    ((EA='$NULL'(_IDA) ; EB='$NULL'(_IDB)) ->
     fail 
     ; 
     EA@=<EB)
   )
  ).

eval_expr(E,EE,R) :-  
  (my_noncompoundterm(E)
   -> 
    EE=E
   ;
    compute_primitive(EE is E,R)).
  
  
compute_group_by(group_by(G,GBVs,C),St,R) :-
  !,
  replace_and_get_aggregates_pairs(C,G,[],AVs,RC),
  build_groups(G,GBVs),
  my_nf_bagof(CG,
%  bagof(CG,
             Ids^
             (et(CG,Ids),
              my_unifiable(CG,G)),
             CGs),
  solve_aggregates_list(AVs,CGs),
  solve(RC,St,R).
  
% Gets pairs (aggregate(Position),Result) from a term including aggregates. 
% It also replaces aggregate(Variable) by Result in the term. Result is a varible which will be unified with the result of the aggregate afterwards
replace_and_get_aggregates_pairs(T,_G,AVs,AVs,T) :- 
  var(T),
  !.
replace_and_get_aggregates_pairs('$NULL'(ID),_G,AVs,AVs,'$NULL'(ID)) :- 
  !.
replace_and_get_aggregates_pairs(C,_G,AVs,[(C,R)|AVs],R) :- 
  arithmetic_function(C,_,_,aggregate,_,0),
  !.
replace_and_get_aggregates_pairs(T,_G,AVs,AVs,T) :- 
  atomic(T),
  !.
replace_and_get_aggregates_pairs(C,G,AVs,[(RC,R)|AVs],R) :- 
  C =.. [F,V],
  arithmetic_function(F,_,_,aggregate,_,1),
  !, 
  G=..[_|Args],
  get_arg_position(V,Args,I),
  RC =.. [F,I].
replace_and_get_aggregates_pairs(C,G,AVsi,AVso,RC) :- 
  C =.. [F|As],
  replace_and_get_aggregates_pairs_list(As,G,AVsi,AVso,RAs),
  RC =.. [F|RAs].

replace_and_get_aggregates_pairs_list([],_G,AVs,AVs,[]) :-
  !.
replace_and_get_aggregates_pairs_list([T|Ts],G,AVsi,AVso,[RT|RTs]) :-
  replace_and_get_aggregates_pairs(T,G,AVsi,AVso1,RT), 
  replace_and_get_aggregates_pairs_list(Ts,G,AVso1,AVso,RTs).
  
% Solves each aggregate(Position,Result) in a list, holding the result in Result, wrt. the list of a computed set CGs
solve_aggregates_list([],_CGs).
solve_aggregates_list([(AF,V)|AVs],CGs) :-
  AF=..[F,I],
  !,
  arg_list(I,CGs,Ns),
  compute_aggr_from_group(F,Ns,V),
  solve_aggregates_list(AVs,CGs).
solve_aggregates_list([(F,V)|AVs],CGs) :-
  compute_aggr_from_group(F,CGs,V),
  solve_aggregates_list(AVs,CGs).

% Computing Aggregate Predicates: compute_pred_aggregate

compute_pred_aggregate(count(G,GBVs,O),_R) :-
  % Count(*) counts all rows, even when some might contain nulls
  % Other aggregates include an additional argument: the attribute w.r.t the aggregation is computed
  !,
  build_groups(G,GBVs),
  my_nf_bagof(CG,
             Ids^
             (et(CG,Ids),
              my_unifiable(CG,G)),
             CGs),
  length(CGs,O).
compute_pred_aggregate(Aggr,_R) :-
  % A tuple with a null in the argument position V is omitted in the aggregate computation
  Aggr=..[AF,G,V,GBVs,O],
  my_aggregate_relation(AF,4),
  G=..[_P|Args],
  get_arg_position(V,Args,I),
  build_groups(G,GBVs),
  my_nf_bagof(N,
             CG^Ids^
             (et(CG,Ids),
              my_unifiable(CG,G),
              arg(I,CG,N),
              N\='$NULL'(_Id)),
             Ns),
  compute_aggr_from_group(AF,Ns,O).
  
%compute_aggr_from_group(count,Ns,O) :- 
%  length(Ns,O).
compute_aggr_from_group(count,Ns,O) :-
  compute_count(Ns,O).
compute_aggr_from_group(sum,Ns,O) :- 
  compute_sum(Ns,O).
compute_aggr_from_group(times,Ns,O) :- 
  compute_times(Ns,O).
compute_aggr_from_group(avg,Ns,O) :- 
  compute_avg(Ns,O).
compute_aggr_from_group(min,Ns,O) :- 
  compute_min(Ns,O).
compute_aggr_from_group(max,Ns,O) :- 
  compute_max(Ns,O).

build_groups(G,GBVs) :-
  G=..[_|Args],
  get_arg_position_list(GBVs,Args,GBVPoss),
  copy_term(G,FG),
  get_ith_arg_list(GBVPoss,FG,FGBVs),
  my_term_variables(FG,FGVs),
  copy_term(GBVs,NVs),
  build_ex_quantifier(FGVs,(et(FG,Ids),abstract_unify_nulls_varlist(FGBVs,NVs,GBVs)),QFG),
  QQFG=Ids^QFG, % Ciao needs this! Instead, it should be written as the remarked line below
  setof(GBVs,QQFG,GBVals),
%  setof(GBVs,Ids^QFG,GBVals),
  !,
  my_member(GBVs,GBVals). % Leaves choicepoint to build groups
build_groups(G,_GBVs) :-
  et(G,_Ids),
  !.
% The next clause applies when no group can be found but there is tuples in the input relation.
build_groups(G,_GBVs) :-
  et(G,_Ids),
  !.
% The next clause applies when no group can be found.
% For instance, counting wrt. to no groups returns 0 
build_groups(_G,[]) :-
  !.

build_ex_quantifier([],G,G).
build_ex_quantifier([V|Vs],G,QG) :-
  build_ex_quantifier(Vs,V^G,QG).

arg_list(_I,[],[]) :-
  !.
arg_list(I,[CG|CGs],Ns) :-
  arg(I,CG,N),
  nonvar(N),
  N='$NULL'(_ID),
  !,
  arg_list(I,CGs,Ns).
arg_list(I,[CG|CGs],[N|Ns]) :-
  arg(I,CG,N),
  arg_list(I,CGs,Ns).

  
get_ith_arg_list([],_T,[]).
get_ith_arg_list([P|Ps],T,[A|As]) :-
  arg(P,T,A),
  get_ith_arg_list(Ps,T,As).
    
get_arg_position_list([],_L,[]).
get_arg_position_list([V|Vs],L,[P|Ps]) :-
  get_arg_position(V,L,P),
  get_arg_position_list(Vs,L,Ps).

get_arg_position(V,L,P) :-
  get_arg_position_from(V,L,1,P).

get_arg_position_from(V,[A|_As],I,I) :-
  V==A,
  !.
get_arg_position_from(V,[_A|As],I,NI) :-
  I1 is I+1,
  get_arg_position_from(V,As,I1,NI).
  
compute_count(Ns,C) :-
  compute_count_acc(Ns,0,C).

compute_count_acc([],C,C) :-
  !. 
compute_count_acc([X|Xs],Ci,Co) :-
  nonvar(X),
  X='$NULL'(_ID),
  !,
  compute_count_acc(Xs,Ci,Co).
compute_count_acc([_X|Xs],Ci,Co) :-
  !,
  C1 is Ci+1,
  compute_count_acc(Xs,C1,Co).

compute_avg([],'$NULL'(_Id)) :-
  !.
compute_avg(Ns,O) :-
  compute_sum(Ns,S),
  length(Ns,L),
  O is S/L.

compute_sum([],'$NULL'(_Id)).
compute_sum([N|Ns],S) :-
  compute_sum_acc(Ns,N,S).

compute_sum_acc([],N,N).
compute_sum_acc([N|Ns],TS,S) :-
  NN is N+TS,
  compute_sum_acc(Ns,NN,S).

compute_times([],'$NULL'(_Id)).
compute_times([N|Ns],S) :-
  compute_times_acc(Ns,N,S).

compute_times_acc([],N,N).
compute_times_acc([N|Ns],TS,S) :-
  NN is N*TS,
  compute_times_acc(Ns,NN,S).

compute_min([],'$NULL'(_Id)).
compute_min([N|Ns],M) :-
  compute_min_acc(Ns,N,M).

compute_min_acc([],M,M).
compute_min_acc([N|Ns],TM,M) :-
  compute_primitive(N<TM,_R),
  !,
  compute_min_acc(Ns,N,M).
compute_min_acc([_N|Ns],TM,M) :-
  compute_min_acc(Ns,TM,M).

compute_max([],'$NULL'(_Id)).
compute_max([N|Ns],M) :-
  compute_max_acc(Ns,N,M).

compute_max_acc([],M,M).
compute_max_acc([N|Ns],TM,M) :-
  compute_primitive(N>TM,_R),
  !,
  compute_max_acc(Ns,N,M).
compute_max_acc([_N|Ns],TM,M) :-
  compute_max_acc(Ns,TM,M).

compute_builtin_relation(G,_St,R) :-  
  functor(G,F,A),
  my_builtin_relation(F,A,_M),
  \+ my_aggregate_relation(F,A),
  \+ (F,A)==(group_by,3),
  !,
  ((my_ground(G) ; contain_null(G)) ->
   call(G)
  ;
  my_raise_exception(G,instantiation,R)).

my_raise_exception(G,Mid,R_V) :-
  (seen;true),
  (Mid = instantiation -> 
   Message = 'Non ground argument(s) found in goal'
   ;
   (Mid = undefined ->
    Message = 'Undefined predicate'
    ;
    (Mid = basic_goal ->
     Message = 'The following is not a valid goal:'
     ;
     (Mid = exec ->
      Message = 'Executing goal:'
      ;
      (Mid = unsupported_in_Prolog ->
       Message = 'Aggregates are not supported in Prolog mode'
       ;
       (Mid = non_number ->
        Message = 'Non-numbers found in result set of'
        ;
        (Mid = type ->
         Message = 'Type error'
         ;
	       (Mid = bounds ->
	        Message = 'Bounds error'
	        ;
	        (Mid = fd_unsupported ->
	         Message = 'FD constraint solving unsupported by underlying Prolog system. '
  	       ;
           (Mid = odbc_unsupported ->
	          Message = 'ODBC connections unsupported by underlying Prolog system. Use either binaries or SWI-Prolog or SICStus Prolog sources. '
  	        ;
  	        Message = 'Unknown'
  	       )
 	        )
	       )
        )
       )
      )
     )
    )
   )
  ),
  write_log_list(['Exception: ',Message,' ']),
  ((nonvar(R_V),R_V=datalog(R,NVs,_Rid,Ls,Fid,_Rs)) ->
    write_with_NVs(G,NVs),
    write_log_list([' in the instanced rule:',nl]),
    display_ruleNVs_list([(R,NVs)],11),
    display_rule_info(Ls,Fid)
   ;
%    write_with_NVs(G,R_V)
    write_with_NVs(G,[])
  ),
  nl_log,
  throw(instantiation_error(G,R)).

% Testing whether a term T1 subsumes a term T2
% i.e., T1 is 'more general' than T2

% Aggregates
% min(p(X,Y),X,[],1) does not subsume min(p(X,2),X,[],1)
% min(p(X,2),X,[],1) does not subsume min(p(X,Y),X,[],1)
% min(p(X,Y),X,[Y],1) does subsume min(p(X,Y),X,[a],1)
% It suffices to test whether they are the same term up to variable renaming
% my_subsumes(General,Specific) :-
%   functor(General,AF,Arity),
%   my_aggregate_relation(AF,_), %Arity),
%   !,
%   functor(Specific,AF,Arity),
%   remove_GBArg(General,G),
%   remove_GBArg(Specific,S),
%   my_equal_up_to_renaming(G,S).
my_subsumes(General,Specific) :-
  functor(General,AF,Arity),
  (my_aggregate_relation(AF,Arity)
   ;
   (AF,Arity)=(group_by,3)
  ),
  !,
  functor(Specific,AF,Arity),
  my_equal_up_to_renaming(General,Specific).

% p(X,Y) do subsume p(X,2)
% p(X,2) do not subsume p(X,Y)
my_subsumes(General,Specific) :-
  \+ \+ (make_ground(Specific),
         General=Specific).

my_equal_up_to_renaming(General,Specific) :-
  \+ \+ (make_ground(General),
         make_ground(Specific),
         General==Specific).

remove_GBArg(A,RA) :-
  A =.. [F,P,V,_GB,_O],
  !,
  RA =.. [F,P,V].
remove_GBArg(A,RA) :-
  A =.. [F,P,_GB,_O],
  RA =.. [F,P].
         
% Replaces all occurrences of '$NULL'(CteOrVar) by '$NULL'(FreshVar) in a term T
abstract_nulls(T,T) :- 
  (var(T)),
  !.
abstract_nulls('$NULL'(_CteOrVar),'$NULL'(_FreshVar)) :- 
  !.
abstract_nulls(T,T) :- 
  (number(T) ; atom(T)),
  !.
abstract_nulls(C,RC) :- 
  C =.. [F|As],
  !, 
  abstract_nulls_list(As,RAs),
  RC =.. [F|RAs].

abstract_nulls_list([],[]) :-
  !.
abstract_nulls_list([T|Ts],[RT|RTs]) :-
  abstract_nulls(T,RT), 
  abstract_nulls_list(Ts,RTs).

% Replaces all occurrences of '$NULL'(CteOrVar) by '$NULL'(Var) in a term T
abstract_unify_nulls(T,_V,T) :- 
  (var(T)),
  !.
abstract_unify_nulls('$NULL'(_CteOrVar),V,'$NULL'(V)) :- 
  !.
abstract_unify_nulls(T,_V,T) :- 
  (number(T) ; atom(T)),
  !.
abstract_unify_nulls(C,V,RC) :- 
  C =.. [F|As],
  !, 
  abstract_unify_nulls_list(As,V,RAs),
  RC =.. [F|RAs].

abstract_unify_nulls_list([],_V,[]) :-
  !.
abstract_unify_nulls_list([T|Ts],V,[RT|RTs]) :-
  abstract_unify_nulls(T,V,RT), 
  abstract_unify_nulls_list(Ts,V,RTs).

abstract_unify_nulls_varlist([],[],[]) :-
  !.
abstract_unify_nulls_varlist([T|Ts],[V|Vs],[RT|RTs]) :-
  abstract_unify_nulls(T,V,RT), 
  abstract_unify_nulls_varlist(Ts,Vs,RTs).

% Instantiates all variables in Term to fresh constants.
make_ground(Term) :-
  numbervars(Term, 0, _).

make_ground_args(_A,[]).
make_ground_args(A,[P|Ps]) :-
  arg(P,A,T),
  make_ground(T),
  make_ground_args(A,Ps).

% Tests whether a term is ground
my_ground(Term) :-
  copy_term(Term,Copy),
  (Term == Copy ->
   true;
   fail).
   
% Tests whether a term does not contain variables
my_no_contains_vars(T) :-
  var(T),
  !,
  fail.
my_no_contains_vars('$NULL'(_ID)) :-
  !.
my_no_contains_vars(C) :- 
  C =.. [_F|As],
  !, 
  my_no_contains_vars_list(As).

my_no_contains_vars_list([]).
my_no_contains_vars_list([T|Ts]) :-
  my_no_contains_vars(T),
  my_no_contains_vars_list(Ts).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove_undefined
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_undefined(Undefined) :-
  setof(Fact, 
        (et(not(Fact),NIds),
         et(Fact,Ids), 
         retract(et(not(Fact),NIds)), 
         retract(et(Fact,Ids))), 
        Undefined).
remove_undefined([]).

ground_nulls :-
  et(Fact,Ids),
  concrete_nulls(Fact,GFact,Grounded),
  (Grounded == grounded ->
   retract(et(Fact,Ids)),
   assertz(et(GFact,Ids))),
  fail.
ground_nulls.
   
% Assigns an unique ID to each occurrence of '$NULL'(Var) in a term T
concrete_nulls(T) :-
  concrete_nulls(T,T,_Grounded).

%::WARNING: the second argument is the very same as the first, why is it used?
concrete_nulls(Var,Var,_Grounded) :- 
  var(Var),
  !.
concrete_nulls('$NULL'(Var),'$NULL'(Var),grounded) :- 
  var(Var),
  !,
  get_null_id(Var).
concrete_nulls(T,T,_) :- 
  (number(T) ; atom(T)),
  !.
concrete_nulls(C,RC,Grounded) :- 
  C =.. [F|As],
  !, 
  concrete_nulls_list(As,RAs,Grounded),
  RC =.. [F|RAs].

concrete_nulls_list([],[],_Grounded) :-
  !.
concrete_nulls_list([T|Ts],[RT|RTs],Grounded) :-
  !, 
  concrete_nulls(T,RT,Grounded), 
  concrete_nulls_list(Ts,RTs,Grounded).

% Tests whether a term contains a null
contain_null(T) :-
  var(T),
  !,
  fail.
contain_null('$NULL'(_ID)) :-
  !.
contain_null(C) :- 
  C =.. [_F|As],
  !, 
  contain_null_list(As).

contain_null_list([T|_Ts]) :-
  contain_null(T).
contain_null_list([_T|Ts]) :-
  contain_null_list(Ts).

/*********************************************************************/
/* Building a predicate dependency graph: build_pdg/1                */
/*********************************************************************/
% A predicate dependency graph is a pair of a list of predicate nodes 
% (name/arity), and a list of arcs (nto/ato + nfrom/afrom, meaning that
% the predicate nto/ato depends on -has in the rhs of any of its defining
% rules- the predicate nfrom/afrom; alternatively, nto/ato - nfrom/afrom 
% is used when the predicate nfrom/afrom appears negated)
% Follows [ZCF+97]

% TODO: Performance: When connected to an RDB, get its database schema instead of querying with datalog/6

build_pdg((Nodes,Arcs)) :-
  write_verb(['Info: Computing predicate dependency graph...',nl]),
  (setof(LLArcs,find_pdg_arcs(LLArcs),LLArcs), ! ; LLArcs=[]),
  concat_lists(LLArcs,LArcs),
  remove_duplicates_var(LArcs,Arcs),
  user_predicates(LHSNodes),
  !,
  rhsnodes(Arcs,RHSNodes),
  my_merge(LHSNodes,RHSNodes,Nodes),
  display_undefined(RHSNodes,LHSNodes), 
  !.
build_pdg(([],[])).

user_predicates(Preds) :-
  setof(N/A, 
        H^B^Ls^NVs^Rid^Fid^Rs^
          ((datalog(':-'(H,B),NVs,Rid,Ls,Fid,Rs)
           ;
            datalog(H,NVs,Rid,Ls,Fid,Rs)),
          functor(H,N,A),
          N\==(':-')), 
        Preds).
        
find_pdg_arcs(Arcs) :-
  datalog(':-'(H,B),_,_,_,_,_), 
  functor(H,N,A), 
  pdg_arcs_from_to(B,N/A,Arcs).

pdg_arcs_from_to((B,Bs),P,[Arc|Arcs]) :-
  pdg_arc(B,P,Arc),
  !,
  pdg_arcs_from_to(Bs,P,Arcs).
pdg_arcs_from_to(B,P,[Arc]) :-
  pdg_arc(B,P,Arc).

% Negative dependency
pdg_arc(not(T),P,P-N/A) :- 
  functor(T,N,A), 
  !.
pdg_arc(group_by(T,_Vs,_C),P,P-N/A) :- 
  functor(T,N,A), 
  !.
pdg_arc(Aggr,P,P-N/A) :- 
  my_aggregate_relation(AF,Arity),
  Aggr =.. [AF,T|Args],
  length([T|Args],Arity),
  functor(T,N,A), 
  !.
pdg_arc(lj(T),P,P-N/A) :- 
  functor(T,N,A), 
  !.
pdg_arc(rj(T),P,P-N/A) :- 
  functor(T,N,A), 
  !.
pdg_arc(fj(T),P,P-N/A) :- 
  functor(T,N,A), 
  !.
% Positive dependency
pdg_arc(T,P,P+N/A) :-      
  functor(T,N,A), !.

rhsnodes([],[]).
rhsnodes([Arc|Arcs],[N/A|Nodes]) :-
  (Arc = P-N/A; Arc = P+N/A),
  rhsnodes(Arcs,Nodes).

display_undefined(RHSNodes,LHSNodes) :-
  my_builtin_preds(BIPreds),
  my_append(LHSNodes,BIPreds,LBIPreds),
  findall(TableName/Arity,my_table('$des',TableName,Arity),TAs),
  my_append(LBIPreds,TAs,Preds),
  my_set_diff(RHSNodes,Preds,UndefPred),
  (UndefPred == [] -> 
   true
   ;
   remove_duplicates_var(UndefPred,RUndefPred),
   my_sort(RUndefPred,SUndefPred),
   write_log_list(['Warning: Undefined predicate(s): ',SUndefPred,nl])).
   
my_builtin_preds(BIPreds) :-
  setof(IR/2,M1^my_infix_relation(IR,M1),IRs),
  setof(IC/2,M2^my_infix_comparison(IC,M2),ICs),
  my_append(IRs,ICs,BIP1),
  setof(OF/Arity,my_outer_join_relation(OF/Arity),OFs),
  my_append(BIP1,OFs,BIP2),
  setof(BP/BPArity,builtin_predicate(BP/BPArity),BPs),
  my_append(BIP2,BPs,BIPreds).

builtin_predicate(is_null/1).
builtin_predicate(is_not_null/1).
builtin_predicate(P/A) :-
  my_aggregate_relation(P,A).

is_null(T) :-
  \+ \+ T='$NULL'(_ID).

is_not_null(T) :-
  T\='$NULL'(_ID).

/*********************************************************************/
/* Building a predicate dependency subgraph: sub_pdg/3               */
/*********************************************************************/
% Given a starting node N and a pdg G, build the subgraph of nodes reachable from N in G

sub_pdg(N,(_Nodes,Arcs),(SNodes,SArcs)) :-
  setof(Path,N^Q^Arcs^reachable(N,Q,Arcs,[],Path),Path),
  concat_lists(Path,G),
  remove_duplicates_var(G,SArcs),
  nodes_in(SArcs,DNodes),
  remove_duplicates_var(DNodes,SNodes).

% reachable(From,To,Arcs,Used,AccUsed)
reachable(F,F,_Arcs,U,U). 
reachable(F,T,Arcs,U,[Arc|U]) :- 
  arc(F,T,Arc,Arcs), my_not(my_member(Arc,U)).
reachable(F,T,Arcs,U,Uo) :- 
  arc(F,R,Arc,Arcs), my_not(my_member(Arc,U)), 
  reachable(R,T,Arcs,[Arc|U],Uo).

arc(F,T,F+T,[F+T|_As]).
arc(F,T,F-T,[F-T|_As]).
arc(F,T,Arc,[_A|As]) :- arc(F,T,Arc,As).

nodes_in([],[]).
nodes_in([Arc|Arcs],[F,T|Nodes]) :-
  (Arc=F+T; Arc=F-T),
  nodes_in(Arcs,Nodes).


/*********************************************************************/
/* Finding a stratification: stratify/3                              */
/*********************************************************************/
% Follows Ullman, but modified for pdg
% Finds a stratification from a given predicate dependency graph, 
% and returns whether it was successful

stratify(S,(N,A),Success) :-
  write_verb(['Info: Computing strata...',nl]),
  assign_1st_stratum(N,FS),
  length(N,Max),
  lfp_recompute_strata(A,FS,S,Max,Success).

assign_1st_stratum([],[]).
assign_1st_stratum([N/A|Ns],[(N/A,1)|SNs]) :-
  assign_1st_stratum(Ns,SNs).

lfp_recompute_strata(A,Si,Sj,Max,Success) :-
  recompute_strata(A,Si,So,Max,false,Change,SSuccess),
  (SSuccess=false -> fail; true),
  (Change=false -> 
    So=Sj, Success=true, !
   ;
    lfp_recompute_strata(A,So,Sj,Max,Success)).
lfp_recompute_strata(_A,S,S,_Max,false).

recompute_strata([],S,S,_Max,C,C,true) :- !. 
recompute_strata([A|As],Si,Sj,Max,Ci,Co,B) :-
  (A = Nt/At-Nf/Af, Add=1 ; A = Nt/At+Nf/Af, Add=0),
  recompute_stratum(Nt/At,Nf/Af,Add,Si,So,Stratum,C1),
  my_or(Ci,C1,C2),
  (Stratum > Max -> fail ; 
   recompute_strata(As,So,Sj,Max,C2,Co,B)).
recompute_strata(_A,S,S,_Max,false). % Non-stratifiable program

recompute_stratum(Nt/At,Nf/Af,Add,Si,Sj,Stratum,Change) :-
  find_stratum(Nt/At,Si,Stt),
  find_stratum(Nf/Af,Si,Stf),
  IStf is Stf+Add,
  my_max(Stt,IStf,Stratum),
  (Stt=Stratum -> Change=false, Si=Sj; Change=true, reassign_stratum(Nt/At,Stratum,Si,Sj)).

find_stratum(N/A,[(N/A,Stratum)|_Ps],Stratum) :- !.
find_stratum(N/A,[_P|Ps],Stratum) :-
  find_stratum(N/A,Ps,Stratum).

reassign_stratum(N/A,Stratum,[(N/A,_St)|Ps],[(N/A,Stratum)|Ps]) :- !.
reassign_stratum(N/A,Stratum,[P|Ps],[P|NPs]) :-
  reassign_stratum(N/A,Stratum,Ps,NPs).


/*********************************************************************/
/* Computing Stratification: compute_stratification/0                */
/*********************************************************************/

compute_stratification :-
  clear_stratification,
  build_pdg(G),
  stratify(S,G,Success),
  assertz(pdg(G)),
  (Success==true -> 
   assertz(strata(S)), !
   ; 
   (assertz(strata([non-stratifiable])), !,
    write_log('Warning: Non stratifiable program.'), nl_log)
  ).


/*********************************************************************/
/* Loading Stratification: load_stratification/2                     */
/*********************************************************************/

load_stratification(S,G) :-
  clear_stratification, 
  assertz(strata(S)),
  assertz(pdg(G)).


/*********************************************************************/
/* Drilling Down Stratification: drilldown_stratification/1          */
/*********************************************************************/

drilldown_stratification(_L) :- %TODO
  compute_stratification.


/*********************************************************************/
/* Rolling Up Stratification: rollup_stratification/1                */
/*********************************************************************/

rollup_stratification(_L) :- %TODO
  compute_stratification.


/*********************************************************************/
/* Reset Stratification: reset_stratification/0                   */
/*********************************************************************/

reset_stratification :- 
  clear_stratification,
  assertz(strata([])),
  assertz(pdg([])).


/*********************************************************************/
/* Clearing Stratification: clear_stratification/0                   */
/*********************************************************************/

clear_stratification :- 
  retractall(strata(_)),
  retractall(pdg(_)).


/*********************************************************************/
/* Save extension table: save_et/1                                   */
/*********************************************************************/

save_et((ESet,CSet,FSet)) :- 
  findall(et(EFact,EIds), (et(EFact,EIds), retractall(et(EFact,EIds))), ESet),
  findall(called(CFact), (called(CFact), retractall(called(CFact))), CSet),
  findall(complete_flag(FFact,CF), (complete_flag(FFact,CF), retractall(complete_flag(FFact,CF))), FSet).


/*********************************************************************/
/* Restoring extension table: restore_et/1                           */
/*********************************************************************/

restore_et((ESet,CSet,FSet)) :- 
  retractall(et(_EFact,_EIds)),
  retractall(called(_CFact)),
  retractall(complete_flag(_FFact,_CF)),
  my_apply(assertz,ESet),
  my_apply(assertz,CSet),
  my_apply(assertz,FSet).


/*********************************************************************/
/* Listing Datalog Rules (Command): list_rules/1                     */
/*********************************************************************/

list_rules(I) :-
  list_rules_wo_number(I,ODLs),
  display_nbr_rules(ODLs).
  
list_rules_wo_number(I,ODLs) :-
  (development(off) ->
    get_source_dlrules(DLs)
    ;
    get_object_dlrules(DLs)),
  store_elapsed_time(computation),
  my_quicksort_pred(DLs,dlrule_compare_asc,ODLs),
  (development(off) ->
    display_source_dlrule_list(ODLs,I)
    ;
    display_dlrule_list(ODLs,I)).


/*********************************************************************/
/* Listing Datalog Rules matching name and arity (Command): list_rules/3  */
/*********************************************************************/

list_rules(N,A,I) :-
  (development(off) ->
    get_source_dlrules(namearity,N/A,DLs)
    ;
    get_object_dlrules(namearity,N/A,DLs)),
  store_elapsed_time(computation),
  my_quicksort_pred(DLs,dlrule_compare_asc,ODLs),
  (development(off) ->
    display_source_dlrule_list(ODLs,I)
    ;
    display_dlrule_list(ODLs,I)),
  display_nbr_rules(ODLs).


/*********************************************************************/
/* Listing Datalog Rules matching a name (Command): list_rules/2     */
/*********************************************************************/

list_rules(N,I) :-
  list_rules_wo_number(N,I,DLs),
  display_nbr_rules(DLs).
  
list_rules_wo_number(N,I,ODLs) :-
  (development(off) ->
    get_source_dlrules(name,N,DLs)
    ;
    get_object_dlrules(name,N,DLs)),
  store_elapsed_time(computation),
  my_quicksort_pred(DLs,dlrule_compare_asc,ODLs),
  (development(off) ->
    display_source_dlrule_list(ODLs,I)
    ;
    display_dlrule_list(ODLs,I)).


/*********************************************************************/
/* Listing Datalog Rules matching a head (Command): list_rules_from_head/2     */
/*********************************************************************/

list_rules_from_head(H,I) :-
  (development(off) ->
    get_source_dlrules(head,H,DLs)
    ;
    get_object_dlrules(head,H,DLs)),
  store_elapsed_time(computation),
  my_quicksort_pred(DLs,dlrule_compare_asc,ODLs),
  (development(off) ->
    display_source_dlrule_list(ODLs,I)
    ;
    display_dlrule_list(ODLs,I)),
  display_nbr_rules(ODLs).


/*********************************************************************/
/* Listing Datalog Rules matching a rule (Command): list_rules_from_rule/2     */
/*********************************************************************/

list_rules_from_rule(R,I) :-
  (development(off) ->
    get_source_dlrules(rule,R,DLs)
    ;
    get_object_dlrules(rule,R,DLs)),
  store_elapsed_time(computation),
  my_quicksort_pred(DLs,dlrule_compare_asc,ODLs),
  (development(off) ->
    display_source_dlrule_list(ODLs,I)
    ;
    display_dlrule_list(ODLs,I)),
  display_nbr_rules(ODLs).


/*********************************************************************/
/* Listing Extension Table Contents (Command): list_et               */
/*********************************************************************/

list_et :- 
  write_log_list(['Answers:',nl]),
  list_answers,
  write_log_list(['Calls:',nl]),
  list_calls.

list_answers :-
  bagof(Fact, Ids^et(Fact,Ids), NBag),
  !,
  (development(on) ->
   Bag=NBag
   ;
   hide_nulls(NBag,Bag)),
  display_ordered_set(Bag,_OBag),
  display_nbr_of_tuples(Bag,'in the answer table').
list_answers :-
  display_ordered_set([],_OBag).

list_calls :-
  bagof(Fact, called(Fact), NBag),
  !,
  (development(on) ->
   Bag=NBag
   ;
   hide_nulls(NBag,Bag)),
  display_ordered_set(Bag,_OBag),
  display_nbr_of_tuples(Bag,'in the call table').
list_calls :- 
  display_ordered_set([],_OBag).


/*********************************************************************/
/* Listing Extension Table Contents matching a Pattern (Command): list_et/1 */
/*********************************************************************/
% Negative information regarding pattern must be explicitly recalled

list_et(N/A) :- 
  !, 
  write_log_list(['Answers:', nl]), 
  list_answers(N/A),
  write_log_list(['Calls:', nl]), 
  list_calls(N/A).
list_et(N) :- 
  list_et(N/_A).

list_answers(N/A) :- 
  get_et_facts(N/A,NBag),
  !,
  (development(on) ->
   Bag=NBag
   ;
   hide_nulls(NBag,Bag)),
  display_ordered_set(Bag,_OBag),
  display_nbr_of_tuples(Bag,'in the answer table').
list_answers(_N/_A) :- 
  display_ordered_set([],_OBag).

list_calls(N/A) :- 
  bagof(Fact,
        F^N^A^(
         (called(Fact), functor(Fact,N,A)); 
         (called(Fact), (Fact=not(F)), functor(F,N,A)) 
        ),
        NBag), !,
  (development(on) ->
   Bag=NBag
   ;
   hide_nulls(NBag,Bag)),
  display_ordered_set(Bag,_OBag),
  display_nbr_of_tuples(Bag,'in the call table').
list_calls(_N/_A) :- 
  display_ordered_set([],_OBag).

get_et_facts(N/A,Facts) :-  
  (var(A) -> Exists=Ids^F^N^A ; Exists=Ids^F^N),
  bagof(Fact,
        Exists^(
         (et(Fact,Ids), functor(Fact,N,A)); 
         (et(Fact,Ids), (Fact=not(F)), functor(F,N,A)) 
        ),
        Facts).  

/*********************************************************************/
/* Listing Builtins (Command): list_builtins                         */
/*********************************************************************/

list_builtins :-
  write_log('Comparison Operators:'), nl_log,
  (my_infix_comparison(OP,M),
   write_log('  '),
   write_tab_log(OP,5), 
   write_log(' '), 
   write_log(M), 
   nl_log, 
   fail
   ; 
   true),
  write_log('Arithmetic Operators:'), 
  nl_log,
  (unary_operator(OP,_,M), 
   write_log('  '), 
   write_tab_log(OP,5), 
   write_log(' '), 
   write_log(M), 
   nl_log, 
   fail
   ; 
   true),
  (my_infix_arithmetic(OP,_,_,M,_),
   write_log('  '), 
   write_tab_log(OP,5), 
   write_log(' '), 
   write_log(M), 
   nl_log, 
   fail
   ;
   true),
  write_log('Arithmetic Functions:'), 
  nl_log,
  (arithmetic_function(F,_,M,arithmetic,_,_),
   write_log('  '), 
   write_tab_log(F,21), 
   write_log(' '), 
   write_log(M), 
   nl_log, 
   fail
   ; 
   true),
  write_log('Aggregate Functions:'), 
  nl_log,
  (arithmetic_function(F,_,M,aggregate,_,_),
   write_log('  '), 
   write_tab_log(F,5), 
   write_log(' '), 
   write_log(M), 
   nl_log, 
   fail
   ; 
   true),
  write_log('Arithmetic Constants:'), 
  nl_log,
  (arithmetic_constant(_,C,M),
   write_log('  '), 
   write_tab_log(C,5), 
   write_log(' '), 
   write_log(M), 
   nl_log, 
   fail
   ; 
   true),
  write_log('Predicates:'), 
  nl_log,
  (my_infix_relation(R,M),
   write_log('  '), 
   write_tab_log(R,16), 
   write_log(' '), 
   write_log(M), 
   nl_log, 
   fail
   ; 
   true),
  (my_builtin_relation(R,A,M),
   write_log('  '), 
   write_tab_log(R/A,16), 
   write_log(' '), 
   write_log(M), 
   nl_log, 
   fail
   ; 
   true),
  write_log('  not / 1          Stratified negation'), 
  nl_log,
  write_log('  answer           Reserved word for the outcome relation of an automatic temporary view'), 
  nl_log.


/*********************************************************************/
/* Asserting Datalog Rules: assert_rules                             */
/*********************************************************************/

assert_rules([],_Simplify,_Error).
assert_rules([RNVs|RNVss],Simplify,Error) :-
  assert_rule(RNVs,Simplify,Error),
  assert_rules(RNVss,Simplify,Error).

% Assert a rule which may be needed to be translated
% For /assert command and SQL CREATE VIEW: Without information regarding lines or file ids
assert_rule((Rule,NVs),Simplify,Error) :-
  my_datetime(DT),
  assert_rule((Rule,NVs),_RNVss,[],asserted(DT),assert,Simplify,Error).

% For consulting programs: With information regarding lines and file ids
assert_rule((Rule,NVs),RNVss,Ls,Fid,Action,Simplify,Error) :-
  preprocess(Rule,_SRules,Rules,[],NVs,SNVs,Action,rule,Causes,Simplify,_Error),
  my_append(NVs,SNVs,ANVs),
  build_datalog_rules(Rule,Rules,ANVs,Ls,Fid,DLs),
  dlrule_to_ruleNV_list(DLs,RNVss),
  ((my_member(simplification,Causes), 
    my_not(my_member(safety,Causes)), 
    my_not(language(sql))) -> 
     write_log_list(['Info: This rule has been translated into:',nl]),
     display_ruleNVs_list(RNVss,2)
    ;
     true),
  assert_list(DLs,Error).

build_datalog_rules(Rule,[],NVs,Ls,Fid,[DL]) :-
  build_datalog_rules(Rule,[Rule],NVs,Ls,Fid,[DL]).
build_datalog_rules(Rule,[Rule],NVs,Ls,Fid,[DL]) :-
  build_datalog_rule(Rule,NVs,_NNVs,Ls,Fid,source,DL).
build_datalog_rules(':-'(H,B),[TRule|TRules],NVs,Ls,Fid,[datalog(TRule,NVs,_Rid,Ls,Fid,compilation(H,B,DLs))|DLs]) :-
  build_datalog_rule_list(TRules,NVs,_NNVs,Ls,Fid,DLs).
  
build_datalog_rule_list([],NVs,NVs,_Ls,_Fid,[]).
build_datalog_rule_list([Rule|Rules],NVs,[NNVs|BNVs],Ls,Fid,[DL|DLs]) :-
  build_datalog_rule(Rule,NVs,NNVs,Ls,Fid,compiled,DL),
  build_datalog_rule_list(Rules,NVs,BNVs,Ls,Fid,DLs).

build_datalog_rule(Rule,NVs,NNVs,Ls,Fid,Kind,datalog(Rule,DLNVs,_Rid,Ls,Fid,Kind)) :-
  my_term_variables(Rule,TVs),
  relevant_var_names(NVs,TVs,RNVs),
  assign_new_var_names(TVs,RNVs,NNVs),
  my_append(RNVs,NNVs,DLNVs).

relevant_var_names([],_TVs,[]).
relevant_var_names([N=V|NVs],TVs,[N=V|RNVs]) :-
  my_member_var(V,TVs),
  !,
  relevant_var_names(NVs,TVs,RNVs).  
relevant_var_names([_NV|NVs],TVs,RNVs) :-
  relevant_var_names(NVs,TVs,RNVs).  
  
assign_new_var_names([],_NVs,[]).
assign_new_var_names([V|Vs],NVs,NNVs) :-
  find_var_name(V,_N,NVs),
  !,
  assign_new_var_names(Vs,NVs,NNVs).  
assign_new_var_names([V|Vs],NVs,[N=V|NNVs]) :-
  name_var(NVs,V,N),
  assign_new_var_names(Vs,[N=V|NVs],NNVs).  
  

/*********************************************************************/
/* Retracting Datalog Rules: retract_rules                           */
/* Deletes the given list of object rules                            */ 
/*********************************************************************/

retract_rule_list(Rs,Error) :-
  (nonvar(Rs) -> retract_g_rule_list(Rs,Error) ; true).

retract_g_rule_list([],_Error).
retract_g_rule_list([R|Rs],Error) :-
  retract_rule(R,Error),
  retract_g_rule_list(Rs,Error).

retract_rule((R,_Vs),Error) :-
  retract_rule(R,Error).
retract_rule(R,Error) :-
  (datalog(R,_,_,_,_,_) ->
   (getFileIdsRule(R,Fids) -> retract_fileids(Fids) ; true),
   retract(datalog(R,_,_,_,_,_))
   ;
   (write_log('Warning: Cannot retract.'),
    nl_log,
    Error=true)
  ).

/*********************************************************************/
/* Retracting Datalog Rules: retract_dlrules                         */
/* Deletes the given list of object dlrules                          */ 
/*********************************************************************/

retract_dlrule_list([],_Error).
retract_dlrule_list([DL|DLs],Error) :-
  retract_dlrule(DL,Error),
  retract_dlrule_list(DLs,Error).

retract_dlrule(datalog(R,NVs,Rid,Ls,Fid,C),Error) :-
  (datalog(R,NVs,Rid,Ls,Fid,C) ->
   (retract(datalog(R,NVs,Rid,Ls,Fid,C)), 
    (R=':-'(H,_B) -> true ; R=H),
    functor(H,F,A),
    functor(OR,F,A),
    (datalog(OR,_,_,_,_,_) -> true ; retract_fileids([Fid])))
   ;
   (write_log('Warning: Cannot retract.'),
    nl_log,
    Error=true)
  ).


/*********************************************************************/
/* Abolishing Datalog Rules (Command): abolishDL                     */
/*********************************************************************/

abolishDL :- 
  retractall(datalog(_,_,_,_,_,_)), 
  retractall(rule_id(_)),
  enable_rdb_datasource.
  
% abolishDL :- 
%   datalog(R,_,_,_,_,_), 
%   retract(datalog(R,_,_,_,_,_)), 
%   fail.
% abolishDL.


/*********************************************************************/
/* Abolishing Extension Table: abolishET                             */
/*********************************************************************/

abolishET :- 
  retractall(et(_F,_Ids)), 
  retractall(called(_C)),
  retractall(complete_flag(_G,_CF)).


/*********************************************************************/
/* Abolishing File Table: abolishFT                                  */
/*********************************************************************/

abolishFT :- 
  retractall(filet(_F,_Fid)).


/*********************************************************************/
/* Get File Ids for Rule Pattern: getFileIdsRule/2                   */
/*********************************************************************/

getFileIdsRule(R,Fids) :- 
  copy_term(R,CR),
  setof(Fid,NVs^Rid^Ls^Rs^(datalog(CR,NVs,Rid,Ls,Fid,Rs)),Fids).


/*********************************************************************/
/* Get File Ids for Head Pattern: getFileIdsHead/2                   */
/*********************************************************************/

getFileIdsHead(H,Fids) :- 
  copy_term(H,CH),
  setof(Fid,B^NVs^Rid^Ls^Rs^
          ((datalog(':-'(CH,B),NVs,Rid,Ls,Fid,Rs); 
            datalog(CH,NVs,Rid,Ls,Fid,Rs))),
        Fids).


/*********************************************************************/
/* Get File Ids: getFileIds/2                                        */
/*********************************************************************/

getFileIds(N/A,Fids) :- 
  !,
  setof(Fid,H^B^NVs^Rid^Ls^Rs^
          ((datalog(':-'(H,B),NVs,Rid,Ls,Fid,Rs); 
            datalog(H,NVs,Rid,Ls,Fid,Rs)),functor(H,N,A)),
        Fids).

/*********************************************************************/
/* Get File Ids: getFileIds/2                                        */
/*********************************************************************/

getFileIds(N,Fids) :- 
  setof(Fid,H^B^NVs^Rid^Ls^A^Rs^
          ((datalog(':-'(H,B),NVs,Rid,Ls,Fid,Rs); 
            datalog(H,NVs,Rid,Ls,Fid,Rs)), functor(H,N,A)),
        Fids).

  
/*********************************************************************/
/* Retracting File Table Entries: retract_fileids/2                  */
/*********************************************************************/

retract_fileids([]).
retract_fileids([Fid|Fids]) :-
  (datalog(_R,_Vs,_Rid,_Ls,Fid,_Rs) ->
   true
   ;
   retractall(filet(_,Fid))),
  retract_fileids(Fids).

  
/*********************************************************************/
/* Displaying                                                        */
/*********************************************************************/

% For ODBC RDB connection:
display_solutions(NRows) :-
  (development(on) ->
    Rows=NRows
    ;
    hide_nulls(NRows,Rows)),
  display_bag(Rows),
  display_nbr_of_tuples(Rows,computed).

% For Datalog DDB
display_solutions(G,U) :-
  et_entries(G,NL),
  (development(on) ->
    L=NL
    ;
    hide_nulls(NL,L)),
  display_ordered_set(L,OL),
  display_nbr_of_tuples(OL,computed),
  display_undefined(U).

display_nbr_of_tuples(T,M) :-
  (integer(T) -> N=T ; length(T,N)),
  (N == 1 -> R = tuple ; R = tuples),
  write_log_list(['Info: ',N,' ',R,' ',M,'.',nl]).

et_entries(S,L) :-
  bagof(S,Ids^et(S,Ids),L),
 !.
et_entries(_S,[]).

display_undefined([]) :- !.
display_undefined(U) :-
  write_log('Undefined:'), nl_log,
  (development(on) ->
    HU=U
    ;
    hide_nulls(U,HU)),
  display_ordered_set(HU,_OHU),
  display_nbr_of_tuples(U,undefined).
  
display_bag(L) :-
  write_log('{'),
  my_display_list(L),
  write_log_list([nl,'}',nl]).

display_ordered_set(L,OL) :-
  write_log('{'),
  my_sort(L,OL),
  my_display_list(OL),
  write_log_list([nl,'}',nl]).

my_display_list([]).
my_display_list([X]) :-
  write_log_list([nl,'  ']),
  write_log_fresh_NVs(X). 
my_display_list([X,Y|Xs]) :-
  write_log_list([nl,'  ']),
  write_log_fresh_NVs(X),
  write_log(','), 
  my_display_list([Y|Xs]).

display_rule_info(Ls,Fid) :-
	(filet(F,Fid)->
		write_log_list(['           File : ',F,nl,
		                '           Lines: ',Ls])
	 ;
		Fid=asserted((Y,M,D,H,Mi,S)),
		write_log_list(['           Asserted at ',H,':',Mi,':',S,' on ',M,'-',D,'-',Y,'.'])
	).

% Displaying number of rules
display_nbr_rules(L) :-
  length(L,N),
  (N==1 -> S = ' listed.' ; S = 's listed.'),
  write_log_list([nl,'Info: ',N,' rule',S,nl]).
  

% Displaying Datalog rules
display_dlrule_list(DLs) :-
  display_dlrule_list(DLs,0).

display_source_dlrule_list(DLs,I) :-
  source_dlrule_to_ruleNV_list(DLs,RNVss),
  display_ruleNVs_list(RNVss,I).

display_dlrule_list(DLs,I) :-
  dlrule_to_ruleNV_list(DLs,RNVss),
  display_ruleNVs_list(RNVss,I).

display_ruleNVs_list(RNVss,I) :-
  (development(on) ->
    DRNVss=RNVss
    ;
    hide_nulls(RNVss,DRNVss)),
  write_rulesNVs_list(DRNVss,I).

% Replaces all occurrences of '$NULL'(Cte) by the constant null in a term T
hide_nulls(V,V) :- 
   var(V),
   !.
hide_nulls('$NULL'(_ID),null) :- 
  !.
hide_nulls(T,T) :- 
  (number(T) ; atom(T)),
  !.
hide_nulls(C,RC) :- 
  C =.. [F|As],
  !, 
  hide_nulls_list(As,RAs),
  RC =.. [F|RAs].

hide_nulls_list([],[]) :-
  !.
hide_nulls_list([T|Ts],[RT|RTs]) :-
  !, 
  hide_nulls(T,RT), 
  hide_nulls_list(Ts,RTs).  
  
write_rulesNVs_list([],_I).
write_rulesNVs_list([RNVs|RNVss],I) :-
  write_datalog_rule(RNVs,I),
  nl_log,
  write_rulesNVs_list(RNVss,I).

% Rules/Facts, no pretty print
write_datalog_rule((':-'(Head,Body),NVs),I) :-
  pretty_print(off),
  !,
  write_indent(I),
  write_with_NVs(Head,NVs), 
  write_log(' :- '),
  write_with_NVs(Body,NVs),
  write_log('.').
% Rules, pretty-print
write_datalog_rule((':-'(Head,Body),NVs),I) :-
  !,
  write_indent(I),
  write_with_NVs(Head,NVs), 
  write_log(' :-'),
  nl_log,
  I1 is I+2,
  write_goals_with_NVs(Body,NVs,I1),
  write_log('.').
% Facts, pretty-print
write_datalog_rule((F,NVs),I) :-
  write_indent(I),
  write_with_NVs(F,NVs),
  write_log('.').

%write_goals_with_NVs([],_NVs,_I).
write_goals_with_NVs((Goal,Goals),NVs,I) :-
  !,
  write_indent(I),
  write_with_NVs(Goal,NVs),
  write_log(','),
  nl_log,
  write_goals_with_NVs(Goals,NVs,I).
write_goals_with_NVs((Goal;Goals),NVs,I) :-
  !,
  write_indent(I),
  write_with_NVs(Goal,NVs),
  nl_log,
  write_indent(I),
  write_log(';'),
  nl_log,
  write_goals_with_NVs(Goals,NVs,I).
write_goals_with_NVs(Goal,NVs,I) :-
  write_indent(I),
  write_with_NVs(Goal,NVs).


/*********************************************************************/
/* Consulting a list of Datalog programs (Command): consultDLlist    */
/*********************************************************************/

consultDLlist([],Success,Success).
consultDLlist([File|Files],Si,So) :-
  consultDL(File,S),
  my_or(S,Si,S1),
  consultDLlist(Files,S1,So).

/*********************************************************************/
/* Consulting a Datalog Program (Command): consultDL                 */
/*********************************************************************/

consultDL(F,true) :-
  current_input(OldInput),
  try_open(F,CFN,St),       % Try to open file F, which has the complete file name CFN
  assertz(consult(CFN,St)),
  my_new_file_id(CFN,Fid),
  write_verb(['Info: Consulting ',F,'...', nl]),
  repeat,
    my_read(T,NVs,Ls),      % Read a term, along with its variable names and line numbers
    process_term(T,RNVss,NVs,Ls,Fid,consult,Error), % Process it, i.e., parse and assert
    ((verbose(on) ; Error == true) -> 
     (development(on) ->
       display_ruleNVs_list(RNVss,2)  % Lists the compiled rules
       ;
       display_ruleNVs_list([(T,NVs)],2)) % Lists the source rule
     ; 
     true), 
    (Error == true ->
      display_rule_info(Ls,Fid),
      nl_log
     ;
      true),
    T == end_of_file,       % Loop back if not at end of file
    !,
  close(St),                % Close the file
  retractall(consult(_,_)),
  set_input(OldInput).
consultDL(F,false) :-
  write_log_list(['Error: Reading file ''',F,'''.', nl]),
  (consult(_,St) -> close(St); true).

write_term_list([],_I,_NVs).
write_term_list([T|Ts],I,NVs) :-
  my_spaces(I,S),
  write_log(S),
  write_with_NVs(T,NVs), 
  write_log('.'), 
  nl_log,
  write_term_list(Ts,I,NVs).

process_term(end_of_file,[(end_of_file,[])],_NVs,_Ls,_Fid,_Action,_Error) :- !.
process_term(NT,RNVss,NVs,Ls,Fid,Action,Error) :-
  (NT=':-'(NHead,NBody),
   T=':-'(Head,Body), !
   ;
   NT=NHead,
   T=Head), 
  my_term_to_string(NHead,SHead,NVs),
  (parse_head(Head,[],HNVs,SHead,[]) -> 
   true
   ; 
   write_log_list(['Error: Syntax error in rule head.',nl]), 
   Error=true),
  (nonvar(NBody)->
    ((my_term_to_string(NBody,SBody,NVs),
      parse_body(Body,HNVs,BNVs,SBody,[]) ->
    true
     ; 
     write_log_list(['Error: Syntax error in rule body.',nl]), 
     Error=true))
    ; 
    BNVs=HNVs),
%  The following yields to non-termination when safety translations are enabled and syntax error is detected
%  ((var(Error);safe(on)) -> 
  (var(Error) -> 
% nulls were not translated to '$NULL' with the following:
%     assert_rule((NT,BNVs),RNVss,Ls,Fid,Action,_Simplify,Error)
     my_append(NVs,BNVs,NNVs),
     assert_rule((T,NNVs),RNVss,Ls,Fid,Action,_Simplify,Error)
    ; 
     RNVss=[(NT,NVs)]), 
  !.

my_new_file_id(F,Fid) :-
  filet(F,Fid), !,
  write_verb(['Warning: Reloading an already loaded program.',nl,
              '         References to source program may have changed.',nl]).
my_new_file_id(F,Fid) :-
  my_max_fid(MaxFid),
  Fid is MaxFid+1,
  assertz(filet(F,Fid)).

my_max_fid(MaxFid) :-
  setof(Fid,F^filet(F,Fid),Fids),
  my_list_max(Fids,1,MaxFid), 
  !.
my_max_fid(0).
  
my_list_max([],M,M).
my_list_max([X|Xs],M,Max) :- 
  (X>M ->
   my_list_max(Xs,X,Max)
   ;
   my_list_max(Xs,M,Max)
  ).


/*********************************************************************/
/* Trying to safe a rule/goal/view/autoview: make_safe/4             */
/*********************************************************************/

% Rules/Views/Autoviews:
make_safe(':-'(IHN,BN),IArgs,NVs,Action,Object,':-'(OH,OB),Transformed,Error) :-
  !,
  (Object==autoview -> (HN=autoview , OH=IHN)
   ;
   (Object==query -> (HN=query , OH=IHN)
    ;
    (HN=IHN, OH=H))),
  concrete_nulls((HN,BN),(H,B),_),
  copy_term((H,B),(CH,CB)),
  make_ground_args(CH,IArgs),
  mark_safe_NVs(CB,H,B,NVs,[],_,Object,Action,Err1),
  (Object==query -> Error=Err1 ; true),
  % Try to reorder when:
  ((my_ground((CH,CB));           % Either all variables are safe,
   (my_ground(CB),Object\=view);  % or body variables are safe,
   (my_term_variables(CB,V1s),    % or only head variables are unsafe. May be safe at run-time
    my_term_variables(CH,V2s),
    same_variables(V1s,V2s)))
    ->
    (safe(on) -> 
      reorder_goals(B,OB) % Goals can actually be reordered to ensure that, upon execution, 
                          % demanded arguments become ground before the call,
                          % but this modifies performance for some cases:
                          % p(X) :- X>1,q(X) could be translated into p(X) :- q(X),X>1
                          % In the first case, q(X) is not actually computed for X<=1
     ; 
      OB=B),
    ((safe(on), B \= OB) ->
     Transformed = true,
     safety_warnings(on),
     write_log_list(['Info: For allowing a (possible) safe computation, the ',Object,':',nl]),
     ((Object=rule;Object=view) ->
       display_ruleNVs_list([(':-'(H,B),NVs)],2)
       ;
       display_ruleNVs_list([(B,NVs)],2)),
     write_log_list(['has been translated into:',nl]),
     ((Object=rule;Object=view) ->
       display_ruleNVs_list([(':-'(H,OB),NVs)],2)
       ;
       display_ruleNVs_list([(OB,NVs)],2))
    ;
    (safe(off) -> Error=Err1; true)
    )
   ;
   Error=true,
   (Object==query -> 
     display_unsafe(IHN,B,IHN,B,NVs,Action,Object)
    ; 
     Error=true),
   OB=B
   ),
   (my_ground(CH) ->
    true
    ;
    (var(Error) -> Error=true; true),
    (%(H,B)=(CH,CB),
     display_unsafe(H,B,CH,CB,NVs,Action,Object),
     fail
     ;
     true
    )
   ).
% Goals:   
make_safe(HN,_IArgs,NVs,Action,Object,H,_,Error) :-
  concrete_nulls(HN,H,_),
  (my_ground(H) ->
   true;                                             % Contains no variables
   Error=true,
   display_unsafe(H,true,H,true,NVs,Action,Object)). % Unsafe: nonground fact

make_safe_list([],[],_,_,_,_,_,Error) :-
  (var(Error) -> Error=false ; true).
make_safe_list([R|Rs],[SR|SRs],IArgs,NVs,Action,Object,Transformed,Error) :-
  make_safe(R,IArgs,NVs,Action,Object,SR,Transformed,Error),
  make_safe_list(Rs,SRs,IArgs,NVs,Action,Object,Transformed,Error).
      
same_variables(V1s,V2s) :-
  length(V1s,L),
  length(V2s,L),
  my_set_diff(V1s,V2s,[]).
  
% (Goals (marked), Head, Body, Variables in negated atoms, Names of variables)
mark_safe_NVs((G,Gs),H,(B,Bs),NVs,PVi,PVo,Object,Action,Error) :-
  !,
  mark_safe_NVs(G,H,B,NVs,PVi,PVo1,Object,Action,E1),
  mark_safe_NVs(Gs,H,Bs,NVs,PVo1,PVo,Object,Action,E2),
  my_u_or(E1,E2,Error).
mark_safe_NVs('='(A,A),_,_,_,PVi,PVo,_,_,_Error) :- 
  !,
  (my_ground(A) ->
   make_ground_pending(PVi,PVo)
   ;
   PVo=PVi).
mark_safe_NVs(is(X,Y),H,B,NVs,PVi,PVo,Object,_,Error) :- 
  !,
  (my_ground(Y) -> 
   make_ground(X), % X gets bound when Y does
   make_ground_pending(PVi,PVo)
   %,Error=false
   ; 
   B=is(_,YB),     % If Y is not safe, 'is' may or will raise an exception
   my_term_variables(Y,Vs),
   PVo=[(X,Vs)|PVi],
   display_computation_warning(B,Y,H,YB,NVs,Object,Error)
  ).
mark_safe_NVs(G,H,B,NVs,PVi,PVo,Object,Action,Error) :-
  functor(G,P,A),
  (is_non_demanded_predicate(P/A) ->
   make_ground(G),   % Defined predicate 
   make_ground_pending(PVi,PVo)
   %,Error=false       % (assumed safe, but actually it may not;
                     %  a global analysis should be performed),
                     % all variables are marked as safe
   ;
   (my_ground(G) ->  % Built-in (including negation)
    true % Error=false      % Nothing to do if all variables are safe (ground goal)
    ;
    (P/A == not/1 -> % Deciding whether not or other built-in
                     % If negation, the rule cannot be computed
     display_neg_error(G,H,B,NVs,Action),
     Error=true
     ;               % If built-in, display computation warning
     display_computation_warning(B,G,H,B,NVs,Object,Error)
    )
   ),
   PVo=PVi
  ).

make_ground_pending([],[]).
make_ground_pending([(X,Vs)|PVi],PVo) :-
  my_ground(Vs),
  !,
  make_ground(X),
  make_ground_pending(PVi,PVo).
make_ground_pending([(X,Vs)|PVi],[(X,Vs)|PVo]) :-
  make_ground_pending(PVi,PVo).

reorder_goals(B,OB) :-
  my_list_to_tuple(Bs,B),
  copy_term(Bs,CBs),
  concrete_nulls(CBs,GBs,_Grounded),
  reorder_goals_by_safety(Bs,GBs,[],[],SOBs),
  reorder_goals_by_efficiency(SOBs,EOBs),
  my_list_to_tuple(EOBs,OB).
  
reorder_goals_by_efficiency(Gs,OGs) :-
  (false ->
    reorder_goals_by_efficiency(Gs,FGs,[],PGs),
    my_append(PGs,FGs,OGs)
    ;
   OGs=Gs).

reorder_goals_by_efficiency_rule_list([],[]).
reorder_goals_by_efficiency_rule_list([R|Rs],[OR|ORs]) :-
  R=':-'(H,B),
  !,
  OR=':-'(H,OB),
  my_list_to_tuple(Bs,B),
  reorder_goals_by_efficiency(Bs,OBs),
  my_list_to_tuple(OBs,OB),
  reorder_goals_by_efficiency_rule_list(Rs,ORs).
reorder_goals_by_efficiency_rule_list([R|Rs],[R|ORs]) :-
  reorder_goals_by_efficiency_rule_list(Rs,ORs).

% Push equalities to the left
reorder_goals_by_efficiency([],[],Gs,Gs).
reorder_goals_by_efficiency([G|As],Bs,IGs,OGs) :-
  G=..['='|_],
  !,
  reorder_goals_by_efficiency(As,Bs,[G|IGs],OGs).
reorder_goals_by_efficiency([G|As],[G|Bs],IGs,OGs) :-
  reorder_goals_by_efficiency(As,Bs,IGs,OGs).

reorder_goals_by_safety([],[],[],[],[]) :-
  !.
reorder_goals_by_safety([B],[_CB],[],[],[B]) :-
  !.
reorder_goals_by_safety([],[],PGs,CPGs,PGs) :- % Give up: No way to find a safe order
  get_safe_goals(PGs,CPGs,_PG1s,_CPG1s,SGs),
  SGs==[],
  !.
reorder_goals_by_safety([],[],PGs,CPGs,OBs) :-
  !,
  reorder_goals_by_safety(PGs,CPGs,[],[],OBs).
reorder_goals_by_safety(Bs,CBs,PGs,CPGs,OBs) :-
  get_safe_goals(PGs,CPGs,PG1s,CPG1s,SGs),
  SGs\==[],
  !,
  my_append(SGs,VGs,OBs),
  reorder_goals_by_safety(Bs,CBs,PG1s,CPG1s,VGs).
reorder_goals_by_safety([B|Bs],[CB|CBs],PGs,CPGs,[B|OBs]) :-
  functor(B,P,A),
  (is_non_demanded_predicate(P/A),
   make_ground(CB)
   ; 
   P/A = ('=')/2,
   (my_ground(CB) -> true ; call(CB))),
  !,
  reorder_goals_by_safety(Bs,CBs,PGs,CPGs,OBs).
reorder_goals_by_safety([BI|Bs],[CBI|CBs],PGs,CPGs,OBs) :-
  !,
  (CBI=not(A) -> 
   BG=A
   ;
   (CBI=is(X,Y) ->
    BG=Y
    ;
    BG=BI)),
  (my_ground(BG) ->
   (CBI=is(X,Y) -> 
    make_ground(X)
    ;
    true),
   OBs=[BI|OB1s],
   reorder_goals_by_safety(Bs,CBs,PGs,CPGs,OB1s)
   ;
   reorder_goals_by_safety(Bs,CBs,[BI|PGs],[CBI|CPGs],OBs)).
reorder_goals_by_safety(Bs,_CBs,[],[],Bs) :-
  !.

get_safe_goals([],[],[],[],[]).
get_safe_goals([G|Gs],[CG|CGs],PGs,CPGs,[G|SGs]) :-
  my_ground(CG),
  !,
  get_safe_goals(Gs,CGs,PGs,CPGs,SGs).
get_safe_goals([G|Gs],[CG|CGs],[G|PGs],[CG|CPGs],SGs) :-
  get_safe_goals(Gs,CGs,PGs,CPGs,SGs).

display_neg_error(G,H,B,NVs,Action) :-
  (write_log('Error: '),
   write_with_NVs(B,NVs),
   write_log_list([' might not be correctly computed because of the unrestricted variable(s):',nl]),
   B=G,
   my_term_variables(B,Vs),
   write_log('  '),
   write_with_NVs_list(Vs,NVs), 
   fail
  ;
   ((Action==consult,verbose(off)) -> 
    write_log(' in rule: '), 
    write_with_NVs(':-'(H,B),NVs), nl_log
    ;
    nl_log)
  ).

is_non_demanded_predicate(group_by/3) :-
  !.
is_non_demanded_predicate(AF/Arity) :-
  my_aggregate_relation(AF,Arity),
  !.
is_non_demanded_predicate(AF/Arity) :-
  my_builtin_relation(AF,Arity,_M),
  !.
is_non_demanded_predicate(Pred/Arity) :-
  my_outer_join_relation(Pred/Arity),
  !.
is_non_demanded_predicate(Pred/Arity) :-
  Pred/Arity \== not/1,
  my_builtin_preds(BIPreds),
  my_not(my_member(Pred/Arity,BIPreds)).

display_unsafe(_H,_B,_CH,_CB,_NVs,_Action,_Object) :-
  safety_warnings(off),
  !.
display_unsafe(H,B,CH,CB,NVs,Action,Object) :-
  my_term_variables(CH,HVs),
  collect_neg_NVs(CB,BVs),
  my_append(HVs,BVs,TVs),
  remove_duplicates_var(TVs,NNVs),
  (NVs==[] ->
   true;
   write_log('Warning: '),
   (Action == consult -> 
    M='Next '
    ;
    M='This '
   ),
   write_log_list([M,Object,' is unsafe because of variable(s):',nl,'  ']),
   (CH=H,
    CB=B,
    write_with_NVs_list(NNVs,NVs),
    fail
    ;
    true),
   write_log_list([nl])),
  (Action == consult, verbose(off) ->
   display_ruleNVs_list([(':-'(H,B),NVs)],0)
   ;
   true).

collect_neg_NVs((G,Gs),Vs) :-
  !,
  collect_neg_NVs(G,GVs),
  collect_neg_NVs(Gs,GsVs),
  my_append(GVs,GsVs,Vs).
collect_neg_NVs(not(P),Vs) :-
  !, 
  my_term_variables(P,Vs).
collect_neg_NVs(_,[]).
   
% display_computation_warning(Reference literal, copied part of the reference literal, head, original part of the reference literal, variable names)
% Ex: (X is Y, _Y, fib(N,M), Y, ['Y'=Y|...])
% Ex: (X < Y, _X < _Y, fib(N,M), X < Y, ['Y'=Y|...])
display_computation_warning(B,SG,H,SB,NVs,Object,_Error) :-
  my_term_to_string(B,S,NVs),
  SB=SG,
  my_term_variables(H,HVs),
  my_term_variables(SB,BVs),
  ((my_set_diff(BVs,HVs,[]),Object\=view) -> 
   I='Warning: ', M=' may raise a computing exception if non-ground at run-time.'
   ; 
   I='Error: ', M=' will raise a computing exception at run-time.',
   assertz(error)
  ),
  (safety_warnings(on) ->
    write_log(I),
    write_string_log(S),
    write_log_list([M,nl])
   ;
   true),
  fail.
display_computation_warning(_,_,_,_,_,_,Error) :-
  (error -> 
   Error=true,
   retractall(error)
   ;
   Error=false). 
  
/*********************************************************************/
/* Finding Datalog Rules matching a pattern: get_rules/3             */
/*********************************************************************/

get_rules(head,H,L) :-
  (setof((R,NVs),H^B^Rid^Ls^Fid^Rs^
          ((R=H;R=':-'(H,B)),datalog(R,NVs,Rid,Ls,Fid,Rs)),L) ->
   true
   ;
   L=[]).
get_rules(predname,N,L) :-
  (setof((R,NVs), H^B^Rid^Ls^Fid^A^Rs^
          ((R=':-'(H,B);R=H),datalog(R,NVs,Rid,Ls,Fid,Rs),
          functor(H,N,A),N\==(':-')),L) ->
   true
   ;
   L=[]).
get_rules(namearity,N/A,L) :-
  (setof((R,NVs),H^B^Rid^Ls^Fid^N^A^Rs^
          ((R=H;R=':-'(H,B)),
          functor(H,N,A),
          datalog(R,NVs,Rid,Ls,Fid,Rs)),L) ->
   true
   ;
   L=[]).


/*********************************************************************/
/* Auxiliary Predicates                                              */
/*********************************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disable_safety_warnings(SW) :-
  retract(safety_warnings(SW)),
  assertz(safety_warnings(off)).

restore_safety_warnings(SW) :-
  retractall(safety_warnings(_)),
  assertz(safety_warnings(SW)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List/Set processing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% concat_lists(+ListOfLists,-List) Appends a list of lists 
%   and returns the flattened list

concat_lists([],[]).
concat_lists([[]|R],S) :-
  concat_lists(R,S).
concat_lists([[C|R1]|R2],[C|S]) :-
  concat_lists([R1|R2],S).

% Appending two lists
my_append([],X,X).
my_append([X|Xs],Y,[X|Zs]) :-
  my_append(Xs,Y,Zs).

% Appending two lists for finding substrings
my_appendfind([],X,X) :-
  !.
my_appendfind([X|Xs],Y,[X|Zs]) :-
  my_appendfind(Xs,Y,Zs).

% Appending a pair (VariableName,Variable) to an input list
append_var(N,V,Vi,Vi) :- 
  my_member('='(N,V),Vi),
  !.
append_var(N,V,Vi,['='(N,V)|Vi]).

% Member of a list
my_member(X,[X|_Xs]).
my_member(X,[_Y|Xs]) :-
  my_member(X,Xs).
  
my_member_chk(X,[X|_Xs]) :-
  !.
my_member_chk(X,[_Y|Xs]) :-
  my_member_chk(X,Xs).
  

my_nth_member(X,N,Xs) :-
  my_nth_member(X,0,N,Xs).

my_nth_member(X,N,N,[X|_Xs]).
my_nth_member(X,CN,N,[_Y|Xs]) :-
  N1 is CN+1,
  my_nth_member(X,N1,N,Xs).

my_member_var(X,[Y|_Ys]) :-
  X==Y.
my_member_var(X,[Y|Ys]) :-
  X\==Y,
  my_member_var(X,Ys).

my_member_var(X,P1,[P2|_Ys]) :-
  \+ \+ (( 
   make_ground(X),
   make_ground(P2),
   P1=P2)),
   !,
   P1=P2.
my_member_var(X,P,[_Y|Ys]) :-
  my_member_var(X,P,Ys).

% Replacing an element of a list
replace_list(_A,_B,[],[]).
replace_list(A,B,[A|Xs],[B|Ys]) :-
  !,
  replace_list(A,B,Xs,Ys).
replace_list(A,B,[X|Xs],[X|Ys]) :-
  replace_list(A,B,Xs,Ys).

% Take the tail list starting in the Nth element of the input list (elements are numbered from 1 on)  
% take_from_N(L,N,O) :-
%   take_from_N(L,1,N,O).

% take_from_N(L,N,N,L) :-
%   !.
% take_from_N([_|L],N,N2,O) :-
%   N1 is N+1,
%   take_from_N(L,N1,N2,O).

% Takes the first N elements from a list. 
% If there are no enough elements, return fresh variables
take_N(0,_L,[]) :-
  !.
take_N(N,[X|Xs],[X|Ys]) :-
  N1 is N-1,
  take_N(N1,Xs,Ys).
take_N(N,[],Xs) :-
  length(Xs,N).
  
% Split a list into two, the first one with N elements
split_list(0,L,[],L) :-
  !.
split_list(N,[X|Xs],[X|Ys],L) :-
  N1 is N-1,
  split_list(N1,Xs,Ys,L).
  
% Split a list into two, the first one with odd position elements, and the second one with even position elements
% [a,b,c,d,e,f] -> [a,c,e] , [b,d,f]
split_list_odd_even([],[],[]) :-
  !.
split_list_odd_even([O,E|Xs],[O|Os],[E|Es]) :-
  split_list_odd_even(Xs,Os,Es).
  
% Bidirectional list to tuple
my_list_to_tuple([L],T) :-
  nonvar(T),
  T\=(_H,_T),
  L=T.
my_list_to_tuple([L],T) :-
  var(T),
  L=T.
%my_list_to_tuple([X],X).
my_list_to_tuple([X,Y|Xs],(X,Ts)) :-
  my_list_to_tuple([Y|Xs],Ts).

%% Appending two tuples
%my_tuple_append((X,Xs),Y,(X,Zs)) :- 
%  !,
%  my_tuple_append(Xs,Y,Zs).
%my_tuple_append(X,Y,(X,Y)).

% Building a conjunctive term
conjunctive_term([T],T) :-
  !.
conjunctive_term([T1,T2],CT) :- 
  !, 
  CT =.. [',',T1,T2].
conjunctive_term([T|Ts],CT) :- 
  CT =.. [',',T,CTT],
  conjunctive_term(Ts,CTT).

% List to set
% Via unification
remove_duplicates(L,S) :-
  remove_duplicates(L,[],S).

remove_duplicates([],L,L).
remove_duplicates([X|Xs],AL,L) :-
  my_member_chk(X,AL), 
  !,
  remove_duplicates(Xs,AL,L).
remove_duplicates([X|Xs],AL,L) :-
  remove_duplicates(Xs,[X|AL],L).

% List to set
% Variables are distinguished
remove_duplicates_var(L,S) :-
  remove_duplicates_var(L,[],S).

remove_duplicates_var([],L,L).
remove_duplicates_var([X|Xs],AL,L) :-
  my_member_var(X,AL), 
  !,
  remove_duplicates_var(Xs,AL,L).
remove_duplicates_var([X|Xs],AL,L) :-
  remove_duplicates_var(Xs,[X|AL],L).

% Multiset difference
my_set_diff([], _, []).
my_set_diff([Element|Elements], Set, Difference) :-
%    my_member(Element, Set),
  my_member_var(Element, Set),
  !,
  my_set_diff(Elements, Set, Difference).
my_set_diff([Element|Elements], Set, [Element|Difference]) :-
  my_set_diff(Elements, Set, Difference).

% Merging two lists; the first one contains no duplicates
my_merge(L,[],L).
my_merge(L,[A|As],Rs) :-
  my_member(A,L), !,
  my_merge(L,As,Rs).
my_merge(L,[A|As],Rs) :-
  my_merge([A|L],As,Rs).

% Quicksort, keeping duplicates
% Sorts in Prolog standard order
my_quicksort(L,OL) :-
  my_quicksort_pred(L,(@=<),OL).

% Quicksort, keeping duplicates
% Sorts as defined by Pred
my_quicksort_pred([],_Pred,[]).
my_quicksort_pred([Head|Tail],Pred,Sorted) :- 
  partition(Head,Pred,Tail,Left,Right),                                 
  my_quicksort_pred(Left,Pred,SortedL),
  my_quicksort_pred(Right,Pred,SortedR),
  my_append(SortedL,[Head|SortedR],Sorted).

partition(_Pivot,_Pred,[],[],[]).
partition(Pivot,Pred,[Head|Tail],[Head|Left],Right) :-
  Compare =.. [Pred,Head,Pivot],
  call(Compare),
  !,
  partition(Pivot,Pred,Tail,Left,Right).
partition(Pivot,Pred,[Head|Tail],Left,[Head|Right]) :-
  partition(Pivot,Pred,Tail,Left,Right).

% Rule compare (ascending) predicate, to be used with my_quicksort_pred for ordering Datalog rules
% Rules are ordered by predicate name, then, for arity, then, first are facts (rules without RHS),
% then rules (with RHS) (both in lexicographic Prolog standard order)

dlrule_compare_asc(datalog(R1,NVs1,_,_,_,_),datalog(R2,NVs2,_,_,_,_)) :-
  rule_compare_asc((R1,NVs1),(R2,NVs2)).

rule_compare_asc(Pair1,Pair2) :-
  Pair1 = (Rule1,_V1s),
  Pair2 = (Rule2,_V2s),
  (Rule1 = ':-'(LHS1,RHS1) -> Kind1 = rule ; LHS1 = Rule1, Kind1 = fact),
  (Rule2 = ':-'(LHS2,RHS2) -> Kind2 = rule ; LHS2 = Rule2, Kind2 = fact),
  (functor(LHS1,F1,A1), 
   functor(LHS2,F2,A2),
   F1 @< F2,
   !
   ;
   functor(LHS1,F,A1), 
   functor(LHS2,F,A2),
   A1 < A2,
   !
   ;
   functor(LHS1,F,A), 
   functor(LHS2,F,A),
   Kind1 == fact,
   Kind2 == rule,
   !
   ;
   functor(LHS1,F,A), 
   functor(LHS2,F,A),
   Kind1 == fact,
   Kind2 == fact,
   !,
   LHS1 @< LHS2   
   ;
   functor(LHS1,F,A), 
   functor(LHS2,F,A),
   Kind1 == rule,
   Kind2 == rule,
   ':-'(LHS1,RHS1) @< ':-'(LHS2,RHS2)).
  

% my_intersect_var(+L1,+L2,-L3): L3 = L1 intersect L2
my_intersect_var([],_L,[]) :-
  !.
my_intersect_var(_L,[],[]) :-
  !.
my_intersect_var([X|Xs],L,[X|RXs]) :-
  my_member_var(X,L),
  !,
  my_intersect_var(Xs,L,RXs).
my_intersect_var([_X|Xs],L,RXs) :-
  !,
  my_intersect_var(Xs,L,RXs).

% my_union_var(+L1,+L2,-L3): L3 = L1 union L2
my_union_var([],Xs,Xs).
my_union_var([X|Xs],Zs,Ys) :-
  my_member_var(X,Zs),
  !,
  my_union_var(Xs,Zs,Ys).
my_union_var([X|Xs],Zs,[X|Ys]) :-
  my_union_var(Xs,Zs,Ys).
  
% my_subtract_var(L1,L2,L3): L3=L1-L2
my_subtract_var(L,[],L).
my_subtract_var(From,[X|Xs],L) :-
  my_remove_var(X,From,To),
  !,
  my_subtract_var(To,Xs,L).

my_remove_var(_X,[],[]).
my_remove_var(X,[Y|Ys],Zs) :-
  X==Y,
  !,
  my_remove_var(X,Ys,Zs).
my_remove_var(X,[Y|Ys],[Y|Zs]) :-
  my_remove_var(X,Ys,Zs).

% my_flatten(Xs,Ys) is true if Ys is a list of the elements in Xs.
% e.g. my_flatten([[[3,c],5,[4,[]]],[1,b],a],[3,c,5,4,1,b,a]).    
my_flatten(Xs,Ys) :-
  my_flatten(Xs,[],Ys).

my_flatten(X,As,[X|As]) :-
  var(X),
  !.
my_flatten([X|Xs],As,Ys) :- 
  my_flatten(Xs,As,As1), 
  my_flatten(X,As1,Ys).
my_flatten(X,As,[X|As]) :-
  integer(X).
my_flatten(X,As,[X|As]) :-
  atom(X), 
  X\=[].
my_flatten([],Ys,Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_term_to_string(T,S) :- 
  my_term_to_string(T,S,[]).

% Term to string my_term_to_string(+Term,-String,+NameVariables)
% NameVariables contains program variable names in Term as pairs Name=Variable
% If there are variables without names, they are assigned new names
my_term_to_string(T,S,NVs) :- 
  assign_variable_names(T,NVs,CNVs),
  my_term_to_string(T,S,0,CNVs).
  
% Term to string my_term_to_string(+Term,-String,+Depth,+NameVariables)
% Depth is used to build parentheses to ease reading
my_term_to_string(V,S,_D,Vs) :- 
  var(V),
  !,
  my_var_name(V,N,Vs),
  name(N,S).
my_term_to_string('',[],_D,_Vs) :-
  !.
my_term_to_string(T,S,_D,_Vs) :- 
  number(T), 
  !,
  name(T,S).
my_term_to_string(T,S,_D,_Vs) :-
  atom(T),
  !,
  atom_codes(T,S1),
  name(NT,S1),
  S1=[D|_], 
  ((my_uppercase([D]) 
   ; 
    [D]="$" 
   ;
    number(NT) 
   ; 
    contains_non_alfanum(S1))->
   [A] = "'",
   my_append([A|S1],[A],S)
   ;
   S=S1).
my_term_to_string(T,S,D,Vs) :-
  T =.. [F,A],
  unary_operator(F,_POp,_D),
  !,
  D1 is D+1,
  my_term_to_string(F,S1,D,Vs),
  my_term_to_string(A,S2,D1,Vs),
  "(" = [OP], ")" = [CP], 
  my_append(S1,[OP|S2],S3),
  my_append(S3,[CP],S).
my_term_to_string([],"[]",_D,_Vs) :-
  !.
my_term_to_string([H|T],S,D,Vs) :-
  !,
  "[" = [OB],
  my_seq_to_string([H|T],S1,D,Vs),
  my_append([OB|S1],"]",S).
my_term_to_string(C,S,D,Vs) :- 
  C =.. [F|As],
  !, 
  (F == (',') ->
    (D==0 ->
     my_terms_to_string(As,S,D,Vs)
     ;
     my_terms_to_string(As,S1,D,Vs),
     "(" = [OP], ")" = [CP], 
     my_append([OP|S1],[CP],S)
     )
    ;
    D1 is D+1,
	  ((F == (';') , As = [A1,A2]) ->
      my_term_to_string(A1,S1,D1,Vs),
      add_parentheses(A1,S1,PS1),
      my_term_to_string(A2,S2,D1,Vs),
      add_parentheses(A2,S2,PS2),
      ";" = [SC], 
      my_append(PS1,[SC|PS2],S3),
	    (D==0 ->
	     S=S3
	     ;
	     "(" = [OP], ")" = [CP], 
	     my_append([OP|S3],[CP],S)
	     )
	    ;
	    (my_infix(F) ->
	     As = [L|Rs],
	     my_term_to_string(L,S1,D1,Vs),
	     name(F,S2),
	     " " = [BL],
	     my_append(S1,[BL|S2],S3), 
	     my_terms_to_string(Rs,S4,D1,Vs),
	     my_append(S3,[BL|S4],S) 
	     ;
	     my_terms_to_string(As,S2,D1,Vs),
	     my_term_to_string(F,S1,D,Vs),
	     "(" = [OP], ")" = [CP], 
	     my_append(S1,[OP|S2],S3),
	     my_append(S3,[CP],S)))).

my_terms_to_string([],"",_D,_Vs) :-
  !.
my_terms_to_string([T1],S,D,Vs) :-
  !,
  my_term_to_string(T1,S,D,Vs).
my_terms_to_string([T1,T2|Ts],S,D,Vs) :- !, 
  my_term_to_string(T1,S1,D,Vs), 
  my_terms_to_string([T2|Ts],S2,D,Vs), 
  "," = [C], 
  my_append(S1,[C|S2],S).

contains_non_alfanum([X|Xs]) :-
  "azAZ09_"=[La,Lz,Ua,Uz,N0,N9,US],
  ((X\==US,(X<N0 ; (X>N9,X<Ua) ; (X>Uz,X<La) ; X>Lz)) ->
   true
   ;
  contains_non_alfanum(Xs)). 
  
my_seq_to_string([],[],_D,_Vs).
my_seq_to_string([T],S,D,Vs) :-
  my_term_to_string(T,S,D,Vs).
my_seq_to_string([T1,T2|Ts],S,D,Vs) :-
  !,
  "," = [CO],
  my_term_to_string(T1,S1,D,Vs),
  my_seq_to_string([T2|Ts],S2,D,Vs),
  my_append(S1,[CO|S2],S).

  
add_parentheses(A,S,PS) :-
  (A=(A1,A2) ; A=(A1;A2)),
  !,
	"(" = [OP], ")" = [CP], 
	my_append([OP|S],[CP],PS).
add_parentheses(_A,S,S).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Program Variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% Get the program variable name, if not found, assign it a new name
my_var_name(V,N,NVs) :-
  find_var_name(V,N,NVs),
  !.
my_var_name(_V,N,NVs) :-
  name_var(NVs,N).
  
% Get the program variable name for a list of variables, if not found, assign it a new name
my_var_name_list([],_INVs,[]).
my_var_name_list([V|Vs],INVs,[N=V|ONVs]) :-
  my_var_name(V,N,INVs),
  my_var_name_list(Vs,INVs,ONVs).
  
% Find a program variable name
find_var_name(V,N,[N=V1|_NVs]) :- 
  V == V1,
  !.
find_var_name(V,N,[_NV|NVs]) :- 
  find_var_name(V,N,NVs).

% Find a list of program variable names
find_var_name_list([],_NVs,[]).
find_var_name_list([V|Vs],NVs,[N=V|ONVs]) :- 
  find_var_name(V,N,NVs),
  find_var_name_list(Vs,NVs,ONVs).

'NVs2Vs'([],[]).
'NVs2Vs'([_N=V|NVs],IV) :-
  'NVs2Vs'(NVs,Vs),
  my_append(Vs,[V],IV).

'Vs2NVs'([],_NVs,[]).
'Vs2NVs'([V|Vs],NVs,[N=V|TNVs]) :-
  find_var_name(V,N,NVs),
%  my_var_name(V,N,NVs),
  'Vs2NVs'(Vs,NVs,TNVs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_list([],_Error).
assert_list([DL|DLs],Error) :-
  (my_assertz(DL) -> true ; Error=true),
  assert_list(DLs,Error).

retract_list([],_Error).
retract_list([DL|DLs],Error) :-
  (retract(DL) -> true ; Error=true),
  retract_list(DLs,Error).

% Retractall
retractallDL(H) :-
  retractall(datalog(H,_,_,_,_,_)),
  retractall(datalog((H:-_B),_,_,_,_,_)).

retract_RNVs_list([]).
retract_RNVs_list([(R,NVs)|RNVss]) :-
  retractall(datalog(R,NVs,_,_,_,_)),
  retract_RNVs_list(RNVss).

my_assertz(DL) :-
  DL=datalog(_R,_NVs,Rid,_Ls,_Fid,_C),
  check_constraints(DL),
  my_new_rule_id(Rid),
  assertz(DL).

my_new_rule_id(Rid) :-
  (rule_id(CRid) ->
    Rid is CRid+1,
    retract(rule_id(CRid)),
    assertz(rule_id(Rid))
   ;
    Rid=0,
    assertz(rule_id(Rid))).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraint checking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_constraints(DL) :-
  check_types(DL),
  check_PK_ctr(DL),
  check_FK_ctrs(DL).
    
check_types(DL) :-
  DL=datalog(R,_,_,_,_,_),
  (R=':-'(H,_B)
   ;
   R=H),
  functor(H,T,A),
%  H=..[T|Args],
%  length(Args,L),
  (my_table('$des',T,A) ->
    (infer_types_list([R],_InfTypes,_TypedArgs) ->
      true
     ;
      get_table_types(T,DeclTypes),
      write_log_list(['Error: Type mismatch ',DeclTypes,' (table declaration)',nl]),
      !,
      fail
    )
   ;
    true).
   
check_PK_ctr(DL) :-
  DL=datalog(Fact,_,_,_,_,_),
  (Fact\=':-'(_Head,_Body) ->
   functor(Fact,TableName,Arity),
   (my_table('$des',TableName,Arity) ->
     (my_primary_key('$des',TableName,PK_AttNames) ->
       build_PK_goal(Fact,TableName,PK_AttNames,Goal),
       (call(datalog(Goal,_,_,_,_,_)) ->
         write_log_list(['Error: Primary key violation when trying to insert: ',Fact,nl]),
         nl_log,
         !,
         fail
        ;
         true
       )
      ;
       true)
    ;
     true)
   ;
    true).
    
build_PK_goal(Fact,TableName,PK_AttNames,Goal) :-
  get_att_positions(TableName,PK_AttNames,Positions),
  Fact =.. [TableName|FactArgs],
  build_PK_goal_arguments(FactArgs,Positions,1,GoalArgs),
  Goal =.. [TableName|GoalArgs].
  
build_PK_goal_arguments(Args,[],_Pos,RArgs) :-
  !,
  length(Args,L),
  length(RArgs,L).
build_PK_goal_arguments([Arg|Args],[Pos|Poss],Pos,[Arg|RArgs]) :-
  !,
  Pos1 is Pos+1,
  build_PK_goal_arguments(Args,Poss,Pos1,RArgs).
build_PK_goal_arguments([_Arg|Args],Poss,Pos,[_RArg|RArgs]) :-
  Pos1 is Pos+1,
  build_PK_goal_arguments(Args,Poss,Pos1,RArgs).
  
check_FK_ctrs(DL) :-
  DL=datalog(Fact,_,_,_,_,_),
  (Fact\=':-'(_Head,_Body) ->
   functor(Fact,TableName,Arity),
   (my_table('$des',TableName,Arity) ->
     check_FK_ctr(TableName,Fact) 
    ;
     true)
   ;
    true).

check_FK_ctr(TableName,Fact) :-
  my_foreign_key('$des',TableName,FK_AttNames,ForeignTableName,PK_AttNames),
  build_FK_goal(Fact,FK_AttNames,ForeignTableName,PK_AttNames,Goal),
  (datalog(Goal,_,_,_,_,_) ->
    %!,
    true
   ;
    write_log_list(['Error: Foreign key violation ',TableName,'.',FK_AttNames,'->',ForeignTableName,'.',PK_AttNames,' when trying to insert: ',Fact,nl]),
    !,
    fail
  ),
  fail  % Looks for all foreign key declarations
  .
check_FK_ctr(_TableName,_Fact).

    
build_FK_goal(Fact,FK_AttNames,ForeignTableName,PK_AttNames,Goal) :-
  project_tuple(Fact,FK_AttNames,ValueList),
  get_att_positions(ForeignTableName,PK_AttNames,PK_AttPositions),
  my_table('$des',ForeignTableName,Arity),
  build_FK_goal_arguments(ValueList,PK_AttPositions,Arity,1,ForeignArgs),
  Goal=..[ForeignTableName|ForeignArgs].

project_tuple(Tuple,AttNames,ValueList) :-
  Tuple=..[TableName|TableArgs],
  get_att_positions(TableName,AttNames,Positions),
  filter_positions(TableArgs,Positions,1,ValueList).

filter_positions(_Args,[],_I,[]) :-
  !.
filter_positions([Arg|Args],[I|Is],I,[Arg|RArgs]) :-
  !,
  I1 is I+1,
  filter_positions(Args,Is,I1,RArgs).
filter_positions([_Arg|Args],Is,I,RArgs) :-
  I1 is I+1,
  filter_positions(Args,Is,I1,RArgs).

get_att_positions(TableName,AttNames,AttPositions) :-
  setof(AttPosition,
        AttName^DataType^
        (my_attribute('$des',AttPosition,TableName,AttName,DataType),
         my_member(AttName,AttNames)
        ),
        AttPositions).
  
build_FK_goal_arguments([],[],Arity,I,Args) :-
  !,
  TL is Arity-I+1,
  length(Args,TL).
build_FK_goal_arguments([Value|Values],[I|Is],Arity,I,[Value|Args]) :-
  !,
  I1 is I+1,
  build_FK_goal_arguments(Values,Is,Arity,I1,Args).
build_FK_goal_arguments(Values,Is,Arity,I,[_Var|Args]) :-
  !,
  I1 is I+1,
  build_FK_goal_arguments(Values,Is,Arity,I1,Args).
  
/*********************************************************************/
/* Finding Datalog source Rules matching a pattern: get_source_dlrules */
/*********************************************************************/

% Get source rules from a list of rules
get_source_dlrules_list([],[]).
get_source_dlrules_list([(Rule,_NVs)|Rules],CRules) :-
  get_source_dlrules(rule,Rule,CRules1),
  get_source_dlrules_list(Rules,CRules2),
  my_append(CRules1,CRules2,CRules).

% Get source rules as they were originally typed. Optionally filtered by name and arity
get_source_dlrules(DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,C),
        SH^SB^CDLs^
        ((C=source,datalog(R,NVs,Rid,Ls,Fid,C)) ;
         (C=compilation(SH,SB,CDLs),datalog(R,NVs,Rid,Ls,Fid,C))),
        DLs).

get_source_dlrules(name,N,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,C),
        SH^SB^CDLs^A^
        (
        ((C=source,datalog(R,NVs,Rid,Ls,Fid,C),(R=':-'(SH,SB) -> true ; R=SH)) ;
         (C=compilation(SH,SB,CDLs),datalog(R,NVs,Rid,Ls,Fid,C))),
         functor(SH,N,A)),
        DLs).

get_source_dlrules(namearity,N/A,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,C),
        SH^SB^CDLs^
        (
        ((C=source,datalog(R,NVs,Rid,Ls,Fid,C),(R=':-'(SH,SB) -> true ; R=SH)) ;
         (C=compilation(SH,SB,CDLs),datalog(R,NVs,Rid,Ls,Fid,C))),
         functor(SH,N,A)),
        DLs).

get_source_dlrules(head,H,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,C),
        SH^SB^CDLs^
        (
        ((C=source,datalog(R,NVs,Rid,Ls,Fid,C),(R=':-'(SH,SB) -> true ; R=SH)) ;
         (C=compilation(SH,SB,CDLs),datalog(R,NVs,Rid,Ls,Fid,C))),
          my_subsumes(H,SH)),
        DLs).

get_source_dlrules(rule,PR,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,C),
        SH^SB^SR^CDLs^
        (
        ((C=source,datalog(R,NVs,Rid,Ls,Fid,C),(R=':-'(SH,SB) -> true ; R=SH), SR=R) ;
         (C=compilation(SH,SB,CDLs),datalog(R,NVs,Rid,Ls,Fid,C),SR=':-'(SH,SB))),
          my_subsumes(PR,SR)),
        DLs).

/*********************************************************************/
/* Finding Datalog Object Rules: get_object_dlrules                  */
/*********************************************************************/
% Gets the rules that are actually used in a computation. 
% They include rules as typed by the programmer (source) and compiled rules  
% from others which cannot be directly computed (compilation)

% Types of rules:
% - Uncompiled: 
%    * Tagged as 'source'
% - Compiled: 
%    * The root of a compilation tagged as 'compilation'(SourceHead,SourceBody,ListOfCompiledRules)
%      The list of compiled rules may contain additional compilations.
%    * A leaf of a compilation tagged as 'compiled'.

% Get object rules from a list of source rules
get_object_dlrules_list([],[]).
get_object_dlrules_list([(Rule,_NVs)|Rules],CRules) :-
  get_object_dlrules(rule,Rule,CRules1),
  get_object_dlrules_list(Rules,CRules2),
  my_append(CRules1,CRules2,CRules).


% Get object rules. 
get_object_dlrules(DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,C),
        datalog(R,NVs,Rid,Ls,Fid,C),
        DLs).

% Get object rules. Filtered by a given source rule
get_object_dlrules(datalog(R,NVs,Rid,Ls,Fid,source),[datalog(R,NVs,Rid,Ls,Fid,source)]) :-
  !.
get_object_dlrules(datalog(R,NVs,Rid,Ls,Fid,compilation(H,B,DLs)),
                  [datalog(R,NVs,Rid,Ls,Fid,compilation(H,B,DLs))|ODLs]) :-
  !,
  get_dependent_dlrules([datalog(R,NVs,Rid,Ls,Fid,compilation(H,B,DLs))],ODLs).

% Get object rules. Filtered by name
get_object_dlrules(name,N,DLs) :-
  get_uncompiled_dlrules(N,UDLs),
  get_root_compiled_dlrules(N,RDLs),
  get_dependent_dlrules(RDLs,DDLs),
  concat_lists([UDLs,RDLs,DDLs],DLs).

% Get object rules. Filtered by name and arity
get_object_dlrules(namearity,N/A,DLs) :-
  get_uncompiled_dlrules(N,A,UDLs),
  get_root_compiled_dlrules(N,RDLs),
  get_dependent_dlrules(RDLs,DDLs),
  concat_lists([UDLs,RDLs,DDLs],DLs).

% Get object rules. Filtered by head
get_object_dlrules(head,H,DLs) :-
  get_uncompiled_dlrules_from_head(H,UDLs),
  get_root_compiled_dlrules_from_head(H,RDLs),
  get_dependent_dlrules(RDLs,DDLs),
  concat_lists([UDLs,RDLs,DDLs],DLs).

% Get object rules. Filtered by rule
get_object_dlrules(rule,R,DLs) :-
  get_uncompiled_dlrules_from_rule(R,UDLs),
  get_root_compiled_dlrules_from_rule(R,RDLs),
  get_dependent_dlrules(RDLs,DDLs),
  concat_lists([UDLs,RDLs,DDLs],DLs).

% Get uncompiled rules. Filtered by name
get_uncompiled_dlrules(N,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,source),
        H^B^A^
        (datalog(R,NVs,Rid,Ls,Fid,source),
         (R=':-'(H,B) -> true ; H=R), 
         functor(H,N,A)),
        DLs).

% Get uncompiled rules. Filtered by name and arity
get_uncompiled_dlrules(N,A,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,source),
        H^B^
        (datalog(R,NVs,Rid,Ls,Fid,source),
         (R=':-'(H,B) -> true ; H=R), 
         functor(H,N,A)),
        DLs).

% Get uncompiled rules. Filtered by head
get_uncompiled_dlrules_from_head(PH,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,source),
        H^B^
        (datalog(R,NVs,Rid,Ls,Fid,source),
         (R=':-'(H,B) -> true ; H=R),
         my_subsumes(PH,H)),
        DLs).

% Get uncompiled rules. Filtered by rule
get_uncompiled_dlrules_from_rule(PR,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,source),
        (datalog(R,NVs,Rid,Ls,Fid,source),
         my_subsumes(PR,R)),
        DLs).


% get_root_compiled_dlrules(RNVss) :-
%   my_nf_bagof((R,NVs),
%         Rid^Ls^Fid^Rs^
%         (datalog(R,NVs,Rid,Ls,Fid,Rs)),
%         RNVss).

% Get rules that are compilation roots. Filtered by source rule name
get_root_compiled_dlrules(N,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,compilation(SH,SB,DLs)),
        H^B^A^
        (datalog(R,NVs,Rid,Ls,Fid,compilation(SH,SB,DLs)),
        (R=':-'(H,B) -> true ; H=R), 
        functor(SH,N,A)),
        DLs).

% Get rules that are compilation roots. Filtered by source rule name and arity
get_root_compiled_dlrules(N,A,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,compilation(SH,SB,DLs)),
        H^B^
        (datalog(R,NVs,Rid,Ls,Fid,compilation(SH,SB,DLs)),
        (R=':-'(H,B) -> true ; H=R), 
        functor(SH,N,A)),
        DLs).

% Get rules that are compilation roots. Filtered by source rule head
get_root_compiled_dlrules_from_head(PH,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,compilation(SH,SB,DLs)),
        (datalog(R,NVs,Rid,Ls,Fid,compilation(SH,SB,DLs)),
         my_subsumes(PH,SH)),
        DLs).

% Get rules that are compilation roots. Filtered by source rule 
get_root_compiled_dlrules_from_rule(PR,DLs) :-
  my_nf_bagof(datalog(R,NVs,Rid,Ls,Fid,compilation(SH,SB,DLs)),
        (datalog(R,NVs,Rid,Ls,Fid,compilation(SH,SB,DLs)),
         my_subsumes(PR,':-'(SH,SB))),
        DLs).

% Get rules result from a compilation. There may be further compilations from a given root
get_dependent_dlrules([],[]).
get_dependent_dlrules([datalog(R,NVs,Rid,Ls,Fid,compilation(H,B,CDLs))|DLs],DDLs) :-
  !,
  my_nf_bagof(CDLs,datalog(R,NVs,Rid,Ls,Fid,compilation(H,B,CDLs)),CDLss),
  concat_lists(CDLss,CCDLs),
  get_dependent_dlrules(CCDLs,HDLs),
  get_dependent_dlrules(DLs,TDLs),
  concat_lists([CCDLs,HDLs,TDLs],DDLs).
get_dependent_dlrules([_|DLs],DDLs) :-
  get_dependent_dlrules(DLs,DDLs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rule Conversions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% datalog(...) to (R,NVs), where either R=H:-B or R=H
dlrule_to_ruleNV_list([],[]).
dlrule_to_ruleNV_list([datalog(R,NVs,_,_,_,_)|DRs],[(R,NVs)|RNVs]) :-
  dlrule_to_ruleNV_list(DRs,RNVs).

% datalog(...) to (R,NVs), where either R=H:-B or R=H
source_dlrule_to_ruleNV_list([],[]).
source_dlrule_to_ruleNV_list([datalog(R,NVs,_,_,_,source)|DRs],[(R,NVs)|RNVs]) :-
  !,
  source_dlrule_to_ruleNV_list(DRs,RNVs).
source_dlrule_to_ruleNV_list([datalog(_R,NVs,_,_,_,compilation(SH,SB,_DLs))|DRs],[(':-'(SH,SB),NVs)|RNVs]) :-
  source_dlrule_to_ruleNV_list(DRs,RNVs).

% R to (R,NVs), where either R=H:-B or R=H
rule_to_ruleNV_list([],_,[]).
rule_to_ruleNV_list([R|Rs],NVs,[(R,RNVs)|RNVss]) :-
  assign_variable_names(R,NVs,RNVs),
  rule_to_ruleNV_list(Rs,NVs,RNVss).

  
% (R,NVs) to R, where either R=H:-B or R=H
ruleNV_to_rule_list([],[]).
ruleNV_to_rule_list([(R,_NVs)|RNVs],[R|Rs]) :-
  ruleNV_to_rule_list(RNVs,Rs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inserting
insert_into_last_but_one_pos([L],X,[X,L]).
insert_into_last_but_one_pos([L1,L2|Ls],X,[L1|RLs]) :-
  insert_into_last_but_one_pos([L2|Ls],X,RLs).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logical
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Prolog implementation of negation
my_not(G) :- 
  call(G), 
  !, 
  fail.
my_not(_G).

% Logical disjunction
my_or(true,true,true).
my_or(true,false,true).
my_or(false,true,true).
my_or(false,false,false).

% Uncertainty disjunction
my_u_or(L,R,true) :-
  ((var(L),R==true);(L==true,var(R))),
  !.
my_u_or(L,R,_O) :-
  (var(L);var(R)),
  !.
my_u_or(L,R,O) :-
  my_or(L,R,O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metapredicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Map
my_apply(_X,[]).
my_apply(X,[L|Ls]) :-
  L = [_H|_T],
  !,
  T =.. [X|L],
  call(T),
  my_apply(X,Ls).
my_apply(X,[Y|Ys]) :-
  !,
  T =.. [X,Y],
  call(T),
  my_apply(X,Ys).
 
% Testing whether the input list contains variables
vars([]).
vars([V|Vs]) :- 
  var(V), 
  vars(Vs).

% Returns always an atom, just in case its input is a number
ensure_atom(N,A) :-
  (number(N) -> number_codes(N,CL), atom_codes(A,CL); N=A).

% Findall
my_nf_bagof(X,G,Xs) :-
  (bagof(X,G,Xs) -> true ; Xs=[]).

% No-failing setof: Returns empty list
my_nf_setof(X,G,Xs) :-
  (setof(X,G,Xs) -> true ; Xs=[]).

% Unifiable: Tests whether two terms are unifiable
my_unifiable(X,Y) :-
  \+ \+ X=Y.

% Copy term for lists
copy_term_list([],[]).
copy_term_list([T|Ts],[CT|CTs]) :-
  copy_term(T,CT),
  copy_term_list(Ts,CTs).

% Literals
my_literal(L) :-
  nonvar(L),
  L=..[_F|Args],
  my_atom_list(Args).
  
my_atom_list([]).  
my_atom_list([A|As]) :-
  (var(A);atomic(A)),
  !,
  my_atom_list(As).  
  
  
% my_goal(not(G)) :-
%   !,
%   my_atom(G).
% my_goal(G) :-
%   !,
%   my_atom(G).

my_atom(A) :-
  atom(A).  
my_atom(T) :-
  T =.. [F|Args],
  F \== not,
  atom(F),
  my_noncompoundterms(Args).

my_noncompoundterms([]).
my_noncompoundterms([Term|Terms]) :-
  my_noncompoundterm(Term),
  my_noncompoundterms(Terms).

my_noncompoundterm(T) :-
% ::WARNING: It worked!
%  atom(T),
  atomic(T),
  !.
my_noncompoundterm(T) :-
  var(T),
  !.
my_noncompoundterm('$NULL'(_ID)).
    
call_list([]).
call_list([H|T]) :-
  call(H),
  call_list(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File I/O
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Opening a File

try_open(F,CFN,St) :- 
  (my_file_exists(F) ->
   my_absolute_filename(F,CFN)
   ;
   atom_concat(F,'.dl',FP), 
   (my_file_exists(FP) ->
     my_absolute_filename(FP,CFN)
    ;
     my_working_directory(CD),
     atom_concat(CD,F,CDF),
     (my_file_exists(CDF) ->
       my_absolute_filename(CDF,CFN)
      ;
       atom_concat(CD,FP,CDFP),
       my_file_exists(CDFP),
       my_absolute_filename(CDFP,CFN)
     )
   )
  ),
  !, 
  (open(CFN,read,St),
   set_input(St) ->
   true
   ;
   write_log_list(['Error: Stream cannot be opened.',nl]),
   fail). 
try_open(F,_,_) :-
  write_log_list(['Error: File ''',F,''' not found.',nl]),
  fail.


% Changing the Current Path

cd_path(NPath) :-
  ensure_atom(NPath,Path),
  (my_directory_exists(Path),
   !,
   my_change_directory(Path),
   pwd_path;
   write_log_list(['Error: Cannot access the path ''', Path, '''.', nl])).


% Displaying the Current Path

pwd_path :-
  write_log('Info: Current directory is:'), 
  nl_log, 
  write_log('  '), 
  my_working_directory(Path), 
  write_log(Path), 
  nl_log.


% Listing Directory Contents

ls :-
  my_working_directory(WorkingPath),
  ls(WorkingPath).

ls(NPath) :-
  ensure_atom(NPath,Path),
  (my_not(my_directory_exists(Path)) ->
   write_log_list(['Warning: Path ''', Path, ''' does not exist (wildcards not allowed).', nl])
   ;
   (
    my_absolute_filename(Path, AbsolutePath),
    my_directory_files(AbsolutePath, Files),
    my_directory_directories(AbsolutePath, Directories),
    write_log_list(['Info: Contents of ', AbsolutePath, nl, nl]), 
    write_log('Files:'), 
    write_dir_files(Files), 
    nl_log,
    write_log('Directories:'), 
    write_dir_directories(Directories), 
    nl_log)).


% Writing each File in a Directory. Path comes without final slash

write_dir_files([]).
write_dir_files([F|Fs]) :-
  nl_log, 
  write_log('  '), 
  write_log(F), 
  write_dir_files(Fs).


% Writing each Directory in a Directory

write_dir_directories([]).
write_dir_directories([F|Fs]) :-
  F \== '.',
  F \== '..', 
  !,
  nl_log,
  write_log('  '),
  write_log(F),
  write_dir_directories(Fs).
write_dir_directories([_F|Fs]) :-
  write_dir_directories(Fs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Writing a term with its textual variable names

write_with_NVs(T,Vs) :-
  my_term_to_string(T,S,Vs),
  write_string_log(S).
  
% Writing a list of terms with their textual variable names

write_list_with_NVs(RV) :-
  write_log('['),
  write_csa_with_NVs(RV),
  write_log(']').

write_csa_with_NVs([]).
write_csa_with_NVs([(T,V)]) :-
  write_with_NVs(T,V).
write_csa_with_NVs([(T1,V1),(T2,V2)|RVs]) :-
  write_with_NVs(T1,V1),
  write_log(','), 
  write_csa_with_NVs([(T2,V2)|RVs]).

write_with_NVs_list(Ts,NVs) :-
  write_log('['),
%  write_csa_with_NVs(Ts,NVs),
  my_list_to_tuple(Ts,TTs),
  write_with_NVs(TTs,NVs),
  write_log(']').


% Comma-separated writing of a a list of terms with their textual variable names

write_csa_with_NVs([],_).
write_csa_with_NVs([T],Vs) :-
  write_with_NVs(T,Vs).
write_csa_with_NVs([T1,T2|RTs],Vs) :-
  write_with_NVs(T1,Vs),
  write_log(','), 
  write_csa_with_NVs([T2|RTs],Vs).

% Verbose output of lists

write_verb(L) :-
  (verbose(on) -> 
    write_verb_list(L)
   ;
    true).
write_verb_list(_L) :-
  verbose(off),
  !.
write_verb_list([]).
write_verb_list([T|Ts]) :-
  (T==nl ->
    nl_log
   ; 
    write_log(T)),
  write_verb_list(Ts).

% Log Output: Both current stream and log file, if enabled

write_log(X) :-
  (output(on) ->
    write(X) 
   ;
    true),
  (log(_F,S) -> 
    write(S,X) 
   ; 
    true).

write_quoted_log(nl) :-
  write_log_list([nl]),
  !.
write_quoted_log(X) :-
  (output(on) ->
    write_term(X,[quoted(true)])
   ;
    true), 
  (log(_F,S) ->
    write_term(S,X,[quoted(true)])
   ;
    true).

write_quoted_log_list([]).
write_quoted_log_list([X|Xs]) :-
  write_quoted_log(X),
  write_quoted_log_list(Xs).

nl_log :-
  (output(on) ->
    nl
   ;
    true),
  nl_only_to_log.

% Log Output: with new program names for variables

write_log_fresh_NVs(X) :-
  assign_variable_names(X,[],NVs),
  write_with_NVs(X,NVs).

% Log Output for lists of terms

write_log_list([]).
write_log_list([T|Ts]) :-
  (T==nl ->
    nl_log
   ;
    write_log(T)),
  write_log_list(Ts).

% Only-to-Log Output

write_only_to_log(S) :-
  (log(_F,H) ->
    name(X,S), 
    write(H,X)
   ;
    true).

nl_only_to_log :-
  (log(_F,H) ->
    nl(H)
   ;
    true).


% Writing a string (Log Output)

write_string_log([]) :-
  !.
write_string_log([C|Cs]) :-
  name(A,[C]),
  write_log(A), 
  write_string_log(Cs).

% Writing a string (Current Stream Output)

write_string(_S) :-
  output(off),
  !.
write_string([]) :-
  !.
write_string([C|Cs]) :-
  name(A,[C]),
  write(A), 
  write_string(Cs).

  
% Writing a term and spaces to fit a given width (Log Output)

write_tab_log(T,L) :-
  my_term_to_string(T,ST,[]),
  write_string_log(ST),
  length(ST,STL),
  SL is L-STL,
  my_spaces(SL,S),
  write_log(S).
  
my_spaces(SL,T):-
  my_spacesS(SL,S),
  name(T,S).
my_spacesS(0,[]) :-
  !.
my_spacesS(N,[Sp|Ts]) :-
  [Sp] = " ",
  N1 is N-1,
  my_spacesS(N1,Ts).

% Displaying the system status

display_status :-
  processC(version,[],_,_),
  processC(timing,[],_,_),
  processC(verbose,[],_,_),
  processC(development,[],_,_),
  processC(pretty_print,[],_,_),
  processC(log,[],_,_), 
  processC(negation,[],_,_),
  processC(safe,[],_,_),
  processC(simplification,[],_,_),
  processC(tc_size,[],_,_),
  processC(tc_domain,[],_,_),
  processC(output,[],_,_),
  processC(pwd,[],_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error Handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Syntax error reporting

syntax_error(Where) :- 
  write_log_list(['Error: Syntax error in ', Where, nl]).
  

% Redefinition error

redefinition_error(F,A) :-
  write_log_list(['Error: Syntax error. Trying to redefine the builtin ',F,'/',A,nl]),
  !, 
  fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For parsing variables, e.g.:
% a --> {p(X)}, X
% a --> {p(X)}, my_string(X)
  
my_string([]) --> 
  [].
my_string([C|Cs]) -->
  [C],
  {[A] = "'",
   A =\= C},
  my_string(Cs).

% For parsing keywords, irrespective of the case

% my_kw(CKW) -->
%   my_string(KW),
%   {capitalizeList(KW,CKW)}.

my_kw([],Cs,Cs).
my_kw([CC|CCs],[C|Cs],Ys) :-
  [C] =\= "'",
  to_uppercase_char(C,CC),
  my_kw(CCs,Cs,Ys).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% my_member_term: analogous to my_member, but for terms
my_member_term(X,T) :- 
  X==T,
  !.
my_member_term(X,C) :- 
  C =.. [_F|As],
  !, 
  my_member_term_list(X,As).

my_member_term_list(_X,[]) :-
  !,
  fail.
my_member_term_list(X,[T|_Ts]) :-
  my_member_term(X,T).
my_member_term_list(X,[_T|Ts]) :-
  my_member_term_list(X,Ts).
      
% Term depth less or equal than a given bound. 
% WARNING: Unused in 2.0
term_depth_leq(T,D) :- 
  (number(T) ; atom(T) ; var(T)),
  !,
  D>=0.
term_depth_leq(C,D) :- 
  C =.. [_F|As],
  !, 
  D1 is D-1,
  term_depth_leq_list(As,D1).

term_depth_leq_list([],_D) :-
  !.
term_depth_leq_list([T|Ts],D) :-
  term_depth_leq(T,D), 
  term_depth_leq_list(Ts,D).

% Replaces the functor of the terms in a list  
replace_functor_term_list([],_RF,[]).
replace_functor_term_list([T|Ts],RF,[RT|RTs]) :-
  T=..[_F|Args],
  RT=..[RF|Args],
  replace_functor_term_list(Ts,RF,RTs).

% Concat a list of atoms
atom_concat_list([A],A).
atom_concat_list([A,B|C],D) :-
  atom_concat(A,B,E),
  atom_concat_list([E|C],D).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Strings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% to_uppercase_char_list(+List,-UppercaseList)
% Converts the input list to uppercase

to_uppercase_char_list([],[]).
to_uppercase_char_list([X|Xs],[U|Cs]) :-
  to_uppercase_char(X,U),
  to_uppercase_char_list(Xs,Cs).

to_uppercase_char(X,U) :-
  [X] >= "a",
  [X] =< "z",
  !,
  [UA] = "A",
  [LA] = "a",
  U is X+UA-LA.
to_uppercase_char(X,X).

to_uppercase(LC,UC) :-
  name(LC,SLC),
  to_uppercase_char_list(SLC,SUC),
  name(UC,SUC).

% to_lowercase_list(+List,-UppercaseList) 
% Converts the input list to lowercase

to_lowercase_list([],[]).
to_lowercase_list([X|Xs],[U|Cs]) :-
  to_lowercase(X,U),
  to_lowercase_list(Xs,Cs).

to_lowercase(X,U) :-
  [X] >= "A",
  [X] =< "Z",
  !,
  [UA] = "A",
  [LA] = "a",
  U is X-UA+LA.
to_lowercase(X,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Timing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_elapsed_time :-
  store_elapsed_time(display),
  get_elapsed_time(Parsing,Computation,Display,Total),   
  timing(Switch),
  (Switch==detailed ->
   write_log_list(['Info: Parsing elapsed time    : ',Parsing,' ms.',nl]),
   write_log_list(['Info: Computation elapsed time: ',Computation,' ms.',nl]),
   write_log_list(['Info: Display elapsed time    : ',Display,' ms.',nl]),
   write_log_list(['Info: Total elapsed time      : ',Total,' ms.',nl])
   ;
   true),
  (Switch==on ->
   write_log_list(['Info: Total elapsed time: ',Total,' ms.',nl])
   ;
   true).

get_elapsed_time(Parsing,Computation,Display,Total) :-
  time(Parsing,Computation,Display),
  Total is Parsing+Computation+Display.
  
store_elapsed_time(parsing) :-
  retract(time(_,Computation,Display)),
  get_elapsed_time(Parsing),
  assertz(time(Parsing,Computation,Display)).
store_elapsed_time(computation) :-
  retract(time(Parsing,_,Display)),
  get_elapsed_time(Computation),
  assertz(time(Parsing,Computation,Display)).
store_elapsed_time(display) :-
  retract(time(Parsing,Computation,_)),
  get_elapsed_time(Display),
  assertz(time(Parsing,Computation,Display)).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellanea
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Maximum of two numbers
my_max(A,B,A) :-
  A>=B,
  !.
my_max(_A,B,B).

% Display a list of datalog rules and its number
display_tuples_and_nbr_info(SDLs,ODLs) :-
  (verbose(on) ->
  	(development(on) -> 
  	  length(ODLs,Nbr)
      ; 
      length(SDLs,Nbr)),
  	(Nbr==1 -> S =' ' ; S='s '),
    write_log_list(['Info: ',Nbr,' rule',S,'retracted:',nl]), 
  	(development(on) -> 
      display_dlrule_list(ODLs,2)
      ; 
      display_source_dlrule_list(SDLs,2))
	 ;
	  true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debugging during development
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deb([]).
deb([X|Xs]) :-
  nl, write(X), deb(Xs).
  
