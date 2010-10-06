/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.1.2.1               */
/*                                                       */
/*    MAIN PROGRAM                                       */
/*                                                       */
/*                                                       */
/* - Stratified Negation                                 */
/* - Full recursion                                      */
/* - Noncompound terms                                   */
/*                                                       */
/*                          Fernando Sáenz (c) 2004-2007 */
/*                                             DISIA UCM */
/*             Please send comments, questions, etc. to: */
/*                                     fernan@sip.ucm.es */
/*                                Visit the Web site at: */
/*                           http://des.sourceforge.net/ */
/*                                                       */
/*********************************************************/

GNU SWI DES SICS TOY 

des_version('1.2.1'). 

/*********************************************************************/
/* Dynamic Predicates                                                */
/*********************************************************************/

:- dynamic(log/2).     % Log file information (filename and associated stream)
:- dynamic(verbose/0). % Verbose mode flag 
:- dynamic(et/1).      % Extension Table 
:- dynamic(called/1).  % Call Patterns
:- dynamic(et_flag/1). % Extension Table flag 
:- dynamic(datalog/3). % Datalog Rules Database 
:- dynamic(strata/1).  % Result from a stratification
:- dynamic(pdg/1).     % Predicate Dependency Graph


/*********************************************************************/
/* Autorun Information (only for reference)                          */
/*********************************************************************/

%Autorun:
% Sicstus:
%  -l des.pl
% SWI Prolog:
%  -g "[des],start"
% GNU Prolog:
%  --entry-goal ['des.pl']
% CIAO Prolog:
%  -l ciaorc


/*********************************************************************/
/* System dependent predicates                                       */
/*********************************************************************/

:- initialization(consult('des1.pl')).


/*********************************************************************/
/* Starting the System: start                                        */
/*********************************************************************/

start :- banner, init_des, exec_des.

init_des :- 
  my_working_directory(D),
  assertz(start_path(D)),
  set_verbose,
  retractall(log(_,_)),
  reset_stratification,
  assertz(et_flag(no)),
  process_batch. %If des.ini exists, their entries are processed as command prompt inputs

process_batch :-
  seeing(OldInput),       % Current input stream
  try_open('des.ini'),    % Open initialization batch
  nl, nl, write('Executing initialización batch des.ini ...'), nl, nl,
  repeat,
    readln(S,E),          % Read a line
    write_prompt, write_string(S), nl_log, 
    nl_log, process_input(S), nl_log,
    E == end_of_file,
  seen,                   % Close the file
  see(OldInput).          % Restore current input stream
process_batch.


/*********************************************************************/
/* Informative banner                                                */
/*********************************************************************/

banner :-
  write_log('*********************************************************'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('* DES: Datalog Educational System v.1.2.1               *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('* - Stratified Negation                                 *'), nl_log,
  write_log('* - Full recursion                                      *'), nl_log,
  write_log('* - Noncompound terms                                   *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('* Type "/help" for commands                             *'), nl_log,
  write_log('* Type "des." if you get out of DES                     *'), nl_log,
  write_log('*   from a Prolog interpreter                           *'), nl_log,
  write_log('*                                                       *'), nl_log,
  write_log('*                          Fernando Sáenz (c) 2004-2007 *'), nl_log,
  write_log('*                                             DISIA UCM *'), nl_log,
  write_log('*             Please send comments, questions, etc. to: *'), nl_log,
  write_log('*                                     fernan@sip.ucm.es *'), nl_log,
  write_log('*                                Visit the Web site at: *'), nl_log,
  write_log('*                           http://des.sourceforge.net/ *'), nl_log,
  write_log('*********************************************************'), nl_log.


/*********************************************************************/
/* Datalog Prompt                                                    */
/*********************************************************************/

des :-
  nl_log, 
  write_prompt,
  flush_output, 
  readln(S,_E), 
  write_only_to_log(S),
  nl_only_to_log,
  nl_log, process_input(S), !, 
  des.
des :-
  write_log('Error: Input processing error.'), nl_log, 
  des.

write_prompt :- write_log('DES> ').


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

end_of_file(C) :- (C = 26;C = -1).


/*********************************************************************/
/* Processing                                                        */
/*********************************************************************/

process_input(end_of_file) :- !.
process_input(Input) :-
  % cd and ls commands can end with a mandatory dot (e.g., "cd ." or "cd ..")
  (((Command=cd;Command=ls;Command=dir), (parse_command(Input,Command,[..],_VarsT); parse_command(Input,Command,[.],_VarsF))) -> 
   (Input=CInput, !);
   (my_append(CInput,".",Input); % Inputs are allowed to end with an optional dot
    CInput=Input)),
  (process_command(CInput); process_view(CInput); process_query(CInput); blank_input(CInput); invalid_input), !.

process_command(SCommand) :- 
  parse_command(SCommand,Command,Arguments,Vars), !,
  processC(Command,Arguments,Vars).

process_view(SRule) :-
  name(':-',[S,D]),
  my_appendfind(SHead,[S,D|SBody],SRule), !,
  parse_head(SHead,Head,[],V),
  parse_body(SBody,BodyList,V,_Vo),
  conjunctive_term(BodyList,Body),
  Rule =.. [':-',Head,Body],
  strata(CurrentStrata),
  pdg(CurrentPDG),
  assertz(datalog(Rule,[],[])),
  compute_stratification,
  solve_query(Head),
  retract(datalog(Rule,[],[])),
  abolishET,	%TODO: Restore previous state
  load_stratification(CurrentStrata,CurrentPDG).

process_query(SQuery) :-
  parse_query(SQuery,Query), !,
  solve_query(Query).

blank_input(SBlanks) :-
  my_blanks(SBlanks,[]).  

invalid_input :-
  write_log('Error: Input not recognized as a query, view or command.'), nl_log, 
  write_log('  Queries  : functor(Arg1,..,ArgN)'), nl_log, 
  write_log('  Views    : head :- body'), nl_log, 
  write_log('  Commands : /Command Argument'), nl_log, 
  write_log('Queries, views and commands can optionally end with a dot.'), nl_log.

processC(SQuit,[],_Vars) :- 
  (SQuit=q; SQuit=quit; SQuit=e; SQuit=exit; SQuit=halt), !, halt.
processC(SCommand,[C],_Vars) :- 
  (SCommand=shell; SCommand=s), !, my_shell(C,S),
  (S=0 -> (nl_log, write_log('Info: Operating system command executed.'));(write_log('Error: Operating system command failed.'), nl_log)).
processC(SHelp,[],_Vars) :- (SHelp=h; SHelp=help), !,
  write_log('Available Commands:'), nl_log,
  write_log(' * Rule Database Commands:'), nl_log,
  write_log('   - /consult filename     Consults a Datalog file, abolishing previous rules'), nl_log,
  write_log('   - /c filename           Shorthand for /consult filename'), nl_log,
  write_log('   - /[filenames]          Consults Datalog files, abolishing previous rules'), nl_log,
  write_log('   - /reconsult filename   Consults a Datalog file, keeping previous rules'), nl_log,
  write_log('   - /r filename           Shorthand for /reconsult filename '), nl_log,
  write_log('   - /[+filenames]         Consults Datalog files, keeping previous rules'), nl_log,
  write_log('   - /assert head:-body    Asserts a rule'), nl_log,
  write_log('   - /retract head:-body   Retracts a rule'), nl_log,
  write_log('   - /retractall head      Retracts all rules matching with head'), nl_log,
  write_log('   - /abolish              Abolishes all Datalog rules'), nl_log,
  write_log('   - /abolish name         Abolishes all Datalog rules matching a predicate name'), nl_log,
  write_log('   - /abolish name/arity   Abolishes all Datalog rules matching the pattern'), nl_log,
  write_log('   - /listing              Lists Datalog rules'), nl_log,
  write_log('   - /listing name         Lists Datalog rules matching a name'), nl_log,
  write_log('   - /listing name/arity   Lists Datalog rules matching the pattern'), nl_log,
  write_log(' * Extension Table Commands:'), nl_log,
  write_log('   - /list_et              Lists contents of the extension table'), nl_log,
  write_log('   - /list_et name         Lists contents of the extension table matching a name'), nl_log,
  write_log('   - /list_et name/arity   Lists contents of the extension table matching the pattern'), nl_log,
  write_log('   - /clear_et             Clears the extension table'), nl_log,
  write_log(' * Operating System Commands:'), nl_log,
  write_log('   - /cd path              Sets the current directory to path'), nl_log,
  write_log('   - /cd                   Sets the current directory to the one DES was started from'), nl_log,
  write_log('   - /pwd                  Displays the current directory'), nl_log,
  write_log('   - /ls                   Displays the contents of the current directory'), nl_log,
  write_log('   - /dir                  Synonym for /ls'), nl_log,
  write_log('   - /ls path              Displays the contents of the given directory'), nl_log,
  write_log('   - /dir path             Synonym for /ls path'), nl_log,
  write_log('   - /shell command        Submits command to the OS shell'), nl_log,
  write_log('   - /s command            Shorthand for /shell'), nl_log,
  write_log(' * Log Commands:'), nl_log,
  write_log('   - /log                  Displays the current log file, if any'), nl_log,
  write_log('   - /log filename         Sets the current log to filename'), nl_log,
  write_log('   - /nolog                Disables logging'), nl_log,
  write_log(' * Informative Commands:'), nl_log,
  write_log('   - /help                 Displays this help'), nl_log,
  write_log('   - /h                    Shorthand for /help'), nl_log,
  write_log('   - /builtins             Lists builtin operators'), nl_log,
  write_log('   - /verbose              Sets verbose output messages (default option)'), nl_log,
  write_log('   - /noverbose            Sets abbreviated messages'), nl_log,
  write_log('   - /strata               Displays the stratification for the loaded program'), nl_log,
  write_log('   - /pdg                  Displays the predicate dependency graph for the loaded program'), nl_log,
  write_log('   - /version              Displays the current system version'), nl_log,
  write_log(' * Miscellanea:'), nl_log,
  write_log('   - /prolog goal          Triggers Prolog evaluation for goal'), nl_log,
  write_log('   - /halt                 Quits DES'), nl_log,
  write_log('   - /quit,/q,/exit,/e     Synonyms/Shorthands for /halt'), nl_log,
  write_log('Any other expression is evaluated as a Datalog query.'), nl_log,
  write_log('Type des. if you get out of DES from a Prolog interpreter.'), nl_log.
processC(SConsult,Files,_Vars) :-
  (SConsult=consult; SConsult=c), !, 
  (Files=[] ->
    write_log('Warning: Nothing consulted.'), nl_log;
    abolishDL, abolishET, 
    remove_duplicates(Files,UFiles),
    consultDLlist(UFiles,false,Success),
    (Success -> compute_stratification; true)).
processC(SReconsult,Files,_Vars) :-
  (SReconsult=reconsult; SReconsult=r), !, 
  (Files=[] ->
    write_log('Warning: Nothing reconsulted.'), nl_log;
    abolishET, 
    remove_duplicates(Files,UFiles),
    consultDLlist(UFiles,false,Success),
    (Success -> compute_stratification; true)).
processC(assert,[R],_Vars) :-
  !, (R=(H:-true) -> DR=H; DR=R), 
  assertz(R), assertz(datalog(DR,[],[])),
  write_verb(['Info: Rule ',DR,' asserted.',nl]), 
  abolishET, 
  drilldown_stratification([DR]).
processC(retract,[R],_Vars) :-
  !, (R=(H:-true) -> DR=H; DR=R),
  ((retract(R), retract(datalog(DR,_,_))) -> 
    (write_verb(['Info: Rule ',DR,' retracted.',nl]), 
     abolishET, !, 
     rollup_stratification([DR]));
    (write_log('Warning: Cannot retract.'), nl_log)).
processC(retractall,[H],_Vars) :-
  !, dlrules(head,H,RV),
  (RV=[] ->
    write_log('Warning: Nothing retracted.'), nl_log;
    retractallDL(H),
    write_verb(['Info: Rule(s) retracted:',nl]),
    texify(RV,R,T),
    (verbose->write_log(T);true),
    abolishET, 
    rollup_stratification(R)).
processC(abolish,[],_Vars) :-
  !, abolishDL, 
  write_verb(['Info: Program abolished.',nl]), 
  abolishET, 
  reset_stratification.
processC(abolish,[N/A],_Vars) :-
  !, dlrules(namearity,N/A,RV), 
  (RV=[] -> 
    write_log('Warning: Nothing abolished.', nl_log);
    abolishDL(N/A),
    write_verb(['Info: Rule(s) retracted:',nl]),
    texify(RV,R,T),
    (verbose->write_log(T);true),
    abolishET, 
    rollup_stratification(R)).
processC(abolish,[N],_Vars) :-
  !, dlrules(predname,N,RV), 
  (RV=[] -> 
    nl_log, write_log('Warning: Nothing abolished.');
    abolishDL(N),
    write_verb(['Info: Rule(s) retracted:',nl]),
    texify(RV,R,T),
    (verbose->write_log(T);true),
    abolishET, 
    rollup_stratification(R)).
processC(listing,[],_Vars) :-
  !, list_rules.
processC(listing,[N/A],_Vars) :-
  !, list_rules(N,A).
processC(listing,[N],_Vars) :-
  !, list_rules(N).
processC(list_et,[],_Vars) :-
  !, list_et.
processC(list_et,[N/A],_Vars) :-
  !, list_et(N/A).
processC(list_et,[N],_Vars) :-
  !, list_et(N).
processC(clear_et,[],_Vars) :- 
  !, retractall(et(_E)), retractall(called(_C)), 
  write_verb([nl, 'Info: Extension table cleared.']).
processC(builtins,[],_Vars) :-
  !, list_builtins.
processC(prolog,[Goal],_Vars) :-
  !, Goal=..[Pred|Args], length(Args,L), length(VArgs,L), DPred =.. [Pred|VArgs],
  ((datalog(DPred,_,_) ; datalog((DPred :- _DBody),_,_)) -> solve_sld(Goal); 
   (write_log('Error: Undefined predicate.'), nl_log)).
processC(cd,[],_Vars) :-
  !, start_path(Path), cd_path(Path).
processC(cd,[Path],_Vars) :-
  !, cd_path(Path).
processC(pwd,[],_Vars) :-
  !, pwd_path.
processC(LS,[],_Vars) :-
  (LS=ls; LS=dir), !, ls.
processC(LS,[P],_Vars) :-
  (LS=ls; LS=dir), !, ls(P).
processC(verbose,[],_Vars) :-
  !, set_verbose,
  write_log('Info: Verbose output.'), nl_log.
processC(noverbose,[],_Vars) :-
  !, (verbose -> retract(verbose); true),
  write_log('Info: Abbreviated output.'), nl_log.
processC(strata,[],_Vars) :-
  !, (strata(S) -> 
       (write_log(S), nl_log); 
       (write_log('Warning: Strata not yet computed.'), nl_log)).
processC(pdg,[],_Vars) :-
  !, (pdg((N,A)) -> 
       (write_log('Nodes: '), write_log(N), nl_log, write_log('Arcs : '), write_log(A), nl_log); 
       (write_log('Info: Predicate Dependency Graph not yet computed.'), nl_log)).
processC(log,[],_Vars) :-
  !, (log(F,_S) -> 
       (write_log('Info: Currently logging to '), write_log(F), write_log('.'), nl_log);
       (write_log('Info: Logging currently disabled.'), nl_log)).
processC(nolog,[],_Vars) :-
  !, (log(F,S) -> 
       (close(S), retract(log(F,S)), write_log('Info: Logging to '), write_log(F), write_log(' disabled.'), nl_log);
       (write_log('Warning: Logging already disabled.'), nl_log)).
processC(log,[F],_Vars) :-
  !, (log(CF,_S) -> 
       (write_log('Warning: Currently logging to '), write_log(CF), 
        write_log('. First, use /nolog for disabling the current log.'), nl_log);
       (my_absolute_filename(F,AFN),
        my_dir_file(AFN,AP,_FN),
        (my_directory_exists(AP) ->
         (open(AFN,write,S),
          assertz(log(AFN,S)), 
          write_verb(['Info: Logging enabled to ',AFN,'.',nl]));
         (write_log('Warning: Directory '), write_log(AP), write_log(' does not exist.'), nl_log)
         )
        )
       ). 
processC(version,[],_Vars) :-
  !, des_version(V), write_log(V), nl_log.
processC(debug,[Goal],Vars) :-
  !, write_log(Goal), nl_log, write_log(Vars), nl_log.
processC(_Error,_L,_Vars) :-
  !, write_log('Error: Unknown command or incorrect number of arguments. Use /help for help.'), nl_log.

set_verbose :-
  (verbose -> true; assertz(verbose)).

write_verb(L) :- (verbose -> write_verbL(L); true).
write_verbL([]).
write_verbL([T|Ts]) :-
  (T=nl, nl_log; write_log(T)),
  write_verbL(Ts).

texify([],[],[]).
texify([(T,L)|RVs],[R|Rs],[T|Ts]) :-
  copy_term(T,R),
  my_list_unify(L),
  texify(RVs,Rs,Ts).


/*********************************************************************/
/* Parsing                                                           */
/*********************************************************************/

parse_command(SCommand,Command,Arguments,Vars) :-
  parse_command(Command,Arguments,Vars,SCommand,[]), !.

parse_command(reconsult,Files,[]) -->
  my_blanks, "/", my_blanks, "[+", my_arguments(Files), "]".
parse_command(consult,Files,[]) -->
  my_blanks, "/", my_blanks, "[", my_arguments(Files), "]".
parse_command(Command,[],[]) -->
  my_blanks, "/", my_blanks, my_command(Command).
parse_command(prolog,[Goal],[]) -->
  my_blanks, "/", my_blanks, my_command(prolog), " ", my_blanks, my_term(Goal,[],_Vout). 
parse_command(abolish,[N/A],[]) -->
  my_blanks, "/", my_blanks, my_command(abolish), " ", my_blanks, my_pattern(N/A).
parse_command(list_et,[N/A],[]) -->
  my_blanks, "/", my_blanks, my_command(list_et), " ", my_blanks, my_pattern(N/A).
parse_command(listing,[N/A],[]) -->
  my_blanks, "/", my_blanks, my_command(listing), " ", my_blanks, my_pattern(N/A).
parse_command(retract,[Rule],[]) -->
  my_blanks, "/", my_blanks, my_command(retract), " ", my_blanks, my_rule(Rule,[],_Vars).
parse_command(retractall,[Head],[]) -->
  my_blanks, "/", my_blanks, my_command(retractall), " ", my_blanks, my_term(Head,[],_Vars).
parse_command(assert,[Rule],[]) -->
  my_blanks, "/", my_blanks, my_command(assert), " ", my_blanks, my_rule(Rule,[],_Vars).
% Yolanda: en Vars se devuelve una lista de 'NombreTextualDeLaVariable'=Variable
parse_command(debug,[Goal],Vars) -->
  my_blanks, "/", my_blanks, my_command(debug), " ", my_blanks, my_term(Goal,[],Vars), 
  {deb(Vars)}.
parse_command(Command,Arguments,[]) -->
  my_blanks, "/", my_blanks, my_command(Command), " ", my_blanks, 
  my_arguments(Arguments), my_blanks.

parse_query(SQuery,Term) :-
  my_term(Term,[],_Vo,SQuery,[]).

parse_head(SHead,Term,Vi,Vo) :-
  my_term(Term,Vi,Vo,SHead,[]).

parse_body(SBody,Terms,Vi,Vo) :-
  my_terms(Terms,Vi,Vo,SBody,[]).

my_command(Command) --> my_chars(Cs), {name(Command,Cs)}.

my_arguments([]) --> [].
my_arguments([A|As]) --> my_charsbutcomma(Cs), my_blanks, ",", my_blanks, {name(A,Cs)}, my_arguments(As).
my_arguments([A]) --> my_charsbutcomma(Cs), {name(A,Cs)}.

my_charsbutcomma([C]) --> my_charbutcomma(C).
my_charsbutcomma([C|Cs]) --> my_charbutcomma(C), my_charsbutcomma(Cs).

my_charbutcomma(C) --> [C], {C\=","}.

my_term(T,Vi,Vo) --> my_blanks, my_atom(F), my_blanks, "(", my_blanks, my_terms(LT,Vi,Vo), my_blanks, ")", my_blanks, {T =.. [F|LT]}.
my_term(F,V,V) --> my_blanks, my_atom(F), my_blanks.
my_term(T,Vi,Vo) --> my_blanks, my_noncompoundterm(L,Vi,Vo1), my_blanks, my_binary_primitive(P), my_blanks, my_noncompoundterm(R,Vo1,Vo), my_blanks, {T =.. [P,L,R]}.
my_term(V,Vi,Vo) --> my_blanks, my_variable(V,Vi,Vo), my_blanks.

my_terms([T|Ts],Vi,Vo) --> my_term(T,Vi,Vo1), my_blanks, ",", my_blanks, my_terms(Ts,Vo1,Vo).
my_terms([T],Vi,Vo) --> my_term(T,Vi,Vo).

my_blanks --> [].
my_blanks --> " ", my_blanks.

my_atom(A) --> my_lowercase(C), my_chars(Cs), {name(A,[C|Cs])}.
my_atom(A) --> my_lowercase(C), {name(A,[C])}.
my_atom(A) --> "'", my_chars(Cs), "'", {name(A,Cs)}.
my_atom(A) --> my_number(A).

my_lowercase(C) --> [C], {C>="a", C=<"z"}.
my_uppercase(C) --> [C], {C>="A", C=<"Z"}.
my_digit(C) --> [C], {C>="0", C=<"9"}.

% my_chars: one or more digits, letters or underscores
my_chars([C]) --> my_char(C).
my_chars([C|Cs]) --> my_char(C), my_chars(Cs).

my_char(C) --> my_lowercase(C).
my_char(C) --> my_uppercase(C).
my_char(C) --> my_digit(C).
my_char(95)--> "_".

my_variable(V,Vi,Vo) --> my_uppercase(C), {name(N,[C]),append_var(N,V,Vi,Vo)}.
my_variable(V,Vi,Vo) --> my_uppercase(C), my_chars(Cs), {name(N,[C|Cs]),append_var(N,V,Vi,Vo)}.
my_variable(V,Vi,Vo) --> "_", my_chars(Cs), {name(N,["_"|Cs]),append_var(N,V,Vi,Vo)}.

my_number(N) --> my_negative_number(N).
my_number(N) --> my_positive_number(N).

my_negative_number(N) --> "-",my_positive_number(P), {N is -P}.

my_positive_number(N) --> my_digits(Is), ".", my_digits(Ds), {concatLsts([Is,".",Ds],Ns),name(N,Ns)}.
my_positive_number(N) --> my_digits(Ds), {name(N,Ds)}.

my_digits([D]) --> my_digit(D).
my_digits([D|Ds]) --> my_digit(D), my_digits(Ds).

my_binary_primitive('=<') --> "=<".
my_binary_primitive('=')  --> "=".
my_binary_primitive(D)    --> [92,61], {name(D,[92,61])}.  % Avoids typing \=
my_binary_primitive('>=') --> ">=".
my_binary_primitive('>')  --> ">".
my_binary_primitive('<')  --> "<".

my_noncompoundterm(A,V,V) --> my_atom(A).
my_noncompoundterm(A,Vi,Vo) --> my_variable(A,Vi,Vo).

my_rule((H:-B),Vi,Vo) --> my_blanks, my_term(H,Vi,Vo1), my_blanks, ":-", my_blanks, my_body(B,Vo1,Vo), my_blanks.
my_rule(H,Vi,Vo) --> my_blanks, my_term(H,Vi,Vo), my_blanks.

my_body(B,Vi,Vo) --> my_terms(Ts,Vi,Vo), {list_cst(Ts,B)}.

my_pattern(N/A) --> my_blanks, my_atom(N), my_blanks, "/", my_blanks, my_atom(A), my_blanks.

% list_cst(+List, -CST) Outputs a term of comma-separated subterms
list_cst([A],A).
list_cst([A|As],(A,TAs)) :- list_cst(As,TAs).

% Appending a pair (VariableName,Variable) to an input list
append_var(N,V,Vi,Vi) :- 
  my_member('='(N,V),Vi), !.
append_var(N,V,Vi,['='(N,V)|Vi]).


/*********************************************************************/
/* Solving Prolog Goals                                              */
/*********************************************************************/

solve_sld(Goal) :- 
  call(Goal), write_log(Goal), nl_log, 
  write_log('? (type ; for more solutions, <Intro> to continue) '), 
  readln(S,_), 
  write_only_to_log(S), nl_only_to_log, 
  S=="", nl_log, write_log(yes), nl_log.
solve_sld(_Goal) :- write_log(no), nl_log.


/*********************************************************************/
/* Solving Datalog Queries                                           */
/*********************************************************************/

solve_query(Query) :-
  strata(S),
  (S==[] ->
    solve_datalog(Query,Undefined);        % No program was yet loaded; so, no strata computed
    (S==[non-stratifiable] -> 
     try_solve_stratified(Query,Undefined); % Although in a non-stratifiable program, try to solve for the given query, hopefully finding a stratifiable subprogram
     solve_stratified(Query,Undefined))),   % Stratifiable program: stratum solving
  display_solutions(Query,Undefined).

try_solve_stratified(Query,Undefined) :-
  (Query=not(Q) -> true; Query=Q),
  pdg(G),
  functor(Q,N,A),
  sub_pdg(N/A,G,SG),
  stratify(NS,SG,B), !,
  (B==false -> 
   (write_log('Warning: Unable to ensure correctness for this query.'), nl_log,
    solve_datalog(Query,Undefined)); % Non-stratifiable yet
   (write_log('Info: Stratifiable subprogram found for the given query.'), nl_log,
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
   solve_datalog(Query,Undefined);
   strata(S),
   sort_by_strata(S,ND,SR),
   build_queries(SR,Queries),
   write_verb(['Info: Computing by stratum of ',Queries, '.',nl]),
   solve_queries(Query,Undefined,Queries)).

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
  my_member(P,R), !,
  filterdrop(Xs,R,Ps).
filterdrop([(_S,_P)|Xs],R,Ps) :-
  filterdrop(Xs,R,Ps).

build_queries([],[]).
build_queries([N/A|Ps],[Q|Qs]) :-
  length(L,A),
  Q =.. [N|L],
  build_queries(Ps,Qs).

solve_queries(Query,Undefined,[Q|Qs]) :-
  solve_datalog(Q,_U),
  solve_queries(Query,Undefined,Qs).
solve_queries(Query,Undefined,[]) :-
  solve_datalog(Query,Undefined).
  
solve_datalog(Query,_Undefined) :- 
  solve_star(Query). % This call is always made to fail
solve_datalog(_Query,Undefined) :-
  remove_undefined(Undefined).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fixpoint Computation: solve_star
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_star(Q) :-
  repeat,
  (remove_calls,
   et_not_changed,       % Sets a flag indicating that the extension table has not changed 
   solve(Q),             % Solves the call through memoization
   fail;                 % Requests all alternatives
   no_change,!,fail).    % When no more alternatives, restart the computation if the extension table has changed,
                         % otherwise, fail and exit

% Building a Predicate with Fresh Variables
duplicate(Q) :-
  build(Q,G), et(G), equal_up_to_vars(Q,G).

/*********************************************************************/
/* Testing whether two predicates are equal up to renaming variables */
/*********************************************************************/

equal_up_to_vars(not(A),not(NA)) :-
  !, equal_up_to_vars(A,NA).
equal_up_to_vars(G,Q) :-
  G =.. [P|Args],
  Q =.. [P|NArgs],
  equal_up_to_vars_list(Args,NArgs).

equal_up_to_vars_list([],[]).
equal_up_to_vars_list([A|As],[NA|NAs]) :-
  var(A), !, var(NA), equal_up_to_vars_list(As,NAs).
equal_up_to_vars_list([A|As],[NA|NAs]) :-
  nonvar(NA), A=NA,
  equal_up_to_vars_list(As,NAs).

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
  retract(et_flag(_F)),
  assertz(et_flag(yes)).

/*********************************************************************/
% Setting Extension Table Flag to 'not changed'
/*********************************************************************/

et_not_changed :-
  retract(et_flag(_F)),
  assertz(et_flag(no)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solving with Extension Table: solve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(true) :- !.
solve((G,Gs)) :- !, solve(G), solve(Gs).
solve(G) :- memo(G).

% Already called. Extension table with an entry for the current call
memo(G) :-
  build(G,Q),           % Builds in Q the same call with fresh variables
  called(Q),            % Tries to find a unifiable call in the extension table for the current call
  my_subsumes(Q,G), !,  % Tests whether the extension table call subsumes the current call
  et(G).                % If so, use the result in the extension table; otherwise, process the new call with the next memo clause
% New call. Extension table without an entry for the current call
memo(G) :-
  assertz(called(G)), % Asserts the current call in the extension table (because: 1) there is no previous call to G, or (2) G is not subsumed by a previous call to G
  (et(G);             % If the et is not empty, the first call have to return all the possible answers computed in a previous pass of the fixpoint computation
   (solve_goal(G),    % Solves the current call using its matching rules
    build(G,Q),       % Builds in Q the same call with fresh variables
    my_not((et(Q),    % Tests whether there is not an entry in the extension table subsuming the current result
            my_subsumes(Q,G))),
    assertz(et(G)),   % Asserts the new result
    et_changed)).     % Sets a flag indicating that the extension table has changed

% Solving a Goal: solve_goal
solve_goal(not(G)) :- % Negation; follows the et mechanism
  !, solve_not(G).
solve_goal(G) :-      % Primitives do not use the et mechanism
  exec_primitive(G).
solve_goal(G) :-      % Solves a goal using all of its matching rules
  (datalog((G:-B),_,_); (datalog(G,_,_),B=true)),
%  clause(G,B), 
  solve(B).

% Solving Negation: solve_not
solve_not(G) :- solve(G), !, fail.
solve_not(_G).

% Executing Prolog Primitives: exec_primitive
exec_primitive(A=B)  :- A=B.
exec_primitive(A\=B) :- A\=B.
exec_primitive(A>B)  :- my_not(vars([A])), my_not(vars([B])), ((number(A), number(B)) -> A>B  ; A@>B).
exec_primitive(A>=B) :- my_not(vars([A])), my_not(vars([B])), ((number(A), number(B)) -> A>=B ; A@>=B).
exec_primitive(A<B)  :- my_not(vars([A])), my_not(vars([B])), ((number(A), number(B)) -> A<B  ; A@<B).
exec_primitive(A=<B) :- my_not(vars([A])), my_not(vars([B])), ((number(A), number(B)) -> A=<B ; A@=<B).
 
% Building a Predicate with Fresh Variables
build(A,B) :-
  copy_term(A,B).

% Testing whether a term T1 subsumes a term T2
% i.e., T1 is 'more general' than T2

my_subsumes(General,Specific) :-
  \+ \+ (make_ground(Specific),
         General=Specific).

% Instantiates all variables in Term to fresh constants.

make_ground(Term) :-
  numbervars(Term, 0, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove_undefined
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_undefined(Undefined) :-
  setof(Fact, 
        (et(not(Fact)), et(Fact)
         , retract(et(not(Fact))), retract(et(Fact))), 
        Undefined).
remove_undefined([]).


/*********************************************************************/
/* Building a predicate dependency graph: build_pdg/1                */
/*********************************************************************/
% A predicate dependency graph is a pair of a list of predicate nodes 
% (name/arity), and a list of arcs (nto/ato + nfrom/afrom, meaning that
% the predicate nto/ato depends on -has in the rhs of any of its defining
% rules- the predicate nfrom/afrom; alternatively, nto/ato - nfrom/afrom 
% is used when the predicate nfrom/afrom appears negated)
% Follows [ZCF+97]

build_pdg((Nodes,Arcs)) :-
  write_verb(['Info: Computing predicate dependency graph...',nl]),
  (setof(LLArcs,find_pdg_arcs(LLArcs),LLArcs);LLArcs=[]),
  concatLsts(LLArcs,LArcs),
  remove_duplicates(LArcs,Arcs),
  setof(N/A, 
        H^B^LN^V^((datalog(':-'(H,B),V,LN);datalog(H,V,LN)),functor(H,N,A),N\==(':-')), 
        LHSNodes),
  rhsnodes(Arcs,RHSNodes),
  my_merge(LHSNodes,RHSNodes,Nodes).
build_pdg(([],[])).

find_pdg_arcs(Arcs) :-
  datalog(':-'(H,B),_,_), functor(H,N,A), pdg_arcs_from_to(B,N/A,Arcs).

pdg_arcs_from_to((B,Bs),P,[Arc|Arcs]) :-
  pdg_arc(B,P,Arc), !, pdg_arcs_from_to(Bs,P,Arcs).
pdg_arcs_from_to(B,P,[Arc]) :-
  pdg_arc(B,P,Arc).

pdg_arc(not(T),P,P-N/A) :- % Negative dependency
  functor(T,N,A), !.
pdg_arc(T,P,P+N/A) :-      % Positive dependency
  functor(T,N,A), !.

rhsnodes([],[]).
rhsnodes([Arc|Arcs],[N/A|Nodes]) :-
  (Arc = P-N/A; Arc = P+N/A),
  rhsnodes(Arcs,Nodes).


/*********************************************************************/
/* Building a predicate dependency subgraph: sub_pdg/3               */
/*********************************************************************/
% Given a starting node N and a pdg G, build the subgraph of nodes reachable from N in G

sub_pdg(N,(_Nodes,Arcs),(SNodes,SArcs)) :-
  setof(Path,N^Q^Arcs^reachable(N,Q,Arcs,[],Path),Path),
  concatLsts(Path,G),
  remove_duplicates(G,SArcs),
  nodes_in(SArcs,DNodes),
  remove_duplicates(DNodes,SNodes).

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
  (Change=false -> So=Sj, Success=true;
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
/* Computing Stratification: compute_stratification/0                        */
/*********************************************************************/

compute_stratification :-
  clear_stratification,
  build_pdg(G),
  stratify(S,G,Success),
  assertz(pdg(G)),
  (Success=true -> assertz(strata(S)); 
   assertz(strata([non-stratifiable])), 
   write_log('Warning: Non stratifiable program.'), nl_log).


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
/* Listing Datalog Rules (Command): list_rules/0                     */
/*********************************************************************/

% TODO: Order the rules
list_rules :- datalog(R,Vs,_), write_with_vars(R,Vs), write_log('.'), nl_log, fail.
list_rules.


/*********************************************************************/
/* Listing Datalog Rules matching name and arity (Command): list_rules/2  */
/*********************************************************************/

% TODO: Order the rules
list_rules(N,A) :- 
  datalog(R,Vs,_), (R=':-'(H,_B); H=R), functor(H,N,A), 
  write_with_vars(R,Vs), write_log('.'), nl_log, fail.
list_rules(_N,_A).


/*********************************************************************/
/* Listing Datalog Rules matching a name (Command): list_rules/1     */
/*********************************************************************/

% TODO: Order the rules
list_rules(N) :- 
  list_rules(N,_A).


/*********************************************************************/
/* Listing Extension Table Contents (Command): list_et               */
/*********************************************************************/

list_et :- 
  write_log('Answers:'), nl_log, list_answers,
  write_log('Calls:'), nl_log, list_calls.

list_answers :-
  bagof(Fact, et(Fact), Set), !, display_ordered_set(Set).
list_answers :-
  display_ordered_set([]).

list_calls :-
  bagof(Fact, called(Fact), Set), !, display_ordered_set(Set).
list_calls :- 
  display_ordered_set([]).

/*********************************************************************/
/* Listing Extension Table Contents matching a Pattern (Command): list_et/1 */
/*********************************************************************/
% Negative information regarding pattern must be explicitly recalled

list_et(N/A) :- 
  !, 
  write_log('Answers:'), nl_log, list_answers(N/A),
  write_log('Calls:'), nl_log, list_calls(N/A).
list_et(N) :- 
  list_et(N/_A).

list_answers(N/A) :- 
  bagof(Fact,
        F^N^A^(
         (et(Fact), functor(Fact,N,A)); 
         (et(Fact), (Fact=not(F)), functor(F,N,A)) 
        ),
        Set), !,
  display_ordered_set(Set).
list_answers(_N/_A) :- 
  display_ordered_set([]).

list_calls(N/A) :- 
  bagof(Fact,
        F^N^A^(
         (called(Fact), functor(Fact,N,A)); 
         (called(Fact), (Fact=not(F)), functor(F,N,A)) 
        ),
        Set), !,
  display_ordered_set(Set).
list_calls(_N/_A) :- 
  display_ordered_set([]).


/*********************************************************************/
/* Listing Builtins (Command): list_builtins                         */
/*********************************************************************/

list_builtins :-
  write_log('  =          syntactic equality'), nl_log,
  write_log('  \\=         syntactic disequality'), nl_log,
  write_log('  >          greater than'), nl_log,
  write_log('  >=         greater or equal than'), nl_log,
  write_log('  <          less than'), nl_log,
  write_log('  =<         less or equal than'), nl_log,
  write_log('  not(goal)  stratified negation'), nl_log.


/*********************************************************************/
/* Abolishing Datalog Rules (Command): abolishDL/1                   */
/*********************************************************************/

abolishDL(N/A) :- !, length(L,A), H =.. [N|L], retractallDL(H).
abolishDL(N) :- 
  datalog(R,_,_), 
  (R =.. [N|_],H=R; R=':-'(H,_B),functor(H,N,_A)), 
  retractallDL(H), fail.
abolishDL(_).


/*********************************************************************/
/* Abolishing Datalog Rules (Command): abolishDL                     */
/*********************************************************************/

abolishDL :- datalog(R,_,_), retract(datalog(R,_,_)), retract(R), fail.
abolishDL.


/*********************************************************************/
/* Abolishing Extension Table: abolishET                             */
/*********************************************************************/

abolishET :- retractall(et(_F)), retractall(called(_C)).


/*********************************************************************/
/* Displaying Solutions of a Datalog Query: display_solutions       */
/*********************************************************************/

display_solutions(G,U) :-
  et_entries(G,L),
  display_ordered_set(L),
  display_undefined(U).

et_entries(S,L) :-
  bagof(S,et(S),L), !.
et_entries(_S,[]).

display_undefined([]) :- !.
display_undefined(U) :-
  write_log('Undefined:'), nl_log,
  display_ordered_set(U).
  
display_ordered_set(L) :-
  write_log('{'),
  my_sort(L,OL),
  display_list(OL),
  nl_log, write_log('}'), nl_log.

display_list([]).
display_list([X]) :-
  nl_log, write_log('  '), write_log(X). 
display_list([X,Y|Xs]) :-
  nl_log, write_log('  '), write_log(X), write_log(','), 
  display_list([Y|Xs]).

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
  seeing(OldInput),
  try_open(F),            % Open file F
  write_log('Info: Consulting '), write_log(F), write_log('...'), nl_log,
  repeat,
    my_read(T,Vs,Ls),     % Read a term, along with its variable names and line numbers
    process_term(T,Vs,Ls),   % Process it
    T == end_of_file,     % Loop back if not at end of file
    !,
  seen,                   % Close the file
  see(OldInput).
consultDL(F,false) :-
  write_log('Error: Reading file '), write_log(F), write_log('.'), nl_log.

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

process_term(end_of_file,_Vs,_Ls).
process_term(T,Vs,Ls) :-
  (verbose -> write_log('  '), write_with_vars(T,Vs), write_log('.'), nl_log; true), 
  assertz(T), assertz(datalog(T,Vs,Ls)).


/*********************************************************************/
/* Finding Datalog Rules matching a pattern: dlrules/3               */
/*********************************************************************/

dlrules(head,H,L) :-
  (setof((R,V),H^B^LN^((R=H;R=':-'(H,B)),datalog(R,V,LN)),L) -> true; L=[]).
dlrules(predname,N,L) :-
  (setof((R,V), H^B^LN^V^A^((R=':-'(H,B);R=H),datalog(R,V,LN),functor(H,N,A),N\==(':-')),L) -> true; L=[]).
dlrules(namearity,N/A,L) :-
  (setof((R,V),H^B^LN^N^A^((R=H;R=':-'(H,B)),functor(H,N,A),datalog(R,V,LN)),L) -> true; L=[]).


/*********************************************************************/
/* Auxiliary Predicates                                              */
/*********************************************************************/

% Appending two lists
my_append([],X,X).
my_append([X|Xs],Y,[X|Zs]) :- my_append(Xs,Y,Zs).

% Appending two lists for finding substrings
my_appendfind([],X,X) :- !.
my_appendfind([X|Xs],Y,[X|Zs]) :- my_appendfind(Xs,Y,Zs).

% Member of a list
%my_member(X,[X|_Xs]) :- !.
my_member(X,[X|_Xs]).
my_member(X,[_Y|Xs]) :- my_member(X,Xs).

% List to set
remove_duplicates(L,S) :-
  remove_duplicates(L,[],S).

remove_duplicates([],L,L).
remove_duplicates([X|Xs],AL,L) :-
  my_member(X,AL), !,
  remove_duplicates(Xs,AL,L).
remove_duplicates([X|Xs],AL,L) :-
  remove_duplicates(Xs,[X|AL],L).

% Merging two lists; the first one contains no duplicates
my_merge(L,[],L).
my_merge(L,[A|As],Rs) :-
  my_member(A,L), !,
  my_merge(L,As,Rs).
my_merge(L,[A|As],Rs) :-
  my_merge([A|L],As,Rs).

% Maximum of two numbers
my_max(A,B,A) :- A>=B,!.
my_max(_A,B,B).

% Logical disjunction
my_or(true,true,true).
my_or(true,false,true).
my_or(false,true,true).
my_or(false,false,false).

% Retractall
retractallDL(H) :-
  retractall(H),
  retractall(datalog(H,_,_)),
  retractall(datalog((H:-_B),_,_)).

% Prolog implementation of negation
my_not(G) :- call(G), !, fail.
my_not(_G).

% Testing whether the input list contains variables
vars([]).
vars([V|Vs]) :- var(V), vars(Vs).

% Appending a list of lists returning the concatenated list

concatLsts([],[]).
concatLsts([[]|R],S):-concatLsts(R,S).
concatLsts([[C|R1]|R2],[C|S]):-concatLsts([R1|R2],S).

% Building a conjunctive term
conjunctive_term([T],T) :- !.
conjunctive_term([T1,T2],CT) :- 
  !, CT =.. [',',T1,T2].
conjunctive_term([T|Ts],CT) :- 
  CT =.. [',',T,CTT],
  conjunctive_term(Ts,CTT).

% Opening a File

try_open(F) :- my_file_exists(F), !, see(F).
try_open(F) :- name(F,L), my_append(L,".dl",FPL), name(FP,FPL), my_file_exists(FP), !, see(FP).

% Changing the Current Path

cd_path(Path) :-
  (my_directory_exists(Path), !,
   my_change_directory(Path),
   pwd_path;
   write_log('Error: Cannot access the given path.'), nl_log).

% Displaying the Current Path

pwd_path :-
  write_log('Info: Current directory is:'), nl_log, write_log('  '), my_working_directory(Path), write_log(Path), nl_log.

% Listing Directory Contents

ls :-
  my_working_directory(WorkingPath),
  ls(WorkingPath).

ls(Path) :-
  (my_not(my_directory_exists(Path)) ->
   (write_log('Warning: Path '), write_log(Path), write_log(' does not exist.'), nl_log);
   (my_directory_files(Path, List),
    my_absolute_filename(Path, AbsolutePath),
    write_log('Info: Contents of '), write_log(AbsolutePath), nl_log, nl_log, 
    write_log('Files:'), write_dir_files(List, Path), nl_log,
    write_log('Directories:'), write_dir_directories(List, Path), nl_log)).

% Writing each File in a Directory. Path comes without final slash

write_dir_files([],_Path).
write_dir_files([F|Fs],Path) :-
  name(Path,SPath), name(F,SF), concatLsts([SPath,"/",SF],SFN), name(FN,SFN),
  my_is_file(FN), !, nl_log, write_log('  '), write_log(F), write_dir_files(Fs,Path).
write_dir_files([_F|Fs],Path) :-
  write_dir_files(Fs,Path).

% Writing each Directory in a Directory

write_dir_directories([],_Path).
write_dir_directories([F|Fs],Path) :-
  name(Path,SPath), name(F,SF), concatLsts([SPath,"/",SF],SFN), name(FN,SFN),
  my_is_directory(FN), F \== '.', F \== '..', !, nl_log, write_log('  '), write_log(F), write_dir_directories(Fs,Path).
write_dir_directories([_F|Fs],Path) :-
  write_dir_directories(Fs,Path).

% Writing terms with their textual variable names

write_with_vars(T,Vs) :-
  my_list_unify(Vs), write_log(T), fail.
write_with_vars(_T,_Vs).

my_list_unify([]) :- !.
my_list_unify([X=X|Ls]) :- 
  my_list_unify(Ls).


% Output: both current stream and log file, if enabled

write_log(X) :-
  write(X), (log(_F,S) -> write(S,X); true).

write_only_to_log(S) :-
  (log(_F,H) -> name(X,S), write(H,X); true).

nl_log :-
  nl, nl_to_log.

nl_only_to_log :-
  nl_to_log.

nl_to_log :-
  (log(_F,H) -> nl(H); true).


% Writing a string

write_string(S) :-
  name(A,S),
  write_log(A).

  
% Debugging when developing

deb([]).
deb([X|Xs]) :-
  nl, write(X), deb(Xs).
  
%The following is needed for Ciao, it does not work with SWI but do for others
:- initialization((start;true)).
