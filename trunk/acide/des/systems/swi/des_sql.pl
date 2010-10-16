/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.2.0                 */
/*                                                       */
/*    SQL Subsystem                                      */
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
/* - CREATE [OR REPLACE] TABLE                           */
/* - CREATE [OR REPLACE] VIEW                            */
/* - DROP TABLE                                          */
/* - DROP VIEW                                           */
/* - INSERT INTO ... VALUES ...                          */
/* - INSERT INTO ... SQL                                 */
/* - DELETE FROM ... [WHERE ...]                         */
/* - Subqueries defining relations                       */
/* - Relation and attribute autorenaming                 */
/* - Correlated subqueries in EXISTS and IN conditions   */
/* - UNION, INTERSECT, EXCEPT|MINUS                      */ 
/* - WITH for recursive views                            */
/* - Projection list wildcards: Relation.*, *            */
/* - Subqueries in comparisons (=, <, >, ...)            */
/* - Expressions in projection list                      */ 
/* - NULL values and outer joins following SQL-2 standard*/
/* - Aggregate functions in projection list and having   */
/*   condition                                           */
/* - GROUP BY clauses                                    */
/* Limitations:                                          */
/* - No multiset answers                                 */
/* TODO:                                                 */
/* - Allow SQL statements in projection list             */
/* - Infer the type of an expression wrt. attribute types*/
/* - Reject SQL sentences with attributes in the         */
/*   projection list which are not in the grouping list  */
/* - 'Identifier not declared' warnings in parsing       */
/*   (e.g., attributes, tables, views,...)               */
/* - Unbalanced parentheses warning                      */
/* - Parametric SQL sentences                            */
/* - More compact SQL to Datalog code translations       */
/* - Operator precedences (not,and,or) in SQL arithmetic */
/*   expressions                                         */
/* - Warn when a view or table is dropped and other      */
/*   view depends on it                                  */
/* - Generate code for distinguished                     */
/*   SELECT ALL and SELECT DISTINCT                      */
/* - ORDER BY                                            */
/* - ALL in comparison subqueries                        */
/* - FROM-less statements for computing expressions      */
/*********************************************************/

% ::WARNING 
% /abolish deletes rules from view definitions, but the schema remains
% Uncaught error:
% create or replace table t(a string primary key)
% create or replace view v(a) as select * from t where a<'x' or b>'y'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SQL Grammar for Valid SQL statements:
% Here, terminal symbols are: parentheses, commas, semicolons, 
% single dots, asterisks, and apostrophes.
% Other terminal symbols are written in capitals.
% The notation T,...,T means one or more occurrences of T, comma-separated
% The notation T ... T means one or more occurrences of T, blank-separated

% CAVEAT: Computable SQL statements follow the grammar in the manual.
%         The following grammar parses extra features which cannot
%         be computed yet (all, order by, ...)

% SQLstmt ::=
%   DDLstmt[;]
%   |
%   DMLstmt[;]
%   |
%   DQLstmt[;]

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DDL (Data Definition Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DDLstmt ::=
%   CREATE [OR REPLACE] TABLE CompleteConstrainedSchema
%   |
%   CREATE [OR REPLACE] VIEW CompleteSchema AS DQLstmt
%   |
%   DROP TABLE TableName
%   |
%   DROP VIEW ViewName
%   |
%   DROP DATABASE
%   
% Schema ::=
%   RelationName
%   |
%   CompleteSchema
%   
% CompleteConstrainedSchema ::=
%   RelationName(Att Type [ColumnConstraint ... ColumnConstraint],...,Att Type [ColumnConstraint ... ColumnConstraint] [, TableConstraints])
%   
% CompleteSchema ::=
%   RelationName(Att Type,...,Att Type)
%
% Type ::=
%   CHAR(n)  % fixed-length string of n characters
%   |
%%   CHARACTER(n)  % equivalent to the former
%%   |
%   CHAR  % fixed-length string of 1 character
%   |
%   VARCHAR(n)  % variable-length string of up to n characters
%   |
%   VARCHAR2(n)  % Oracle's variable-length string of up to n characters
%   |
%   VARCHAR  % variable-length string of up to the maximum length of the underlying Prolog atom
%   |
%   STRING  % As VARCHAR
%   |
%%   CHARACTER VARYING(n)  % equivalent to the former
%%   |
%   INT
%   |
%   INTEGER  % equivalent to the former
%   |
%%   SMALLINT
%%   |
%%   NUMERIC(p,d) % a total of p digits, where d of those are in the decimal place
%%   |
%   REAL
%   |
%%   DOUBLE PRECISION  % equivalent to the former
%%   |
%%   FLOAT(n)  % with precision of at least n digits
%%   |
%%   DATE % four digit year, month and day
%%   |
%%   TIME % hours, minutes and seconds
%%   | 
%%   TIMESTAMP % combination of date and time
%
%
% ColumnConstraint ::=
%   PRIMARY KEY
%   |
%   REFERENCES TableName[(Att)]
%   
% TableConstraints ::=
%   TableConstraint,...,TableConstraint
%
% TableConstraint ::=
%   PRIMARY KEY (Att,...,Att)
%   |
%   FOREIGN KEY (Att,...,Att) REFERENCES TableName[(Att,...,Att)]
%   
% RelationName is a user identifier for naming tables, views and aliases
% TableName is a user identifier for naming tables
% ViewName is a user identifier for naming views
% Att is a user identifier for naming relation attributes

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DML (Data Manipulation Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DMLstmt ::=
%   INSERT INTO TableName VALUES (Cte,...,Cte)
%   |
%   INSERT INTO TableName DQLstmt
%   |
%   DELETE FROM TableName
%   |
%   DELETE FROM TableName WHERE Condition
%   |
%   UPDATE TableName SET Att1=Expr1,...,Attn=Exprn WHERE Condition

% Cte is a constant

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements:
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DQLstmt ::=
%   (DQLstmt) 
%   |
%   UBSQL

% UBSQL ::= 
%   SELECTstmt
%   |
%   DQLstmt UNION DQLstmt
%   |
%   DQLstmt EXCEPT DQLstmt
%   |
%   DQLstmt MINUS DQLstmt
%   |
%   DQLstmt INTERSECT DQLstmt
%   |
%   WITH LocalViewDefinition,...,LocalViewDefinition DQLstmt

% LocalViewDefinition ::=
%   [RECURSIVE] CompleteSchema AS DQLstmt

% SELECTstmt ::=
%   SELECT [[ALL|DISTINCT]] SelectExpressionList
%   FROM Rels
%   [WHERE WhereCondition]
%   [GROUP BY Atts]
%   [HAVING HavingCondition]
%   [ORDER BY OrderDescription]

% Atts ::=
%   Att,...,Att

% OrderDescription ::=
%   Att [[ASC|DESC]],...,Att [[ASC|DESC]]

% SelectExpressionList ::= 
%   *
%   |
%   SelectExpression,...,SelectExpression

% SelectExpression ::=
%   UnrenamedSelectExpression
%   |
%   RenamedExpression

% UnrenamedSelectExpression ::=
%   Att
%   |
%   RelationName.Att
%   |
%   RelationName.*
%   |
%   ArithmeticExpression 
%   |
%   DQLstmt 

% RenamedExpression ::=
%   UnrenamedExpression [AS] Identifier

% ArithmeticExpression ::=
%   Op1 ArithmeticExpression
%   |
%   ArithmeticExpression Op2 ArithmeticExpression
%   |
%   ArithmeticFunction(ArithmeticExpression,...,
%                      ArithmeticExpression)
%   |
%   Number
%   |
%   Att
%   |
%   RelationName.Att
%   |
%   ArithmeticConstant

% Op1 ::=
%   - | \ 

% Op2 ::=
%   ^ | ** | * | / | // | rem | \/ | # | + | - | /\ | << | >> 

% ArithmeticFunction ::=
%     sqrt/1 | ln/1 | log/1 | log/2 | sin/1 | cos/1 | tan/1 | cot/1
%   | asin/1 | acos/1 | atan/1 | acot/1 | abs/1 | float/1 
%   | integer/1 | sign/1 | gcd/2 | min/2 | max/2 | truncate/1 
%   | float_integer_part/1 | float_fractional_part/1 
%   | round/1 | floor/1 | ceiling/1

% Aggregate Functions:
%   avg/1 | count/1 | count/0 | max/1 | min/1 | sum/1

% ArithmeticConstant ::=
%   pi | e

% Rels ::=
%   Rel,...,Rel

% Rel ::=
%   UnrenamedRel
%   |
%   RenamedRel

% UnrenamedRel ::=
%   TableName
%   |
%   ViewName
%   |
%   DQLstmt
%   |
%   JoinRel

% RenamedRel ::=
%   UnrenamedRel [AS] Identifier

% JoinRel ::=
%   Rel [NATURAL] JoinOp Rel [JoinCondition]

% JoinOp ::=
%   INNER JOIN
%   |
%   LEFT [OUTER] JOIN
%   |
%   RIGHT [OUTER] JOIN
%   |
%   FULL [OUTER] JOIN

% JoinCondition ::=
%   ON WhereCondition
%   |
%   USING (Atts)

% WhereCondition ::=
%   BWhereCondition
%   |
%   UBWhereCondition

% HavingCondition 
%   As WhereCondition, but including aggregate functions

% BWhereCondition ::=
%   (WhereCondition)

% UBWhereCondition ::=
%   TRUE
%   |
%   FALSE
%   |
%   EXISTS DQLstmt
%   |
%   NOT (WhereCondition)
%   |
%   (AttOrCte,...,AttOrCte) [NOT] IN DQLstmt
%   |
%   WhereExpression IS [NOT] NULL
%   |
%   WhereExpression [NOT] IN DQLstmt
%   |
%   WhereExpression Operator [[ALL|ANY]] WhereExpression 
%   |
%   WhereCondition [AND|OR] WhereCondition

% WhereExpression ::=
%   Att
%   |
%   Cte
%   |
%   ArithmeticExpression
%   |
%   DQLstmt

% AggrArithmeticExpression ::=
%   [AVG|MIN|MAX|SUM](Att)
%   |
%   COUNT([*|Att])

% AttOrCte ::=
%   Att 
%   |
%   Cte

% Operator ::=
%   = | <> | < | > | >= | <= 

% Cte ::=
%   Number
%   |
%   'String'
%   |
%   NULL

% Number is an integer or floating-point number
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database Schema
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% my_table(DB,RelationName,Arity)
:- dynamic(my_table/3).
% my_view(DB,RelationName,Arity,SQLSyntacticTree,DatalogRules,LocalViewDefinitions,StringConstants)
:- dynamic(my_view/7).
% my_attribute(DB,Position,RelationName,AttributeName,DataType)
:- dynamic(my_attribute/5).
% my_primary_key(DB,TableName,AttributeNames)
:- dynamic(my_primary_key/3).
% my_foreign_key(DB,TableName,AttributeNames,ForeignTableName,ForeignAttributeNames)
:- dynamic(my_foreign_key/5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NULLs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic('$NULL'/1).
'$NULL'(0).

get_null_id(ID) :-
  retract('$NULL'(ID)),
  ID1 is ID+1,
  assertz('$NULL'(ID1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_sql_query(SQLst). Parses a SQL string and gets its 
%   syntactic tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_sql_query(SQLst) -->
  {retractall(dictionary(_)),
   assertz(dictionary([]))}, % WARNING: ONLY FOR TEST CASE GENERATION
  my_blanks_star, 
  my_SQL(SQLst),
  my_blanks_star, 
  my_optional_semicolon, 
  my_blanks_star.  

% DQL Statement
my_SQL(SQLst) -->
  my_DQL(SQLst).
% DML Statement
my_SQL(SQLst) -->
  my_DML(SQLst).
% DDL Statement
my_SQL(SQLst) -->
  my_DDL(SQLst).

% DDL Statements
% CREATE TABLE
my_DDL(CRTSchema) -->
  my_create_or_replace(CR),
  my_blanks,
  my_kw("TABLE"),
  my_blanks,
  my_complete_constrained_typed_schema(Schema,Ctrs),
  {atom_concat(CR,'_table',CRT),
   CRTSchema=..[CRT,Schema,Ctrs]}.
   
% CREATE VIEW
my_DDL(CRVSchema) -->
  my_create_or_replace(CR),
  my_blanks,
  my_kw("VIEW"),
  my_blanks,
  my_complete_untyped_schema(Schema),
  my_blanks,
  my_kw("AS"),
  my_blanks,
  my_DQL((LSQLst,Schema)),
  {atom_concat(CR,'_view',CRVF),
   CRVSchema =.. [CRVF,(LSQLst,_AS),Schema]}.
% my_DDL(CRVSchema) -->
%   my_create_or_replace(CR),
%   my_blanks,
%   my_kw("VIEW"),
%   my_blanks,
%   my_user_identifier(_Name),
%   my_blanks,
%   my_kw("AS"),
%   my_blanks,
%   my_DQL((LSQLst,Schema)),
% %  {Schema=..[Name|_]}, % The schema should be built, but this information in not yet known
%   {atom_concat(CR,'_view',CRVF),
%    CRVSchema =.. [CRVF,(LSQLst,_AS),Schema]}.

% DROP TABLE
my_DDL(drop_table(Name)) -->
  my_kw("DROP"),
  my_blanks,
  my_kw("TABLE"),
  my_blanks,
  my_user_identifier(Name).

% DROP VIEW
my_DDL(drop_view(Name)) -->
  my_kw("DROP"),
  my_blanks,
  my_kw("VIEW"),
  my_blanks,
  my_user_identifier(Name).

% DROP SCHEMA
my_DDL(drop_database) -->
  my_kw("DROP"),
  my_blanks,
  my_kw("DATABASE").

my_create_or_replace(create_or_replace) -->
  my_kw("CREATE"),
  my_blanks,
  my_kw("OR"),
  my_blanks,
  my_kw("REPLACE").
my_create_or_replace(create) -->
  my_kw("CREATE").

my_complete_untyped_schema(Schema) -->
  my_user_identifier(Name),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_untyped_columns(Cs),
  my_blanks_star,
  ")",
  {Schema =.. [Name|Cs]}.

my_column_name_list([C]) --> 
  my_untyped_column(C).
my_column_name_list([C|Cs]) -->
  my_untyped_column(C),
  my_blanks_star, 
  ",", 
  my_blanks_star, 
  my_column_name_list(Cs).
 
my_untyped_columns([C:_T]) --> 
  my_untyped_column(C).
my_untyped_columns([C:_T|CTs]) -->
  my_untyped_column(C),
  my_blanks_star, 
  ",", 
  my_blanks_star, 
  my_untyped_columns(CTs).
 
my_untyped_column(C) --> 
  my_user_identifier(C).

% my_columns([C]) --> 
%   my_user_identifier(C).
% my_columns([C|Cs]) -->
%   my_user_identifier(C),
%   my_blanks_star, 
%   ",", 
%   my_blanks_star, 
%   my_columns(Cs).

my_complete_constrained_typed_schema(Schema,Ctrs) -->
  my_user_identifier(Name),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_constrained_typed_columns(Cs,CCtrs),
  my_blanks_star,
  my_optional_table_constraints(TCtrs),
  my_blanks_star,
  ")",
  {Schema =.. [Name|Cs],
   my_append(CCtrs,TCtrs,Ctrs)}.

my_constrained_typed_columns([C:T],Ctrs) --> 
  my_constrained_typed_column(C:T,Ctrs).
my_constrained_typed_columns([C:T|CTs],Ctrs) -->
  my_constrained_typed_column(C:T,CCtrs),
  my_blanks_star, 
  ",", 
  my_blanks_star, 
  my_constrained_typed_columns(CTs,RCtrs),
  {my_append(CCtrs,RCtrs,Ctrs)}.

my_constrained_typed_column(C:T,Ctrs) --> 
  my_user_identifier(C),
  my_blanks,
  my_sql_type(T),
  my_blanks,
  my_column_constraints(C,Ctrs).
my_constrained_typed_column(C:T,[true]) --> 
  my_user_identifier(C),
  my_blanks,
  my_sql_type(T).

my_column_constraints(C,[Ctr]) -->
  my_column_constraint(C,Ctr).
my_column_constraints(C,[Ctr|Ctrs]) -->
  my_column_constraint(C,Ctr),
  my_blanks,
  my_column_constraints(C,Ctrs).

my_column_constraint(C,primary_key([C])) -->
  my_kw("PRIMARY"),
  my_blanks,
  my_kw("KEY").
my_column_constraint(C,foreign_key([C],TableName,[Att])) -->
  my_kw("REFERENCES"),
  my_blanks,
  my_user_identifier(TableName),
  my_blanks_star, 
  "(",
  my_blanks_star,
  my_untyped_column(Att),
  my_blanks_star,
  ")".
my_column_constraint(C,foreign_key([C],TableName,[C])) -->
  my_kw("REFERENCES"),
  my_blanks,
  my_user_identifier(TableName).

my_optional_table_constraints(Ctrs) -->
  ",",
  my_blanks_star,
  my_table_constraints(Ctrs).
my_optional_table_constraints([]) -->
  [].
  
my_table_constraints([Ctr]) -->
  my_table_constraint(Ctr).  
my_table_constraints([Ctr|Ctrs]) -->
  my_table_constraint(Ctr),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_table_constraints(Ctrs).  

my_table_constraint(primary_key(Cs)) -->
  my_kw("PRIMARY"),
  my_blanks,
  my_kw("KEY"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_column_name_list(Cs),
  my_blanks_star,
  ")".
my_table_constraint(foreign_key(Cs,FTableName,FCs)) -->
  my_kw("FOREIGN"),
  my_blanks,
  my_kw("KEY"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_column_name_list(Cs),
  my_blanks_star,
  ")",
  my_blanks_star,
  my_kw("REFERENCES"),
  my_blanks,
  my_user_identifier(FTableName),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_column_name_list(FCs),
  my_blanks_star,
  ")".
my_table_constraint(foreign_key(Cs,FTableName,Cs)) -->
  my_kw("FOREIGN"),
  my_blanks,
  my_kw("KEY"),
  my_blanks_star,
  "(",
  my_blanks_star,
  my_column_name_list(Cs),
  my_blanks_star,
  ")",
  my_blanks_star,
  my_kw("REFERENCES"),
  my_blanks,
  my_user_identifier(FTableName).
  
% Types
% char(n)
my_sql_type(string(char(N))) -->
  my_kw("CHAR"), 
  my_blanks_star, 
  "(",
  my_blanks_star, 
  my_positive_integer(N),
  my_blanks_star, 
  ")".
% char  
my_sql_type(string(char(1))) -->
  my_kw("CHAR").
% varchar(n)
my_sql_type(string(varchar(N))) -->
  my_kw("VARCHAR"), 
  my_blanks_star, 
  "(",
  my_blanks_star, 
  my_positive_integer(N),
  my_blanks_star, 
  ")".
my_sql_type(string(varchar(N))) -->
  my_kw("VARCHAR2"), 
  my_blanks_star, 
  "(",
  my_blanks_star, 
  my_positive_integer(N),
  my_blanks_star, 
  ")".
my_sql_type(string(varchar)) -->
  my_kw("VARCHAR"). 
my_sql_type(string(varchar)) -->
  my_kw("STRING"). 
% integer
my_sql_type(number(integer)) -->
  my_kw("INTEGER").
% int
my_sql_type(number(integer)) -->
  my_kw("INT").
% real
my_sql_type(number(float)) -->
  my_kw("REAL").
  
% DML Statements
% DELETE FROM 
my_DML(delete_from(TableName,true)) -->
  my_kw("DELETE"),
  my_blanks,
  my_kw("FROM"),
  my_blanks,
  my_tablename(TableName).
% DELETE FROM ... WHERE 
my_DML(delete_from(TableName,Condition)) -->
  my_kw("DELETE"),
  my_blanks,
  my_kw("FROM"),
  my_blanks,
  my_tablename(TableName),
  my_blanks,
  my_kw("WHERE"),
  my_blanks,
  my_where_condition(Condition).

% INSERT INTO ... [VALUES(...) | SQL]
my_DML(insert_into(TableName,VS)) -->
  my_kw("INSERT"),
  my_blanks,
  my_kw("INTO"),
  my_blanks,
  my_tablename(TableName),
  my_blanks,
  my_insert_values_sql(VS).

% UPDATE ... SET ... [WHERE ]
my_DML(update(TableName,Assignments,true)) -->
  my_kw("UPDATE"),
  my_blanks,
  my_tablename(TableName),
  my_blanks,
  my_kw("SET"),
  my_blanks,
  my_update_assignments(Assignments).
my_DML(update(TableName,Assignments,Condition)) -->
  my_kw("UPDATE"),
  my_blanks,
  my_tablename(TableName),
  my_blanks,
  my_kw("SET"),
  my_blanks,
  my_update_assignments(Assignments),
  my_blanks,
  my_kw("WHERE"),
  my_blanks,
  my_where_condition(Condition).
  
my_update_assignments([Column,Expression]) -->
  my_update_assignment(Column,Expression).
my_update_assignments([Column,Expression|Assignments]) -->
  my_update_assignment(Column,Expression),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_update_assignments(Assignments).

my_update_assignment(expr(ColumnName,_,string),Expression) -->
  my_column(attr(_T,ColumnName,_AS)),
  my_blanks_star,
  "=",
  my_blanks_star,
  my_sql_proj_expression(Expression,_Type).
  
my_insert_values_sql(Cs) -->
  my_kw("VALUES"),
  {!},
  my_blanks_star,
  "(",
  my_blanks_star,
  my_sql_constants(Cs),
  my_blanks_star,
  ")".
my_insert_values_sql(SQLst) -->
  my_DQL(SQLst).

  
% DQL Statements
my_DQL(SQLst) -->
  my_b_DQL(SQLst).
my_DQL(SQLst) -->
  my_ub_DQL(SQLst).
 
my_b_DQL(SQLst) -->
  "(",
  my_blanks_star,
  my_DQL(SQLst),
  my_blanks_star,
  ")".
  
% WITH
my_ub_DQL((with(SQLst,SQLsts),_AS)) -->
  my_kw("WITH"),
  my_blanks,
  {!},
  my_local_view_definition_list(SQLsts),
  my_blanks,
  my_DQL(SQLst).
% SELECT
my_ub_DQL(SQLst) -->
  my_select_DQL(SQLst).
% UNION
my_ub_DQL((union(R1,R2),_AS)) -->
  my_b_DQL(R1),
  my_blanks,
  my_kw("UNION"),
  my_blanks,
  my_DQL(R2).
my_ub_DQL((union(R1,R2),_AS)) -->
  my_select_DQL(R1),
  my_blanks,
  my_kw("UNION"),
  my_blanks,
  my_DQL(R2).
% EXCEPT
my_ub_DQL((except(R1,R2),_AS)) -->
  my_b_DQL(R1),
  my_blanks,
  my_set_difference_kw, % EXCEPT or MINUS
  my_blanks,
  my_DQL(R2).
my_ub_DQL((except(R1,R2),_AS)) -->
  my_select_DQL(R1),
  my_blanks,
  my_set_difference_kw, % EXCEPT or MINUS
  my_blanks,
  my_DQL(R2).
% INTERSECT
my_ub_DQL((intersect(R1,R2),_AS)) -->
  my_b_DQL(R1),
  my_blanks,
  my_kw("INTERSECT"),
  my_blanks,
  my_DQL(R2).
my_ub_DQL((intersect(R1,R2),_AS)) -->
  my_select_DQL(R1),
  my_blanks,
  my_kw("INTERSECT"),
  my_blanks,
  my_DQL(R2).

my_set_difference_kw -->
  my_kw("EXCEPT").
my_set_difference_kw -->
  my_kw("MINUS").
  
my_local_view_definition_list([V]) -->
  my_local_view_definition(V).
my_local_view_definition_list([V|Vs]) -->
  my_local_view_definition(V),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_local_view_definition_list(Vs).

my_local_view_definition((SQLst,Schema)) -->
  my_optional_recursive,
  my_complete_untyped_schema(Schema),
  my_blanks,
  my_kw("AS"),
  my_blanks,
  my_DQL((SQLst,Schema)).

my_optional_recursive -->
  my_kw("RECURSIVE"),
  my_blanks.
my_optional_recursive -->
  [].

% SELECT
my_select_DQL((select(DistinctAll,ProjList,
               from(Relations),
               where(WhereCondition),
               group_by(GroupList),
               having(HavingCondition),
               order_by(OrderArgs,OrderSpecs)),_AS)) -->
  my_select_stmt(DistinctAll),
  my_projection_list(ProjList),
  my_blanks,
  my_kw("FROM"),
  my_blanks,
  my_relations(Relations),
  my_where_clause(WhereCondition),
  my_group_by_clause(GroupList),
  my_having_clause(HavingCondition),
  my_order_by_clause(OrderArgs,OrderSpecs).

my_where_clause(WhereCondition) -->
  my_blanks,
  my_kw("WHERE"),
  my_blanks,
  {!},
  my_where_condition(WhereCondition).
my_where_clause(true) -->
  [].

my_group_by_clause(GroupList) -->
  my_blanks,
  my_kw("GROUP"),
  my_blanks,
  my_kw("BY"),
  my_blanks,
  {!},
  my_column_list(GroupList).
my_group_by_clause([]) -->
  [].

my_having_clause(HavingCondition) -->
  my_blanks,
  my_kw("HAVING"),
  my_blanks,
  {!},
  my_having_condition(HavingCondition).
my_having_clause(true) -->
  [].

my_order_by_clause(OrderArgs,OrderSpecs) -->
  my_blanks,
  my_kw("ORDER"),
  my_blanks,
  my_kw("BY"),
  my_blanks,
  {!},
  my_order_list(OrderArgs,OrderSpecs).
my_order_by_clause([],[]) -->
  [].
  
my_select_stmt(DistinctAll) -->
  my_kw("SELECT"),
  my_blanks,
  my_select_distinct_all(DistinctAll).

my_select_distinct_all(all) -->
  my_kw("ALL"),
  my_blanks,
  {!}.
my_select_distinct_all(distinct) -->
  my_kw("DISTINCT"),
  my_blanks,
  {!}.
my_select_distinct_all(all) -->
  [].

my_sql_constants([C|Cs]) -->
  my_sql_constant(C),
  my_blanks_star, 
  my_remaining_sql_constants(Cs).

my_remaining_sql_constants(Cs) -->
  ",", 
  {!},
  my_blanks_star, 
  my_sql_constants(Cs).
my_remaining_sql_constants([]) -->
  [].

my_relations([R|Rs]) --> 
  my_p_ren_relation(R), 
  my_blanks_star,
  my_remaining_relations(Rs).

my_remaining_relations(Rs) -->
  ",", 
%  {!},   Does not work with WITH statements, where commas separate view definitions
  my_blanks_star, 
  my_relations(Rs).
my_remaining_relations([]) -->
  [].

my_p_ren_relation(R) --> 
  my_relation(R).
my_p_ren_relation(R) --> 
  my_ren_relation(R).

my_ren_relation((R,[I|Args])) -->
  my_relation((R,[I|Args])),
  my_blanks, 
  my_optional_as,
  my_user_identifier(I).

my_relation(R) --> 
  my_b_relation(R).
my_relation(R) --> 
  my_ub_relation(R).

my_b_relation(R) --> 
  "(",
  my_blanks_star,
  my_relation(R),
  my_blanks_star,
  ")".

my_ub_relation((R,_AS)) --> 
  my_join_relation(R).
%,
%  {!}.
my_ub_relation(R) --> 
  my_non_join_relation(R).

my_non_join_relation((T,_)) -->
  my_tablename(T),
  {!}.
my_non_join_relation((T,_)) -->
  my_viewname(T),
  {!}.
my_non_join_relation((R,AS)) -->
  my_DQL((R,AS)).

my_optional_as -->
  my_kw("AS"),  
  my_blanks,
  {!}.
my_optional_as -->
  [].

my_join_relation(JR,SIn,SOut) :-
  look_ahead_join_op(JOp,SIn,SOut1),
  my_list_diff(SIn,SOut1,SDiff),
  my_p_ren_relation(LR,SDiff,[]),
  my_remainder_join_relation(LR,JOp,JR,SOut1,SOut).

% look_ahead_join_op looks the input list for a join operator. 
% This way, my_ub_join_relation may fail in advance and avoid cycling
look_ahead_join_op(JOp,SIn,SOut) :-
  my_chars(_Cs,SIn,SOut),
  my_blanks(SOut,SOut1),
  my_kw("NATURAL",SOut1,SOut2),
  my_blanks(SOut2,SOut3),
  my_join_operator(JOp,SOut3,_SOut4).
look_ahead_join_op(JOp,SIn,SOut) :-
  my_chars(_Cs,SIn,SOut),
  my_blanks(SOut,SOut1),
  my_join_operator(JOp,SOut1,_SOut2).

% L1-L2=LO LO+L2=L1
my_list_diff(L1,L2,LO) :- 
  my_append(LO,L2,L1).

% NATURAL
my_remainder_join_relation(LR,JOp,JR) -->
  my_blanks,
  my_kw("NATURAL"),
  my_blanks,
  my_join_operator(JOp),
  my_blanks,
  my_p_ren_relation(RR),
  {JR =.. [JOp,LR,RR,equijoin(natural)]}.
% ON
my_remainder_join_relation(LR,JOp,JR) -->
  my_blanks,
  my_join_operator(JOp),
  my_blanks,
  my_p_ren_relation(RR),
  my_join_condition(Cond),
  {JR =.. [JOp,LR,RR,Cond]}.

my_join_operator(inner_join) -->
  my_kw("INNER"),
  my_blanks,
  my_kw("JOIN").
my_join_operator(Outer_join) -->
  my_outer_kind(Outer_join),
  my_optional_outer,
  my_kw("JOIN").

my_outer_kind(left_join) -->
  my_kw("LEFT"),
  my_blanks,
  {!}.
my_outer_kind(right_join) -->
  my_kw("RIGHT"),
  my_blanks,
  {!}.
my_outer_kind(full_join) -->
  my_kw("FULL"),
  my_blanks.

my_optional_outer -->
  my_kw("OUTER"),
  my_blanks,
  {!}.
my_optional_outer -->
  [].
    
my_join_condition(Condition) -->
  my_blanks,
  my_kw("ON"),
  my_blanks,
  {!},
  my_on_condition(Condition).
my_join_condition(equijoin(Atts)) -->
  my_blanks,
  my_kw("USING"),
  my_blanks_star,
  "(",
  {!},
  my_blanks_star,
  my_column_list(Atts),
  my_blanks_star,
  ")".
my_join_condition(true) -->
  [].

my_where_condition(C) --> 
  my_condition(C).

my_on_condition(C) --> 
  my_condition(C).

my_having_condition(C) --> 
  my_condition(C).

my_condition(C) --> 
  my_b_condition(C).
%  ,
%  {!}.
my_condition(C) --> 
  my_ub_condition(C).


my_b_condition(C) --> 
  "(",
  my_blanks_star,
  my_condition(C),
  my_blanks_star,
  ")".

my_ub_condition(exists(R)) --> 
  my_kw("EXISTS"),
  my_blanks_star,
  my_b_DQL(R).
my_ub_condition(exists(R)) --> 
  my_kw("EXISTS"),
  my_blanks,
%  {!},
  my_ub_DQL(R).
my_ub_condition(not(exists(R))) --> 
  my_kw("NOT"),
  my_blanks,
  my_ub_condition(exists(R)).
my_ub_condition(in(L,R)) --> 
  my_column_or_constant_tuple(L),
  my_blanks,
  my_kw("IN"),
  my_blanks,
  my_DQL(R).
my_ub_condition(not_in(L,R)) --> 
  my_column_or_constant_tuple(L),
  my_blanks,
  my_kw("NOT"),
  my_blanks,
  my_kw("IN"),
  my_blanks,
  my_DQL(R).
my_ub_condition(in([E],R)) --> 
  my_sql_expression(E,_T),
  my_blanks,
  my_kw("IN"),
  my_blanks,
  my_DQL(R).
% my_ub_condition(C) --> 
%   my_basic_condition(C).
% my_ub_condition(not(C)) --> % TODO: Implement operator precedence
%   my_kw("NOT"),
%   my_blanks_star,
%   my_condition(C).
my_ub_condition(not(C)) --> 
  my_kw("NOT"),
  my_blanks_star,
  my_b_condition(C).
my_ub_condition(and(C1,C2)) --> 
  my_b_condition(C1),
  my_blanks,
  my_kw("AND"),
  my_blanks,
  my_condition(C2).
my_ub_condition(and(C1,C2)) --> 
  my_basic_condition(C1),
  my_blanks,
  my_kw("AND"),
  my_blanks,
  my_condition(C2).
my_ub_condition(or(C1,C2)) --> 
  my_b_condition(C1),
  my_blanks,
  my_kw("OR"),
  my_blanks,
  my_condition(C2).
my_ub_condition(or(C1,C2)) --> 
  my_basic_condition(C1),
  my_blanks,
  my_kw("OR"),
  my_blanks,
  my_condition(C2).
my_ub_condition(C) --> 
  my_basic_condition(C).
    
my_basic_condition(true) --> 
  my_kw("TRUE").
my_basic_condition(false) --> 
  my_kw("FALSE").
my_basic_condition(is_null(R)) --> 
  my_sql_expression(R,_T), 
  my_blanks, 
  my_kw("IS"), 
  my_blanks, 
  my_kw("NULL").
my_basic_condition(not(is_null(R))) --> 
  my_sql_expression(R,_T), 
  my_blanks, 
  my_kw("IS"), 
  my_blanks, 
  my_kw("NOT"), 
  my_blanks, 
  my_kw("NULL").
my_basic_condition(exists(R)) --> 
  my_kw("EXISTS"),
  my_blanks_star,
  my_b_DQL(R).
my_basic_condition(exists(R)) --> 
  my_kw("EXISTS"),
  my_blanks,
%  {!},
  my_ub_DQL(R).
my_basic_condition(not(exists(R))) --> 
  my_kw("NOT"),
  my_blanks,
  my_ub_condition(exists(R)).
my_basic_condition(in(L,R)) --> 
  my_column_or_constant_tuple(L),
  my_blanks,
  my_kw("IN"),
  my_blanks,
  my_DQL(R).
my_basic_condition(not_in(L,R)) --> 
  my_column_or_constant_tuple(L),
  my_blanks,
  my_kw("NOT"),
  my_blanks,
  my_kw("IN"),
  my_blanks,
  my_DQL(R).
my_basic_condition(in([E],R)) --> 
  my_sql_expression(E,_T),
  my_blanks,
  my_kw("IN"),
  my_blanks,
  my_DQL(R).
my_basic_condition(C) --> 
  my_sql_expression(L,_LT), 
  my_blanks_star, 
  my_relop(O), 
  my_blanks_star, 
  my_sql_expression(R,_RT),
  {C=..[O,L,R]}.
my_basic_condition(not(C)) --> 
  my_kw("NOT"),
  my_blanks_star,
  my_b_condition(C).

my_column_or_constant_tuple(Cs) --> 
  "(",
  my_blanks_star,
  my_column_or_constant_list(Cs),
  my_blanks_star,
  ")".
my_column_or_constant_tuple([C]) --> 
  my_column_or_constant(C).

my_column_list([C,C2|Cs]) -->
  my_column(C),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_column_list([C2|Cs]).
my_column_list([C]) -->
  my_column(C).

my_column_or_constant_list([C,C2|Cs]) -->
  my_column_or_constant(C),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_column_or_constant_list([C2|Cs]).
my_column_or_constant_list([C]) -->
  my_column_or_constant(C).

my_column_or_constant(C) --> 
  my_column(C).
my_column_or_constant(C) --> 
  my_sql_constant(C).

my_order_list([C,C2|Cs],[O,O2|Os]) -->
  my_column_order(C,O),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_order_list([C2|Cs],[O2|Os]).
my_order_list([C],[O]) -->
  my_column_order(C,O).

my_column_order(C,asc) -->
  my_column(C),
  my_blanks,
  my_kw("ASC").
my_column_order(C,desc) -->
  my_column(C),
  my_blanks,
  my_kw("DESC").
my_column_order(C,asc) -->
  my_column(C).

my_relop(RO) --> 
  my_set_op(RO).
my_relop(RO) --> 
  my_tuple_op(RO).

my_set_op(SO) -->
  my_tuple_op(TO),
  my_blanks_star,
  my_kw("ALL"),
  {atom_concat(TO,'_all',SO)}.
my_set_op(SO) -->
  my_tuple_op(TO),
  my_blanks_star,
  my_kw("ANY"),
  {atom_concat(TO,'_any',SO)}.
  
my_tuple_op(RO) --> 
  {map_cond(RO,_), 
   name(RO,SRO)},
  my_string(SRO).

my_projection_list(*) --> 
  "*".
my_projection_list([A|As]) --> 
  my_p_ren_argument(A), 
  my_blanks_star, 
  ",", 
  {!},
  my_blanks_star, 
  my_projection_list(As).
my_projection_list([A]) --> 
  my_p_ren_argument(A).

my_p_ren_argument(A) --> 
  my_ren_argument(A).
my_p_ren_argument(A) --> 
  my_sql_argument(A,_AS).

my_ren_argument(Arg) -->
  my_sql_argument(Arg,AS),
  my_blanks, 
  my_optional_as, 
  my_user_identifier(AS).

%my_sql_argument(attr(R,C,AS),AS) --> % Identifers are assumed to be references to table or view attributes, even when they do not exist already (because of the view construction)
%  my_column(attr(R,C,AS)).           % In sqlst2rast, references to expressions are known, so that incorrectly assumed attributes can be changed to such references
my_sql_argument((R,(*)),'$') -->  % Cannot be renamed
  my_relname(R),
  ".*".
my_sql_argument(E,AS) -->
  my_sql_proj_expression(E,AS).
  
my_sql_proj_expression(expr(E,AS,Type),AS) -->
  my_sql_expression(E,Type).

my_column(attr(R,C,_AS)) --> 
  my_relname(R),
  ".",
  my_colname(C).
my_column(attr(_T,C,_AS)) --> 
  my_colname(C).

% my_expr_reference(expr_ref(AS)) -->
%   my_user_identifier(AS),
%   {my_not(my_attribute('$des',_Pos,AS,_Name,_Type))}.

my_relname(T) --> 
  my_user_identifier(T).

my_tablename(T) --> 
  my_user_identifier(T).
% The following is omitted to work without schema (due to ODBC connections)
%   {my_table('$des',T,_TA),
%    my_not(my_view('$des',T,_VA,_SQL,_DLs,_LVDs,_SCs))}.

my_viewname(V) --> 
  my_user_identifier(V).
%  {my_view('$des',V,_VA,_SQL,_DLs,_LVDs,_SCs)}. % Maybe under construction and not yet known

my_colname(C) --> 
  my_user_identifier(C).
%  {my_attribute('$des',_Pos,_T,C,_Type)}. % Maybe from a view under construction and not yet known

%my_user_identifier: An identifier either: 
% - starting by a letter, followed by letters, digits or underscores
% - characters enclosed between marks
% Returns an atom
my_user_identifier(I) --> 
  my_alfa(A),
  my_alfanums(Is),
  {name(I,[A|Is]),
   my_not(my_sql_identifier(I))}.
my_user_identifier(I) --> 
  my_sql_left_quotation_mark(Mark),
  my_chars(Is),
  {name(I,Is),
   my_not(my_sql_identifier(I))},
  my_sql_right_quotation_mark(Mark).
  
my_sql_left_quotation_mark(square_brackets) -->
  "[".
my_sql_left_quotation_mark(double_quotes) -->
  """".
my_sql_left_quotation_mark(back_quotes) -->
  "`".

my_sql_left_quotation_mark("[",'ACCESS') :-
  !. 
my_sql_left_quotation_mark("""",'ORACLE') :-
  !. 
my_sql_left_quotation_mark("`",'MySQL') :-
  !.
my_sql_left_quotation_mark("",_).
  
my_sql_right_quotation_mark(square_brackets) -->
  "]".
my_sql_right_quotation_mark(double_quotes) -->
  """".
my_sql_right_quotation_mark(back_quotes) -->
  "`".

my_sql_right_quotation_mark("]",'ACCESS') :-
  !. 
my_sql_right_quotation_mark("""",'ORACLE') :-
  !. 
my_sql_right_quotation_mark("`",'MySQL') :-
  !.
my_sql_right_quotation_mark("",_).

my_sql_identifier(I) :-
  to_uppercase(I,CI),
  sql_identifier(CI).

sql_identifier('ALL').
sql_identifier('AND').
sql_identifier('ANSWER').
sql_identifier('AS').
sql_identifier('CREATE').
sql_identifier('DELETE').
sql_identifier('DISTINCT').
sql_identifier('DROP').
sql_identifier('EXCEPT').
sql_identifier('EXISTS').
sql_identifier('FALSE').
sql_identifier('FROM').
sql_identifier('FULL').
sql_identifier('IN').
sql_identifier('INNER').
sql_identifier('INSERT').
sql_identifier('INTERSECT').
sql_identifier('INTO').
sql_identifier('JOIN').
sql_identifier('LEFT').
sql_identifier('NATURAL').
sql_identifier('NOT').
sql_identifier('NULL').
sql_identifier('ON').
sql_identifier('OR').
sql_identifier('OUTER').
sql_identifier('RECURSIVE').
sql_identifier('REPLACE').
sql_identifier('RIGHT').
sql_identifier('SELECT').
sql_identifier('TABLE').
sql_identifier('TRUE').
sql_identifier('UNION').
sql_identifier('USING').
sql_identifier('VALUES').
sql_identifier('VIEW').
sql_identifier('WHERE').
sql_identifier('WITH').
sql_identifier(C) :-
  arithmetic_constant(_Value,LC,_Text),
  to_uppercase(LC,C).
sql_identifier(F) :-
  arithmetic_function(LF,_PrologF,_Text,_Kind,_Type,_Arity),
  to_uppercase(LF,F).

my_alfanums([A|As]) --> 
  my_alfanum(A),
  {!},
  my_alfanums(As).
my_alfanums([]) --> 
  [].

my_alfa(C) --> 
  my_lowercase(C).
my_alfa(C) --> 
  my_uppercase(C).

my_alfanum(C) --> 
  my_alfa(C).
my_alfanum(C) --> 
  my_digit(C).
my_alfanum(95)--> 
  "_".

my_sql_constant(cte(C,number(_N))) --> 
  my_number(C).
my_sql_constant(cte(C,string(S))) --> 
  my_sql_string_constant(cte(C,string(S))).
my_sql_constant(cte('$NULL'(N),_T)) --> 
  my_kw("NULL"),
  {get_null_id(N)}.

my_sql_string_constant(cte(C,string(_S))) --> 
  "'",
  my_string(Cs),
%  {name(C,Cs)},
  {atom_codes(C,Cs)},
  "'",
  {add_to_dictionary(C)}. % WARNING: ONLY FOR TEST CASE GENERATION
  
:- dynamic(dictionary/1). 
add_to_dictionary(SC) :-
  (retract(dictionary(SCs)) ->
   (my_member(SC,SCs) ->
     assertz(dictionary(SCs))
    ;
     assertz(dictionary([SC|SCs]))
   )
   ;
   assertz(dictionary([SC]))).
   
my_optional_semicolon -->
  ";",
  {!}.
my_optional_semicolon -->
  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing SQL expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% my_sql_expression(C,varchar(_N)) -->
%   my_sql_string_constant(C).
my_sql_expression(cte(C,T),T) -->
  my_sql_constant(cte(C,T)).
my_sql_expression(E,number(_N)) -->
  my_sql_arithmetic_expression(E).
my_sql_expression(E,_Type) -->
  my_DQL(E).

% Arithmetic expressions

my_sql_arithmetic_expression(E) --> 
  my_sql_aterm(T),
  my_blanks_star, 
  my_sql_arithmetic_expression_r(T,E).
my_sql_arithmetic_expression_r(E0,E) --> 
  {my_priority_operator(low,SOP,OP)}, 
  my_string(SOP),
  my_blanks_star, 
  my_sql_aterm(T),
  my_blanks_star, 
  {TOP =.. [OP,E0,T]},
  my_sql_arithmetic_expression_r(TOP,E).
my_sql_arithmetic_expression_r(E,E) -->
  [].

my_sql_aterm(T) --> 
  my_sql_power(P),
  my_blanks_star, 
  my_sql_aterm_r(P,T).
my_sql_aterm_r(T0,T) --> 
  {my_priority_operator(medium,SOP,OP)}, 
  my_string(SOP),
  my_blanks_star, 
  my_sql_power(P),
  my_blanks_star, 
  {TOP =.. [OP,T0,P]},
  my_sql_aterm_r(TOP,T).
my_sql_aterm_r(T,T) -->
  [].

my_sql_power(P) --> 
  my_sql_factor(F),
  my_blanks_star, 
  my_sql_power_r(F,P).
my_sql_power_r(P0,TOP) --> 
  {my_priority_operator(high,SOP,OP)}, 
  my_string(SOP), 
  my_blanks_star, 
  my_sql_factor(P1),
  my_blanks_star,
  {TOP =.. [OP,P0,P]},
  my_sql_power_r(P1,P).
my_sql_power_r(P,P) -->
  [].

my_sql_factor('$NULL'(ID)) -->
  my_kw("NULL"),
  {get_null_id(ID)}.
my_sql_factor(N) -->
  my_number(N).
my_sql_factor(C) -->
  my_arithmetic_constant(C).
my_sql_factor(C) -->
  my_column(C).
%my_sql_factor(E) -->
%  my_expr_reference(E).
my_sql_factor(count) -->
  my_kw("COUNT"),
  my_blanks_star,
  "(",
  my_blanks_star,
  "*",
  my_blanks_star,
  ")".
my_sql_factor(T) --> 
  {my_arithmetic_function(SF,F,Arity),
   to_uppercase_char_list(SF,USF)},
  my_kw(USF), 
  my_blanks_star,
  "(",
  my_blanks_star,
  my_sql_function_arguments(Arity,As),
  my_blanks_star,
  ")",
  {T=..[F|As]}.
my_sql_factor(E) -->
  "(",
  my_blanks_star,
  my_sql_arithmetic_expression(E),
  my_blanks_star,
  ")".
my_sql_factor(T) --> 
  {my_unary_operator(SOP,OP)},
  my_string(SOP),
  my_blanks_star, 
  my_sql_arithmetic_expression(E),
  {T=..[OP,E]}.

my_sql_function_arguments(1,[E]) -->
  !,
  my_sql_arithmetic_expression(E).
my_sql_function_arguments(A,[E|Es]) -->
  {A>1},
  my_sql_arithmetic_expression(E),
  my_blanks_star,
  ",",
  my_blanks_star,
  {A1 is A-1},
  my_sql_function_arguments(A1,Es).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_sql_query(+SQLstr,+SQLst) Solves a SQL query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_sql_query(QueryStr,Query) :-
  (current_db('$des',_Handle) ->
    solve_des_sql_query(Query)
   ;
    solve_rdb_sql_query(QueryStr,Query)).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_rdb_sql_query(+SQLst) Solves an RDB SQL query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_rdb_sql_query(QueryStr,Query) :-
  is_dql_statement(QueryStr,Query),
  !,
  my_odbc_dql_query(QueryStr,Schema,Rows),
  store_elapsed_time(computation),
  display_answer_schema(Schema),
  display_solutions(Rows),
  display_elapsed_time.
solve_rdb_sql_query(QueryStr,Query) :-
  is_dml_statement(QueryStr,Query,Type),
  !,
  processC(clear_et,[],_NVs,yes),
  my_odbc_dml_query(QueryStr,NumberOfRows),
  store_elapsed_time(computation),
  display_nbr_of_tuples(NumberOfRows,Type),
  display_elapsed_time.
solve_rdb_sql_query(QueryStr,Query) :-
  is_ddl_statement(QueryStr,Query,Message),
  !,
  (my_odbc_get_dbms(access),
   prolog_system(sicstus) ->
     write_log_list(['Warning: DDL queries not allowed for MS Access ODBC in SICStus Prolog platform.',nl])
    ;
     my_odbc_ddl_query(QueryStr),
     store_elapsed_time(computation),
     write_verb_list(['Info: ',Message,'.',nl]),
     processC(clear_et,[],_NVs,yes),
     display_elapsed_time).
  

% A DQL query: select, union, except, intersect
is_dql_statement(_,(SELECT,_Ren)) :-
  SELECT=..[select|_],
  !.
is_dql_statement(_,(with(_,_),_Ren)).
is_dql_statement(_,(union(_,_),_Ren)).
is_dql_statement(_,(except(_,_),_Ren)).
is_dql_statement(_,(intersect(_,_),_Ren)).
is_dql_statement(QueryStr,unknown) :-
  my_guessed_dql_statement(QueryStr,_Remainder),
  !.
  
% A DML query: insert, delete or update
is_dml_statement(_,insert_into(_,_),inserted).
is_dml_statement(_,delete_from(_,_),deleted).
is_dml_statement(_,update(_,_,_),updated).
is_dml_statement(QueryStr,unknown,Message) :-
  my_guessed_dml_statement(Message,QueryStr,_Remainder),
  !.

% A DDL query: insert, delete or update
is_ddl_statement(_,create_table(_,_),'Table created').
is_ddl_statement(_,create_or_replace_table(_,_),'Table created').
is_ddl_statement(_,create_view(_,_),'View created').
is_ddl_statement(_,create_or_replace_view(_,_),'View created').
is_ddl_statement(_,drop_table(_),'Table dropped').
is_ddl_statement(_,drop_view(_),'View dropped').
is_ddl_statement(_,drop_database,'Database dropped').
is_ddl_statement(QueryStr,unknown,Message) :-
  my_guessed_ddl_statement(Message,QueryStr,_Remainder),
  !.
  
% Guess whether it is a DQL statement
my_guessed_dql_statement -->
  my_blanks_star,
  my_opening_parenthesis_star,
  my_blanks_star,
  my_kw("SELECT"),
  my_blanks.
my_guessed_dql_statement -->
  my_blanks_star,
  my_opening_parenthesis_star,
  my_blanks_star,
  my_kw("WITH"),
  my_blanks.
  
% Guess whether it is a DML statement
my_guessed_dml_statement(inserted) -->
  my_blanks_star,
  my_kw("INSERT"),
  my_blanks.
my_guessed_dml_statement(deleted) -->
  my_blanks_star,
  my_kw("DELETE"),
  my_blanks.
my_guessed_dml_statement(updated) -->
  my_blanks_star,
  my_kw("UPDATE"),
  my_blanks.
  
% Guess whether it is a DDL statement
my_guessed_ddl_statement('Table created') -->
  my_blanks_star,
  my_kw("CREATE"),
  my_blanks,
  my_kw("TABLE"),
  my_blanks.
my_guessed_ddl_statement('Table created') -->
  my_blanks_star,
  my_kw("CREATE"),
  my_blanks,
  my_kw("OR"),
  my_blanks,
  my_kw("REPLACE"),
  my_blanks,
  my_kw("TABLE"),
  my_blanks.
my_guessed_ddl_statement('View created') -->
  my_blanks_star,
  my_kw("CREATE"),
  my_blanks,
  my_kw("VIEW"),
  my_blanks.
my_guessed_ddl_statement('View created') -->
  my_blanks_star,
  my_kw("CREATE"),
  my_blanks,
  my_kw("OR"),
  my_blanks,
  my_kw("REPLACE"),
  my_blanks,
  my_kw("VIEW"),
  my_blanks.
my_guessed_ddl_statement('Table dropped') -->
  my_blanks_star,
  my_kw("DROP"),
  my_blanks,
  my_kw("TABLE"),
  my_blanks.
my_guessed_ddl_statement('View dropped') -->
  my_blanks_star,
  my_kw("DROP"),
  my_blanks,
  my_kw("VIEW"),
  my_blanks.
my_guessed_ddl_statement('Database dropped') -->
  my_blanks_star,
  my_kw("DROP"),
  my_blanks,
  my_kw("DATABASE"),
  my_blanks.
% Other (unknown) statements will be sent without expecting any result set  
my_guessed_ddl_statement('Statement has been executed') -->
  [].

my_opening_parenthesis_star -->
  "(",
  my_blanks_star,
  my_opening_parenthesis_star.
my_opening_parenthesis_star -->
  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_des_sql_query(+SQLst) Solves a DES SQL query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CREATE TABLE TableName
solve_des_sql_query(create_table(Schema,Ctrs)) :-
  functor(Schema,TableName,_Arity),
  (my_table('$des',TableName,_) -> 
   write_log_list(['Error: Table already defined.',nl])
   ;
   create_table(Schema,Ctrs),
   store_elapsed_time(computation),
   display_elapsed_time
  ).
% CREATE OR REPLACE TABLE TableName
solve_des_sql_query(create_or_replace_table(Schema,Ctrs)) :-
  functor(Schema,TableName,_Arity),
  (my_table('$des',TableName,_) -> 
   drop_table_k(TableName)
   ;
   true),
  create_table(Schema,Ctrs),
  store_elapsed_time(computation),
  display_elapsed_time.
% CREATE VIEW ViewName
solve_des_sql_query(create_view(SQLst,Schema)) :-
  translate_trusted_views(SQLst,Schema,TSQLst,TSchema),
  functor(TSchema,TableName,_Arity),
  (my_table('$des',TableName,_) -> 
   write_log_list(['Error: Object ',TableName,' already defined.',nl])
   ;
   create_view(TSQLst,TSchema,[]),
   store_elapsed_time(computation),
   display_elapsed_time
  ).
% CREATE OR REPLACE VIEW ViewName
solve_des_sql_query(create_or_replace_view(SQLst,Schema)) :-
  translate_trusted_views(SQLst,Schema,TSQLst,TSchema),
  create_or_replace_view(TSQLst,TSchema),
  store_elapsed_time(computation),
  display_elapsed_time.
% DROP TABLE TableName
solve_des_sql_query(drop_table(TableName)) :-
  (my_not(my_table('$des',TableName,_Arity)) -> 
   write_log_list(['Error: Table not defined.',nl])
   ;
   drop_table(TableName),
   store_elapsed_time(computation),
   display_elapsed_time
  ).
% DROP VIEW ViewName
solve_des_sql_query(drop_view(TableName)) :-
  (my_not(my_table('$des',TableName,_Arity)) -> 
   write_log_list(['Error: View not defined.',nl])
   ;
   drop_view(TableName),
   store_elapsed_time(computation),
   display_elapsed_time
  ).
% DROP DATABASE
solve_des_sql_query(drop_database) :-
  write_log_list(['Info: This will drop all views, tables, and Datalog rules.',nl,'      Do you want to proceed? (y/n) [n]: ']),
  my_get0(Char),
  %WARNING: System-dependent. Newline character is assumed to be the charcode 10.
  (Char==10 ->
    write_verb_list(['Info: Nothing dropped',nl])
   ;
    my_get0(_NL),
    ([Char] == "y" ->
      reset_elapsed_time,
      store_elapsed_time(parsing),
      drop_database,
      store_elapsed_time(computation),
      display_elapsed_time
     ;
      write_verb_list(['Info: Nothing dropped',nl])
    ) 
  ).
% INSERT INTO TableName VALUES(...)
solve_des_sql_query(insert_into(TableName,Cs)) :-
  Cs=[_H|_T],
  !,
  (my_not(my_view('$des',TableName,Arity,_SQLst,_DLs,_LVDs,_SCs)) ->
   (my_table('$des',TableName,Arity) ->
    (length(Cs,Arity) ->
     my_nf_bagof(Cte,Type^my_member(cte(Cte,Type),Cs),Ctes),
     Tuple=..[TableName|Ctes],
     assert_rule((Tuple,[]),simplify,Error),
     store_elapsed_time(computation),
     functor(G,TableName,Arity),
     (var(Error) -> 
       retractall(complete_flag(G,_CF)),
       display_nbr_of_tuples([Tuple],inserted)
      ;
       display_nbr_of_tuples([],inserted) 
     ),
     display_elapsed_time
     ;
     write_log_list(['Error: Incorrect number of arguments.',nl]))
    ;
    write_log_list(['Error: Table ',TableName,' does not exist.',nl]))
   ;
   write_log_list(['Error: Cannot insert into views.',nl])).
% INSERT INTO TableName SQLStmt
solve_des_sql_query(insert_into(TableName,SQLst)) :-
  (my_not(my_view('$des',TableName,Arity,_SQLst,_DLs,_LVDs,_SCs)) ->
   (my_table('$des',TableName,Arity) ->
    solve_des_sql_query_k(SQLst,_Schema,_TableRen,_Query,_Undefined),
    insert_tuples(TableName,Arity),
    store_elapsed_time(computation),
    display_elapsed_time
    ;
    write_log_list(['Error: Table ',TableName,' does not exist.',nl]))
   ;
   write_log_list(['Error: Cannot insert into views.',nl])).

% DELETE FROM
solve_des_sql_query(delete_from(TableName,Condition)) :-
  (my_not(my_view('$des',TableName,Arity,_SQLst,_DLs,_LVDs,_SCs)) ->
   (my_table('$des',TableName,Arity) ->
    solve_des_sql_query_k((select(all,*,from([(TableName,_Ren)]),where(Condition),group_by([]),having(true),order_by([],[])),_AS),_Schema,_TableRen,_Query,_Undefined),
    delete_tuples(TableName,Arity),
    store_elapsed_time(computation),
    display_elapsed_time,
    abolishET 
%    rollup_stratification(_Rules)
    ;
    write_log_list(['Error: Table ',TableName,' does not exist.',nl]))
   ;
   write_log_list(['Error: Cannot delete from views.',nl])).
  
% UPDATE 
solve_des_sql_query(update(TableName,Assignments,Condition)) :-
  (my_not(my_view('$des',TableName,Arity,_SQLst,_DLs,_LVDs,_SCs)) ->
   (my_table('$des',TableName,Arity) ->
    % The following will leave in the ET tuples for the updated table with the following information:
    %   Table name and arity, and the column names and values of tuples that need to be updated: 
    %    answer(OldVal1,...,OldValArity,
    %           ColName1,NewValI1,...,ColNameN,ValIM) : I1,...,IM in {1..Arity}
    solve_des_sql_query_k((select(all,[(TableName,(*))|Assignments],from([(TableName,_Ren)]),where(Condition),group_by([]),having(true),order_by([],[])),_AS),_Schema,_TableRen,_Query,_Undefined),
    update_tuples(TableName,Arity),
    store_elapsed_time(computation),
    display_elapsed_time,
    abolishET 
%    rollup_stratification(_Rules)
    ;
    write_log_list(['Error: Table ',TableName,' does not exist.',nl]))
   ;
   write_log_list(['Error: Cannot update views.',nl])).
  

% DQL Statements
solve_des_sql_query(SQLst) :-
  save_et(ET),
  (solve_des_sql_query_k(SQLst,Schema,TableRen,Query,Undefined) ->
    store_elapsed_time(computation),
    display_answer_schema(Schema,TableRen),
    display_solutions(Query,Undefined),
    display_elapsed_time,
    restore_et(ET)
   ;
    restore_et(ET),
    fail
  ).

% delete_tables used by des_tc.pl
delete_tables([]).
delete_tables([T|Ts]) :-
  solve_des_sql_query(delete_from(T,true)),
  delete_tables(Ts).
	
% Display answer schema for ODBC RDB
display_answer_schema(Schema) :-
  write_log_list([Schema,' ->',nl]).

% Display answer schema for DES DDB
display_answer_schema([_|Args],TableRen) :-
  write_log('answer('),
  (development(on) ->
    HArgs=Args
    ;
    hide_nulls(Args,HArgs)),
  write_csas(HArgs,TableRen),
  write_log_list([') ->',nl]).

write_csas([A],TableRen) :-
  write_csa(A,TableRen).
write_csas([A1,A2|As],TableRen) :-
  write_csa(A1,TableRen),
  write_log(', '),
  write_csas([A2|As],TableRen).

write_csa(expr(expr_ref(E),_AS,_Type),_TableRen) :-
  !,
  write_log(E).
write_csa(expr(attr(RT,C,R),_AS,_Type),TableRen) :-
  !,
  write_csa(attr(RT,C,R),TableRen).
write_csa(expr(_E,AS,_Type),_TableRen) :-
  write_log(AS).
write_csa(attr(RT,C,_R),TableRen) :-
  find_table_name(RT,TableRen,TableRen,T),
%  setof((T,RT2),my_member((T,RT2),TableRen),[(T,RT2)]), % Unambiguous only-one occurrence of table T in the renaming
  setof((T,RT),my_member((T,RT),TableRen),TRT), % Unambiguous only-one occurrence of table T in the renaming
  TRT=[(T,RT)], % Instead of simply putting [(T,RT)] as the last argument of the above setof, this is needed because GNU-Prolog otherwise fails
  !,
  write_log_list([T,'.',C]).
write_csa(attr(RT,C,_R),TableRen) :-
  find_table_name(RT,TableRen,TableRen,T),
  !,
  (atom_concat('$t',_N,T) ->
   write_log(C)
   ;
   atom_concat('$t',N,RT),
   write_log_list([T,'_',N,'.',C])).
write_csa(attr(T,C,_R),_TableRen) :- % Lost renamings
  (atom_concat('$t',_N,T) ->
   write_log(C)
   ;
   write_log_list([T,'.',C])).

find_table_name(RT,[(T,RT)|_RTTs],_TableRen,T) :-
  my_not(atom_concat('$',_,T)),
  !.
find_table_name(RT,[(RT,RT1)|_RTTs],TableRen,T) :-
  atom_concat('$',_,RT),
  find_table_name(RT1,TableRen,TableRen,T).
find_table_name(RT,[(RT2,_RT1)|RTTs],TableRen,T) :-
  RT \= RT2,
  find_table_name(RT,RTTs,TableRen,T).
  
% Solving SQL queries, untouching ET
% WITH
solve_des_sql_query_k((with(SQLst,SQLsts),_AS),Schema,TableRen,Query,Undefined) :-
  !,
  create_prototype_view_list(SQLsts,_LocalViewDefs),
  create_or_replace_view_list_k(SQLsts),
  solve_des_sql_query_k(SQLst,Schema,TableRen,Query,Undefined),
  drop_view_k_list(SQLsts).
% SELECT, ...
solve_des_sql_query_k(SQLst,Schema,TableRen,Query,Undefined) :-
  sqlst2dlsts(SQLst,Schema,TableRen,UDLsts),
  UDLsts=[':-'(Head,_)|_],
  functor(Head,Pred,Arity),
  replace_predicate_names_and_assert(Pred,Arity,answer,UDLsts,DLsts,DVs,Error),
  abolishET, 
  drilldown_stratification(DVs),
  (var(Error) ->
   my_member(':-'(Query,_),DLsts),
   Query =.. [answer|_],
   solve_datalog_query(Query,Undefined)
   ;
   true),
  get_source_dlrules_list(DVs,SDVs),
  retract_dlrule_list(SDVs,_Error).

% Create table
create_table(Schema,Ctrs) :-
  functor(Schema,TableName,Arity),
  assertz(my_table('$des',TableName,Arity)),
  Schema =.. [TableName|Args],
  assert_attr_list(1,TableName,Args),
  (post_table_constraints(TableName,Ctrs) ->
    write_verb_list(['Info: Table created.',nl])
    ;
    drop_table_k(TableName),
    write_log_list(['Error: Imposing constraints.',nl])
  ).

post_table_constraints(_TableName,[]).   
post_table_constraints(TableName,[Ctr|Ctrs]) :-
  post_table_constraint(TableName,Ctr),
  post_table_constraints(TableName,Ctrs).   
  
post_table_constraint(_TableName,true).  
post_table_constraint(TableName,primary_key(Atts)) :-
  exists_atts(TableName,Atts),
  (my_primary_key('$des',TableName,_Atts) ->
   write_log_list(['Error: Primary key multiply defined.',nl]),
   !,
   fail
   ;
   true),
  assertz(my_primary_key('$des',TableName,Atts)).
post_table_constraint(TableName,foreign_key(Atts,FTableName,FAtts)) :-
  (my_table('$des',FTableName,_Arity) ->
   true
   ;
   write_log_list(['Error: Table ',FTableName,' is unknown.',nl]),
   !,
   fail),
  (my_not(my_view('$des',FTableName,_A,_S,_D,_L,_SC)) ->
   true
   ;
   write_log_list(['Error: Referenced object ',FTableName,' is a view, not a table.',nl]),
   !,
   fail),
  (TableName\==FTableName ->
   true
   ;
   write_log_list(['Error: Autoreference for ',TableName,' is not allowed.',nl]),
   !,
   fail),
  exists_atts(TableName,Atts),
  remove_duplicates(Atts,RAtts),
  length(Atts,L),
  (length(RAtts,L) ->
   true
   ;
   write_log_list(['Error: Duplicated columns in column list ',TableName,'.',Atts,'.',nl]),
   !,
   fail),
  exists_atts(FTableName,FAtts),
  remove_duplicates(FAtts,RFAtts),
  length(FAtts,FL),
  (length(RFAtts,FL) ->
   true
   ;
   write_log_list(['Error: Duplicated columns in referenced column list ',FTableName,'.',FAtts,'.',nl]),
   !,
   fail),
  (L==FL ->
   true
   ;
   write_log_list(['Error: Different number of arguments in referenced colummn list ',FTableName,'.',FAtts,'.',nl]),
   !,
   fail),
  same_type_atts(TableName,Atts,FTableName,FAtts),
  (my_primary_key('$des',FTableName,FAtts) ->
   true
   ;
   write_log_list(['Error: Referenced column list ',FTableName,'.',FAtts,' is not a primary key.',nl]),
   !,
   fail),
  assertz(my_foreign_key('$des',TableName,Atts,FTableName,FAtts)).
  
exists_atts(_TableName,[]).  
exists_atts(TableName,[Att|Atts]) :-
  (my_attribute('$des',_Pos,TableName,Att,_Type) ->
   exists_atts(TableName,Atts)
   ;
   write_log_list(['Error: Unknown column ',Att,'.',nl]),
   !,
   fail).

same_type_atts(_TableName,[],_FTableName,[]).
same_type_atts(TableName,[Att|Atts],FTableName,[FAtt|FAtts]) :-
  my_attribute('$des',_Pos,TableName,Att,Type),
  my_attribute('$des',_FPos,FTableName,FAtt,FType),
  (Type==FType ->
   same_type_atts(TableName,Atts,FTableName,FAtts)
   ;
   write_log_list(['Error: Type mismatch ',TableName,'.',Att,':',Type,' <> ',FTableName,'.',FAtt,':',FType,'.',nl]),
   !,
   fail).
   
assert_attr_list(_I,_Table,[]) :- !.
assert_attr_list(I,Table,[C:T|CTs]) :-
  assertz(my_attribute('$des',I,Table,C,T)),
  I1 is I+1,
  assert_attr_list(I1,Table,CTs).

% Create view
create_view(SQLst,Schema,LVDs) :-
  create_view_k(SQLst,Schema,LVDs),
  abolishET, 
  compute_stratification.
  
% Create view, untouching ET, no stratification computation
create_view_k((with(SQLst,SQLsts),Schema),Schema,_LVDs) :-
  !,
  create_prototype_view_list(SQLsts,LocalViewDefs),
  create_or_replace_view_list_k(SQLsts),
  create_view_k(SQLst,Schema,LocalViewDefs).
create_view_k((SQLst,Schema),Schema,LocalViewDefs) :-
  functor(Schema,TableName,Arity),
  assertz(my_table('$des',TableName,Arity)),
  Schema =.. [TableName|TypedArgs],
  assert_attr_list(1,TableName,TypedArgs),
  sqlst2dlsts((SQLst,_AS),Schema,_TableRen,DLsts),
  DLsts=[':-'(Head,_)|_],
  (functor(Head,Pred,Arity) ->
   replace_predicate_names_and_assert(Pred,Arity,TableName,DLsts,_RDLsts,DVs,_Error),
   dictionary(SCs),
   assertz(my_view('$des',TableName,Arity,SQLst,DVs,LocalViewDefs,SCs)),
   (infer_types_and_assert(DVs,TypedArgs) ->
     write_verb_list(['Info: View created.',nl])
    ;
     write_log_list(['Error: Type conflict(s).',nl]),
     !,
     create_view_k_error(Schema)
   )
   ;
   write_log_list(['Error: Incorrect number of arguments in view.',nl]),
   !,
   create_view_k_error(Schema)).
create_view_k(_SQLst,Schema,_LVDs) :-
  create_view_k_error(Schema).
  
create_view_k_error(Schema) :-
  Schema =.. [TableName|_Args],
  retractall(my_table('$des',TableName,_Arity)),
  retractall(my_attribute('$des',_Pos,TableName,_Att,_Type)),
  !,
  fail.

% Predicate,Arity,NewPredicate,Rules,ReplacedRules,RNVss,Error
replace_predicate_names_and_assert(P,Arity,Q,DLsts,RDLsts,DVs,Error) :-
  replace_functor(P,Q,DLsts,ARDLsts),
  number_codes(Arity,SArity),
  "_"=[US],
  atom_codes(AArity,[US|SArity]),
  atom_codes(P,[P1,P2|_]),
  atom_codes(AP,[P1,P2]),
  atom_concat(Q,AArity,NQ),
  atom_concat(NQ,'_',NLV),
  replace_functor_substring(AP,NLV,ARDLsts,RDLsts),
  assign_variable_names_list(RDLsts,DVs),
  disable_safety_warnings(SW),
  assert_rules(DVs,simplify,Error),
  restore_safety_warnings(SW).

% Create views in a list, untouching ET, no stratification computation
create_or_replace_view_list_k([]).
create_or_replace_view_list_k([(SQLst,Schema)|Vs]) :-
  create_or_replace_view_k((SQLst,Schema),Schema),
  create_or_replace_view_list_k(Vs).

% Create prototype views from SQL schemas. Returns the list of view names, and defines the prototype view schema as facts
create_prototype_view_list(Schemas,TableNames) :-
  check_no_redefinitions(Schemas),
  build_prototype_view_list(Schemas,TableNames).

check_no_redefinitions([]).
check_no_redefinitions([(_SQLst,Schema)|Vs]) :-
  functor(Schema,TableName,Arity),
  (my_table('$des',TableName,Arity) -> 
    write_log_list(['Error: Syntax error. Trying to redefine ',TableName,'/',Arity,nl]),
    !,
    fail
    ;
    true),
  check_no_redefinitions(Vs).

build_prototype_view_list([],[]).
build_prototype_view_list([(_SQLst,Schema)|Vs],[TableName|TableNames]) :-
  functor(Schema,TableName,Arity),
  assertz(my_table('$des',TableName,Arity)),
  Schema =.. [TableName|Args],
  assert_attr_list(1,TableName,Args),
  assertz(my_view('$des',TableName,Arity,_SQL,_DVs,_LVDs,_SCs)),
  build_prototype_view_list(Vs,TableNames).

% Create or replace view
create_or_replace_view((SQLst,Schema),Schema) :-
  functor(Schema,TableName,_Arity),
  (my_table('$des',TableName,_) -> 
   drop_view_k(TableName),
   !
   ;
   true),
  create_view((SQLst,Schema),Schema,[]).
  
% Create or replace view, untouching ET, no stratification computation
create_or_replace_view_k((SQLst,Schema),Schema) :-
  functor(Schema,TableName,_Arity),
  (my_table('$des',TableName,_) -> 
   drop_view_k(TableName),
   !
   ;
   true),
  create_view_k((SQLst,Schema),Schema,[]).


% Drop table
drop_table(TableName) :-
  my_table('$des',TableName,Arity),
  retractall(my_table('$des',TableName,Arity)),
  retractall(my_attribute('$des',_Pos,TableName,_Attr,_Type)),
  retractall(my_primary_key('$des',TableName,_PKAtts)),
  retractall(my_foreign_key('$des',TableName,_PAtts,_FKTableName,_FKAtts)),
  retractall(my_foreign_key('$des',_TableName,_Atts,TableName,_FAtts)),
  get_object_dlrules(namearity,TableName/Arity,ODLs),
  retract_dlrule_list(ODLs,_Error),
  abolishET, 
  rollup_stratification(_DLsts),
  write_verb_list(['Info: Table ''',TableName,''' dropped.',nl]).

% Drop table, untouching ET, no stratification computation
drop_table_k(TableName) :-
  my_table('$des',TableName,Arity),
  retractall(my_table('$des',TableName,Arity)),
  retractall(my_attribute('$des',_Pos,TableName,_Attr,_Type)),
  retractall(my_primary_key('$des',TableName,_PKAtts)),
  retractall(my_foreign_key('$des',TableName,_PAtts,_FKTableName,_FKAtts)),
  retractall(my_foreign_key('$des',_TableName,_Atts,TableName,_FAtts)),
  get_object_dlrules(namearity,TableName/Arity,ODLs),
  retract_dlrule_list(ODLs,_Error),
  write_verb_list(['Info: Table ''',TableName,''' dropped.',nl]).

% Drop view
drop_view(TableName) :-
  my_table('$des',TableName,Arity),
  retractall(my_table('$des',TableName,Arity)),
  retractall(my_attribute('$des',_Pos,TableName,_Attr,_Type)),
  my_view('$des',TableName,Arity,SQLst,RNVss,LVDs,SCs),
  drop_viewname_list_k(LVDs),
  get_object_dlrules_list(RNVss,ODLs),
  retract_dlrule_list(ODLs,_Error),
  retractall(my_view('$des',TableName,Arity,SQLst,RNVss,LVDs,SCs)),
  abolishET, 
  rollup_stratification(RNVss),
  write_verb_list(['Info: View ''',TableName,''' dropped.',nl]).

% Drop view, untouching ET, no stratification computation
drop_view_k(TableName) :-
  my_table('$des',TableName,Arity),
  retractall(my_table('$des',TableName,Arity)),
  retractall(my_attribute('$des',_Pos,TableName,_Attr,_Type)),
  my_view('$des',TableName,Arity,SQLst,RNVss,LVDs,SCs),
  drop_viewname_list_k(LVDs),
  get_object_dlrules_list(RNVss,ODLs),
  retract_dlrule_list(ODLs,_Error),
  retractall(my_view('$des',TableName,Arity,SQLst,RNVss,LVDs,SCs)),
  write_verb_list(['Info: View ''',TableName,''' dropped.',nl]).

% Drop views in a list
drop_view_list([]).
drop_view_list([(_SQLst,Schema)|Vs]) :-
  Schema =.. [TableName|_Args],
  solve_des_sql_query(drop_view(TableName)),
  drop_view_list(Vs).

% Drop views in a list, untouching ET
drop_view_k_list([]).
drop_view_k_list([(_SQLst,Schema)|Vs]) :-
  Schema =.. [TableName|_Args],
  drop_view_k(TableName),
  drop_view_k_list(Vs).

% Drop view names in a list, untouching ET
drop_viewname_k_list([]).
drop_viewname_k_list([ViewName|VNs]) :-
  drop_view_k(ViewName),
  drop_viewname_k_list(VNs).

% Drop database
drop_database :-
  get_viewnames(AllViewNames),
  get_localviewnames(LocalViewNames),
  my_set_diff(AllViewNames,LocalViewNames,ViewNames),
  drop_viewname_list_k(ViewNames),
  get_tablenames(TableNames),
  drop_tablename_list_k(TableNames),
  abolishDL,
  abolishET, 
  rollup_stratification(_),
  write_verb_list(['Info: Database dropped.',nl]).

drop_tablename_list_k([]).
drop_tablename_list_k([TableName|TableNames]) :-
  drop_table_k(TableName),
  drop_tablename_list_k(TableNames).

drop_viewname_list_k([]).
drop_viewname_list_k([ViewName|ViewNames]) :-
  drop_view_k(ViewName),
  drop_viewname_list_k(ViewNames).

get_tablenames(TableNames) :-
  get_tablenames(_TableName,TableNames).

get_viewnames(ViewNames) :-
  get_viewnames(_ViewName,ViewNames).

% Testing whether the des database is empty
empty_des_rdb :-
  get_tablenames(Ts),
  !,
  Ts==[],
  get_viewnames(Vs),
  !,
  Vs==[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*********************************************************************/
/* Listing the DB schema: list_schema/0                              */
/*********************************************************************/

list_schema :-
  list_schema(_N).

/*********************************************************************/
/* Listing the DB schema for either the whole database, or a given   */
/* table or view: list_schema/1                                      */
/*********************************************************************/

list_schema(Name) :-
  get_tablenames(Name,TableNames),
  (TableNames \== [] ->
   (var(Name) -> 
     write_log_list(['Info: Table(s):',nl])
     ;
     write_log_list(['Info: Table:',nl])),
   list_schema_list(TableNames)
   ;
   (var(Name) -> 
     write_log_list(['Info: No tables.',nl])
     ;
     TableNotFound=true)),
  get_viewnames(Name,ViewNames),
  (ViewNames \== [] ->
   (var(Name) -> 
     write_log_list(['Info: View(s):',nl])
     ;
     write_log_list(['Info: View:',nl])),
   list_schema_list(ViewNames)
   ;
   (var(Name) -> 
     write_log_list(['Info: No views.',nl])
     ;
     ViewNotFound=true)),
  (nonvar(Name), TableNotFound == true, ViewNotFound == true ->
     write_log_list(['Info: No table or view found with that name.',nl])
     ;
     true
   ).

% Get table names. First argument, if bound, represent a table for which it is asked if exists
% First clause deals with '$des' database   
get_tablenames(Name,TableNames) :-
  current_db('$des',_),
  !,
  my_nf_bagof(Name,
        Arity^SQLst^DLs^LVDs^SCs^
        (my_table('$des',Name,Arity),
         my_not(my_view('$des',Name,Arity,SQLst,DLs,LVDs,SCs))),
        TableNames).
% Next clauses deal with ODBC databases
get_tablenames(Name,TableNames) :-
  var(Name),
  !,
  my_odbc_get_tablenames(TableNames).
get_tablenames(Name,[Name]) :-
  my_odbc_exists_table(Name),
  !.
get_tablenames(_Name,[]).

get_viewnames(Name,ViewNames) :-
  current_db('$des',_),
  !,
  my_nf_bagof(Name,
        Arity^SQLst^DLs^LVDs^SCs^
        my_view('$des',Name,Arity,SQLst,DLs,LVDs,SCs),
        ViewNames).
% Next clauses deal with ODBC databases
get_viewnames(Name,ViewNames) :-
  var(Name),
  !,
  my_odbc_get_viewnames(ViewNames).
get_viewnames(Name,[Name]) :-
  my_odbc_exists_view(Name),
  !.
get_viewnames(_Name,[]).

get_localviewnames(ViewNames) :-
  current_db('$des',_),
  !,
  my_nf_bagof(LVDs,
        Name^Arity^SQLst^DLs^SCs^
        my_view('$des',Name,Arity,SQLst,DLs,LVDs,SCs),
        ListViewNames),
  concat_lists(ListViewNames,DViewNames),
  remove_duplicates(DViewNames,ViewNames).
get_localviewnames([]).

list_schema_list([]).
list_schema_list([TableName|TableNames]) :-
  get_table_typed_schema(TableName,Table),
  (my_view(_DB,TableName,_Arity,SQLst,RNVss,LVDs,_SCs) -> 
    Type = view 
   ;
    Type = table),
  write_log_list([' * ',Table,nl]), 
  (Type==view -> 
   write_log_list(['    - Defining SQL Statement:',nl]),
   display_sql(SQLst,8),
   write_log_list(['    - Datalog equivalent rules:',nl]),
   (development(off) ->
    ORNVss=RNVss
    ;
    get_object_dlrules_list(RNVss,ODLs),
    dlrule_to_ruleNV_list(ODLs,ORNVss)),
   display_ruleNVs_list(ORNVss,8),
   (LVDs \== [] ->
     write_log_list(['    - Local view definitions:',nl,'        ',LVDs,nl])
    ;
     true)
   ; 
   list_table_constraints(TableName)),
  list_schema_list(TableNames).

list_table_constraints(TableName) :-
  (my_primary_key('$des',TableName,Atts) ->
   write_log_list(['    - PK: ',Atts,nl])
   ;
   true),
  !,
  ((my_foreign_key('$des',TableName,FKAtts,FTableName,RFKAtts),
    write_log_list(['    - FK: ',TableName,'.',FKAtts,' -> ',FTableName,'.',RFKAtts,nl]),
    fail)
   ;
   true).   

get_table_untyped_schema(TableName,Table) :-
  get_table_untyped_arguments(TableName,ColNames),
  Table =.. [TableName|ColNames].
  
get_table_typed_schema(TableName,Table) :-
  get_table_typed_arguments(TableName,ColNamesTypes),
  Table =.. [TableName|ColNamesTypes].

get_table_untyped_arguments(TableName,ColNames) :-
  current_db('$des',_Handle),
  !,
  setof((Pos,ColName),Type^my_attribute('$des',Pos,TableName,ColName,Type),PosColNames),
  my_nf_bagof(ColName,Pos^(my_member((Pos,ColName),PosColNames)),ColNames).
get_table_untyped_arguments(TableName,ColNames) :-
  my_odbc_get_colnames(TableName,ColNames).
  
get_table_typed_arguments(TableName,ColNameTypes) :-
  current_db('$des',_Handle),
  !,
  setof((Pos,ColName,Type),Type^my_attribute('$des',Pos,TableName,ColName,Type),PosColNameTypes),
  findall(ColName:Type,(my_member((Pos,ColName,Type),PosColNameTypes)),ColNameTypes).
get_table_typed_arguments(TableName,ColNameTypes) :-
  my_odbc_get_table_typed_arguments(TableName,ColNameTypes).
  
get_table_types(TableName,TypeNames) :-
  current_db('$des',_Handle),
  !,
  setof((Pos,ColName,Type),Type^my_attribute('$des',Pos,TableName,ColName,Type),PosColNameTypes),
  findall(Type,(my_member((Pos,ColName,Type),PosColNameTypes)),TypeNames).
get_table_types(TableName,TypeNames) :-
  my_odbc_get_table_typenames(TableName,TypeNames).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sqlst2dlsts(+SQLst,-Schema,-TableRen,-DLsts) 
% Translates a SQL Syntactic Tree
% into a list of Datalog Syntactic Trees
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sqlst2dlsts(SQLst,Schema,TableRen,DLsts) :-
  sqlst2rast(SQLst,RAst,[],TableRen),
  (RAst = (_RA,Schema) ; true),
  rast2crast(RAst,CRAst),
  crast2dlsts(CRAst,0,_,[],_OMap,TableRen,_ORen,DDLsts),
  disjunctive_to_conjunctive_rule_list(DDLsts,[],_NVs,_Cause,UDLsts),
  force_simplify_rules(UDLsts,DLsts,_Simplified),
%  UDLsts=DLsts,
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sqlst2rast(+SQLst,-RAst,+IRen,-ORen) 
% Translates a SQL Syntactic Tree (SQLST)
% into a Relational Algebra Syntactic Tree (RAST)
% Table and subquery autorenaming is done for unrenamed 
% tables and subqueries. All renamings are annotated in ORen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SQLst ::=
%   (
%    with(SQLst,SQLsts),
%    Renaming
%   )
%   |
%   (
%    select(AllDistinct, 
%           Args,
%           from(Rels),
%           where(Cond),
%           group_by(Cols),
%           having(Cond),
%           order_by(Cols,OrderSpecs)
%          ),
%    Renaming
%   )
%   |
%   (
%    union(SQL,SQL),
%    Renaming
%   )
%   |
%   (
%    except(SQL,SQL),
%    Renaming
%   )
%   |
%   (
%    intersect(SQL,SQL),
%    Renaming
%   )
%
% AllDistinct ::=
%   all
%   |
%   distinct
%
% Args ::=
%   *
%   |
%   [Arg,...,Arg]
%
% Arg ::=
%   attr(RelName,Attr,Renaming)
%   |
%   expr(Expression,Renaming,Type)
%
% Expression ::=
%   Arithmetic expression
%   |
%   SQLstmt
%
% Rels ::=
%   [Rel,...,Rel]
%
% Rel ::=
%   (Table,Renaming)
%   |
%   SQLst
%   |
%   JoinOp(Rel,Rel,JoinCond)
%
% JoinCond ::=
%   Cond
%   |
%   equijoin(natural)
%   |
%   equijoin([Attr,...,Attr])
%
% JoinOp ::=
%   inner_join
%   |
%   left_join
%   |
%   right_join
%   |
%   full_join
%
% Cond ::=
%   exists(SQLst)
%   |
%   in([AttrCte,...,AttrCte],SQLst)
%   |
%   not_in([AttrCte,...,AttrCte],SQLst)
%   |
%   AttrCte Operator AttrCte 
%   |
%   AttrCte Operator SQLst
%   |
%   SQLst Operator AttrCte
%   |
%   SQLst Operator SQLst
%   |
%   and(Cond,Cond)
%   |
%   or(Cond,Cond)
%   |
%   not(Cond)
%   |
%   true
%   |
%   false

sqlst2rast((union(R1,R2),[RR|RAS]),(union(AR1,AR2),[RR|RAS]),IRen,ORen) :-
  !,
  sqlst2rast(R1,AR1,IRen,Ren),
  sqlst2rast(R2,AR2,Ren,ORen),
  relation_autorenaming(RR),
  relation_arguments(AR1,RAS),
  arguments_autorenaming(RR,RAS).
sqlst2rast((except(R1,R2),[RR|RAS]),(minus(AR1,AR2),[RR|RAS]),IRen,ORen) :-
  !,
  sqlst2rast(R1,AR1,IRen,Ren),
  sqlst2rast(R2,AR2,Ren,ORen),
  relation_autorenaming(RR),
  relation_arguments(AR1,RAS),
  arguments_autorenaming(RR,RAS).
sqlst2rast((intersect(R1,R2),[RR|RAS]),(intersect(AR1,AR2),[RR|RAS]),IRen,ORen) :-
  !,
  sqlst2rast(R1,AR1,IRen,Ren),
  sqlst2rast(R2,AR2,Ren,ORen),
  relation_autorenaming(RR),
  relation_arguments(AR1,RAS),
  arguments_autorenaming(RR,RAS).
sqlst2rast((select(_DistinctAll,UProjList,
            from(SQLRelations),
            where(WhereCondition),
            group_by(GroupList),
            having(HavingCondition),
            order_by(OrderArgs,OrderSpecs)),[RR|_RArgs]),
           (pi(ProjList,
               sigma(RACondition,RARelation),
               group_by(GroupList),
               having(RAHavingCondition),
               order_by(OrderArgs,OrderSpecs)),[RR|ProjList]),
           IRen,ORen) :-
  !,
  sqlcond2racond(WhereCondition,RACondition,IRen,Ren11),
  sqlcond2racond(HavingCondition,RAHavingCondition,Ren11,Ren1),
  sqlrel2rarel(SQLRelations,RARelation,Ren1,Ren2),
  relation_autorenaming(RR),
  arguments_completion(UProjList,RARelation,UProjList,Ren2,ORen,ProjList),
  arguments_autorenaming(RR,ProjList),
  arguments_autorenaming(_GR,GroupList),
  arguments_autorenaming(_OR,OrderArgs).
sqlst2rast((inner_join(LRel,RRel,SQLCondition),[RR|_RArgs]),
           (pi(ProjList,
               sigma(RACondition,RARelation),
               group_by([]),
               having(true),
               order_by([],[])),[RR|ProjList]),
           IRen,ORen) :-
  !,
  sqlrel2rarel([LRel,RRel],RARelation,IRen,Ren1),
  RARelation = times((_RALR,LAS),(_RARR,RAS)),
  arguments_completion(*,RARelation,[],Ren1,Ren2,Atts),
  relation_autorenaming(RR),
  arguments_autorenaming(RR,Atts),
  sqljoincond2racond(SQLCondition,RACondition,LAS,RAS,Atts,ProjList,Ren2,ORen).
sqlst2rast((left_join(LRel,RRel,SQLCondition),[RR|_RArgs]),
           (pi(ProjList,
               sigma(true,left_join((RALR,LAS),(RARR,RAS),RACondition)),
               group_by([]),
               having(true),
               order_by([],[])),[RR|ProjList]),
           IRen,ORen) :-
  !,
  sqlrel2rarel([LRel,RRel],RARelation,IRen,Ren1),
  RARelation = times((RALR,LAS),(RARR,RAS)),
  arguments_completion(*,RARelation,[],Ren1,Ren2,Atts),
  relation_autorenaming(RR),
  arguments_autorenaming(RR,Atts),
  sqljoincond2racond(SQLCondition,RACondition,LAS,RAS,Atts,ProjList,Ren2,ORen).
sqlst2rast((right_join(LRel,RRel,SQLCondition),[RR|_RArgs]),
           (pi(ProjList,
               sigma(true,right_join((RALR,LAS),(RARR,RAS),RACondition)),
               group_by([]),
               having(true),
               order_by([],[])),[RR|ProjList]),
           IRen,ORen) :-
  !,
  sqlrel2rarel([LRel,RRel],RARelation,IRen,Ren1),
  RARelation = times((RALR,LAS),(RARR,RAS)),
  arguments_completion(*,RARelation,[],Ren1,Ren2,Atts),
  relation_autorenaming(RR),
  arguments_autorenaming(RR,Atts),
  sqljoincond2racond(SQLCondition,RACondition,LAS,RAS,Atts,ProjList,Ren2,ORen).
sqlst2rast((full_join(LRel,RRel,SQLCondition),[RR|_RArgs]),
           (pi(ProjList,
               sigma(true,full_join((RALR,LAS),(RARR,RAS),RACondition)),
               group_by([]),
               having(true),
               order_by([],[])),[RR|ProjList]),
           IRen,ORen) :-
  !,
  sqlrel2rarel([LRel,RRel],RARelation,IRen,Ren1),
  RARelation = times((RALR,LAS),(RARR,RAS)),
  arguments_completion(*,RARelation,[],Ren1,Ren2,Atts),
  relation_autorenaming(RR),
  arguments_autorenaming(RR,Atts),
  sqljoincond2racond(SQLCondition,RACondition,LAS,RAS,Atts,ProjList,Ren2,ORen).
sqlst2rast((T,[RR|RArgs]),(T,[RR|RArgs]),Ren,[(T,RR)|Ren]) :-
  my_table('$des',T,_Arity),
  !,
  relation_autorenaming(RR),
  get_table_untyped_arguments(T,Args),
  build_ren_arguments(T,Args,RArgs),
  arguments_autorenaming(RR,RArgs).

build_ren_arguments(_T,[],[]).
build_ren_arguments(T,[A|As],[attr(T,A,_RA)|RAs]) :-
  build_ren_arguments(T,As,RAs).

relation_arguments((_R,[_RR|RAS]),RAS).

relation_autorenaming(AS) :-
  (var(AS) ->
   get_new_predicate_name(t,AS)
   ;
   true).

arguments_autorenaming(_RR,[]) :-
  !.
arguments_autorenaming(Rel,[expr(E,AS,_Type)|As]) :-
  !,
  argument_autorenaming(AS),
  expr_arguments_autorenaming(E),
  arguments_autorenaming(Rel,As).
arguments_autorenaming(Rel,[attr(Rel,_A,AS)|As]) :-
  !,
  argument_autorenaming(AS),
  arguments_autorenaming(Rel,As).
arguments_autorenaming(RR,[attr(_Rel,_A,AS)|As]) :-
  argument_autorenaming(AS),
  arguments_autorenaming(RR,As).

expr_arguments_autorenaming(E) :-
  (number(E) ; atom(E)),
  !.
expr_arguments_autorenaming(expr_ref(_AS)) :- 
  !.
expr_arguments_autorenaming(attr(_Rel,_A,AS)) :- 
  !,
  argument_autorenaming(AS).
expr_arguments_autorenaming(E) :- 
  E =.. [_F|Args],
  !, 
  expr_arguments_autorenaming_list(Args).

expr_arguments_autorenaming_list([]) :-
  !.
expr_arguments_autorenaming_list([E|Es]) :-
  !, 
  expr_arguments_autorenaming(E), 
  expr_arguments_autorenaming_list(Es).

argument_autorenaming(AS) :-
  (var(AS) ->
   get_new_predicate_name(a,AS)
   ;
   true).

arguments_completion(*,(pi(ProjList,_S,_G,_H,_O),[RR|RArgs]),_PL,IRen,ORen,RArgs) :-
  !,
  rel_ren_projlist(ProjList,RR,IRen,ORen).
arguments_completion(*,(T,[AS|_RArgs]),_PL,Ren,Ren,ProjList) :-
  my_table('$des',T,_Arity),
  !,
  my_nf_bagof(attr(AS,C,CRen),
             I^Type^(my_attribute('$des',I,T,C,Type),
                     argument_autorenaming(CRen)),
             ProjList).
arguments_completion(*,(_Rel,[_AS|RArgs]),_PL,Ren,Ren,RArgs) :-
  !.
arguments_completion(*,times(T1,T2),_PL,IRen,ORen,ProjList) :-
  arguments_completion(*,T1,[],IRen,Ren1,PL1),
  arguments_completion(*,T2,[],Ren1,ORen,PL2),
  my_append(PL1,PL2,ProjList).
arguments_completion([],_RAst,_PL,Ren,Ren,[]) :-
  !.
arguments_completion([(Rel,(*))|As],(RAst,[Rel|RArgs]),PL,IRen,ORen,CAs) :-
  !,
  arguments_completion(As,RAst,PL,IRen,ORen,CA1s),
  my_append(RArgs,CA1s,CAs).
arguments_completion([(Rel,(*))|As],RAst,PL,IRen,ORen,CAs) :-
  !,
  rel_arguments(Rel,RAst,IRen,Ren1,RAs),
  arguments_completion(As,RAst,PL,Ren1,ORen,CA1s),
  my_append(RAs,CA1s,CAs).
% An incorrectly assumed attribute (which is in fact a reference to an expression)
% is translated into a reference to an expression
arguments_completion([attr(_Rel,A,AS)|As],RAst,PL,IRen,ORen,[expr(expr_ref(A),AS,_Type)|CAs]) :-
  pl_expr_member(expr(_E,A,_T),PL),
  !,
  arguments_completion(As,RAst,PL,IRen,ORen,CAs).
% References to renamed attributes
arguments_completion([attr(_Rel,A,AS)|As],RAst,PL,IRen,ORen,[attr(R,C,AS)|CAs]) :-
  pl_attr_member(attr(R,C,A),PL),
  !,
  arguments_completion(As,RAst,PL,IRen,ORen,CAs).
arguments_completion([attr(Rel,A,AS)|As],RAst,PL,IRen,ORen,[attr(Rel,A,AS)|CAs]) :-
  argument_completion(attr(Rel,A,AS),RAst),
  !,
  arguments_completion(As,RAst,PL,IRen,ORen,CAs).
arguments_completion([expr(E,AS,Type)|As],RAst,PL,IRen,ORen,[expr(RE,AS,Type)|CAs]) :-
  expr_argument_completion(E,RAst,PL,RE),
  arguments_completion(As,RAst,PL,IRen,ORen,CAs).

% Projection list member. Checks whether the attribute attr(Rel,A,AS) can be found in the projection list.
% Note that the attribute can be a variable
% Reference to the attribute name:
pl_attr_member(attr(Rel,A,_AS),[attr(Rel,Arg,_RArg)|_PL]) :-
  A==Arg,
  !.
% Reference to the attribute renaming:
pl_attr_member(attr(Rel,A,AS),[attr(Rel,A,RArg)|_PL]) :-
  AS==RArg,
  !.
pl_attr_member(attr(Rel,A,AS),[_Attr|PL]) :-
  pl_attr_member(attr(Rel,A,AS),PL).
  
pl_expr_member(expr(E,A,Type),[expr(E,AS,Type)|_PL]) :-
  A==AS,
  !.
pl_expr_member(expr(E,A,Type),[_Arg|PL]) :-
  pl_expr_member(expr(E,A,Type),PL).

expr_argument_completion(E,_RAst,_PL,E) :- 
  (number(E) ; atom(E)),
  !.
%expr_argument_completion(attr(_Rel,A,_AS),_RAst,PL,expr_ref(A)) :- 
expr_argument_completion(attr(Rel,A,_AS),_RAst,PL,expr_ref(A)) :- 
  var(Rel),
  pl_expr_member(expr(_E,A,_Type),PL),
  !.
%expr_argument_completion(attr(_Rel,A,_AS),_RAst,PL,attr(R,C,A)) :- 
expr_argument_completion(attr(Rel,A,_AS),_RAst,PL,attr(R,C,A)) :- 
  var(Rel),
  pl_attr_member(attr(R,C,A),PL),
  !.
expr_argument_completion(attr(Rel,A,AS),RAst,_PL,attr(Rel,A,AS)) :- 
  !,
  argument_completion(attr(Rel,A,AS),RAst).
expr_argument_completion((SQLst,RR),_RAst,_PL,RAst) :-
  var(RR),
  !,
  sqlst2rast((SQLst,RR),RAst,[],_ORen).  
expr_argument_completion(E,RAst,PL,RE) :- 
  E =.. [F|Args],
  !, 
  expr_argument_completion_list(Args,RAst,PL,RArgs),
  RE =.. [F|RArgs].

expr_argument_completion_list([],_RAst,_PL,[]) :-
  !.
expr_argument_completion_list([E|Es],RAst,PL,[RE|REs]) :-
  !, 
  expr_argument_completion(E,RAst,PL,RE), 
  expr_argument_completion_list(Es,RAst,PL,REs).

argument_completion(attr(Rel,A,AS),RAst) :-
  (var(Rel) -> argument_completion_ng(attr(Rel,A,AS),RAst) ; true).

argument_completion_ng(attr(RT,C,_AS),(T,[RT|_RArgs])) :-
  my_attribute('$des',_Nth,T,C,_Type).
argument_completion_ng(A,times(R1,R2)) :-
  argument_completion_ng(A,R1),
  !
  ;
  argument_completion_ng(A,R2).
argument_completion_ng(attr(AS,A,_AS),(pi(ProjList,_S,_G,_H,_O),[AS|_Args])) :-
  find_argument(A,ProjList).
  
find_argument(A,[attr(_Rel,A,_RenA)|_As]) :-
  !.
find_argument(RenA,[attr(_Rel,_A,RenA)|_As]) :-
  !.
find_argument(A,[_|As]) :-
  find_argument(A,As).
  
rel_ren_projlist([],_AS,Ren,Ren).
rel_ren_projlist([attr(Rel,_Col,_ColRen)|As],AS,IRen,ORen) :-
  rel_ren_projlist(As,AS,[(AS,Rel)|IRen],ORen).

rel_arguments(Rel,RAst,IRen,ORen,RAs) :-
  arguments_completion(*,RAst,[],IRen,ORen,ProjList),
  (my_member((Rel,RelId),ORen) -> true ; RelId = Rel),
  filter_rel_arg(RelId,ProjList,RAs).

filter_rel_arg(_RelId,[],[]).
filter_rel_arg(RelId,[attr(RelId,A,AS)|RAs],[attr(RelId,A,AS)|FRAs]) :-
  !, 
  filter_rel_arg(RelId,RAs,FRAs).
filter_rel_arg(RelId,[_|RAs],FRAs) :-
  filter_rel_arg(RelId,RAs,FRAs).

% SQL condition to RA condition
%sqlcond2racond(not(exists(Rel)),not(exists(RRel)),IRen,ORen) :-
%  !,
%  sqlst2rast(Rel,RRel,IRen,ORen).
sqlcond2racond(exists(Rel),exists(RRel),IRen,ORen) :-
  !,
  sqlst2rast(Rel,RRel,IRen,ORen).
sqlcond2racond(not_in(Args,Rel),not_in(Args,RRel),IRen,ORen) :-
  !,
  sqlst2rast(Rel,RRel,IRen,ORen).
sqlcond2racond(in(Args,Rel),in(Args,RRel),IRen,ORen) :-
  !,
  arguments_autorenaming(_RR,Args),
  sqlst2rast(Rel,RRel,IRen,ORen).
sqlcond2racond(is_null(Rel),is_null(RRel),IRen,ORen) :-
  !,
  sqlcond2racond(Rel,RRel,IRen,ORen).
sqlcond2racond(not(C),not(RC),IRen,ORen) :-
  !,
  sqlcond2racond(C,RC,IRen,ORen).
sqlcond2racond((SQLst,RR),RAst,IRen,ORen) :-
  var(RR),
  !,
  sqlst2rast((SQLst,RR),RAst,IRen,ORen).
sqlcond2racond(SQLC,RAC,IRen,ORen) :-
  SQLC =.. [Op,L,R],
%  WARNING: UNREMARKED because of "select * from t where a <> null"
%  my_sql_op(Op),
  my_sql_op(Op),
  !,
  sqlcond2racond(L,LRRel,IRen,Ren),
  sqlcond2racond(R,RRRel,Ren,ORen),
  RAC =.. [Op,LRRel,RRRel].
% sqlcond2racond(A,A,Ren,Ren) :-
%   arguments_autorenaming(_R,[A]),
%   !.
% sqlcond2racond(C,C,Ren,Ren).
sqlcond2racond(E,E,Ren,Ren) :-
  expr_arguments_autorenaming(E).
  
% expr_arguments_autorenaming(A) :-
%   arguments_autorenaming(_R,[A]),
%   !.
% expr_arguments_autorenaming(E) :-
%   E=..[_F|Args],
%   expr_arguments_autorenaming_list(Args).
% 
% expr_arguments_autorenaming_list([]).
% expr_arguments_autorenaming_list([A|As]):-
%   expr_arguments_autorenaming(A),
%   expr_arguments_autorenaming_list(As).

% SQL join condition to RA condition
sqljoincond2racond(equijoin(natural),C,_LAS,_RAS,IProjList,OProjList,Ren,Ren) :-
  !,
  eq_common_atts(IProjList,OProjList,C).
sqljoincond2racond(equijoin(Atts),C,LAS,RAS,ProjList,ProjList,Ren,Ren) :-
  !,
  build_equijoin(Atts,LAS,RAS,ProjList,C).
sqljoincond2racond(SQLCondition,RACondition,_LAS,_RAS,ProjList,ProjList,IRen,ORen) :-
  sqlcond2racond(SQLCondition,RACondition,IRen,ORen).

% IAtts: Input attribute list
% OAtts: Output attribute list (common attributes removed)
% EqCommonAtts: Output conjunctive equality condition on common attributes
eq_common_atts(IAtts,OAtts,EqCommonAtts) :-
  list_eq_common_atts(IAtts,OAtts,LEqCommonAtts),
  conjunctive_cond(LEqCommonAtts,EqCommonAtts).

list_eq_common_atts([],[],[]).
list_eq_common_atts([attr(RelA,A,RenA)|Atts],OAtts,[attr(RelA,A,RenA)=attr(RelB,A,RenB)|EqAtts]) :-
  my_member(attr(RelB,A,RenB),Atts),
  RelA \== RelB,
  !,
  list_eq_common_atts(Atts,OAtts,EqAtts).
list_eq_common_atts([Att|Atts],[Att|OAtts],EqAtts) :-
  list_eq_common_atts(Atts,OAtts,EqAtts).

build_equijoin([],_LAS,_RAS,_ProjList,true).
build_equijoin([attr(_RAtt,Att,_ArrRen)],[LAS|_LRArgs],[RAS|_RRArgs],ProjList,attr(LAS,Att,RA)=attr(RAS,Att,RB)) :-
  my_member(attr(LAS,Att,RA),ProjList),
  my_member(attr(RAS,Att,RB),ProjList).
build_equijoin([Att1,Att2|Atts],LAS,RAS,ProjList,and(C1,C2)) :-
  build_equijoin([Att1],LAS,RAS,ProjList,C1),
  build_equijoin([Att2|Atts],LAS,RAS,ProjList,C2).

% Building a conjunctive condition from a list of conditions
conjunctive_cond([],true) :-
  !.
conjunctive_cond([C],C) :-
  !.
conjunctive_cond([C1,C2],and(C1,C2)) :- 
  !.
conjunctive_cond([C|Cs],and(C,CC)) :- 
  conjunctive_cond(Cs,CC).

% SQL relation to RA relation
sqlrel2rarel([R],CR,IRen,ORen) :-
  sqlst2rast(R,CR,IRen,ORen).
sqlrel2rarel([A,B|Rs],times(CA,RRs),IRen,ORen) :-
  sqlst2rast(A,CA,IRen,Ren),
  sqlrel2rarel([B|Rs],RRs,Ren,ORen).
         

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rast2crast(+RAst,-CRAst) Translates a 
% Relational Algebra Syntactic Tree into a
% Canonical Relational Algebra Syntactic Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EXISTS condition
rast2crast((pi(As,sigma(exists(Rel),Rs),G,H,O),AS),
           (pi(As,sigma(exists(CRel),CRs),G,H,O),AS)) :-
  !,
  rast2crast(Rel,CRel),
  rast2crast(Rs,CRs).

% IN condition
rast2crast((pi(As,sigma(in(Args,Rel),Rs),G,H,O),AS),
           (pi(As,sigma(in(Args,CRel),CRs),G,H,O),AS)) :-
  !,
  rast2crast(Rel,CRel),
  rast2crast(Rs,CRs).

% NOT IN condition
rast2crast((pi(As,sigma(not_in(Args,Rel),Rs),G,H,O),AS),
           (pi(As,sigma(not_in(Args,CRel),CRs),G,H,O),AS)) :-
  !,
  rast2crast(Rel,CRel),
  rast2crast(Rs,CRs).

% IS NULL condition
rast2crast((pi(As,sigma(is_null(Rel),Rs),G,H,O),AS),
           (pi(As,sigma(is_null(CRel),CRs),G,H,O),AS)) :-
  !,
  rast2crast(Rel,CRel),
  rast2crast(Rs,CRs).

% Sigma condition
rast2crast((pi(As,sigma(C,Rs),G,H,O),AS),
           (pi(As,sigma(SC,Rs),G,H,O),AS)) :-
  !,
  simplify_cond(C,SC).
    
% TIMES
rast2crast(times(R1,R2),times(CR1,CR2)) :-
  !,
  rast2crast(R1,CR1),
  rast2crast(R2,CR2).

% OUTER JOINS
rast2crast(left_join(R1,R2,C),left_join(CR1,CR2,C)) :-
  !,
  rast2crast(R1,CR1),
  rast2crast(R2,CR2).
rast2crast(right_join(R1,R2,C),right_join(CR1,CR2,C)) :-
  !,
  rast2crast(R1,CR1),
  rast2crast(R2,CR2).  
rast2crast(full_join(R1,R2,C),full_join(CR1,CR2,C)) :-
  !,
  rast2crast(R1,CR1),
  rast2crast(R2,CR2).
  
% Table
rast2crast((T,AS),(T,AS)) :-
  my_table('$des',T,_),
  !.

% UNION operator
rast2crast((union(RA1,RA2),AS),(union(CRA1,CRA2),AS)) :-
  !,
  rast2crast(RA1,CRA1),
  rast2crast(RA2,CRA2). 

% INTERSECT operator
rast2crast((intersect(RA1,RA2),AS),(intersect(CRA1,CRA2),AS)) :-
  !,
  rast2crast(RA1,CRA1),
  rast2crast(RA2,CRA2). 

% MINUS operator
rast2crast((minus(RA1,RA2),AS),(minus(CRA1,CRA2),AS)) :-
  !,
  rast2crast(RA1,CRA1),
  rast2crast(RA2,CRA2). 

% Argument
rast2crast(A,A) :-
  !.

% Basic condition
basic_condition(true) :-
  !.
basic_condition(false) :-
  !.
basic_condition(C) :-
  C=..[Op,LA,RA],
  map_cond(Op,_),
  my_sql_constant_or_column(LA),
  my_sql_constant_or_column(RA),
  !.
basic_condition(not(C)) :-
  basic_condition(C),
  !.

% Simplify condition
simplify_cond(true,true) :-
  !.
simplify_cond(false,false) :-
  !.
simplify_cond(not(true),false) :-
  !.
simplify_cond(not(false),true) :-
  !.
simplify_cond(not(not(C)),SC) :-
  !,
  simplify_cond(C,SC).
simplify_cond(not(and(C1,C2)),SC) :-
  !,
  complement_cond(C1,NC1),
  complement_cond(C2,NC2),
  simplify_cond(or(NC1,NC2),SC).
simplify_cond(not(or(C1,C2)),SC) :-
  !,
  complement_cond(C1,NC1),
  complement_cond(C2,NC2),
  simplify_cond(and(NC1,NC2),SC).
simplify_cond(not(C),CC) :-
  complement_cond(C,CC),
  !.
simplify_cond(and(C1,C2),and(SC1,SC2)) :-
  !,
  simplify_cond(C1,SC1),
  simplify_cond(C2,SC2).
simplify_cond(or(C1,C2),or(SC1,SC2)) :-
  !,
  simplify_cond(C1,SC1),
  simplify_cond(C2,SC2).
simplify_cond(C,C).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% crast2dlsts(+CRAst,+Number,-LastNbr,+Mapping,-OMapping,
%             +Renaming,-ORenaming,-DLsts) 
% Translates a Canonical Relational Algebra Syntactic Tree 
% CRAst into a list of Datalog Syntactic Trees DLsts
% Mapping holds the correspondence between table columns and
% goal arguments 
% Renaming holds the already computed table and subquery renamings 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crast2dlsts((union(R1,R2),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
  !,
  N1 is N+1,
  crast2dlsts(R1,N1,N2,IMap,Map1,IRen,SRen,DLsts1),
  N3 is N2+1,
  DLsts1 = [':-'(H1,_)|_],
  crast2dlsts(R2,N3,LN,IMap,_Map2,SRen,ORen,DLsts2),
  DLsts2 = [':-'(H2,_)|_],
  H1 =.. [_|A1s],
  H2 =.. [_|A2s],
  get_new_predicate_name(p,N,_,PN),
  HH1 =.. [PN|A1s],
  HH2 =.. [PN|A2s],
  my_append([':-'(HH1,H1),':-'(HH2,H2)|DLsts1],DLsts2,DLsts),
  simplify_arglist_expr(RArgs,SRArgs),
  map_rel_id_var(RR,SRArgs,A1s,Map1,OMap).
crast2dlsts((minus(R1,R2),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
  !,
  N1 is N+1,
  crast2dlsts(R1,N1,N2,IMap,Map1,IRen,SRen,DLsts1),
  N3 is N2+1,
  DLsts1 = [':-'(SG1,_)|_],
  crast2dlsts(R2,N3,LN,Map1,Map2,SRen,ORen,DLsts2),
  DLsts2 = [':-'(H2,_)|_],
  SG1 =.. [_|A1s],
  H2 =.. [P2|_],
  SG2 =.. [P2|A1s],
  get_new_predicate_name(p,N,_,PN),
  HH1 =.. [PN|A1s],
  my_append([':-'(HH1,(SG1,not(SG2)))|DLsts1],DLsts2,DLsts),
  simplify_arglist_expr(RArgs,SRArgs),
  map_rel_id_var(RR,SRArgs,A1s,Map2,OMap).
crast2dlsts((intersect(R1,R2),[RR|RArgs]),N,LN,IMap,OMap,IRen,ORen,DLsts) :-
  !,
  N1 is N+1,
  crast2dlsts(R1,N1,N2,IMap,Map1,IRen,SRen,DLsts1),
  N3 is N2+1,
  DLsts1 = [':-'(SG1,_)|_],
  crast2dlsts(R2,N3,LN,Map1,Map2,SRen,ORen,DLsts2),
  DLsts2 = [':-'(H2,_)|_],
  SG1 =.. [_|A1s],
  H2 =.. [P2|_],
  SG2 =.. [P2|A1s],
  get_new_predicate_name(p,N,_,PN),
  HH1 =.. [PN|A1s],
  my_append([':-'(HH1,(SG1,SG2))|DLsts1],DLsts2,DLsts),
  simplify_arglist_expr(RArgs,SRArgs),
  map_rel_id_var(RR,SRArgs,A1s,Map2,OMap).
% Build the datalog rule for a pi operator:
% - rel_subgoals builds the conjunction of source relations (times)
%   It also computes the mapping of variables' subgoals to columns
% - exprs_subgoals builds the subgoals for expressions in the projection list
% - map_cols maps the projection list (ArgList) with the head arguments
% - build_id generates the name of the predicate (head)
% - cond_subgoals adds the condition to the subgoals
crast2dlsts((pi(ArgList,sigma(Condition,Relation),group_by(GroupByList),having(HavingCondition),order_by(OrderByList,_OrderingList)),[RR|ArgList]),
            N,LN,IMap,OMap,IRen,ORen,[':-'(Head,Body)|DLsts]) :-
  !,
  rel_subgoals(Relation,N,N1,RSGs,DLsts1,IMap,Map1,IRen,Ren1),
  simplify_arglist_expr(ArgList,SArgList),
  exprs_subgoals(SArgList,RR,IsAggr,Map1,Map2,Ren1,ESGs),
  map_cols(SArgList,Map2,Ren1,Args),
  map_cols(GroupByList,Map2,Ren1,GroupByArgs),
  map_cols(OrderByList,Map2,Ren1,_OrderByArgs),
  get_new_predicate_name(p,N,_,PN),
  Head =.. [PN|Args],
  cond_subgoals(sigma,Condition,N1,N2,Map2,Map3,Ren1,Ren2,DLsts2,WSGs),
  cond_subgoals(having,HavingCondition,N2,LN,Map3,OMap,Ren2,ORen,DLsts3,HSGs),
  my_append(DLsts1,DLsts2,DLsts4),
  my_append(DLsts3,DLsts4,DLsts),
  (GroupByList=[] -> true ; IsAggr=true),
  build_body(IsAggr,RSGs,ESGs,WSGs,HSGs,GroupByArgs,Body).
  
build_body(IsAggr,RSGs,ESGs,WSGs,_HSGs,_GroupByArgs,Body) :-
  var(IsAggr),
  !, 
  goals_append(RSGs,ESGs,SGs1),
  goals_append(SGs1,WSGs,SGs),
  reorder_goals(SGs,Body).
build_body(true,RSGs,ESGs,WSGs,HSGs,GroupByArgs,group_by(OGSGs,GroupByArgs,OCSGs)) :-
  goals_append(RSGs,WSGs,GSGs),
  reorder_goals(GSGs,OGSGs),
  goals_append(ESGs,HSGs,CSGs),
  reorder_goals(CSGs,OCSGs).

% Simplification of the projection list  
simplify_arglist_expr([],[]).
simplify_arglist_expr([expr(attr(Rel,C,_RC),AS,_Type)|Args],[attr(Rel,C,AS)|SArgs]) :-
  !,
  simplify_arglist_expr(Args,SArgs).
simplify_arglist_expr([Arg|Args],[Arg|SArgs]) :-
  !,
  simplify_arglist_expr(Args,SArgs).

% Subgoals built for the expressions in the projection list
exprs_subgoals(ArgList,RR,IsAggr,IMap,OMap,Ren,SGs) :-
  build_exprs_mappings(ArgList,RR,IMap,OMap),
  build_exprs_subgoals(ArgList,IsAggr,OMap,Ren,SGs).

build_exprs_mappings([],_RR,Map,Map).
build_exprs_mappings([expr(_SQLExpr,AS,_Type)|Args],RR,IMap,OMap) :-
  build_exprs_mappings(Args,RR,[(_Var,RR,AS)|IMap],OMap).
build_exprs_mappings([_Arg|Args],RR,IMap,OMap) :-
  build_exprs_mappings(Args,RR,IMap,OMap).

build_exprs_subgoals([],_IsAggr,_Map,_Ren,true).
build_exprs_subgoals([expr(SQLExpr,AS,Type)|Args],IsAggr,Map,Ren,SGs) :-
  !,
  build_expr_subgoals(expr(SQLExpr,AS,Type),IsAggr,Map,Ren,ESGs),
  build_exprs_subgoals(Args,IsAggr,Map,Ren,NSGs),
  goals_append(ESGs,NSGs,SGs).
build_exprs_subgoals([_Arg|Args],IsAggr,Map,Ren,SGs) :-
  build_exprs_subgoals(Args,IsAggr,Map,Ren,SGs).

build_expr_subgoals(expr(SQLExpr,AS,_Type),IsAggr,Map,Ren,Var=DLExpr) :-
  translate_expr(SQLExpr,DLExpr,IsAggr,Map,Ren),
  ((var(DLExpr),    % Expression reference
    my_member((Var,_,AS),Map),
    DLExpr\==Var)   % Avoid autoreferences
  ;
   my_member((Var,_,AS),Map)).
% build_expr_subgoals(expr(SQLExpr,AS,Type),IsAggr,Map,Ren,SG) :-
%   translate_expr(SQLExpr,DLExpr,IsAggr,Map,Ren),
%   my_member((Var,_,AS),Map),
%   (Type = number(_N) ->
%     ((abstract_nulls(DLExpr,NDLExpr),my_ground(NDLExpr)) ->
%      Var is DLExpr,
%      SG=true
%     ;
%      SG=is(Var,DLExpr))
%    ;
%     SG='='(Var,DLExpr)
%    ).

translate_expr(cte(Cte,_Type),Cte,_IsAggr,_Map,_Ren) :-
  !.
translate_expr(attr(Rel,Col,AS),Var,_IsAggr,Map,Ren) :-
  !,
  map_cols([attr(Rel,Col,AS)],Map,Ren,[Var]).
translate_expr(expr_ref(AS),Var,_IsAggr,Map,_Ren) :-
  !,
  my_member((Var,_,AS),Map).
translate_expr(SQLE,DLE,IsAggr,Map,Ren) :- 
  SQLE =.. [F|SQLArgs],
  !, 
  length(SQLArgs,A),
  (arithmetic_function(F,_,_,aggregate,_,A) ->
   IsAggr=true
   ;
   true
  ),
  translate_expr_list(SQLArgs,DLArgs,IsAggr,Map,Ren),
  DLE =.. [F|DLArgs].

translate_expr_list([],[],_IsAggr,_Map,_Ren) :-
  !.
translate_expr_list([T|Ts],[RT|RTs],IsAggr,Map,Ren) :-
  !, 
  translate_expr(T,RT,IsAggr,Map,Ren), 
  translate_expr_list(Ts,RTs,IsAggr,Map,Ren).

% Subgoals for the SQL relation
rel_subgoals(times(RelA,B),N,LN,(SGA,As),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  build_subgoal(RelA,N,N1,SGA,DLsts1,IMap,SGMap,IRen,SRen), 
  rel_subgoals(B,N1,LN,As,DLsts2,SGMap,OMap,SRen,ORen),
  my_append(DLsts1,DLsts2,DLsts).
rel_subgoals(left_join(RelA,RelB,C),N,LN,lj(SGA,SGB,SGC),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  build_subgoal(RelA,N,N1,SGA,DLstsA,IMap,Map1,IRen,Ren1), 
  build_subgoal(RelB,N1,N2,SGB,DLstsB,Map1,Map2,Ren1,Ren2), 
  on_subgoals(C,N2,LN,Map2,OMap,Ren2,ORen,DLstsC,SGC),
  concat_lists([DLstsA,DLstsB,DLstsC],DLsts).
rel_subgoals(right_join(RelA,RelB,C),N,LN,rj(SGA,SGB,SGC),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  build_subgoal(RelA,N,N1,SGA,DLstsA,IMap,Map1,IRen,Ren1), 
  build_subgoal(RelB,N1,N2,SGB,DLstsB,Map1,Map2,Ren1,Ren2), 
  on_subgoals(C,N2,LN,Map2,OMap,Ren2,ORen,DLstsC,SGC),
  concat_lists([DLstsA,DLstsB,DLstsC],DLsts).
rel_subgoals(full_join(RelA,RelB,C),N,LN,fj(SGA,SGB,SGC),DLsts,IMap,OMap,IRen,ORen) :-
  !, 
  build_subgoal(RelA,N,N1,SGA,DLstsA,IMap,Map1,IRen,Ren1), 
  build_subgoal(RelB,N1,N2,SGB,DLstsB,Map1,Map2,Ren1,Ren2), 
  on_subgoals(C,N2,LN,Map2,OMap,Ren2,ORen,DLstsC,SGC),
  concat_lists([DLstsA,DLstsB,DLstsC],DLsts).
rel_subgoals(Rel,N,LN,SG,DLsts,IMap,OMap,IRen,ORen) :-
  build_subgoal(Rel,N,LN,SG,DLsts,IMap,OMap,IRen,ORen).

build_subgoal((Table,[TableId|_RArgs]),N,N,SG,[],IMap,OMap,Ren,Ren) :-
  my_table('$des',Table,Arity), 
  !,
  Length is Arity+1, 
  length(SGs,Length),
  SGs=[Table|Vars], 
  SG=..SGs, 
  map_table_id_var((Table,TableId),Vars,1,IMap,OMap,Ren).
build_subgoal((pi(ArgList,R,G,H,O),[RR|RArgs]),N,LN,SG,DLsts,IMap,OMap,IRen,ORen) :-
  !,
  N1 is N+1,
  crast2dlsts((pi(ArgList,R,G,H,O),[RR|RArgs]),N1,LN,IMap,Map1,IRen,ORen,DLsts),
  DLsts = [':-'(SG,_)|_],
  SG=..[_|Vars],
  map_rel_id_var(RR,ArgList,Vars,Map1,OMap).
build_subgoal(Rel,N,LN,SG,DLsts,IMap,OMap,IRen,ORen) :-
  N1 is N+1,
  crast2dlsts(Rel,N1,LN,IMap,OMap,IRen,ORen,DLsts),
  DLsts = [':-'(SG,_)|_].
 
% map_rel_id_var(+AS,+ArgList,+Vars,+IMap,-OMap)
% Maps a SQL relation id to a variable
map_rel_id_var(_AS,[],[],Mapping,Mapping).
map_rel_id_var(AS,[attr(_,Col,ColRen)|Cols],[Var|Vars],IMap,OMap) :-
  !,
  map_rel_id_var(AS,Cols,Vars,[(Var,AS,Col),(Var,AS,ColRen)|IMap],OMap).
map_rel_id_var(AS,[attr(_,Col,_ColRen)|Cols],[Var|Vars],IMap,OMap) :-
  !,
  map_rel_id_var(AS,Cols,Vars,[(Var,AS,Col)|IMap],OMap).
map_rel_id_var(AS,[expr(_E,Ren,_Type)|Cols],[Var|Vars],IMap,OMap) :-
  map_rel_id_var(AS,Cols,Vars,[(Var,AS,Ren)|IMap],OMap).
  
% map_table_id_var(+(Table,TableId),+Vars,+IMap,-OMap,+Renaming)
% Maps a table (or table id) to a variable
map_table_id_var((_Table,_TableId),[],_,Mapping,Mapping,_Renaming).
map_table_id_var((Table,TableId),[Var|Vars],N,Mapping,Mapping,Renaming) :-
  my_member((Table,TableId),Renaming),
  my_attribute('$des',N,Table,Col,_Type), 
  my_member((Var,TableId,Col),Mapping), 
  !,
  N1 is N+1, 
  map_table_id_var((Table,TableId),Vars,N1,Mapping,Mapping,Renaming).
map_table_id_var((Table,TableId),[Var|Vars],N,IMapping,OMapping,Renaming) :-
  my_member((Table,TableId),Renaming),
  my_attribute('$des',N,Table,Col,_Type), 
  N1 is N+1, 
  map_table_id_var((Table,TableId),Vars,N1,[(Var,TableId,Col)|IMapping],OMapping,Renaming).

% map_cols(+Cols,+Mapping,+Renaming,-Arguments)
% maps a list of projected arguments with the arguments of the head
map_cols([],_Mapping,_Renaming,[]).
map_cols([attr(TableId,_Col,ColRen)|Cols],Mapping,Renaming,[Var|Xs]) :-
  my_member((Var,TableId,ColRen),Mapping), 
  !, 
  map_cols(Cols,Mapping,Renaming,Xs).
map_cols([attr(TableId,Col,_ColRen)|Cols],Mapping,Renaming,[Var|Xs]) :-
  my_member((Var,TableId,Col),Mapping), 
  !, 
  map_cols(Cols,Mapping,Renaming,Xs).
map_cols([attr(Table,Col,_ColRen)|Cols],Mapping,Renaming,[Var|Xs]) :-
  my_member((Table,TableId),Renaming),
  my_member((Var,TableId,Col),Mapping), 
  !, 
  map_cols(Cols,Mapping,Renaming,Xs).
map_cols([expr(_Expr,ColRen,_Type)|Cols],Mapping,Renaming,[Var|Xs]) :-
  my_member((Var,_TableId,ColRen),Mapping), 
  !, 
  map_cols(Cols,Mapping,Renaming,Xs).
% map_cols([Constant|Cols],Mapping,Renaming,[Constant|Xs]) :-
%   atomic(Constant), 
map_cols([cte(Constant,_Type)|Cols],Mapping,Renaming,[Constant|Xs]) :-
  map_cols(Cols,Mapping,Renaming,Xs).

% Subgoals for the ON condition (JOIN clauses)
on_subgoals(C,N,LN,IMap,OMap,IRen,ORen,DLsts,SGs) :-
  cond_subgoals(on,C,N,LN,IMap,OMap,IRen,ORen,DLsts,SGs).
  %,
  %(CSGs == ('.') -> OSGs = true ; OSGs = CSGs).

% Subgoals for the WHERE condition (SELECT statements)
cond_subgoals(SOH,C,N,LN,IMap,OMap,IRen,ORen,DLsts,SGs) :-
  translate_cond(SOH,C,N,LN,IMap,OMap,IRen,ORen,DLsts,SGs).

translate_cond(_SOH,true,N,N,Map,Map,Ren,Ren,[],true) :- 
%translate_cond(_SOH,true,N,N,Map,Map,Ren,Ren,[],('.')) :- 
  !.
translate_cond(_SOH,false,N,N,Map,Map,Ren,Ren,[],(false)) :- 
  !.
% translate_cond(sigma,'='(A,B),N,N,Map,Map,Ren,Ren,[],(true)) :-
%  map_cols([A,B],Map,Ren,[VA,VB]),
%  (var(VA); var(VB)), !,
%  VA=VB.
translate_cond(SOH,not(C),N,LN,IMap,OMap,IRen,ORen,DLsts,not(NC)) :-
  !,
  translate_cond(SOH,C,N,LN,IMap,OMap,IRen,ORen,DLsts,NC).
translate_cond(_SOH,and(C1,C2),N,LN,IMap,OMap,IRen,ORen,DLsts,NC) :-
  translate_cond(SOH,C1,N,N1,IMap,Map1,IRen,Ren1,DLsts1,NC1),
  translate_cond(SOH,C2,N1,LN,Map1,OMap,Ren1,ORen,DLsts2,NC2),
%  tuple_append(NC1,NC2,NC),
  goals_append(NC1,NC2,NC),
  my_append(DLsts1,DLsts2,DLsts).
translate_cond(SOH,or(C1,C2),N,LN,IMap,OMap,IRen,ORen,DLsts,(NC1;NC2)) :-
  translate_cond(SOH,C1,N,N1,IMap,Map1,IRen,Ren1,DLsts1,NC1),
  translate_cond(SOH,C2,N1,LN,Map1,OMap,Ren1,ORen,DLsts2,NC2),
  my_append(DLsts1,DLsts2,DLsts).
translate_cond(_SOH,exists(Rel),N,LN,IMap,OMap,IRen,ORen,DLsts,(Goal)) :-
  !,
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  crast2dlsts(Rel,N1,LN,IMap,OMap,IRen,ORen,UDLsts),
  UDLsts=[':-'(Head,_)|_],
  functor(Head,H,_),
  replace_head_list(H,H,UDLsts,RUDLsts),
  add_correlated_vars(IMap,RUDLsts,DLsts),
  DLsts=[':-'(Goal,_)|_].
translate_cond(_SOH,not_in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,DLsts,not(Goal)) :-
  !,
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  map_cols(Args,IMap,IRen,Vars),
  crast2dlsts(Rel,N1,LN,IMap,OMap,IRen,ORen,UDLsts),
  add_correlated_vars(IMap,UDLsts,CDLsts),
  CDLsts = [':-'(Goal,_)|_],
  Goal =.. [_F|GVars],
  my_append(Vars,_CorrVars,GVars),
  copy_term(CDLsts,DLsts).
translate_cond(_SOH,in(Args,Rel),N,LN,IMap,OMap,IRen,ORen,DLsts,(Goal)) :-
  !,
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  map_cols(Args,IMap,IRen,Vars),
  crast2dlsts(Rel,N1,LN,IMap,OMap,IRen,ORen,UDLsts),
  add_correlated_vars(IMap,UDLsts,DLsts),
  DLsts = [':-'(Goal,_)|_],
  Goal =.. [_F|GVars],
  my_append(Vars,_CorrVars,GVars).
%translate_cond(_SOH,is_null(Arg),N,N,Map,Map,Ren,Ren,[],(Var='$NULL'(_ID))) :-
translate_cond(_SOH,is_null(Arg),N,N,Map,Map,Ren,Ren,[],(is_null(Var))) :-
  my_sql_constant_or_column(Arg),
  !,
  map_cols([Arg],Map,Ren,[Var]).
translate_cond(_SOH,is_null(Rel),N,LN,IMap,OMap,IRen,ORen,DLsts,(Goal,GVar='$NULL'(_ID))) :-
  !,
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  crast2dlsts(Rel,N1,LN,IMap,OMap,IRen,ORen,UDLsts),
  add_correlated_vars(IMap,UDLsts,DLsts),
  DLsts = [':-'(Goal,_)|_],
  Goal =.. [_F,GVar].
translate_cond(_SOH,C,N,LN,IMap,OMap,IRen,ORen,DLsts,Goals) :-
   C=..[RelOp,Exp1,Exp2],
   map_cond(RelOp,DLOp),
   !,
   translate_expr_varcte(Exp1,N ,N1,IMap,Map1,IRen,Ren1,DLsts1,VarCte1,Goals1),
   translate_expr_varcte(Exp2,N1,LN,Map1,OMap,Ren1,ORen,DLsts2,VarCte2,Goals2),
   my_append(DLsts1,DLsts2,DLsts),
%   tuple_append(Goals1,Goals2,Goals3),
   goals_append(Goals1,Goals2,Goals3),
   NC=..[DLOp,VarCte1,VarCte2],
%   tuple_append(Goals3,NC,Goals).
   goals_append(Goals3,NC,Goals).
 
translate_expr_varcte(Exp,N,N1,IMap,OMap,IRen,ORen,DLsts,VarCte,Goals) :-   
  translate_expr(Exp,N,N1,IMap,OMap,IRen,ORen,DLsts,DLExp,Goals1),
  (my_var_or_constant(DLExp) ->
    VarCte=DLExp,
    Goals=Goals1
    ;
%    tuple_append(VarCte is DLExp,Goals1,Goals)).
    goals_append(VarCte = DLExp,Goals1,Goals)).
   
my_var_or_constant(T) :-
  (var(T) ; number(T) ; atom(T) ; T='$NULL'(_ID)), !.
  
% Translating SQL expressions
%translate_expr(C,N,N,Map,Map,Ren,Ren,[],Var,('.')) :-
translate_expr(C,N,N,Map,Map,Ren,Ren,[],Var,true) :-
  my_sql_constant_or_column(C),
  !,
  map_cols([C],Map,Ren,[Var]).
%translate_expr(expr_ref(AS),N,N,Map,Map,Ren,Ren,[],Var,('.')) :-
translate_expr(expr_ref(AS),N,N,Map,Map,Ren,Ren,[],Var,true) :-
  !,
  my_member((Var,_,AS),Map).
translate_expr(Rel,N,LN,IMap,OMap,IRen,ORen,DLsts,Var,Goal) :-
  my_dql_relation(Rel),
  !,
  N1 is N+1,
  get_new_predicate_name(p,N1,_,_PN),
  crast2dlsts(Rel,N1,LN,IMap,OMap,IRen,ORen,DLsts),
  DLsts=[':-'(Goal,_Body)|_Rules],
  Goal =.. [_F,Var].
translate_expr(SQLE,N,LN,IMap,OMap,IRen,ORen,DLsts,DLE,Goals) :- 
  SQLE =.. [F|SQLArgs],
  !, 
  translate_expr_list(SQLArgs,N,LN,IMap,OMap,IRen,ORen,DLsts,DLArgs,Goals),
  DLE =.. [F|DLArgs].

%translate_expr_list([],N,N,Map,Map,Ren,Ren,[],[],('.')) :-
translate_expr_list([],N,N,Map,Map,Ren,Ren,[],[],true) :-
  !.
translate_expr_list([T|Ts],N,LN,IMap,OMap,IRen,ORen,DLsts,[RT|RTs],Goals) :-
  !, 
  translate_expr(T,N,N1,IMap,Map1,IRen,Ren1,DLsts1,RT,Goals1), 
  translate_expr_list(Ts,N1,LN,Map1,OMap,Ren1,ORen,DLsts2,RTs,Goals2),
  my_append(DLsts1,DLsts2,DLsts),
%  tuple_append(Goals1,Goals2,Goals).
  goals_append(Goals1,Goals2,Goals).
  
my_dql_relation((_SQLst,RR)) :-
  RR \= (_C,_R).

my_sql_constant_or_column(Arg) :-
  my_sql_constant(Arg).
my_sql_constant_or_column(Arg) :-
  my_sql_column(Arg).
  
my_sql_constant(cte(_Cte,_Type)).
my_sql_column(attr(_RT,C,_R)) :-
  my_attribute('$des',_P,_T,C,_Type),
  !.

my_sql_op(Op) :-
  map_cond(Op,_).
my_sql_op(and).
my_sql_op(or).
%my_sql_op(not).
%my_sql_op(in).
%my_sql_op(exists).
%my_dl_op(Op) :-
%  map_cond(_,Op).

% map_cond(+RelationalOperator,-DatalogOperator).
map_cond('<=','=<').
map_cond('=','=').
map_cond('<>','\\='). 
map_cond('>=','>=').
map_cond('>','>').
map_cond('<','<').

map_cond('<=_all','=<').
map_cond('=_all','=').
map_cond('<>_all','\\='). 
map_cond('>=_all','>=').
map_cond('>_all','>').
map_cond('<_all','<').

map_cond('<=_any','=<').
map_cond('=_any','=').
map_cond('<>_any','\\='). 
map_cond('>=_any','>=').
map_cond('>_any','>').
map_cond('<_any','<').

% complement
complement_cond(true,false) :-
  !.
complement_cond(false,true) :-
  !.
complement_cond(not(C),CC) :-
  simplify_cond(C,CC),
  !.
complement_cond(and(C1,C2),or(CC1,CC2)) :-
  !,
  complement_cond(C1,CC1),
  complement_cond(C2,CC2).
complement_cond(or(C1,C2),and(CC1,CC2)) :-
  !,
  complement_cond(C1,CC1),
  complement_cond(C2,CC2).
complement_cond(C,NC) :-
  C=..[Op|As],
  map_cond(Op,_),
  !,
  complement_RA_op(Op,NOp),
  NC=..[NOp|As].

% complemented RA operator
complement_RA_op('<=','>').
complement_RA_op('=','<>').
complement_RA_op('<>','='). 
complement_RA_op('>=','<').
complement_RA_op('>','<=').
complement_RA_op('<','>=').

% adds correlated variables
add_correlated_vars(Map,UDLsts,DLsts) :-
  my_term_variables(Map,MapVars),
  lfp_add_rule_correlated_vars(MapVars,[],CHs,[':-'('$p0','$p1')|UDLsts],[_|CDLsts]),
  add_head_correlated_void_vars_list(CDLsts,CHs,DLsts).

add_head_correlated_void_vars_list([],_CHs,[]).
add_head_correlated_void_vars_list([':-'(H,B)|UDLsts],CHs,[':-'(CH,B)|TDLsts]) :-
  add_head_correlated_void_vars(H,CHs,CH),
  add_head_correlated_void_vars_list(UDLsts,CHs,TDLsts).
add_head_correlated_void_vars_list([H|UDLsts],CHs,[H|TDLsts]) :-
  add_head_correlated_void_vars_list(UDLsts,CHs,TDLsts).

add_head_correlated_void_vars(H,CHs,CH) :-
  functor(H,F,A),
  my_member((F,A1,Vs),CHs),
  !,
  length(Vs,LVs),
  N is A1+LVs-A,
  void_list(N,Voids),
  H=..[F|Args],
  my_append(Args,Voids,ExtArgs),
  CH=..[F|ExtArgs].
add_head_correlated_void_vars(H,_CHs,H).

void_list(0,[]) :-
  !.
void_list(N,[void|Voids]) :-
  N1 is N-1,
  void_list(N1,Voids).

lfp_add_rule_correlated_vars(MapVars,CHsi,CHso,DLstsi,DLstso) :-
  add_head_correlated_vars_list(MapVars,DLstsi,CHsi,CHs1,DLsts1),
%  set_head_void_args_list(DLsts1,DLsts2),
  DLsts1=DLsts2,
  add_body_correlated_vars_list(DLsts2,CHs1,DLsts3),
  (DLstsi==DLsts3 ->
   DLstso=DLsts3,
   CHso=CHsi
   ;
   lfp_add_rule_correlated_vars(MapVars,CHs1,CHso,DLsts3,DLstso)).

add_head_correlated_vars_list(_MapVars,[],CPs,CPs,[]).
add_head_correlated_vars_list(MapVars,[':-'(H,B)|UDLsts],ICPs,OCPs,[':-'(CH,B)|TDLsts]) :-
  !,
  add_head_correlated_vars(MapVars,H,B,ICPs,NICPs,CH),
  add_head_correlated_vars_list(MapVars,UDLsts,NICPs,OCPs,TDLsts).
add_head_correlated_vars_list(MapVars,[H|UDLsts],ICPs,OCPs,[CH|TDLsts]) :-
  add_head_correlated_vars(MapVars,H,true,ICPs,NICPs,CH),
  add_head_correlated_vars_list(MapVars,UDLsts,NICPs,OCPs,TDLsts).

add_head_correlated_vars(MapVars,H,B,ICPs,OCPs,CH) :-
  my_term_variables(H,HVars),
  my_term_variables(B,BVars),
  my_subtract_var(BVars,HVars,FCVars),
  my_intersect_var(FCVars,MapVars,CVars),  
  functor(H,F,A),
  (my_member((F,AI,Vs),ICPs) ->
    my_union_var(Vs,CVars,UOCVars),
    my_quicksort_pred(UOCVars,'@<',OCVars),
    add_atom_vars(OCVars,H,AI,CH),
    replace_list((F,AI,Vs),(F,AI,OCVars),ICPs,OCPs)
    ;
    my_quicksort_pred(CVars,'@<',OCVars),
    add_atom_vars(OCVars,H,A,CH),
    OCPs=[(F,A,OCVars)|ICPs]).
  
add_atom_vars(Vars,H,A,HC) :-
  H =.. [P|HVars],
  take_N(A,HVars,OVars),
  my_append(OVars,Vars,HCVars),
  HC =.. [P|HCVars].

% set_head_void_args_list([],[]).
% set_head_void_args_list([':-'(CH,B)|UDLsts],[':-'(VCH,B)|TDLsts]) :-
%   !,
%   set_head_void_args(CH,B,VCH),
%   set_head_void_args_list(UDLsts,TDLsts).
% set_head_void_args_list([CH|UDLsts],[VCH|TDLsts]) :-
%   set_head_void_args(CH,true,VCH),
%   set_head_void_args_list(UDLsts,TDLsts).

% set_head_void_args(CH,B,VCH) :-
%   my_term_variables(CH,CHVars),
%   my_term_variables(B,BVars),
%   my_subtract_var(CHVars,BVars,UVars),
%   replace_unsafe_vars(CH,UVars,VCH).
%   
% replace_unsafe_vars(CH,UVars,VCH) :-
%   CH=..[F|Args],
%   replace_unsafe_vars_list(Args,UVars,RArgs),
%   VCH=..[F|RArgs].

% replace_unsafe_vars_list([],_UVars,[]).
% replace_unsafe_vars_list([A|Args],UVars,[void|RArgs]) :-
%   my_membervar(A,UVars),
%   !,
%   replace_unsafe_vars_list(Args,UVars,RArgs).
% replace_unsafe_vars_list([A|Args],UVars,[A|RArgs]) :-
%   replace_unsafe_vars_list(Args,UVars,RArgs).

add_body_correlated_vars_list([],_CHs,[]).
add_body_correlated_vars_list([':-'(H,B)|UDLsts],CHs,[':-'(H,CB)|TDLsts]) :-
  !,
  add_body_correlated_vars(B,CHs,CB),
  add_body_correlated_vars_list(UDLsts,CHs,TDLsts).
add_body_correlated_vars_list([H|UDLsts],CHs,[H|TDLsts]) :-
  add_body_correlated_vars_list(UDLsts,CHs,TDLsts).

add_body_correlated_vars((B,Bs),CHs,(CB,CBs)) :-
  !,
  add_body_correlated_vars(B,CHs,CB),
  add_body_correlated_vars(Bs,CHs,CBs).
add_body_correlated_vars(B,CHs,CB) :-
  functor(B,F,_A),
  (my_member((F,AI,Vs),CHs) ->
    add_atom_vars(Vs,B,AI,CB)
    ;
    CB=B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjunctive_to_conjunctive_rule_list(+DDLsts,+NameVars,-NameVars,-Exploded,-DLsts) 
% Translates a list of Datalog rules with disjunctions (DDLsts)
% into a list of Datalog rules without disjunctions (DLsts)
% True goals are removed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjunctive_to_conjunctive_rule_list([],NVs,NVs,_Exploded,[]).
disjunctive_to_conjunctive_rule_list([DR|DRs],INVs,ONVs,Exploded,RRs) :-
  DR = ':-'(H,B),
  !,
  disjunctive_to_conjunctive_rule(H,('.'),B,INVs,NVs1,Exploded,[],Rs),
  disjunctive_to_conjunctive_rule_list(DRs,NVs1,ONVs,Exploded,TRs),
  my_append(Rs,TRs,RRs).
disjunctive_to_conjunctive_rule_list([R|DRs],INVs,ONVs,Exploded,[R|RRs]) :-
  disjunctive_to_conjunctive_rule_list(DRs,INVs,ONVs,Exploded,RRs).
  
disjunctive_to_conjunctive_rule(H,LBs,(B,RBs),INVs,ONVs,Exploded,IRs,ORs) :-
  !,
  disjunctive_to_conjunctive_rule_6(H,LBs,B,RBs,INVs,ONVs,Exploded,IRs,ORs).
disjunctive_to_conjunctive_rule(H,Bs,B,INVs,ONVs,Exploded,IRs,ORs) :-
  disjunctive_to_conjunctive_rule_6(H,Bs,B,('.'),INVs,ONVs,Exploded,IRs,ORs).

disjunctive_to_conjunctive_rule_6(H,LBs,(LB,RB),RBs,INVs,ONVs,Exploded,IRs,Rs) :-
  !,
  tuple_append(RB,RBs,NRBs),
  disjunctive_to_conjunctive_rule_6(H,LBs,LB,NRBs,INVs,ONVs,Exploded,IRs,Rs).
disjunctive_to_conjunctive_rule_6(H,LBs,(LB;RB),RBs,INVs,ONVs,true,IRs,ORs) :-
  !,
  disjunctive_to_conjunctive_rule(H,LBs,(LB,RBs),INVs,NVs1,Exploded,IRs,Rs),
  copy_term((H,LBs,RB,RBs,INVs),(CH,CLBs,CRB,CRBs,CNVs)),
  disjunctive_to_conjunctive_rule(CH,CLBs,(CRB,CRBs),CNVs,NVs2,Exploded,Rs,ORs),
  my_append(NVs1,NVs2,ONVs).
disjunctive_to_conjunctive_rule_6(H,LBs,B,('.'),NVs,NVs,_Exploded,IRs,ORs) :-  
  !,
  tuple_append(LBs,B,NLBs),
  my_append(IRs,[':-'(H,NLBs)],ORs).
disjunctive_to_conjunctive_rule_6(H,LBs,B,RBs,INVs,ONVs,Exploded,IRs,ORs) :-  
  !,
  tuple_append(LBs,B,NLBs),
  disjunctive_to_conjunctive_rule(H,NLBs,RBs,INVs,ONVs,Exploded,IRs,ORs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type Inference
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

infer_types_and_assert(DVs,TypedArgs) :-
  ruleNV_to_rule_list(DVs,Rs),
  Rs=[R|_TRs],
  get_tablename_arity(R,TableName,_Arity),
  infer_types_list(Rs,_InferredTypes,TypedArgs),
  retractall(my_attribute('$des',_Pos,TableName,_ColName,_DVs)),
  assert_attr_list(1,TableName,TypedArgs).

get_tablename_arity(':-'(H,_B),TableName,Arity) :-
  !,
  functor(H,TableName,Arity).
get_tablename_arity(H,TableName,Arity) :-
  functor(H,TableName,Arity).

infer_types_list(Rs,InferredTypes,TypedArgs) :-
  copy_term(Rs,CRs),
  annotate_types_term_list(CRs,TCRs),
  CRs=[CR|_],
  get_tablename_arity(CR,TableName,Arity),
  my_nf_bagof(Types,
             Body^Pattern^
             (functor(Pattern,TableName,Arity),
              (my_member(':-'(Pattern,Body),TCRs)
               ;
               my_member(Pattern,TCRs)),
              Pattern=..[TableName|Types]),
             TypesList),
  most_concrete_types_list(TypesList,InferredTypes),
  assign_types_to_cols(InferredTypes,TypedArgs).
       
%most_concrete_types_list([],[]).
most_concrete_types_list([Types],Types).
most_concrete_types_list([Types1,Types2|TypesLists],CTypes) :-
  most_concrete_type_list(Types1,Types2,Types3),
  most_concrete_types_list([Types3|TypesLists],CTypes).

most_concrete_type_list([],[],[]).
most_concrete_type_list([Type1|Types1],[Type2|Types2],[CType|CTypes]) :-
  most_concrete_type(Type1,Type2,CType),
  most_concrete_type_list(Types1,Types2,CTypes).

assign_types_to_cols([],[]).
assign_types_to_cols([Type|Types],[_C:Type|TypedArgs]) :-
  assign_types_to_cols(Types,TypedArgs).
  
annotate_types_term_list([],[]).
annotate_types_term_list([T|Ts],[TT|TTs]) :-
  annotate_types_term(T,TT), 
  annotate_types_term_list(Ts,TTs).

% annotate_types_term_list([],[]).
% annotate_types_term_list([D|Ds],[TD|TDs]) :-
%   annotate_types_term(D,TD),
%   annotate_types_term_list(Ds,TDs).

annotate_types_term(T,T) :- 
  var(T),
  !.
annotate_types_term(T,T) :- 
  is_type(T),
  !.
annotate_types_term(L,Ts) :- 
  my_is_list(L),
  !,
  annotate_types_term_list(L,Ts).
annotate_types_term('$NULL'(_ID),_T) :- 
  !.
annotate_types_term(T,number(_N)) :- 
  number(T),
  !.
annotate_types_term(T,string(_S)) :- 
  atom(T),
  !.
annotate_types_term(is(L,R),is(NT,NT)) :- 
  !,
  NT=number(_T),
  (var(L) -> L=NT; true),
  get_expr_type(L,NT),
  get_expr_type(R,NT).
annotate_types_term(L=R,TL=TR) :- 
  !,
  (var(L) -> L=TR ; true),
  (var(R) -> R=TL ; true),
  get_expr_type(L,TL),
  get_expr_type(R,TR),
  TL=TR.
annotate_types_term(C,RC) :- 
  C =.. [TableName|As],
  length(As,Arity),
  my_table('$des',TableName,Arity),
  !, 
  annotate_types_arg_list(TableName,1,As,TAs),
  RC =.. [TableName|TAs].
annotate_types_term(C,RC) :- 
  C =.. [F|As],
  !, 
  annotate_types_term_list(As,RAs),
  RC =.. [F|RAs].

compatible_types(TL,TR) :-
  \+ \+ TL=TR.
  
is_type(T) :-
  my_ground(T),
  T==number(integer),
  !.
is_type(T) :-
  my_ground(T),
  T==number(float),
  !.
is_type(string(varchar)) :-
  !.
is_type(T) :-
  \+ \+ T = number(_T),
  !.
is_type(string(char(N))) :-
  (integer(N) ->
    true
   ;
    my_raise_exception(string(varchar(N),type,[]))).
is_type(string(varchar(N))) :-
  (integer(N) ->
    true
   ;
    my_raise_exception(string(varchar(N),type,[]))).

annotate_types_arg_list(_TableName,_I,[],[]).
annotate_types_arg_list(TableName,I,[A|As],[TA|TAs]) :-
  annotate_types_arg(TableName,I,A,TA),
  I1 is I+1,
  annotate_types_arg_list(TableName,I1,As,TAs).

annotate_types_arg(TableName,I,A,TA) :-
  my_attribute('$des',I,TableName,_ColName,Type),
  (var(A) ->
    A=TA,
    TA=Type
   ;
    get_expr_type(A,T),
    most_concrete_type(T,Type,TA)).

get_expr_type(A,_T) :-
  var(A),
  !.
get_expr_type(C,TC) :- 
  C =.. [F|As],
  length(As,Arity),
  arithmetic_function(F,_,_,aggregate,_,Arity),
  !, 
  get_aggregate_type(C,TC).
get_expr_type(C,T) :- 
  C =.. [F|As],
  length(As,Arity),
  arithmetic_function(F,_,_,arithmetic,T,Arity),
  !.
get_expr_type(A,string(_S)) :-
  atom(A),
  !.
get_expr_type(A,number(integer)) :-
  integer(A),
  !.
get_expr_type(A,number(float)) :-
  float(A),
  !.
get_expr_type('$NULL'(_ID),_T) :-
  !.
get_expr_type(C,T) :- 
  C =.. [F,_L,_R],
  my_infix_arithmetic(F,_PO,T,_D,_P),
  !.
get_expr_type(A,A) :- % A type already substituted
  !.
  
get_expr_type_list([],[]).
get_expr_type_list([E|Es],[T|Ts]) :-
  get_expr_type(E,T), 
  get_expr_type_list(Es,Ts).

% Get the type of aggregate functions:
get_aggregate_type(count,number(_T)) :-
  !.
get_aggregate_type(count(_A),number(_T)) :-
  !.
get_aggregate_type(sum(Var),Var) :-
  !,
  (Var=number(_) ->
    true
   ;
   my_raise_exception(sum(Var),type,[])).
get_aggregate_type(avg(Var),number(_T)) :-
  !,
  (Var=number(_) ->
    true
   ;
   my_raise_exception(avg(Var),type,[])).
get_aggregate_type(A,Var) :-
  A=..[F,Var],
  (F == min ; F == max).

most_concrete_type(T1,T2,T2) :-
  var(T1),
  !.
most_concrete_type(T1,T2,T1) :-
  var(T2),
  !.
most_concrete_type(T1,T2,T1) :-
  T1==T2,
  !.
most_concrete_type(number(T1),number(T2),number(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(string(T1),string(T2),string(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(char(T1),char(T2),char(T)) :-
  most_concrete_type(T1,T2,T),
  !.
most_concrete_type(varchar(T1),varchar(T2),varchar(T)) :-
  most_concrete_type(T1,T2,T),
  !.

%type_subsumed_by_list(ConcreteTypes,GeneralTypes)
type_subsumed_by_list([],[]).
type_subsumed_by_list([CT|CTs],[GT|GTs]) :-
  type_subsumed_by(CT,GT),
  type_subsumed_by_list(CTs,GTs).

%type_subsumed_by(ConcreteType,GeneralType)
type_subsumed_by(T,T).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tracing 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tracing Datalog predicates

trace_datalog(Query,NVs,Ordering) :-
  processC(clear_et,[],_NVs,_Yes),
  my_term_to_string(Query,QueryStr,NVs),
  output(Output),
  set_flag(output,off),
  process_datalog(QueryStr),
  set_flag(output,Output),
  functor(Query,Predicate,Arity),
  trace(datalog,Predicate/Arity,Ordering).
 
% Tracing SQL Views
 
trace_sql(ViewName,Ordering) :-
  (view_arity(ViewName,Arity) ->
   length(Args,Arity),
   Query=..[ViewName|Args],
   my_term_to_string(Query,QueryStr),
   output(Output),
   set_flag(output,off),
   process_datalog(QueryStr),
   set_flag(output,Output),
   trace(sql,ViewName/Arity,Ordering)
  ;
   write_log_list(['Error: View ''',ViewName,''' does not exist.',nl])
  ).

 
trace(Language,Name/Arity,Ordering) :-
  pdg(PDG),
  sub_pdg(Name/Arity,PDG,SubPDG),
  pdg_to_pdt(Name/Arity,SubPDG,PDT),
  pdt_traverse_order(Ordering,PDT,OrderedNodes),
  (Language==sql ->
    filter_views_and_tables(OrderedNodes,Nodes)
   ;
    (development(on) ->
      Nodes=OrderedNodes
     ;
      filter_sources(OrderedNodes,Nodes)
    )
  ),
  trace_nodes(Language,Nodes).
  
% Find an order for visiting nodes  
pdt_traverse_order(postorder,node(N/A,[]),[N/A]) :-
  !.
pdt_traverse_order(postorder,node(N/A,Children),NAs) :-
  pdt_traverse_order_list(postorder,Children,TNAs),
  my_append(TNAs,[N/A],NAs).

pdt_traverse_order(preorder,node(N/A,[]),[N/A]) :-
  !.
pdt_traverse_order(preorder,node(N/A,Children),[N/A|NAs]) :-
  pdt_traverse_order_list(preorder,Children,NAs).

pdt_traverse_order_list(postorder,[],[]).
pdt_traverse_order_list(postorder,[PDT|PDTs],NAs) :-
  pdt_traverse_order(postorder,PDT,NAs1),
  pdt_traverse_order_list(postorder,PDTs,NAs2),
  my_append(NAs1,NAs2,NAs).

pdt_traverse_order_list(preorder,[],[]).
pdt_traverse_order_list(preorder,[PDT|PDTs],NAs) :-
  pdt_traverse_order(preorder,PDT,NAs1),
  pdt_traverse_order_list(preorder,PDTs,NAs2),
  my_append(NAs1,NAs2,NAs).

filter_views_and_tables([],[]).
filter_views_and_tables([N/A|NAs],[N/A|Vs]) :-
  (view_arity(N,A)
   ;
   table_arity(N,A)
  ),
  !,
  filter_views_and_tables(NAs,Vs).
filter_views_and_tables([_NA|NAs],Vs) :-
  filter_views_and_tables(NAs,Vs).

filter_sources([],[]).
filter_sources([N/A|NAs],[N/A|Vs]) :-
  length(Args,A),
  Head=..[N|Args],
  (Rule=Head ; Rule=':-'(Head,_Body)),
  (datalog(Rule,_,_,_,_,source),
   !
   ;
   my_not(datalog(Rule,_,_,_,_,_)),
   my_not(my_builtin_pred(N)),
   !
  ),
  filter_sources(NAs,Vs).
filter_sources([_NA|NAs],Vs) :-
  filter_sources(NAs,Vs).

trace_nodes(Language,[]) :-
  node_type(Language,NodeType),
  write_log_list(['Info: No more ',NodeType,'s to trace.',nl]).
trace_nodes(Language,[N/A|NAs]) :-
  node_type(Language,NodeType),
  write_log_list(['Info: Tracing ',NodeType,' ''',N,'''.',nl]),
  ((Language==sql, verbose(on)) -> list_schema_list([N]) ; true),
  list_answers(N/A),
  (NAs==[] ->
    trace_nodes(Language,[])
   ;
    write_log_list(['Info : Remaining ',NodeType,'s: ',NAs,nl]),
    write_log_list(['Input: Continue? (y/n) [y]: ']),
    my_get0(Char1),
    %WARNING: System-dependent. Newline character is assumed to be the charcode 10.
    ((Char1==10 ; (my_get0(_Char2), [Char1]=="y")) ->
      trace_nodes(Language,NAs)
      ;
      true
    )
  ).
  
node_type(Language,NodeType) :-  
  (Language==sql ->
    NodeType=view
   ;
    NodeType=predicate).



% Predicate dependency graph to Predicate dependency tree
% Recursive calls do not occur in the tree
pdg_to_pdt(N/A,PDG,PDT) :-
  pdg_to_pdt(N/A,PDG,PDT,[],_Visited).
  
% Non-empty pdt  
% pdt ::= node(N,nodes)
% nodes ::= [pdt1,...,pdtn] , where pdti, i>=0, are children of N
pdg_to_pdt(N/A,(_NAs,[]),node(N/A,[]),V,V) :-
  !,
  my_not(my_member_chk(N/A,V)).
pdg_to_pdt(N/A,(NAs,Es),node(N/A,Children),Vin,Vout) :-
  my_nf_setof(DN/DA,
    (
     (my_member(N/A+DN/DA,Es)
       ;
      my_member(N/A-DN/DA,Es))
     ,
     my_not(my_member_chk(DN/DA,[N/A|Vin]))
    ),
    DNDAs),
  pdg_to_pdt_list(DNDAs,(NAs,Es),Children,[N/A|Vin],Vout).

pdg_to_pdt_list([],_PDG,[],V,V).
pdg_to_pdt_list([N/A|NAs],PDG,PDT,Vin,Vout) :-
  my_member_chk(N/A,Vin),
  !,
  pdg_to_pdt_list(NAs,PDG,PDT,Vin,Vout).
pdg_to_pdt_list([N/A|NAs],PDG,[PDT|PDTs],Vin,Vout) :-  
  pdg_to_pdt(N/A,PDG,PDT,Vin,Vout1),
  pdg_to_pdt_list(NAs,PDG,PDTs,Vout1,Vout).
  
% Get the view arity, if exists the view. Otherwise, it fails  
view_arity(ViewName,Arity) :-
  current_db('$des',_),
  !,
  my_view('$des',ViewName,Arity,_,_,_,_).
view_arity(ViewName,Arity) :-
  my_odbc_get_table_arity(ViewName,Arity).
  
% Get the table arity, if exists the table. Otherwise, it fails  
table_arity(TableName,Arity) :-
  current_db('$des',_),
  !,
  my_table('$des',TableName,Arity).
table_arity(TableName,Arity) :-
  my_odbc_get_table_arity(TableName,Arity).
  
% Translate each original, trusted view name View into $trust_View
translate_trusted_views(SQLst,Schema,TSQLst,TSchema) :-
  trusting(on),
  !,
  translate_trusted_schema(Schema,ViewName,TSchema),
  translate_trusted_sql_st(SQLst,ViewName,TSQLst).
translate_trusted_views(SQLst,Schema,SQLst,Schema).

% The view name is translated into a trusted version provided there is an existing view with the same name
translate_trusted_schema(Schema,ViewName,TSchema) :-
  Schema=..[ViewName|Args],
  length(Args,Arity),
  (my_view(_,ViewName,Arity,_,_,_,_) ->
    name_trusted(ViewName,TViewName)
   ;
    TViewName=ViewName),
  TSchema=..[TViewName|Args],
  add_to_trusted_list(TViewName).

% Add the trusted view to the list of all the trusted views (they will be eventually droped after trusted debugging)
add_to_trusted_list(TViewName) :-
  retract(trusted_views(TVNs)),
  assertz(trusted_views([TViewName|TVNs])).
  
% Replaces each view name with its trusted version name (currently, preceding the original name by '$trust_')
translate_trusted_sql_st(T,_V,T) :- 
  var(T),
  !.
translate_trusted_sql_st(attr(Rel,Name,Ren),_V,attr(Rel,Name,Ren)) :- 
  var(Rel),
  !.
translate_trusted_sql_st(attr(Rel,Name,Ren),V,attr(TRel,Name,Ren)) :- 
  !,
  translate_trusted_relation(Rel,V,TRel).
translate_trusted_sql_st((Rel,Ren),V,(TRel,Ren)) :- 
  atom(Rel),
  !,
  translate_trusted_relation(Rel,V,TRel).
translate_trusted_sql_st((Rel,Ren),_V,(Rel,Ren)) :- 
  var(Rel),
  !.
translate_trusted_sql_st(T,_V,T) :- 
  (number(T) ; atom(T)),
  !.
translate_trusted_sql_st(C,V,RC) :- 
  C =.. [F|As],
  !, 
  translate_trusted_sql_st_list(As,V,RAs),
  RC =.. [F|RAs].

translate_trusted_sql_st_list([],_V,[]) :-
  !.
translate_trusted_sql_st_list([T|Ts],V,[RT|RTs]) :-
  translate_trusted_sql_st(T,V,RT), 
  translate_trusted_sql_st_list(Ts,V,RTs).
  
% The relation name is translated into a trusted version provided there is an existing view with the same trusted name
translate_trusted_relation(Rel,V,TRel) :-
  Rel==V,
  !,
  name_trusted(Rel,TRel).
translate_trusted_relation(Rel,_V,TRel) :-
  name_trusted(Rel,TRel),
  my_view(_,TRel,_A,_,_,_,_),
  !.
translate_trusted_relation(Rel,_V,Rel).
  
name_trusted(ViewName,TViewName) :-
  atom_concat('$trust_',ViewName,TViewName).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ancillary Stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% my_is_list(+L)
% Checks whether its input argument is a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_is_list([]).
my_is_list([_X|Xs]) :-
  my_is_list(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_new_predicate_name(+Name,+InputId,-OutputId,-IdName) 
% Returns an identifier of the form: '$<Name><InputId>' and
% also the next Id number .
% e.g., get_new_predicate_name(p,1,2,'$p1')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_new_predicate_name(O,IId,OId,Id) :-
  OId is IId+1,
  atom_concat('$',O,TO),
  atom_codes(TO,STO),
  number_codes(IId,SIId),
  my_append(STO,SIId,SId),
  atom_codes(Id,SId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_new_predicate_name(+Name,-IdName) 
% Returns an identifier of the form: '$<Name><Id>' s.t.
% there is no other such predicate already defined
% e.g., get_new_predicate_name(p,'$p1')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_new_predicate_name(O,Id) :-
  get_predicate_id(O,OId),
  build_predicate_name_id(O,OId,PId),
  (O==p, (datalog(P,_,_,_,_,_);datalog(':-'(P,_),_,_,_,_,_)), functor(P,PId,_) ->
   get_new_predicate_name(O,Id)
   ;
   Id=PId
  ).
  
build_predicate_name_id(O,OId,Id) :-
  atom_concat('$',O,TO),
  atom_codes(TO,STO),
  number_codes(OId,SOId),
  my_append(STO,SOId,SId),
  atom_codes(Id,SId).

get_predicate_id(O,OId) :-
  (retract(id(O,IId)) -> 
   OId is IId+1
   ; 
   OId is 0),
  assertz(id(O,OId)).

reset_id :-
  retractall(id(_O,_I)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_tuples(+TableName,+Arity)) 
% Insert computed tuples (kept in the extension table)
% from a SQL statement into a table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_tuples(TableName,Arity) :-
  N=answer,
  et(R,_Ids),
  functor(R,N,A),
  !,
  (A==Arity ->
   bagof((Fact,[]),
         F^Ids^(
         (et(Fact,Ids), functor(Fact,N,A)); 
         (et(Fact,Ids), (Fact=not(F)), functor(F,N,A)) 
         ),
         Bag),
   replace_functor(N,TableName,Bag,RBag),
   assert_rules(RBag,simplify,Error),
   (var(Error) -> 
     display_nbr_of_tuples(RBag,inserted)
    ;
     display_nbr_of_tuples([],inserted))
   ;
   write_log_list(['Error: Incorrect number of arguments.'])).
insert_tuples(_TableName,_Arity) :-
  write_log_list(['Warning: No tuple met the ''where'' condition for inserting.',nl]).

% Replaces all occurrences of functor O by N in a term T
replace_functor(_O,_N,T,T) :- 
  (number(T) ; var(T)),
  !.
replace_functor(O,N,O,N) :- 
  atom(O),
  !.
replace_functor(O,N,C,RC) :- 
  C =.. [F|As],
  !, 
  (F == O -> RF = N ; RF = F),
  replace_functor_list(O,N,As,RAs),
  RC =.. [RF|RAs].

replace_functor_list(_O,_N,[],[]) :-
  !.
replace_functor_list(O,N,[T|Ts],[RT|RTs]) :-
  !, 
  replace_functor(O,N,T,RT), 
  replace_functor_list(O,N,Ts,RTs).

% Replaces all occurrences of a head matching a given functor in a list of rules by a term
replace_head_list(_O,_N,[],[]).
replace_head_list(O,N,[':-'(H,B)|DLs],[':-'(N,B)|RDLs]) :-
  functor(H,O,_),
  !,
  replace_head_list(O,N,DLs,RDLs).
replace_head_list(O,N,[H|DLs],[N|RDLs]) :-
  H \= ':-'(_,_),
  functor(H,O,_),
  !,
  replace_head_list(O,N,DLs,RDLs).
replace_head_list(O,N,[DL|DLs],[DL|RDLs]) :-
  replace_head_list(O,N,DLs,RDLs).
  
% Replaces all occurrences in a term T of functors starting by O by the same functor but replacing N by O
replace_functor_substring(_O,_N,T,T) :- 
  (number(T) ; var(T)),
  !.
replace_functor_substring(O,N,O,N) :- 
  atom(O),
  !.
replace_functor_substring(O,N,C,RC) :- 
  C =.. [F|As],
  !, 
  (atom_concat(O,R,F) -> atom_concat(N,R,RF) ; RF = F),
  replace_functor_substringL(O,N,As,RAs),
  RC =.. [RF|RAs].

replace_functor_substringL(_O,_N,[],[]) :-
  !.
replace_functor_substringL(O,N,[T|Ts],[RT|RTs]) :-
  !, 
  replace_functor_substring(O,N,T,RT), 
  replace_functor_substringL(O,N,Ts,RTs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete_tuples(+TableName,+Arity)) 
% Delete computed tuples (kept in the extension table)
% from a SQL statement from a table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_tuples(TableName,Arity) :-
  N=answer,
  et(R,_Ids),
  functor(R,N,A),
  !,
  (A==Arity ->
   bagof((Fact,[]),
         F^Ids^(
         (et(Fact,Ids), functor(Fact,N,A)); 
         (et(Fact,Ids), (Fact=not(F)), functor(F,N,A)) 
         ),
         Set),
   replace_functor(N,TableName,Set,RSet),
   retract_rule_list(RSet,_Error),
   display_nbr_of_tuples(RSet,deleted)
   ;
   write_log_list(['Error: Incorrect number of arguments.'])).
delete_tuples(_TableName,_Arity) :-
  write_log_list(['Warning: No tuple met the ''where'' condition for deleting.',nl]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_tuples(+TableName,+Arity)) 
% Update computed tuples kept in the extension table as 
%   answer(OldVal1,...,OldValArity,
%          ColName1,NewValI1,...,ColNameN,ValIM) 
%   : I1,...,IM in {1..Arity}
% from a SQL statement in a table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_tuples(TableName,Arity) :-
  bagof(
        ((OldFact,[]),(NewFact,[])),
        Answer^Ids^A^Args^OldValues^Assignments^NewValues^
         (
          (et(Answer,Ids) ; (et(A,Ids), A=not(Answer))),
          Answer=..[answer|Args],
          split_list(Arity,Args,OldValues,Assignments),
          build_new_values(TableName,Assignments,OldValues,NewValues),
          OldFact=..[TableName|OldValues],
          NewFact=..[TableName|NewValues]
         ),
        Bag
       ),
  bagof(DelTuple,NewTuple^my_member((DelTuple,NewTuple),Bag),DelTuples),
  bagof(InsTuple,OldTuple^my_member((OldTuple,InsTuple),Bag),InsTuples),
  retract_rule_list(DelTuples,Error),
  (var(Error) -> 
    assert_rules(InsTuples,simplify,Error),
    (var(Error) -> 
      display_nbr_of_tuples(InsTuples,updated)
     ;
      write_log_list(['Error: Inserting modified tuples.']))
   ;
    write_log_list(['Error: Deleting tuples.'])).
update_tuples(_TableName,_Arity) :-
  write_log_list(['Warning: No tuple met the ''where'' condition for updating.',nl]).
  
build_new_values(TableName,Assignments,OldValues,NewValues) :-
  split_list_odd_even(Assignments,ColumnNames,Values),
  get_att_positions(TableName,ColumnNames,Positions),
  replace_positions(Positions,Values,OldValues,NewValues).
  
replace_positions(Positions,Values,OldValues,NewValues) :-
  replace_positions(Positions,Values,1,OldValues,NewValues).

replace_positions([],[],_Position,Xs,Xs) :-
  !.
replace_positions([Position|Positions],[Value|Values],Position,[_X|Xs],[Value|Ys]) :-
  !,
  NewPosition is Position+1,
  replace_positions(Positions,Values,NewPosition,Xs,Ys).
replace_positions(Positions,Values,Position,[X|Xs],[X|Ys]) :-
  NewPosition is Position+1,
  replace_positions(Positions,Values,NewPosition,Xs,Ys).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tuple_append(+Tuple1,+Tuple2,-Tuple) Appends the two input
%   tuples, returning a concatenated tuple
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tuple_append(('.'), A, A) :- !.
tuple_append(A, ('.'), A) :- !.
tuple_append((A,B), C, (A,D)) :-
  !, 
  tuple_append(B, C, D).
tuple_append(A, B, (A,B)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% goals_append(+Goals1,+Goals2,-Goals) Appends the two input
%   goals, returning a concatenated goal and excluding
%   true goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goals_append(true, (A,B), C) :-
  !,
  goals_append(A,B,C).
goals_append((A,B), true, C) :-
  !,
  goals_append(A,B,C).
goals_append(true, true, true) :-
  !.
goals_append(true, A, A) :-
  !.
goals_append(A,true, A) :-
  !.
goals_append((A,B), C, E) :-
  !, 
  goals_append(B, C, D),
  goals_append(A, D, E).
goals_append(A, (B,C), (A,D)) :-
  !,
  goals_append(B, C, D).
goals_append(A, B, (A,B)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Display a SQL statement from its syntactic tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_sql(SQL) :-
  display_sql(SQL,0).

display_sql(SQL,I) :-
  (pretty_print(off) -> write_indent(I) ; true),
  write_sql(SQL,I),
  write_log(';'),
  nl_log.

write_sql((SQL,_C),I) :-
  !,
  write_sql(SQL,I).
write_sql(with(SQLst,SQLsts),I) :-
  !,
  pp_indent(I),
  write_string_log("WITH "),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql_list(SQLsts,I1),
  pp_nl_or_blank,
  write_sql(SQLst,I).
write_sql(select(DistinctAll,As,from(Rs),where(Cs),group_by(G),having(H),order_by(OAs,OSs)),I) :-
  !,
  pp_indent(I),
  write_string_log("SELECT "),
  (DistinctAll=distinct -> write_string_log("DISTINCT ") ; write_string_log("ALL ")),
  write_proj_list(As,I),
  pp_nl_or_blank,
  pp_indent(I),
  write_string_log("FROM "),
  I1 is I+2,
  write_rel_list(Rs,I1),
  (Cs == true ->
   true
   ;
   pp_nl_or_blank,
   pp_indent(I),
   write_string_log("WHERE "),
   write_sql_cond(Cs,I1)
   ),
  (G==[] ->
   true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_string_log("GROUP BY "),
    write_attr_list(G)),
  (H==true ->
   true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_string_log("HAVING "),
    write_sql_cond(H,I1)),
  (OAs==[] ->
   true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_string_log("ORDER BY "),
    write_attr_ord_list(OAs,OSs)).
write_sql(SetSQL,I) :-
  SetSQL =.. [SetOp,LSQLst,RSQLst],
  set_operator_name(SetOp,SetOps),
  !,
  I1 is I+2,
  pp_indent_or_blank(I),
  write_log('('),
  pp_nl_or_blank,
  write_sql(LSQLst,I1),
  pp_nl_or_blank,
  pp_indent(I),
  write_log(')'),
  pp_nl_or_blank,
  pp_indent(I),
  write_string_log(SetOps),
  pp_nl_or_blank,
  pp_indent(I),
  write_log('('),
  pp_nl_or_blank,
  write_sql(RSQLst,I1),
  pp_nl_or_blank,
  pp_indent(I),
  write_log(')').

write_sql_list([],_I).
write_sql_list([(SQLst,AS)],I) :-
  pp_indent_or_blank(I),
  write_log_list([AS,' AS']),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql((SQLst,AS),I1).
write_sql_list([(SQLst,AS),SQLst2|SQLsts],I) :-
  write_sql_list([(SQLst,AS)],I),
  write_log(','),
  pp_nl_or_blank,
  write_sql_list([SQLst2|SQLsts],I).
  
pp_indent(I) :-
  (pretty_print(on) -> write_indent(I) ; true).

pp_indent_or_blank(I) :-
  (pretty_print(on) -> write_indent(I) ; write_log(' ')).

pp_nl_or_blank :-
  (pretty_print(on) -> 
     nl_log
     ;
     write_log(' ')
  ).

pp_nl :-
  (pretty_print(on) -> 
     nl_log
     ;
     true
  ).

% verbose(on) shows table names from autorenaming

write_proj_list(*,_I) :-
  write_log(*).
write_proj_list([],_I).
write_proj_list([expr(E,AS,_Type)],I) :-
  !,
  write_expr(E,I),
  (atom_concat('$',_,AS), verbose(off) -> 
   true 
   ;
   write_log(' AS '),
   write_log(AS)).
write_proj_list([attr(T,A,R)],_I) :-
  !,
  write_attr(attr(T,A,R)).
write_proj_list([cte(Cte,_Type)],_I) :-
  write_log(Cte).
write_proj_list([(T,(*))],_I) :-
  !,
  write_log_list([T,'.',*]).
write_proj_list([A],_I) :-
  write_log(A).
write_proj_list([A1,A2|As],I) :-
  write_proj_list([A1],I),
  write_log(', '),
  write_proj_list([A2|As],I).

write_attr_ord_list([],[]).  
write_attr_ord_list([A],[O]) :-
  write_attr(A),
  to_uppercase(O,UO),
  write_log_list([' ',UO]).
write_attr_ord_list([A1,A2|As],[O1,O2|Os]) :-
  write_attr_ord_list([A1],[O1]),
  write_log(', '),
  write_attr_ord_list([A2|As],[O2|Os]).
    
write_attr_list([]).  
write_attr_list([A]) :-
  write_attr(A).  
write_attr_list([A1,A2|As]) :-
  write_attr(A1),
  write_log(', '),
  write_attr_list([A2|As]).
    
write_attr(attr(T,A,_R)) :-
  var(T),
  !,
  write_log(A).
write_attr(attr(T,A,_R)) :-
  (atom_concat('$',_,T), verbose(off) -> 
   true 
   ;
   write_log(T),
   write_log('.')),
  write_log(A).

write_expr(cte('$NULL'(ID),_Type),_I) :-
  !,
  ((development(on) ; var(ID)) -> 
   write_log('$NULL'(ID)) 
   ;
   write_log(null)).
write_expr(cte(N,number(_N)),_I) :-
  !,
  write_log(N).
write_expr(cte(S,string(_S)),_I) :-
  !,
  write_log_list(['''',S,'''']).
write_expr(attr(Rel,A,AS),_I) :- 
  !,
  write_attr(attr(Rel,A,AS)).
write_expr(E,I) :- 
  E =.. [Op,Arg],
  unary_operator(Op,_POp,_D),
  !,
  write_log_list([Op,'(']),
  write_expr(Arg,I),
  write_log(')').
write_expr(E,I) :- 
  is_DQL(E),
  !,
  write_log('('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(E,I1),
  write_log(' )').
write_expr(E,I) :- 
  E =.. [Op,Arg1,Arg2],
  my_infix_arithmetic(Op,_POp,_T,_D,_P),
  !,
  write_expr(Arg1,I),
  write_log_list([' ',Op,' ']),
  write_expr(Arg2,I).
write_expr(E,I) :- 
  E =.. [F,Arg1|Args],
  !, 
  write_log_list([F,'(']),
  write_expr_list([Arg1|Args],I),
  write_log(')').
write_expr(E,_I) :- 
  write_log(E).

write_expr_list([E],I) :-
  write_expr(E,I).
write_expr_list([E1,E2|Es],I) :-
  !, 
  write_expr(E1,I),
  write_log(','), 
  write_expr_list([E2|Es],I).

write_sql_arg_list([attr(T,A,_R)],I) :-
  !,
  (atom_concat('$',_,T), verbose(off) -> 
   true 
   ;
   write_log(T),
   write_log('.')),
  write_sql_arg(A,I).
write_sql_arg_list([A],I) :-
  write_sql_arg(A,I).
write_sql_arg_list([A1,A2|As],I) :-
  write_sql_arg_list([A1],I),
  write_log(', '),
  write_sql_arg_list([A2|As],I).

write_sql_arg(cte(N,number(_T)),_I) :-
  !,
  write_log(N).
write_sql_arg(cte(S,string(_S)),_I) :-
%   atomic(A), 
%   sql_cte(A),
  !,
  write_log_list(['\'',S,'\'']).
write_sql_arg((SQLst,RR),I) :-
  !,
  pp_nl,
  pp_indent(I),
  write_log('('),
  pp_nl,
  I1 is I+2,
  write_sql((SQLst,RR),I1),
  write_log(')').
write_sql_arg(A,_I) :-
  write_log(A).

% sql_cte(C) :-
%   my_not(sql_object(C)).

% sql_object(C) :-
%   my_attribute('$des',_P,_T,C,_Type),
%   !.
% sql_object(C) :-
%   my_table('$des',C,_A).

write_rel_list([],_I).
write_rel_list([R],I) :-
  write_pren_rel(R,I).
write_rel_list([R1,R2|Rs],I) :-
  write_pren_rel(R1,I),
  write_log(', '),
  write_rel_list([R2|Rs],I).
  
write_pren_rel((R,[RR|_RArgs]),I) :-
  !,
  write_rel(R,I),
  (atom_concat('$',_,RR), verbose(off) -> 
   true
   ;
   write_log(' AS '),
   write_log(RR)
  ).
write_pren_rel(R,_I) :-
  write_log(R).

write_par_rel(I,_I1,(TableOrView,AS)) :-
  my_table('$des',TableOrView,_Arity),
  !,
  write_pren_rel((TableOrView,AS),I).
write_par_rel(_I,I1,(R,AS)) :-
  R=..[J,_,_,_],
  join_name(J,_),
  !,
  write_rel((R,AS),I1).
write_par_rel(_I,I1,R) :-
  pp_nl,
  pp_indent(I1),
  write_log('('),
  write_rel(R,I1),
  pp_nl,
  pp_indent(I1),
  write_log(')').
  
write_rel((SQL,_C),I) :-
  !,
  write_rel(SQL,I).
write_rel(TableOrView,I) :-
  my_table('$des',TableOrView,_Arity),
  !,
  pp_nl,
  pp_indent(I),
  write_log(TableOrView).
write_rel(JoinRel,I) :-
  JoinRel=..[J,LR,RR,equijoin(natural)],
  join_name(J,JN),
  !,
  I1 is I+2,
  write_log('('),
  write_par_rel(I,I1,LR),
  pp_nl_or_blank,
  pp_indent(I),
  write_log('NATURAL '),
  write_string_log(JN),
  write_log(' '),
  write_par_rel(I,I1,RR),
  pp_nl,
  pp_indent(I),
  write_log(')').
write_rel(JoinRel,I) :-
  JoinRel=..[J,LR,RR,equijoin(Attrs)],
  join_name(J,JN),
  !,
  I1 is I+2,
  write_log('('),
  write_par_rel(I,I1,LR),
  pp_nl_or_blank,
  pp_indent(I),
  write_string_log(JN),
  write_log(' '),
  write_par_rel(I,I1,RR),
  pp_nl_or_blank,
  pp_indent(I),
  write_log('USING ('),
  write_attr_list(Attrs),
  write_log(')'),
  pp_nl,
  pp_indent(I),
  write_log(')').
write_rel(JoinRel,I) :-
  JoinRel=..[J,LR,RR,C],
  join_name(J,JN),
  !,
  I1 is I+2,
  pp_nl,
  pp_indent(I),
  write_log('('),
  write_par_rel(I,I1,LR),
  pp_nl_or_blank,
  pp_indent(I),
  write_string_log(JN),
  write_log(' '),
  write_par_rel(I,I1,RR),
  (C==true ->
    true
   ;
    pp_nl_or_blank,
    pp_indent(I),
    write_log('ON '),
    write_sql_cond(C,I)),
  pp_nl,
  pp_indent(I),
  write_log(')').
write_rel(SQL,I) :-
%  I1 is I+2,
  pp_nl,
  write_sql(SQL,I),
  !.
write_rel(Rel,_I) :-
  write_log(Rel).

join_name(inner_join,"INNER JOIN").
join_name(left_join,"LEFT JOIN").
join_name(right_join,"RIGHT JOIN").
join_name(full_join,"FULL JOIN").

set_operator_name(union,"UNION").
set_operator_name(except,"EXCEPT").
set_operator_name(intersect,"INTERSECT").

write_sql_cond(and(C1,C2),I) :-
  write_log('('),
  write_sql_cond(C1,I),
  write_log(' AND '),
  write_sql_cond(C2,I),
  write_log(')').
write_sql_cond(or(C1,C2),I) :-
  write_log('('),
  write_sql_cond(C1,I),
  write_log(' OR '),
  write_sql_cond(C2,I),
  write_log(')').
write_sql_cond(exists((SQL,_C)),I) :-
  write_log('EXISTS ('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQL,I1),
  write_log(' )').
write_sql_cond(in(Args,SQL),I) :-
  write_log('('),
  write_sql_arg_list(Args,I),
  write_log(') IN ('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQL,I1),
  write_log(' )').
write_sql_cond(not_in(Args,SQL),I) :-
  write_log('('),
  write_sql_arg_list(Args,I),
  write_log(') NOT IN ('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQL,I1),
  write_log(' )').
write_sql_cond(not(C),I) :-
  write_log('NOT'),
  write_log('('),
  write_sql_cond(C,I),
  write_log(')').
write_sql_cond(C,I) :-
  C =.. [Op,A1,A2],
  !,
  write_expr(A1,I),
  write_log(' '),
  write_op(Op),
  write_log(' '),
  write_expr(A2,I).
write_sql_cond(is_null(SQL),I) :-
  is_DQL(SQL),
  !,
  write_log('('),
  pp_nl_or_blank,
  I1 is I+2,
  write_sql(SQL,I1),
  write_log(' ) IS NULL').
write_sql_cond(is_null(Expr),I) :-
  !,
  write_expr(Expr,I),
  write_log(' IS NULL').
write_sql_cond(C,_I) :-
  write_log(C).

is_DQL((_SQL,_AS)).
  
write_op(Op) :-
  atom_concat(ROP,'_all',Op),
  !,
  write_log_list([ROP,' ALL']).  
write_op(Op) :-
  atom_concat(ROP,'_any',Op),
  !,
  write_log_list([ROP,' ANY']).
write_op(Op) :-
  write_log(Op).
  
write_ra_cond(exists((_RA,AS,_SQL)),I) :-
  !,
  write_indent(I),
  write_log('EXISTS ('),
  write_log(AS),
  write_log(')').
write_ra_cond(C,I) :-
  write_sql_cond(C,I).

write_indent(0) :-
  !.
write_indent(I) :- 
  write_log(' '),
  I1 is I-1,
  write_indent(I1).

