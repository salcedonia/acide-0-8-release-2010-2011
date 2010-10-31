==============================
INSTALLATION (quick guide)
==============================

==============================
Linux Binary Distribution
==============================
- Start ./des from its installation path

==============================
Windows Binary Distribution
==============================
- Double-click on deswin.exe for starting the Windows application
- Execute des.exe for starting the console application

==============================
Linux Source Distributions
==============================
You can write a script for starting DES according to the 
selected Prolog interpreter, as follows:
(a) Ciao Prolog: 
    $CIAO -l ciaorc 
    Provided that $CIAO is the variable which holds the
    absolute filename of the Ciao Prolog executable.
(b) GNU Prolog: 
    $GNU --entry-goal ['des.pl'] 
    Provided that $GNU is the variable which holds the 
    absolute filename of the GNU Prolog executable.
(c) SICStus Prolog: 
    $SICSTUS -l des.pl 
    Provided that $SICSTUS is the variable which holds 
    the absolute filename of the SICStus Prolog executable.
(d) SWI Prolog: 
    $SWI -g "[des]"
    Provided that $SWI is the variable which holds the 
    absolute filename of the SWI Prolog executable.

==============================
Windows Source Distributions
==============================
1. Create a shortcut in the desktop for running the Prolog 
   interpreter of your choice. 
2. Modify the start directory in the "Properties" dialog box 
   of the shortcut to the installation directory for DES. 
   This allows the system to consult the needed files at startup.
3. Append the following options to the Prolog executable complete 
   filename, depending on the Prolog interpreter you use:
   (a) Ciao Prolog: -l ciaorc
   (b) GNU Prolog: --entry-goal ['des.pl']
   (c) SICStus Prolog: -l des.pl
   (d) SWI Prolog: -g "[des]" (remove --win_app if present)
Another alternative is to write a batch file similar to the 
script file described just in the above section.
    
==============================
More Information: 
==============================

- See User Manual
  'Documentation' entry in 
  http://www.fdi.ucm.es/profesor/fernan/des/html/download.html
- http://des.sourceforge.net


Version 2.0 of DES (released on August, 31st, 2010)

* Enhancements:
  o Connection to RDBs via ODBC connections (DSN providers as MySQL, MS Access, Oracle, ...) RDB tables and views can be queried both from SQL and Datalog
  o Duplicates are allowed in answers, both for Datalog and SQL
  o Datalog and SQL tracers
  o New commands:
    - /open_db Name [Options] Open and set the current ODBC connection to Name, where Options=[user(Username)] [password(Password)]. This connection must be already defined at the OS layer
    - /close_db Close the current ODBC connection
    - /current_db Display the current ODBC connection name and DSN provider
    - /duplicates Display whether duplicates are enabled
    - /duplicates Switch Enable or disable duplicates (on or off, resp.)
    - /trace_sql View [Order] Trace a SQL view in the given order (postorder or the default preorder)
    - /trace_datalog Goal [Order] Trace a Datalog basic goal in the given order (postorder or the default preorder)
    - /output Switch Enable or disable display output (on or off, resp.)
    - /save_ddb Filename Save the current Datalog database to a file
    - /restore_ddb Filename Restore the Datalog database in the given file (same as consult)
  o Results from SELECT SQL statements (those sent to an ODBC connection) can contain duplicates
  o Added UPDATE SQL statement
  o Added varchar2 Oracle SQL datatype
  o Remarks can now start with '--', as in Oracle SQL
  o Both EXCEPT and MINUS are allowed to express SQL set difference
  o SQL user identifiers can be enclosed between quotation marks (either double quotes "", or square brackets [], or backquotes ``)
  o Closing the opened log file, if any, on quitting
  o Added timing information to SQL query processing, including listings which may include view processing from RDBs
  o Some dead code removal

* Changes:
  o New port to SICStus 4.x, which replaces the old port to SICStus 3.x
  o The command debug is renamed as debug_datalog
  o Executables have been built with SWI-Prolog, instead of SICStus Prolog
  
* Fixed bugs:
  o Asserting rules with a number as atom/string changed the type to number, as in /assert t('1'), which asserted t(1) instead of t('1')
  o Disjunctions in aggregate goals might lead to missing answers, as in group_by((p(X,Y),(Y=a;Y=b)),[X],C=count)
  o Some infix builtins were accepted without delimiting blanks, as Xis1, posed as a goal, and interpreted as X is 1

* Caveats and limitations:
  o See Section 10 of the user manual
  
* Known bugs:
  o The projection list of a natural outer join is not correct in all cases
  o Disjunctions in having conditions in the group_by clause may display errors which are not
  o Operator precedence in SQL conditions are not correctly handled. Use parentheses to ensure correct operator applications
