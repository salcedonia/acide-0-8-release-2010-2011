/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.2.0                 */
/*                                                       */
/*    CONSULT GLUE CODE                                  */
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


:- use_module(library(compiler)).

consult(X) :-
  ensure_loaded(X).
