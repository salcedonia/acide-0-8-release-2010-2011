header{
package acide.process.parser.grammar;
}
class GrammarLexer extends Lexer;
LPAREN: 'a' ;
RPAREN: 's' ;
PLUS  : 'd' ;
MINUS : 'f' ;
STAR  : 'g' ;
INT   : ('0'..'9')+ ;
WS    : ( ' '
        | '\r' '\n'
        | '\n'
        | '\t'
        )
        {$setType(Token.SKIP);}
      ;    

class GrammarParser extends Parser;
options{k=2;}
expr:   mexpr ((PLUS|MINUS) mexpr)*
    ;      

mexpr      
    :   atom (STAR atom)*
    ;    

atom:   INT 
    |   LPAREN expr RPAREN 
    ;
