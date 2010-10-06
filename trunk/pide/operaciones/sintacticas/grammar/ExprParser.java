// $ANTLR 2.7.7 (2006-11-01): "microcalc.g" -> "ExprParser.java"$
package operaciones.sintacticas.grammar;

import javax.swing.JOptionPane;

import antlr.TokenBuffer;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;
import antlr.ANTLRException;
import antlr.LLkParser;
import antlr.Token;
import antlr.TokenStream;
import antlr.RecognitionException;
import antlr.NoViableAltException;
import antlr.MismatchedTokenException;
import antlr.SemanticException;
import antlr.ParserSharedInputState;
import antlr.collections.impl.BitSet;

public class ExprParser extends antlr.LLkParser       implements ExprLexerTokenTypes
 {

protected ExprParser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public ExprParser(TokenBuffer tokenBuf) {
  this(tokenBuf,1);
}

protected ExprParser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public ExprParser(TokenStream lexer) {
  this(lexer,1);
}

public ExprParser(ParserSharedInputState state) {
  super(state,1);
  tokenNames = _tokenNames;
}

	public final void expr() throws RecognitionException, TokenStreamException {
		
		
		try {      // for error handling
			mexpr();
			{
			_loop14:
			do {
				if ((LA(1)==PLUS||LA(1)==MINUS)) {
					{
					switch ( LA(1)) {
					case PLUS:
					{
						match(PLUS);
						break;
					}
					case MINUS:
					{
						match(MINUS);
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}
					}
					mexpr();
				}
				else {
					break _loop14;
				}
				
			} while (true);
			}
		}
		catch (RecognitionException ex) {
			reportError(ex);
			recover(ex,_tokenSet_0);
			//JOptionPane.showMessageDialog(null,ex,"RecognitionException",JOptionPane.ERROR_MESSAGE);
			throw new RecognitionException(ex);
		}
	}
	
	public final void mexpr() throws RecognitionException, TokenStreamException {
		
		
		try {      // for error handling
			atom();
			{
			_loop17:
			do {
				if ((LA(1)==STAR)) {
					match(STAR);
					atom();
				}
				else {
					break _loop17;
				}
				
			} while (true);
			}
		}
		catch (RecognitionException ex) {
			reportError(ex);
			recover(ex,_tokenSet_1);
			//JOptionPane.showMessageDialog(null,ex,"RecognitionException",JOptionPane.ERROR_MESSAGE);
			throw new RecognitionException(ex);
		}
	}
	
	public final void atom() throws RecognitionException, TokenStreamException {
		
		
		try {      // for error handling
			switch ( LA(1)) {
			case INT:
			{
				match(INT);
				break;
			}
			case LPAREN:
			{
				match(LPAREN);
				expr();
				match(RPAREN);
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			reportError(ex);
			recover(ex,_tokenSet_2);
			//JOptionPane.showMessageDialog(null,ex,"RecognitionException",JOptionPane.ERROR_MESSAGE);
			throw new RecognitionException(ex);
		}
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"LPAREN",
		"RPAREN",
		"PLUS",
		"MINUS",
		"STAR",
		"INT",
		"WS"
	};
	
	private static final long[] mk_tokenSet_0() {
		long[] data = { 32L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_0 = new BitSet(mk_tokenSet_0());
	private static final long[] mk_tokenSet_1() {
		long[] data = { 224L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_1 = new BitSet(mk_tokenSet_1());
	private static final long[] mk_tokenSet_2() {
		long[] data = { 480L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_2 = new BitSet(mk_tokenSet_2());
	
	}
