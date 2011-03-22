// $ANTLR 2.7.7 (2006-11-01): "microcalc.g" -> "ExprLexer.java"$
package acide.process.parser.grammar;

import java.io.InputStream;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;
import antlr.TokenStreamRecognitionException;
import antlr.CharStreamException;
import antlr.CharStreamIOException;
import java.io.Reader;
import java.util.Hashtable;
import antlr.InputBuffer;
import antlr.ByteBuffer;
import antlr.CharBuffer;
import antlr.Token;
import antlr.RecognitionException;
import antlr.NoViableAltForCharException;
import antlr.TokenStream;
import antlr.LexerSharedInputState;

/**
 * 
 */
public class ExprLexer extends antlr.CharScanner implements
		ExprLexerTokenTypes, TokenStream {
	
	/**
	 * 
	 * @param in
	 */
	public ExprLexer(InputStream in) {
		this(new ByteBuffer(in));
	}

	/**
	 * 
	 * @param in
	 */
	public ExprLexer(Reader in) {
		this(new CharBuffer(in));
	}

	/**
	 * 
	 * @param ib
	 */
	public ExprLexer(InputBuffer ib) {
		this(new LexerSharedInputState(ib));
	}

	@SuppressWarnings("rawtypes")
	/**
	 * 
	 */
	public ExprLexer(LexerSharedInputState state) {
		super(state);
		caseSensitiveLiterals = true;
		setCaseSensitive(true);
		literals = new Hashtable();
	}

	@SuppressWarnings("unused")
	/**
	 * 
	 */
	public Token nextToken() throws TokenStreamException {
		Token theRetToken = null;
		tryAgain: for (;;) {
			Token _token = null;
			int _ttype = Token.INVALID_TYPE;
			resetText();
			try { // for char stream error handling
				try { // for lexical error handling
					switch (LA(1)) {
					case '(': {
						mLPAREN(true);
						theRetToken = _returnToken;
						break;
					}
					case ')': {
						mRPAREN(true);
						theRetToken = _returnToken;
						break;
					}
					case '+': {
						mPLUS(true);
						theRetToken = _returnToken;
						break;
					}
					case '-': {
						mMINUS(true);
						theRetToken = _returnToken;
						break;
					}
					case '*': {
						mSTAR(true);
						theRetToken = _returnToken;
						break;
					}
					case '0':
					case '1':
					case '2':
					case '3':
					case '4':
					case '5':
					case '6':
					case '7':
					case '8':
					case '9': {
						mINT(true);
						theRetToken = _returnToken;
						break;
					}
					case '\t':
					case '\n':
					case '\r':
					case ' ': {
						mWS(true);
						theRetToken = _returnToken;
						break;
					}
					default: {
						if (LA(1) == EOF_CHAR) {
							uponEOF();
							_returnToken = makeToken(Token.EOF_TYPE);
						} else {
							throw new NoViableAltForCharException((char) LA(1),
									getFilename(), getLine(), getColumn());
						}
					}
					}
					if (_returnToken == null)
						continue tryAgain; // found SKIP token
					_ttype = _returnToken.getType();
					_ttype = testLiteralsTable(_ttype);
					_returnToken.setType(_ttype);
					return _returnToken;
				} catch (RecognitionException e) {
					throw new TokenStreamRecognitionException(e);
				}
			} catch (CharStreamException cse) {
				if (cse instanceof CharStreamIOException) {
					throw new TokenStreamIOException(
							((CharStreamIOException) cse).io);
				} else {
					throw new TokenStreamException(cse.getMessage());
				}
			}
		}
	}

	/**
	 * 
	 * @param _createToken
	 * @throws RecognitionException
	 * @throws CharStreamException
	 * @throws TokenStreamException
	 */
	public final void mLPAREN(boolean _createToken)
			throws RecognitionException, CharStreamException,
			TokenStreamException {
		int _ttype;
		Token _token = null;
		int _begin = text.length();
		_ttype = LPAREN;
		@SuppressWarnings("unused")
		int _saveIndex;

		match('(');
		if (_createToken && _token == null && _ttype != Token.SKIP) {
			_token = makeToken(_ttype);
			_token.setText(new String(text.getBuffer(), _begin, text.length()
					- _begin));
		}
		_returnToken = _token;
	}

	/**
	 * 
	 * @param _createToken
	 * @throws RecognitionException
	 * @throws CharStreamException
	 * @throws TokenStreamException
	 */
	public final void mRPAREN(boolean _createToken)
			throws RecognitionException, CharStreamException,
			TokenStreamException {
		int _ttype;
		Token _token = null;
		int _begin = text.length();
		_ttype = RPAREN;

		match(')');
		if (_createToken && _token == null && _ttype != Token.SKIP) {
			_token = makeToken(_ttype);
			_token.setText(new String(text.getBuffer(), _begin, text.length()
					- _begin));
		}
		_returnToken = _token;
	}

	/**
	 * 
	 * @param _createToken
	 * @throws RecognitionException
	 * @throws CharStreamException
	 * @throws TokenStreamException
	 */
	public final void mPLUS(boolean _createToken) throws RecognitionException,
			CharStreamException, TokenStreamException {
		int _ttype;
		Token _token = null;
		int _begin = text.length();
		_ttype = PLUS;

		match('+');
		if (_createToken && _token == null && _ttype != Token.SKIP) {
			_token = makeToken(_ttype);
			_token.setText(new String(text.getBuffer(), _begin, text.length()
					- _begin));
		}
		_returnToken = _token;
	}

	/**
	 * 
	 * @param _createToken
	 * @throws RecognitionException
	 * @throws CharStreamException
	 * @throws TokenStreamException
	 */
	public final void mMINUS(boolean _createToken) throws RecognitionException,
			CharStreamException, TokenStreamException {
		int _ttype;
		Token _token = null;
		int _begin = text.length();
		_ttype = MINUS;

		match('-');
		if (_createToken && _token == null && _ttype != Token.SKIP) {
			_token = makeToken(_ttype);
			_token.setText(new String(text.getBuffer(), _begin, text.length()
					- _begin));
		}
		_returnToken = _token;
	}

	/**
	 * 
	 * @param _createToken
	 * @throws RecognitionException
	 * @throws CharStreamException
	 * @throws TokenStreamException
	 */
	public final void mSTAR(boolean _createToken) throws RecognitionException,
			CharStreamException, TokenStreamException {
		int _ttype;
		Token _token = null;
		int _begin = text.length();
		_ttype = STAR;
		
		match('*');
		if (_createToken && _token == null && _ttype != Token.SKIP) {
			_token = makeToken(_ttype);
			_token.setText(new String(text.getBuffer(), _begin, text.length()
					- _begin));
		}
		_returnToken = _token;
	}

	/**
	 * 
	 * @param _createToken
	 * @throws RecognitionException
	 * @throws CharStreamException
	 * @throws TokenStreamException
	 */
	public final void mINT(boolean _createToken) throws RecognitionException,
			CharStreamException, TokenStreamException {
		int _ttype;
		Token _token = null;
		int _begin = text.length();
		_ttype = INT;

		{
			int _cnt8 = 0;
			_loop8: do {
				if (((LA(1) >= '0' && LA(1) <= '9'))) {
					matchRange('0', '9');
				} else {
					if (_cnt8 >= 1) {
						break _loop8;
					} else {
						throw new NoViableAltForCharException((char) LA(1),
								getFilename(), getLine(), getColumn());
					}
				}

				_cnt8++;
			} while (true);
		}
		if (_createToken && _token == null && _ttype != Token.SKIP) {
			_token = makeToken(_ttype);
			_token.setText(new String(text.getBuffer(), _begin, text.length()
					- _begin));
		}
		_returnToken = _token;
	}

	/**
	 * 
	 * @param _createToken
	 * @throws RecognitionException
	 * @throws CharStreamException
	 * @throws TokenStreamException
	 */
	public final void mWS(boolean _createToken) throws RecognitionException,
			CharStreamException, TokenStreamException {
		int _ttype;
		Token _token = null;
		int _begin = text.length();
		_ttype = WS;

		{
			switch (LA(1)) {
			case ' ': {
				match(' ');
				break;
			}
			case '\r': {
				match('\r');
				match('\n');
				break;
			}
			case '\n': {
				match('\n');
				break;
			}
			case '\t': {
				match('\t');
				break;
			}
			default: {
				throw new NoViableAltForCharException((char) LA(1),
						getFilename(), getLine(), getColumn());
			}
			}
		}
		_ttype = Token.SKIP;
		if (_createToken && _token == null && _ttype != Token.SKIP) {
			_token = makeToken(_ttype);
			_token.setText(new String(text.getBuffer(), _begin, text.length()
					- _begin));
		}
		_returnToken = _token;
	}
}
