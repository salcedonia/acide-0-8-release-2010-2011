package gui.editor.editorManager.utils.logic;

import java.awt.Color;
import java.util.Hashtable;
import java.util.ResourceBundle;

import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import language.Language;

import operations.lexicon.Comments;
import operations.lexicon.DelimiterList;
import operations.lexicon.TokenType;
import operations.lexicon.TokenTypeList;
import operations.log.Log;

import org.apache.log4j.Logger;

/**
 * 
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class SyntaxDocument extends DefaultStyledDocument {

	/**
	 * serialVersionUID.
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private DefaultStyledDocument _defaultStyledDocument;
	/**
	 * 
	 */
	private Element _rootElement;
	/**
	 * 
	 */
	private MutableAttributeSet _normal;
	/**
	 * 
	 */
	private MutableAttributeSet _comment;
	/**
	 * 
	 */
	private MutableAttributeSet _brace;
	/**
	 * 
	 */
	private MutableAttributeSet[] _keyword;
	/**
	 * 
	 */
	private Hashtable<String, Object>[] _keywords;

	/**
	 * Constructor of the class.
	 */
	@SuppressWarnings("unchecked")
	public SyntaxDocument() {

		super();

		ResourceBundle labels = Language.getInstance().getLabels();

		try {

			TokenTypeList tokenTypeList = TokenTypeList.getInstance();

			_logger.info(labels.getString("s321"));
			_defaultStyledDocument = this;
			_rootElement = _defaultStyledDocument.getDefaultRootElement();
			putProperty(DefaultEditorKit.EndOfLineStringProperty, "\n");
			_normal = new SimpleAttributeSet();
			_comment = new SimpleAttributeSet();

			StyleConstants.setForeground(_normal, Color.black);
			StyleConstants.setForeground(_comment, Comments.getInstance()
					.getLineCommentColor());
			StyleConstants.setItalic(_comment, true);
			StyleConstants.setBold(_comment, false);

			_brace = new SimpleAttributeSet();
			StyleConstants.setBackground(_brace, Color.blue);
			StyleConstants.setForeground(_brace, Color.white);

			_keyword = new MutableAttributeSet[tokenTypeList.getSize()];
			for (int i = 0; i < tokenTypeList.getSize(); i++) {
				// Set the types
				_keyword[i] = new SimpleAttributeSet();
				TokenType tt = tokenTypeList.getTokenType(i);
				StyleConstants.setForeground(_keyword[i], tt.getColor());
				StyleConstants.setItalic(_keyword[i], tt.isItalic());
				StyleConstants.setBold(_keyword[i], tt.isBold());
			}

			_keywords = new Hashtable[tokenTypeList.getSize()];
			Object dummyObject = new Object();
			for (int i = 0; i < tokenTypeList.getSize(); i++) {
				_keywords[i] = new Hashtable<String, Object>();
				TokenType tt = tokenTypeList.getTokenType(i);
				for (int j = 0; j < tt.getTokenListSize(); j++) {
					String s = tt.getToken(j);
					if (!tt.isCaseSensitive())
						s = s.toLowerCase();
					_keywords[i].put(s, dummyObject);
				}
			}
		}

		catch (Exception e) {
			_logger.info(labels.getString("s322"));
			e.printStackTrace();
		}
		_logger.info(labels.getString("s323"));
	}

	/**
	 * Override to apply syntax highlighting after the document has been
	 * updated.
	 * 
	 * @param offset
	 * @param str
	 * @param a
	 */
	public void insertString(int offset, String str, AttributeSet a)
			throws BadLocationException {
		super.insertString(offset, str, a);
		processChangedLines(offset, str.length());
	}

	/**
	 * 
	 * Override to apply syntax highlighting after the document has been updated
	 *
	 * @param offset
	 * @param lenght
	 * 
	 * @exception BadLocationException
	 */
	public void remove(int offset, int length) throws BadLocationException {
		super.remove(offset, length);
		processChangedLines(offset, 0);
	}

	/**
	 * Determine how many lines have been changed, then apply highlighting to
	 * each line
	 *
	 * @param offset
	 * @param length
	 * @throws BadLocationException
	 */
	private void processChangedLines(int offset, int length)
			throws BadLocationException {
		String content = _defaultStyledDocument.getText(0,
				_defaultStyledDocument.getLength());
		// The lines affected by the latest document update
		int startLine = _rootElement.getElementIndex(offset);
		int endLine = _rootElement.getElementIndex(offset + length);
		// Do the actual highlighting
		for (int i = startLine; i <= endLine; i++) {
			applyHighlighting(content, i);
			if (Comments.getInstance().getLineComment() != "")
				applyComments(content, i);
		}
	}

	/**
	 * Parse the line to determine the appropriate highlighting
	 *
	 * @param content
	 * @param line
	 * @throws BadLocationException
	 */
	private void applyHighlighting(String content, int line)
			throws BadLocationException {
		int startOffset = _rootElement.getElement(line).getStartOffset();
		int endOffset = _rootElement.getElement(line).getEndOffset() - 1;
		int lineLength = endOffset - startOffset;
		int contentLength = content.length();
		if (endOffset >= contentLength)
			endOffset = contentLength - 1;
		// set normal attributes for the line
		_defaultStyledDocument.setCharacterAttributes(startOffset, lineLength,
				_normal, true);
		// check for tokens
		checkForTokens(content, startOffset, endOffset);
	}

	/**
	 * Parse the line for tokens to highlight.
	 * 
	 * @param content
	 * @param startOffset
	 * @param endOffset
	 */
	private void checkForTokens(String content, int startOffset, int endOffset) {
		while (startOffset <= endOffset) {
			// skip the delimiters to find the start of a new token
			while (isDelimiter(content.substring(startOffset, startOffset + 1),
					startOffset, content)) {
				if (startOffset < endOffset)
					startOffset++;
				else
					return;
			}
			// Extract and process the entire token
			startOffset = getOtherToken(content, startOffset, endOffset);
		}
	}

	/**
	 * 
	 * @param content
	 * @param startOffset
	 * @param endOffset
	 * @return
	 */
	private int getOtherToken(String content, int startOffset, int endOffset) {
		int endOfToken = startOffset + 1;
		while (endOfToken <= endOffset) {
			if (isDelimiter(content.substring(endOfToken, endOfToken + 1),
					endOfToken, content))
				break;
			endOfToken++;
		}
		String token = content.substring(startOffset, endOfToken);
		int pos = isKeyword(token);
		if (pos != -1) {
			_defaultStyledDocument.setCharacterAttributes(startOffset,
					endOfToken - startOffset, _keyword[pos - 1], false);
		}

		return endOfToken + 1;
	}

	/**
	 * 
	 * @param character
	 * @param pos
	 * @param content
	 * @return
	 */
	protected boolean isDelimiter(String character, int pos, String content) {
		DelimiterList.getInstance();
		for (int i = 0; i < DelimiterList.getInstance().getSize(); i++) {
			String operands = DelimiterList.getInstance().getDelimiterAt(i);

			if (Character.isWhitespace(character.charAt(0)))
				return true;
			int spos = 0;

			while (operands.indexOf(character, spos) != -1) {
				if (((pos - operands.indexOf(character, spos) + operands
						.length()) <= content.length())
						&& (pos - operands.indexOf(character, spos) >= 0)) {
					String sAux = content.substring(
							pos - operands.indexOf(character, spos), pos
									- operands.indexOf(character, spos)
									+ operands.length());
					if (sAux.equals(operands)) {
						int posD = isKeyword(sAux);
						if (posD != -1) {
							_defaultStyledDocument.setCharacterAttributes(pos
									- operands.indexOf(character, spos),
									sAux.length(), _keyword[posD - 1], false);
						}
						return true;
					}
				}
				spos++;
			}
		}
		return false;
	}

	/**
	 * 
	 * @param token
	 * @return
	 */
	protected int isKeyword(String token) {
		Object o;
		Object a = null;
		TokenTypeList ltt = TokenTypeList.getInstance();
		String aux = token;
		boolean encontrado = false;
		int i = 0;
		while (!encontrado && i < _keywords.length) {
			if (!ltt.getTokenType(i).isCaseSensitive()) {
				token = token.toLowerCase();
			} else {
				token = aux;
			}
			o = _keywords[i].get(token);
			if (o != null) {
				a = o;
				encontrado = true;
			}
			i++;
		}
		return a == null ? (-1) : i;
	}

	/**
	 * 
	 */
	protected String getEscapeString(String quoteDelimiter) {
		return "\\" + quoteDelimiter;
	}

	/**
	 * 
	 * @param offset
	 * @return
	 * @throws BadLocationException
	 */
	protected String addMatchingBrace(int offset) throws BadLocationException {
		StringBuffer whiteSpace = new StringBuffer();
		int line = _rootElement.getElementIndex(offset);
		int i = _rootElement.getElement(line).getStartOffset();
		while (true) {
			String temp = _defaultStyledDocument.getText(i, 1);
			if (temp.equals(" ") || temp.equals("\t")) {
				whiteSpace.append(temp);
				i++;
			} else
				break;
		}
		return "{\n" + whiteSpace.toString() + whiteSpace.toString() + "\n"
				+ whiteSpace.toString() + "}";
	}

	/**
	 * 
	 * @param content
	 * @param line
	 */
	private void applyComments(String content, int line) {
		Comments comment = Comments.getInstance();
		String s = comment.getLineComment();
		String text = "";
		int startOffset = _rootElement.getElement(line).getStartOffset();
		int endOffset = _rootElement.getElement(line).getEndOffset() - 1;
		int lineLength = endOffset - startOffset;
		try {
			text = _defaultStyledDocument.getText(startOffset, lineLength);
			int pos = text.indexOf(s);
			if (pos >= 0)
				_defaultStyledDocument.setCharacterAttributes(
						startOffset + pos, lineLength - pos, _comment, true);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}

	}

	/**
	 * 
	 * @param pos
	 */
	public void setBrace(int pos) {
		try {
			if ((pos + 1) < _defaultStyledDocument.getLength()) {
				_defaultStyledDocument.setCharacterAttributes(pos, 1, _brace,
						true);
			}
		} catch (Exception e) {

		}
	}

	/**
	 * 
	 * @param pos
	 */
	public void removeBrace(int pos) {
		try {
			_defaultStyledDocument
					.setCharacterAttributes(pos, 1, _normal, true);
		} catch (Exception e) {

		}
	}
}
