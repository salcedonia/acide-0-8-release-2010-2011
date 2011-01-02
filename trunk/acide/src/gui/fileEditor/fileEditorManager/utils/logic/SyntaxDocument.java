/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package gui.fileEditor.fileEditorManager.utils.logic;

import java.awt.Color;
import java.util.Hashtable;
import java.util.Properties;
import java.util.ResourceBundle;

import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import language.AcideLanguageManager;

import operations.lexicon.Remark;
import operations.lexicon.DelimiterList;
import operations.lexicon.TokenType;
import operations.lexicon.TokenTypeList;
import operations.log.AcideLog;

/**
 * Syntax document for the editors of ACIDE - A Configurable IDE.
 * 
 * @version 0.8
 * @see Properties
 */
public class SyntaxDocument extends DefaultStyledDocument {

	/**
	 * Syntax document class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Default styled document.
	 */
	private DefaultStyledDocument _defaultStyledDocument;
	/**
	 * Root element.
	 */
	private Element _rootElement;
	/**
	 * Mutable attribute set normal.
	 */
	private MutableAttributeSet _normal;
	/**
	 * Mutable attribute set comment.
	 */
	private MutableAttributeSet _comment;
	/**
	 * Mutable attribute set brace.
	 */
	private MutableAttributeSet _brace;
	/**
	 * Mutable attribute set keyword.
	 */
	private MutableAttributeSet[] _keyword;
	/**
	 * Keywords available.
	 */
	private Hashtable<String, Object>[] _keywords;

	/**
	 * Creates a new syntax document.
	 * 
	 * @param fileEditorPanel
	 */
	@SuppressWarnings("unchecked")
	public SyntaxDocument() {

		super();

		ResourceBundle labels = AcideLanguageManager.getInstance().getLabels();

		try {

			TokenTypeList tokenTypeList = TokenTypeList.getInstance();

			// Updates the log
			AcideLog.getLog().info(labels.getString("s321"));

			_defaultStyledDocument = this;
			_rootElement = _defaultStyledDocument.getDefaultRootElement();

			putProperty(DefaultEditorKit.EndOfLineStringProperty, "\n");

			_normal = new SimpleAttributeSet();
			_comment = new SimpleAttributeSet();

			StyleConstants.setForeground(_normal, Color.black);
			StyleConstants.setForeground(_comment, Remark.getInstance()
					.getColor());
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

		catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(labels.getString("s322"));
			exception.printStackTrace();
		}

		// Updates the log
		AcideLog.getLog().info(labels.getString("s323"));
	}

	/**
	 * Overrides to apply syntax highlighting after the document has been
	 * updated.
	 * 
	 * @param offset
	 *            string offset.
	 * @param string
	 *            string to insert.
	 * @param attributeSet
	 *            attribute set.
	 */
	public void insertString(int offset, String string,
			AttributeSet attributeSet) throws BadLocationException {
		super.insertString(offset, string, attributeSet);
		processChangedLines(offset, string.length());
	}

	/**
	 * 
	 * Overrides to apply syntax highlighting after the document has been
	 * updated
	 * 
	 * @param offset
	 *            offset to apply.
	 * @param length
	 *            highlighting length.
	 * 
	 * @exception BadLocationException
	 */
	public void remove(int offset, int length) throws BadLocationException {
		super.remove(offset, length);
		processChangedLines(offset, 0);
	}

	/**
	 * Determines how many lines have been changed, then apply highlighting to
	 * each line.
	 * 
	 * @param offset
	 *            offset to apply.
	 * @param length
	 *            highlighting length.
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
			if (Remark.getInstance().getContent() != "")
				applyComments(content, i);
		}
	}

	/**
	 * Parses the line to determine the appropriate highlighting.
	 * 
	 * @param content
	 *            file content.
	 * @param line
	 *            line to parse.
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
	 * Parses the line for tokens to highlight.
	 * 
	 * @param content
	 *            file content.
	 * @param offsetStart
	 *            offset start.
	 * @param offsetEnd
	 *            offset end.
	 */
	private void checkForTokens(String content, int offsetStart, int offsetEnd) {

		while (offsetStart <= offsetEnd) {

			// skip the delimiters to find the start of a new token
			while (isDelimiter(content.substring(offsetStart, offsetStart + 1),
					offsetStart, content)) {
				if (offsetStart < offsetEnd)
					offsetStart++;
				else
					return;
			}
			// Extract and process the entire token
			offsetStart = getOtherToken(content, offsetStart, offsetEnd);
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
		boolean found = false;
		int i = 0;
		while (!found && i < _keywords.length) {
			if (!ltt.getTokenType(i).isCaseSensitive()) {
				token = token.toLowerCase();
			} else {
				token = aux;
			}
			o = _keywords[i].get(token);
			if (o != null) {
				a = o;
				found = true;
			}
			i++;
		}
		return a == null ? (-1) : i;
	}

	/**
	 * Returns the escape string.
	 * 
	 * @return the escape string.
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

		Remark comment = Remark.getInstance();
		String s = comment.getContent();
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
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

	}

	/**
	 * Sets a new brace in the position given as a parameter.
	 * 
	 * @param position
	 *            brace position to be inserted.
	 */
	public void setBrace(final int position) {

		/*
		 * Prevents the java.lang.IllegalStateException: Attempt to mutate in
		 * notification cause the exception is thrown because you can't make
		 * changes to text inside an event handler that is being notified of
		 * changes to the text (because of the possibilities for endless
		 * recursion). The text is locked against modifications while handling a
		 * modification (character entry in this case).
		 */
		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				try {
					if ((position + 1) < _defaultStyledDocument.getLength()) {
						_defaultStyledDocument.setCharacterAttributes(position,
								1, _brace, true);
					}
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
			}
		});
	}

	/**
	 * Removes the brace at the position given as a parameter.
	 * 
	 * @param position
	 *            brace position to be removed.
	 */
	public void removeBrace(int position) {
		try {
			_defaultStyledDocument.setCharacterAttributes(position, 1, _normal,
					true);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
