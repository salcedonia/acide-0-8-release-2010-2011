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
 * ACIDE - A Configurable IDE styled document for the editors.
 * 
 * @version 0.8
 * @see Properties
 */
public class AcideStyledDocument extends DefaultStyledDocument {

	/**
	 * ACIDE - A Configurable IDE styled document class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE styled document root element.
	 */
	private Element _rootElement;
	/**
	 * ACIDE - A Configurable IDE styled document mutable attribute set normal.
	 */
	private MutableAttributeSet _normal;
	/**
	 * ACIDE - A Configurable IDE styled document mutable attribute set remark.
	 */
	private MutableAttributeSet _remark;
	/**
	 * ACIDE - A Configurable IDE styled document mutable attribute set brace.
	 */
	private MutableAttributeSet _brace;
	/**
	 * ACIDE - A Configurable IDE styled document mutable attribute set keyword.
	 */
	private MutableAttributeSet[] _keyword;
	/**
	 * ACIDE - A Configurable IDE styled document keywords available.
	 */
	private Hashtable<String, Object>[] _keywords;
	/**
	 * File editor panel name. It is used for knowing where the focus has to be
	 * set when a CTRL+Z or CTRL+Y is pressed.
	 */
	private String _fileEditorPanelName;

	/**
	 * Creates a new ACIDE - A Configurable IDE styled document.
	 */
	@SuppressWarnings("unchecked")
	public AcideStyledDocument() {

		super();

		// Gets the labels
		ResourceBundle labels = AcideLanguageManager.getInstance().getLabels();

		try {

			TokenTypeList tokenTypeList = TokenTypeList.getInstance();

			// Updates the log
			AcideLog.getLog().info(labels.getString("s321"));

			// Gets the default root element
			_rootElement = getDefaultRootElement();

			// The string "\n" is the end of the line
			putProperty(DefaultEditorKit.EndOfLineStringProperty, "\n");

			// NORMAL
			_normal = new SimpleAttributeSet();

			// Foreground in black
			StyleConstants.setForeground(_normal, Color.black);

			// REMARK
			_remark = new SimpleAttributeSet();
			// Foreground as defines the Remark class
			StyleConstants.setForeground(_remark, Remark.getInstance()
					.getColor());
			// Italic
			StyleConstants.setItalic(_remark, true);
			// Not bold
			StyleConstants.setBold(_remark, false);

			// BRACE
			_brace = new SimpleAttributeSet();
			// Foreground in blue
			StyleConstants.setBackground(_brace, new Color(100, 50, 255));
			// Foreground in white
			StyleConstants.setForeground(_brace, Color.white);

			// KEYWORD
			_keyword = new MutableAttributeSet[tokenTypeList.getSize()];
			for (int index = 0; index < tokenTypeList.getSize(); index++) {

				// Creates the keyword list
				_keyword[index] = new SimpleAttributeSet();

				// Gets the token type from the list
				TokenType tokenType = tokenTypeList.getTokenType(index);

				// Foreground color as defines the token type of the list
				StyleConstants.setForeground(_keyword[index],
						tokenType.getColor());
				// Italic as defines the token type of the list
				StyleConstants.setItalic(_keyword[index], tokenType.isItalic());
				// Bold as defines the token type of the list
				StyleConstants.setBold(_keyword[index], tokenType.isBold());
			}

			// KEYWORDS
			_keywords = new Hashtable[tokenTypeList.getSize()];
			Object dummyObject = new Object();

			for (int index1 = 0; index1 < tokenTypeList.getSize(); index1++) {

				_keywords[index1] = new Hashtable<String, Object>();

				// Gets the token type from the list
				TokenType tokenType = tokenTypeList.getTokenType(index1);

				for (int index2 = 0; index2 < tokenType.getTokenListSize(); index2++) {

					// Gets the token
					String token = tokenType.getToken(index2);

					// If the token is case sensitive
					if (!tokenType.isCaseSensitive())
						// Parse to lower case
						token = token.toLowerCase();

					_keywords[index1].put(token, dummyObject);
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
	 * updated.
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

		// Gets the text content
		String content = getText(0, getLength());

		// The lines affected by the latest document update
		int startLine = _rootElement.getElementIndex(offset);
		int endLine = _rootElement.getElementIndex(offset + length);

		// Does the actual highlighting
		for (int index = startLine; index <= endLine; index++) {

			// Applies the highlighting
			applyHighlighting(content, index);

			// If there is a defined remark
			if (Remark.getInstance().getContent() != "")
				// Applies the remark highlighting
				applyRemarkStyle(content, index);
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
		setCharacterAttributes(startOffset, lineLength, _normal, true);

		// check for tokens for the highlighting
		applyStyles(content, startOffset, endOffset);
	}

	/**
	 * Parses the line in order to apply the different token styles.
	 * 
	 * @param content
	 *            file content.
	 * @param offsetStart
	 *            offset start.
	 * @param offsetEnd
	 *            offset end.
	 */
	private void applyStyles(String content, int offsetStart, int offsetEnd) {

		while (offsetStart <= offsetEnd) {

			// skip the delimiters to find the start of a new token
			while (isDelimiter(content.substring(offsetStart, offsetStart + 1),
					offsetStart, content)) {
				if (offsetStart < offsetEnd)
					offsetStart++;
				else
					return;
			}
			// Extracts and processes the entire token
			offsetStart = applyNextTokenStyle(content, offsetStart, offsetEnd);
		}
	}

	/**
	 * Applies the token style for the next token in the file content.
	 * 
	 * @param content
	 *            file content.
	 * @param startOffset
	 *            start offset.
	 * @param endOffset
	 *            end offset.
	 * 
	 * @return the position to continue to explore from.
	 */
	private int applyNextTokenStyle(String content, int startOffset,
			int endOffset) {

		// Gets the end of the token
		int endOfToken = startOffset + 1;

		// Looks for the next delimiter
		while (endOfToken <= endOffset) {
			if (isDelimiter(content.substring(endOfToken, endOfToken + 1),
					endOfToken, content))
				break;
			endOfToken++;
		}

		// Gets the token
		String token = content.substring(startOffset, endOfToken);

		// Checks if the token is a keyword
		int index = isKeyword(token);

		// If it is a keyword
		if (index != -1)

			// Applies the keyword style
			setCharacterAttributes(startOffset, endOfToken - startOffset,
					_keyword[index - 1], false);

		return endOfToken + 1;
	}

	/**
	 * Checks if a character given as a parameter is a delimiter or not.
	 * 
	 * @param character
	 *            character to check.
	 * @param characterPostion
	 *            character position.
	 * @param content
	 *            text content.
	 * 
	 * @return true if it is a delimiter and false in other case.
	 */
	protected boolean isDelimiter(String character, int characterPostion,
			String content) {

		// Creates the delimiter list
		DelimiterList.getInstance();

		for (int index = 0; index < DelimiterList.getInstance().getSize(); index++) {

			// Gets the delimiter
			String delimiter = DelimiterList.getInstance()
					.getDelimiterAt(index);

			// If it is the white space
			if (Character.isWhitespace(character.charAt(0)))

				// Returns true
				return true;

			int position = 0;

			while (delimiter.indexOf(character, position) != -1) {

				if (((characterPostion - delimiter.indexOf(character, position) + delimiter
						.length()) <= content.length())
						&& (characterPostion
								- delimiter.indexOf(character, position) >= 0)) {

					// Gets the delimiter
					String delimiterAux = content.substring(
							characterPostion
									- delimiter.indexOf(character, position),
							characterPostion
									- delimiter.indexOf(character, position)
									+ delimiter.length());

					if (delimiterAux.equals(delimiter)) {

						// Gets the index in the keyword list
						int delimiterPosition = isKeyword(delimiterAux);

						// If the token is a keyword
						if (delimiterPosition != -1) {

							// Applies the keyword style
							setCharacterAttributes(
									characterPostion
											- delimiter.indexOf(character,
													position),
									delimiterAux.length(),
									_keyword[delimiterPosition - 1], false);
						}
						return true;
					}
				}
				position++;
			}
		}
		return false;
	}

	/**
	 * Checks if a token given as a parameter is a keyword.
	 * 
	 * If it is a keyword returns the index in the keyword list and -1 in other
	 * case.
	 * 
	 * @param token
	 *            token to check.
	 * 
	 * @return the index + 1 in the keyword list.
	 */
	protected int isKeyword(String token) {

		Object object;
		Object tokenFound = null;
		TokenTypeList tokenTypeList = TokenTypeList.getInstance();
		String tokenAux = token;

		boolean found = false;
		int index = 0;

		while (!found && index < _keywords.length) {

			if (!tokenTypeList.getTokenType(index).isCaseSensitive()) {
				token = token.toLowerCase();
			} else {
				token = tokenAux;
			}

			object = _keywords[index].get(token);

			if (object != null) {
				tokenFound = object;
				found = true;
			}
			index++;
		}
		return tokenFound == null ? (-1) : index;
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
	public String addMatchingBrace(int offset) throws BadLocationException {

		// TODO: UNUSED

		StringBuffer whiteSpace = new StringBuffer();

		int line = _rootElement.getElementIndex(offset);
		int index = _rootElement.getElement(line).getStartOffset();

		while (true) {

			// Gets the text
			String text = getText(index, 1);

			if (text.equals(" ") || text.equals("\t")) {
				whiteSpace.append(text);
				index++;
			} else
				break;
		}

		return "{\n" + whiteSpace.toString() + whiteSpace.toString() + "\n"
				+ whiteSpace.toString() + "}";
	}

	/**
	 * Applies the remark style to the file content given as a parameter.
	 * 
	 * @param content
	 *            file content.
	 * @param line
	 *            current line.
	 */
	private void applyRemarkStyle(String content, int line) {

		// Gets the remark configuration
		Remark remark = Remark.getInstance();

		// Gets the remark content
		String remarkContent = remark.getContent();
		String text = "";
		int startOffset = _rootElement.getElement(line).getStartOffset();
		int endOffset = _rootElement.getElement(line).getEndOffset() - 1;
		int lineLength = endOffset - startOffset;

		try {

			// Gets the current text to analyze
			text = getText(startOffset, lineLength);

			// Gets the remark content position
			int position = text.indexOf(remarkContent);

			// If it is a valid position
			if (position >= 0)

				// Applies the remark style
				setCharacterAttributes(startOffset + position, lineLength
						- position, _remark, true);
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Adds the brace highlighting in the position given as a parameter.
	 * 
	 * @param position
	 *            brace position to be inserted.
	 */
	public void addHighlightBrace(final int position) {

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
					if ((position + 1) < getLength()) {
						setCharacterAttributes(position, 1, _brace, true);
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
	 * Removes the brace highlighting at the position given as a parameter.
	 * 
	 * It consist of set that position in the style that it had before.
	 * 
	 * @param position
	 *            brace position to be removed.
	 */
	public void removeHighlightBrace(final int position) {

		SwingUtilities.invokeLater(new Runnable() {
			
			/*
			 * (non-Javadoc)
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				try {

					// Gets the keyword index
					int index = isKeyword(getText(position, 1));

					// If it is a keyword
					if (index != -1)

						// Applies the keyword style
						setCharacterAttributes(position, 1, _keyword[index - 1], true);
					else

						// Applies the normal style
						setCharacterAttributes(position, 1, _normal, true);

				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
				}
			}
		});
	}

	/**
	 * Returns the file editor panel name.
	 * 
	 * @return the file editor panel name.
	 */
	public String getFileEditorPanelName() {
		return _fileEditorPanelName;
	}

	/**
	 * Sets a new value to the file editor panel name.
	 * 
	 * @param fileEditorPanelName
	 *            new value to set.
	 */
	public void setFileEditorPanelName(String fileEditorPanelName) {
		_fileEditorPanelName = fileEditorPanelName;
	}
}
