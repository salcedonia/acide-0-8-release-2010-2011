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
package acide.gui.consolePanel.utils;

import java.awt.Font;
import java.util.Hashtable;

import javax.swing.event.DocumentEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.lexicon.tokens.AcideLexiconTokenGroup;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE styled document for the editors.
 * 
 * @version 0.8
 * @see DefaultStyledDocument
 */
public class AcideConsolePanelStyledDocument extends DefaultStyledDocument {

	/**
	 * ACIDE - A Configurable IDE styled document class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE styled document root element.
	 */
	private Element _rootElement;
	/**
	 * ACIDE - A Configurable IDE styled document normal mutable attribute set.
	 */
	private MutableAttributeSet _normalAttributeSet;
	/**
	 * ACIDE - A Configurable IDE styled document single remarks mutable
	 * attribute set.
	 */
	private MutableAttributeSet _singleRemarksAttributeSet;
	/**
	 * ACIDE - A Configurable IDE styled document keyword mutable attribute set.
	 */
	private MutableAttributeSet[] _keywordAttributeSet;
	/**
	 * ACIDE - A Configurable IDE styled document keywords available.
	 */
	private Hashtable<String, Object>[] _keywords;
	/**
	 * Lexicon configuration.
	 */
	private AcideLexiconConfiguration _lexiconConfiguration;

	/**
	 * Creates a new ACIDE - A Configurable IDE styled document.
	 */
	public AcideConsolePanelStyledDocument(
			AcideLexiconConfiguration lexiconConfiguration) {

		super();

		try {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s321"));

			// Gets the default root element
			_rootElement = getDefaultRootElement();

			// Stores the lexicon configuration
			_lexiconConfiguration = lexiconConfiguration;

			// Builds the attribute sets
			buildAttributeSets();
		}

		catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s322"));
			exception.printStackTrace();
		}

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s323"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE style document attribute sets.
	 */
	private void buildAttributeSets() {

		// The string "\n" is the end of the line
		putProperty(DefaultEditorKit.EndOfLineStringProperty, "\n");

		// The string "\t" inserts 8 blank spaces
		putProperty(DefaultEditorKit.insertTabAction, "\t");

		// Builds the normal attributes set
		buildNormaAttributeSet();

		// Builds the single remarks attribute set
		buildSingleRemarksAttributeSet();

		// Builds the keywords attributes set
		buildKeywordAttributeSet();
	}

	/**
	 * Initializes the styled document properties.
	 */
	public void init() {

		// Builds the attribute sets
		buildAttributeSets();

		// Applies the styles
		try {

			// Changes the styled document
			processChangedLines(0, getLength());
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.text.AbstractDocument#insertString(int,
	 * java.lang.String, javax.swing.text.AttributeSet)
	 */
	@Override
	public void insertString(int offset, String string,
			AttributeSet attributeSet) throws BadLocationException {

		super.insertString(offset, string, attributeSet);

		// Changes the styled document
		processChangedLines(offset, string.length());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.text.AbstractDocument#remove(int, int)
	 */
	@Override
	public void remove(int offset, int length) throws BadLocationException {

		super.remove(offset, length);

		// Changes the styled document
		processChangedLines(offset, 0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.text.AbstractDocument#fireInsertUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	@Override
	protected void fireInsertUpdate(DocumentEvent documentEvent) {

		super.fireInsertUpdate(documentEvent);

		try {

			// Changes the styled document
			processChangedLines(documentEvent.getOffset(),
					documentEvent.getLength());
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.text.AbstractDocument#fireRemoveUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	@Override
	protected void fireRemoveUpdate(DocumentEvent documentEvent) {

		super.fireRemoveUpdate(documentEvent);

		try {

			// Changes the styled document
			processChangedLines(documentEvent.getOffset(),
					documentEvent.getLength());
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
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
	public void processChangedLines(int offset, int length)
			throws BadLocationException {

		// Gets the text content
		String content = getText(0, getLength());

		// The lines affected by the latest document update
		int startLine = _rootElement.getElementIndex(offset);
		int endLine = _rootElement.getElementIndex(offset + length);

		// Does the actual highlighting line by line
		for (int line = startLine; line <= endLine; line++) {

			// Applies the highlighting
			applyHighlighting(content, line);

			// If there is a defined remark
			if (_lexiconConfiguration.getRemarksManager().getSymbol() != "")

				// Applies the remark highlighting
				applyRemarkStyle(line);
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

		// Avoids exceptions
		if (_rootElement.getElement(line) != null) {

			// Gets the start offset
			int startOffset = _rootElement.getElement(line).getStartOffset();

			// Gets the end offset
			int endOffset = _rootElement.getElement(line).getEndOffset() - 1;

			// Gets the line length
			int lineLength = endOffset - startOffset;

			// Gets the content length
			int contentLength = content.length();

			// Avoids the out of bounds exception
			if (endOffset >= contentLength)
				endOffset = contentLength - 1;

			// set normal attributes for the line
			setCharacterAttributes(startOffset, lineLength,
					_normalAttributeSet, true);

			// check for tokens for the highlighting
			applyStyles(content, startOffset, endOffset);
		}
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
					_keywordAttributeSet[index - 1], false);

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

		for (int index = 0; index < _lexiconConfiguration
				.getDelimitersManager().getSize(); index++) {

			// Gets the delimiter
			String delimiter = _lexiconConfiguration.getDelimitersManager()
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
									_keywordAttributeSet[delimiterPosition - 1],
									false);
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
	 * <p>
	 * Checks if a token given as a parameter is a keyword.
	 * </p>
	 * <p>
	 * If it is a keyword returns the index in the keyword list and -1 in other
	 * case.
	 * </p>
	 * 
	 * @param token
	 *            token to check.
	 * 
	 * @return the index + 1 in the keyword list.
	 */
	protected int isKeyword(String token) {

		Object object;
		Object tokenFound = null;

		String tokenAux = token;

		boolean found = false;
		int index = 0;

		while (!found && index < _keywords.length) {

			if (!_lexiconConfiguration.getTokenTypeManager()
					.getTokenGroupAt(index).isCaseSensitive()) {
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
	 * Applies the remark style to the file content given as a parameter.
	 * 
	 * @param line
	 *            current line.
	 */
	private void applyRemarkStyle(int line) {

		// Gets the remark content
		String remarkContent = _lexiconConfiguration.getRemarksManager()
				.getSymbol();

		String text = "";

		// Avoids exceptions
		if (_rootElement.getElement(line) != null) {

			int startOffset = _rootElement.getElement(line).getStartOffset();
			int endOffset = _rootElement.getElement(line).getEndOffset() - 1;
			int lineLength = endOffset - startOffset;

			// If the line length is positive
			if (lineLength > 0) {

				try {

					// Gets the current text to analyze
					text = getText(startOffset, lineLength);

					// Gets the remark content position
					int position = text.indexOf(remarkContent);

					// If it is a valid position
					if (position >= 0)

						// Applies the remark style
						setCharacterAttributes(startOffset + position,
								lineLength - position,
								_singleRemarksAttributeSet, true);
				} catch (BadLocationException exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
			}
		}
	}

	/**
	 * Builds and configure the keyword attribute set.
	 */
	@SuppressWarnings("unchecked")
	public void buildKeywordAttributeSet() {

		// Creates the keyword attribute set
		_keywordAttributeSet = new MutableAttributeSet[_lexiconConfiguration
				.getTokenTypeManager().getSize()];

		// Adds the keywords to the attribute set list
		for (int index = 0; index < _lexiconConfiguration.getTokenTypeManager()
				.getSize(); index++) {

			_keywordAttributeSet[index] = new SimpleAttributeSet();

			// Gets the token group from the list
			AcideLexiconTokenGroup tokenGroup = _lexiconConfiguration
					.getTokenTypeManager().getTokenGroupAt(index);

			// Foreground color as defines the token type of the list
			StyleConstants.setForeground(_keywordAttributeSet[index],
					tokenGroup.getColor());

			switch (tokenGroup.getFontStyle()) {

			case Font.PLAIN:
				StyleConstants.setItalic(_keywordAttributeSet[index], false);
				StyleConstants.setBold(_keywordAttributeSet[index], false);
				break;
			case Font.ITALIC:
				StyleConstants.setItalic(_keywordAttributeSet[index], true);
				StyleConstants.setBold(_keywordAttributeSet[index], false);
				break;
			case Font.BOLD:
				StyleConstants.setItalic(_keywordAttributeSet[index], false);
				StyleConstants.setBold(_keywordAttributeSet[index], true);
				break;
			case Font.BOLD + Font.ITALIC:
				StyleConstants.setItalic(_keywordAttributeSet[index], true);
				StyleConstants.setBold(_keywordAttributeSet[index], true);
				break;
			}
		}

		// Creates the keywords hash map
		_keywords = new Hashtable[_lexiconConfiguration.getTokenTypeManager()
				.getSize()];
		Object dummyObject = new Object();

		for (int index1 = 0; index1 < _lexiconConfiguration
				.getTokenTypeManager().getSize(); index1++) {

			_keywords[index1] = new Hashtable<String, Object>();

			// Gets the token type from the list
			AcideLexiconTokenGroup tokenType = _lexiconConfiguration
					.getTokenTypeManager().getTokenGroupAt(index1);

			for (int index2 = 0; index2 < tokenType.getSize(); index2++) {

				// Gets the token
				String token = tokenType.getTokenAt(index2);

				// If the token is case sensitive
				if (!tokenType.isCaseSensitive())
					// Parse to lower case
					token = token.toLowerCase();

				_keywords[index1].put(token, dummyObject);
			}
		}
	}

	/**
	 * <p>
	 * Builds and configure the normal attribute set. In this case it is
	 * important to mention that if some style is applied to the attribute set
	 * then it will affect directly on the file editor configuration.
	 * </p>
	 * <p>
	 * For instance, if we are interested in changing the foreground color, then
	 * it is mandatory not to add the foreground property to the normal
	 * attribute set.
	 * </p>
	 */
	public void buildNormaAttributeSet() {

		// Creates the normal attribute set
		_normalAttributeSet = new SimpleAttributeSet();
	}

	/**
	 * Builds and configure the single remarks attribute set.
	 */
	public void buildSingleRemarksAttributeSet() {

		// Creates the single remarks attribute set
		_singleRemarksAttributeSet = new SimpleAttributeSet();

		// Set its foreground color as defines the remarks manager in the
		// lexicon configuration
		StyleConstants.setForeground(_singleRemarksAttributeSet,
				_lexiconConfiguration.getRemarksManager().getColor());

		switch (_lexiconConfiguration.getRemarksManager().getFontStyle()) {

		case Font.PLAIN:
			StyleConstants.setBold(_singleRemarksAttributeSet, false);
			StyleConstants.setItalic(_singleRemarksAttributeSet, false);
			break;
		case Font.BOLD:
			StyleConstants.setBold(_singleRemarksAttributeSet, true);
			StyleConstants.setItalic(_singleRemarksAttributeSet, false);
			break;
		case Font.ITALIC:
			StyleConstants.setBold(_singleRemarksAttributeSet, false);
			StyleConstants.setItalic(_singleRemarksAttributeSet, true);
			break;
		case Font.BOLD + Font.ITALIC:
			StyleConstants.setBold(_singleRemarksAttributeSet, true);
			StyleConstants.setItalic(_singleRemarksAttributeSet, true);
			break;
		}

		// If it is case sensitive then parses it to lower case
		if (_lexiconConfiguration.getRemarksManager().getIsCaseSensitive())
			_lexiconConfiguration.getRemarksManager().getSymbol().toLowerCase();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE styled document
	 * lexicon configuration.
	 * 
	 * @param lexiconConfiguration
	 *            new value to set.
	 */
	public void setLexiconConfiguration(
			AcideLexiconConfiguration lexiconConfiguration) {
		_lexiconConfiguration = lexiconConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE styled document lexicon
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE styled document lexicon
	 *         configuration.
	 */
	public AcideLexiconConfiguration getLexiconConfiguration() {
		return _lexiconConfiguration;
	}
}
