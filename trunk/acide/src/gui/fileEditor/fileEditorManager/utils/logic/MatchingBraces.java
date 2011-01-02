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

import java.util.Properties;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

/**																
 * Handles the matching braces of the editor of ACIDE - A Configurable IDE.
 *					
 * @version 0.8	
 * @see Properties																												
 */
public class MatchingBraces {

	/**
	 * Finds the matching bracket in a document with an offset.
	 * 
	 * @param doc document to check.
	 * @param offset offset to check.
	 * 
	 * @return the matching bracket in a document with an offset.
	 * 
	 * @throws BadLocationException
	 */
	public static int findMatchingBracket(Document doc, int offset)
			throws BadLocationException {
		if (doc.getLength() == 0)
			return -1;
		char c = doc.getText(offset, 1).charAt(0);
		char cprime; // c` - corresponding character
		boolean direction; // true = back, false = forward

		switch (c) {
		case '(':
			cprime = ')';
			direction = false;
			break;
		case ')':
			cprime = '(';
			direction = true;
			break;
		case '[':
			cprime = ']';
			direction = false;
			break;
		case ']':
			cprime = '[';
			direction = true;
			break;
		case '{':
			cprime = '}';
			direction = false;
			break;
		case '}':
			cprime = '{';
			direction = true;
			break;
		default:
			return -1;
		}

		int count;

		// How to merge these two cases is left as an exercise
		// for the reader.

		// Go back or forward
		if (direction) {
			// Count is 1 initially because we have already
			// `found' one closing bracket
			count = 1;

			// Get text[0,offset-1];
			String text = doc.getText(0, offset);

			// Scan backwards
			for (int i = offset - 1; i >= 0; i--) {
				// If text[i] == c, we have found another
				// closing bracket, therefore we will need
				// two opening brackets to complete the
				// match.
				char x = text.charAt(i);
				if (x == c)
					count++;

				// If text[i] == cprime, we have found a
				// opening bracket, so we return i if
				// --count == 0
				else if (x == cprime) {
					if (--count == 0)
						return i;
				}
			}
		} else {
			// Count is 1 initially because we have already
			// `found' one opening bracket
			count = 1;

			// So we don't have to + 1 in every loop
			offset++;

			// Number of characters to check
			int len = doc.getLength() - offset;

			// Get text[offset+1,len];
			String text = doc.getText(offset, len);

			// Scan forwards
			for (int i = 0; i < len; i++) {
				// If text[i] == c, we have found another
				// opening bracket, therefore we will need
				// two closing brackets to complete the
				// match.
				char x = text.charAt(i);

				if (x == c)
					count++;

				// If text[i] == cprime, we have found an
				// closing bracket, so we return i if
				// --count == 0
				else if (x == cprime) {
					if (--count == 0)
						return i + offset;
				}
			}
		}

		// Nothing found
		return -1;
	}

	/**
	 * Finds the start of a word.
	 * 
	 * @param line number of the line.
	 * @param pos position of the brace.
	 * @param noWordSep no word step.
	 * 
	 * @return The start of the word.
	 */
	public static int findWordStart(String line, int pos, String noWordSep) {
		char ch = line.charAt(pos - 1);

		if (noWordSep == null)
			noWordSep = "";
		boolean selectNoLetter = (!Character.isLetterOrDigit(ch) && noWordSep
				.indexOf(ch) == -1);

		int wordStart = 0;
		for (int i = pos - 1; i >= 0; i--) {
			ch = line.charAt(i);
			if (selectNoLetter
					^ (!Character.isLetterOrDigit(ch) && noWordSep.indexOf(ch) == -1)) {
				wordStart = i + 1;
				break;
			}
		}

		return wordStart;
	}

	/**
	 * Finds the end of the word.
	 * 
	 * @param line number of line.
	 * @param pos position of the word.
	 * @param noWordSep no separation for the word.
	 * 
	 * @return the end of the word.
	 */
	public static int findWordEnd(String line, int pos, String noWordSep) {
		char ch = line.charAt(pos);

		if (noWordSep == null)
			noWordSep = "";
		boolean selectNoLetter = (!Character.isLetterOrDigit(ch) && noWordSep
				.indexOf(ch) == -1);

		int wordEnd = line.length();
		for (int i = pos; i < line.length(); i++) {
			ch = line.charAt(i);
			if (selectNoLetter
					^ (!Character.isLetterOrDigit(ch) && noWordSep.indexOf(ch) == -1)) {
				wordEnd = i;
				break;
			}
		}
		return wordEnd;
	}
}
