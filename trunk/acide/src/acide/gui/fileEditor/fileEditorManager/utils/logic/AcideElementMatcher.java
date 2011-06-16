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
package acide.gui.fileEditor.fileEditorManager.utils.logic;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

/**																
 * ACIDE - A Configurable IDE element matcher.
 *					
 * @version 0.8																												
 */
public class AcideElementMatcher {

	/**
	 * Finds the matching elements in a document.
	 * 
	 * @param document document to check.
	 * @param offset element offset.
	 * 
	 * @return the matching element offset in a document.
	 * 
	 * @throws BadLocationException
	 */
	public static int findMatchingBracket(Document document, int offset)
			throws BadLocationException {
		
		if (document.getLength() == 0)
			return -1;
		
		// Gets the character at the document
		char character = document.getText(offset, 1).charAt(0);
		
		// Calculates the corresponding character
		char correspondingCharacter;
		boolean isBackwards; // true = back, false = forward

		switch (character) {
		case '(':
			correspondingCharacter = ')';
			isBackwards = false;
			break;
		case ')':
			correspondingCharacter = '(';
			isBackwards = true;
			break;
		case '[':
			correspondingCharacter = ']';
			isBackwards = false;
			break;
		case ']':
			correspondingCharacter = '[';
			isBackwards = true;
			break;
		case '{':
			correspondingCharacter = '}';
			isBackwards = false;
			break;
		case '}':
			correspondingCharacter = '{';
			isBackwards = true;
			break;
		default:
			return -1;
		}

		// Count is 1 initially because we have already 'found' one closing bracket
		int count = 1;

		if (isBackwards) {

			// Get text[0,offset-1];
			String text = document.getText(0, offset);

			// Scan backwards
			for (int index = offset - 1; index >= 0; index--) {
				// If text[i] == c, we have found another
				// closing bracket, therefore we will need
				// two opening brackets to complete the
				// match.
				char currentCharacter = text.charAt(index);
				if (currentCharacter == character)
					count++;

				// If text[i] == cprime, we have found a
				// opening bracket, so we return i if
				// --count == 0
				else if (currentCharacter == correspondingCharacter) {
					if (--count == 0)
						return index;
				}
			}
		} else {

			// So we don't have to + 1 in every loop
			offset++;

			// Number of characters to check
			int numberOfCharactersToCheck = document.getLength() - offset;

			// Get text[offset + 1, numberOfCharactersToCheck];
			String text = document.getText(offset, numberOfCharactersToCheck);

			// Scan forwards
			for (int index = 0; index < numberOfCharactersToCheck; index++) {
				// If text[i] == c, we have found another
				// opening bracket, therefore we will need
				// two closing brackets to complete the
				// match.
				char currentCharacter = text.charAt(index);

				if (currentCharacter == character)
					count++;

				// If text[i] == cprime, we have found an
				// closing bracket, so we return i if
				// --count == 0
				else if (currentCharacter == correspondingCharacter) {
					if (--count == 0)
						return index + offset;
				}
			}
		}

		// Nothing found
		return -1;
	}
}
