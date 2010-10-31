package gui.editor.utils;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

/**
 * 
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class MatchingBraces {

	/**
	 * Find the matching bracket in a document with an offset.
	 * 
	 * @param doc Document to check.
	 * @param offset Offset to check.
	 * 
	 * @return The matching bracket in a document with an offset.
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
	 * @param line Number of the line.
	 * @param pos Position of the brace.
	 * @param noWordSep No word step.
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
	 * @param line Number of line.
	 * @param pos Position of the word.
	 * @param noWordSep No separation for the word.
	 * 
	 * @return The end of the word.
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