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
package acide.configuration.lexicon.tokens;

import java.io.Serializable;

import acide.utils.ObjectList;

/**
 * ACIDE - A Configurable IDE lexicon configuration token type manager.
 * 
 * @version 0.8
 * @see Serializable
 */
public class AcideLexiconTokenTypeManager implements Serializable {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type manager class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type manager
	 * object list.
	 */
	private ObjectList _list;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration token type
	 * manager.
	 */
	public AcideLexiconTokenTypeManager() {
		super();
		_list = new ObjectList();
	}

	/**
	 * Returns a token type from the list in the given position.
	 * 
	 * @param pos
	 *            position to get.
	 * @return a token type from the list in the given position.
	 */
	public AcideLexiconTokenType getTokenType(int pos) {
		return (AcideLexiconTokenType) _list.getObjectAt(pos);
	}

	/**
	 * Sets a new token in the list.
	 * 
	 * @param tokenType
	 *            new value to set.
	 */
	public void setTokenType(AcideLexiconTokenType tokenType) {
		_list.insert(_list.size(), tokenType);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * manager list size.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         manager list size.
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * Inserts a token type into the ACIDE - A Configurable IDE lexicon
	 * configuration token type manager list.
	 * 
	 * @param tokenType
	 *            new token type.
	 * @param token
	 *            token name.
	 */
	public void insertTokenType(AcideLexiconTokenType tokenType, String token) {

		boolean found = false;
		boolean found2 = false;
		int index = 0;

		for (int index1 = 0; index1 < getSize(); index1++) {

			String s1 = getTokenType(index1).getName();
			String s2 = tokenType.getName();
			if (s1.equals(s2)) {
				found = true;
				index = index1;
			}
			for (int index2 = 0; index2 < getTokenType(index1).getTokenListSize(); index2++) {
				String s3 = token;
				String s4 = getTokenType(index1).getToken(index2);

				if (s3.equals(s4)) {
					found2 = true;
				}
			}
		}

		if (!found) {
			if (!found2) {
				setTokenType(tokenType);
			}
		} else {
			if (!found2) {
				getTokenType(index).setToken(token);
			}
		}
	}

	/**
	 * Removes a token from the ACIDE - A Configurable IDE lexicon configuration
	 * token type manager list.
	 * 
	 * @param token
	 *            token to remove.
	 */
	public void removeTokenAs(String token) {

		for (int index1 = 0; index1 < getSize(); index1++) {
			for (int index2 = 0; index2 < getTokenType(index1)
					.getTokenListSize(); index2++) {

				// Gets the token from the token list
				String tokenFromList = getTokenType(index1).getToken(index2);
				
				// If they are equals
				if (token.equals(tokenFromList)) {
					
					// Removes the token
					getTokenType(index1).removeToken(index2);
				}
			}
			
			// If its list is empty 
			if (getTokenType(index1).getTokenListSize() == 0) {
				
				// Removes the token
				removeTokenType(index1);
			}
		}
	}

	/**
	 * Returns a token from the ACIDE - A Configurable IDE lexicon configuration
	 * token type manager list.
	 * 
	 * @param token
	 *            token name.
	 * @return a token from the list.
	 */
	public AcideLexiconTokenType getTokenAt(String token) {

		for (int index1 = 0; index1 < getSize(); index1++)
			for (int index2 = 0; index2 < getTokenType(index1)
					.getTokenListSize(); index2++) {

				// Gets the token from the list
				String tokenFromList = getTokenType(index1).getToken(index2);

				// If are equals
				if (token.equals(tokenFromList))
					
					// Returns the token
					return getTokenType(index1);
			}

		return null;
	}

	/**
	 * Removes a token type from the ACIDE - A Configurable IDE lexicon
	 * configuration token type manager list.
	 * 
	 * @param index
	 *            position to remove.
	 */
	public void removeTokenType(int index) {
		_list.removeAt(index);
	}

	/**
	 * Resets the ACIDE - A Configurable IDE lexicon configuration token type
	 * manager list.
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
