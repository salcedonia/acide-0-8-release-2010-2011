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
 * ACIDE - A Configurable IDE lexicon configuration token type list.
 * 
 * @version 0.8
 * @see Serializable
 */
public class TokenTypeList implements Serializable {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type list class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type list object
	 * list.
	 */
	private ObjectList _list;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type list unique
	 * class instance.
	 */
	private static TokenTypeList _instance;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration token type
	 * list.
	 */
	public TokenTypeList() {
		super();
		_list = new ObjectList();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * list unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         list unique class instance.
	 */
	public static TokenTypeList getInstance() {
		if (_instance == null)
			_instance = new TokenTypeList();
		return _instance;
	}

	/**
	 * Loads a new ACIDE - A Configurable IDE lexicon configuration token type
	 * list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void load(TokenTypeList list) {
		_instance = list;
	}

	/**
	 * Returns a token type from the list in the given position.
	 * 
	 * @param pos
	 *            position to get.
	 * @return a token type from the list in the given position.
	 */
	public TokenType getTokenType(int pos) {
		return (TokenType) _list.getObjectAt(pos);
	}

	/**
	 * Sets a new token in the list.
	 * 
	 * @param tokenType
	 *            new value to set.
	 */
	public void setTokenType(TokenType tokenType) {
		_list.insert(_list.size(), tokenType);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * list size.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         list size.
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * Inserts a token type into the ACIDE - A Configurable IDE lexicon
	 * configuration token type list.
	 * 
	 * @param tokenType
	 *            new token type.
	 * @param token
	 *            token name.
	 */
	public void insertTokenType(TokenType tokenType, String token) {

		boolean found = false;
		boolean found2 = false;
		int pos = 0;

		for (int i = 0; i < getSize(); i++) {

			String s1 = getTokenType(i).getName();
			String s2 = tokenType.getName();
			if (s1.equals(s2)) {
				found = true;
				pos = i;
			}
			for (int j = 0; j < getTokenType(i).getTokenListSize(); j++) {
				String s3 = token;
				String s4 = getTokenType(i).getToken(j);

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
				getTokenType(pos).setToken(token);
			}
		}
	}

	/**
	 * Removes a token from the ACIDE - A Configurable IDE lexicon configuration
	 * token type list.
	 * 
	 * @param token
	 *            token to remove.
	 */
	public void removeTokenAs(String token) {

		for (int i = 0; i < getSize(); i++) {
			for (int j = 0; j < getTokenType(i).getTokenListSize(); j++) {
				String s1 = token;
				String s2 = getTokenType(i).getToken(j);
				if (s1.equals(s2)) {
					getTokenType(i).removeToken(j);
				}
			}
			if (getTokenType(i).getTokenListSize() == 0) {
				removeTokenType(i);
			}
		}
	}

	/**
	 * Returns a token from the ACIDE - A Configurable IDE lexicon configuration
	 * token type list.
	 * 
	 * @param token
	 *            token name.
	 * @return a token from the list.
	 */
	public TokenType getTokenAt(String token) {

		for (int i = 0; i < getSize(); i++)
			for (int j = 0; j < getTokenType(i).getTokenListSize(); j++) {
				String s1 = token;
				String s2 = getTokenType(i).getToken(j);
				if (s1.equals(s2))
					return getTokenType(i);
			}

		return null;
	}

	/**
	 * Removes a token type from the ACIDE - A Configurable IDE lexicon
	 * configuration token type list.
	 * 
	 * @param pos
	 *            position to remove.
	 */
	public void removeTokenType(int pos) {
		_list.removeAt(pos);
	}

	/**
	 * Resets the ACIDE - A Configurable IDE lexicon configuration token type
	 * list.
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
