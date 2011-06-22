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

import acide.configuration.utils.ObjectList;

/**
 * <p>
 * ACIDE - A Configurable IDE lexicon configuration token manager.
 * </p>
 * <p>
 * Handles the lexicon configuration token handling methods and classes.
 * </p>
 * <p>
 * The ACIDE - A Configurable IDE lexicon configuration has a token group list,
 * in other words, tokens with the same configuration are grouped together.
 * Below this level, each token group has another token list which defines its
 * configuration.
 * </p>
 * 
 * @version 0.8
 * @see Serializable
 */
public class AcideLexiconTokenManager implements Serializable {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration token manager class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token manager object
	 * list.
	 */
	private ObjectList _list;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration token
	 * manager.
	 */
	public AcideLexiconTokenManager() {

		super();

		// Creates the object list
		_list = new ObjectList();
	}

	/**
	 * Returns a token group from the list at the given index.
	 * 
	 * @param index
	 *            index to get.
	 * @return a token group from the list at the given index.
	 */
	public AcideLexiconTokenGroup getTokenGroupAt(int index) {
		return (AcideLexiconTokenGroup) _list.getObjectAt(index);
	}

	/**
	 * Inserts a new token in the ACIDE - A Configurable IDE lexicon
	 * configuration token manager object list.
	 * 
	 * @param tokenGroup
	 *            new value to set.
	 */
	public void insertTokenGroup(AcideLexiconTokenGroup tokenGroup) {
		_list.insert(_list.size(), tokenGroup);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token group
	 * manager list size.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token group
	 *         manager list size.
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * Inserts a token into the ACIDE - A Configurable IDE lexicon configuration
	 * token group manager list.
	 * 
	 * @param tokenGroup
	 *            new token group.
	 * @param token
	 *            token name.
	 */
	public void insertToken(AcideLexiconTokenGroup tokenGroup, String token) {

//		boolean tokenGroupExists = false;
//		boolean tokenExists = false;
//
//		for (int index1 = 0; index1 < getSize(); index1++) {
//
//			// If the token group matches
//			if (getTokenGroupAt(index1).getName().equals(tokenGroup.getName())) {
//
//				// The token group already exists
//				tokenGroupExists = true;
//
//				// Look for it in the current checked token group token list
//				for (int index2 = 0; index2 < getTokenGroupAt(index1).getSize(); index2++) {
//
//					// If the new token and the current token match
//					if (token
//							.equals(getTokenGroupAt(index1).getTokenAt(index2))) {
//
//						// The token already exists in the token list
//						tokenExists = true;
//					}
//				}
//			}
//		}
//
//		// It the token group does not exist yet
//		if (!tokenGroupExists) {
//
//			// Inserts the token in the token group
//			tokenGroup.insertToken(token);
//			
//			// And inserts the token group with the new token
//			insertTokenGroup(tokenGroup);
//		} else {
//
//			// If the token group already exists
//
//			// If the token does not exist yet
//			if (!tokenExists) {
//
//				// Inserts the token in the token group
//				tokenGroup.insertToken(token);
//			} else
//
//				// Updates the token in the token group
//				tokenGroup.getList().setValue(token);
//		}

		boolean found = false;
		boolean found2 = false;
		int index = 0;

		for (int index1 = 0; index1 < getSize(); index1++) {

			String s1 = getTokenGroupAt(index1).getName();
			String s2 = tokenGroup.getName();
			if (s1.equals(s2)) {
				found = true;
				index = index1;
			}
			for (int index2 = 0; index2 < getTokenGroupAt(index1).getSize(); index2++) {
				String s3 = token;
				String s4 = getTokenGroupAt(index1).getTokenAt(index2);

				if (s3.equals(s4)) {
					found2 = true;
				}
			}
		}

		if (!found) {
			if (!found2) {
				insertTokenGroup(tokenGroup);
			}
		} else {
			if (!found2) {
				getTokenGroupAt(index).insertToken(token);
			}
		}
	}

	/**
	 * Removes a token from the ACIDE - A Configurable IDE lexicon configuration
	 * token manager object list.
	 * 
	 * @param token
	 *            token to remove.
	 */
	public void removeTokenAs(String token) {

		for (int index1 = 0; index1 < getSize(); index1++) {
			for (int index2 = 0; index2 < getTokenGroupAt(index1).getSize(); index2++) {

				// Gets the token from the token list
				String tokenFromList = getTokenGroupAt(index1).getTokenAt(
						index2);

				// If they are equals
				if (token.equals(tokenFromList)) {

					// Removes the token
					getTokenGroupAt(index1).removeTokenAt(index2);
				}
			}

			// If its list is empty
			if (getTokenGroupAt(index1).getSize() == 0) {

				// Removes the token
				removeTokenGroupAt(index1);
			}
		}
	}

	/**
	 * Returns a token from the ACIDE - A Configurable IDE lexicon configuration
	 * token manager object list.
	 * 
	 * @param token
	 *            token name.
	 * @return a token from the list.
	 */
	public AcideLexiconTokenGroup getTokenAt(String token) {

		for (int index1 = 0; index1 < getSize(); index1++)
			for (int index2 = 0; index2 < getTokenGroupAt(index1).getSize(); index2++) {

				// Gets the token from the list
				String tokenFromList = getTokenGroupAt(index1).getTokenAt(
						index2);

				// If are equals
				if (token.equals(tokenFromList))

					// Returns the token
					return getTokenGroupAt(index1);
			}

		return null;
	}

	/**
	 * Removes a token group from the ACIDE - A Configurable IDE lexicon
	 * configuration token group manager object list.
	 * 
	 * @param index
	 *            position to remove.
	 */
	public void removeTokenGroupAt(int index) {
		_list.removeAt(index);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token
	 * manager object list.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token
	 *         manager object list.
	 */
	public ObjectList getList() {
		return _list;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * token manager object list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setList(ObjectList list) {
		_list = list;
	}
}
