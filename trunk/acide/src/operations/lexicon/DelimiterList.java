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
package operations.lexicon;

import java.io.Serializable;

import utils.ObjectList;

/**
 * ACIDE - A Configurable IDE lexicon configuration delimiter list.
 * 
 * @version 0.8
 * @see Serializable
 */
public class DelimiterList implements Serializable {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration delimiter list class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration delimiter list object
	 * list.
	 */
	private ObjectList _list;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration delimiter list unique
	 * class instance.
	 */
	private static DelimiterList _instance;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration delimiter list.
	 */
	public DelimiterList() {

		super();
		_list = new ObjectList();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration delimiter list unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration delimiter list unique class instance.
	 */
	public static DelimiterList getInstance() {
		if (_instance == null)
			_instance = new DelimiterList();
		return _instance;
	}

	/**
	 * Loads the ACIDE - A Configurable IDE lexicon configuration delimiter list.
	 * 
	 * @param delimiterList
	 *            new value to load.
	 */
	public void load(DelimiterList delimiterList) {
		_instance = delimiterList;
	}

	/**
	 * Returns the delimiter at the position given as a parameter.
	 * 
	 * @param index
	 *            position to get.
	 * @return the delimiter at the position given as a parameter.
	 */
	public String getDelimiterAt(int index) {
		return (String) _list.getObjectAt(index);
	}

	/**
	 * Insert a new delimiter.
	 * 
	 * @param delimiter
	 *            new value to set.
	 */
	public void setDelimiter(String delimiter) {
		_list.insert(_list.size(), delimiter);
	}

	/**
	 * Returns the list size.
	 * 
	 * @return the list size.
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * Inserts a new delimiter in the list.
	 * 
	 * @param delimiter
	 *            new delimiter to insert.
	 */
	public void insertDelimiter(String delimiter) {

		boolean found = false;

		for (int i = 0; i < getSize(); i++) {
			String s1 = getDelimiterAt(i);

			if (s1.equals(delimiter))
				found = true;
		}

		if (!found)
			setDelimiter(delimiter);
	}

	/**
	 * Deletes a delimiter given as a parameter.
	 * 
	 * @param delimiter
	 *            delimiter to delete.
	 */
	public void deleteDelimiter(String delimiter) {

		for (int index = 0; index < this.getSize(); index++) {

			String s1 = this.getDelimiterAt(index);
			if (s1.equals(delimiter))
				_list.removeAt(index);
		}
	}

	/**
	 * Resets the list creating a new one.
	 */
	public void reset() {
		_list = new ObjectList();
	}
	
	public ObjectList getList(){
		
		return _list;
	}
}
