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
package acide.configuration.utils;

import java.io.Serializable;
import java.util.ArrayList;

import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE serializable object list of the lexicon of the
 * application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ObjectList implements Serializable {

	/**
	 * ACIDE - A Configurable IDE serializable object list serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE object array list.
	 */
	private ArrayList<Object> _list;

	/**
	 * Creates a new ACIDE - A Configurable IDE serializable object list.
	 */
	public ObjectList() {

		super();

		// Creates the list of objects
		_list = new ArrayList<Object>();
	}

	/**
	 * Returns the is empty flag.
	 * 
	 * @return The is empty flag.
	 */
	public boolean isEmpty() {
		return _list.isEmpty();
	}

	/**
	 * Insert an object into the object array list at the position given as a
	 * parameter.
	 * 
	 * @param index
	 *            index to insert.
	 * @param object
	 *            object to insert.
	 */
	public void insert(int index, Object object) {
		try {
			if (_list.get(index) != null)
				_list.set(index, object);
		} catch (IndexOutOfBoundsException exception) {
			try {
				_list.add(index, object);
			} catch (IndexOutOfBoundsException exception2) {

				// Updates the log
				AcideLog.getLog().error(exception2.getMessage());
				exception2.printStackTrace();
			}
		}
	}

	/**
	 * Returns the object from the object array list at the index given as a
	 * parameter.
	 * 
	 * @param index
	 *            index to get.
	 * 
	 * @return the object from the object array list at the index given as a
	 *         parameter.
	 */
	public Object getObjectAt(int index) {
		try {
			return (Object) _list.get(index);
		} catch (IndexOutOfBoundsException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			return null;
		}
	}

	/**
	 * Removes an element at the index of the object array list given as a
	 * parameter.
	 * 
	 * @param index
	 *            index to remove.
	 */
	public void removeAt(int index) {
		try {

			// Removes the object from the list specified by the index
			_list.remove(index);
			_list.trimToSize();
		} catch (IndexOutOfBoundsException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Returns the object array list size.
	 * 
	 * @return the object array list size.
	 */
	public int size() {
		return _list.size();
	}

	/**
	 * Clears the object array list.
	 */
	public void clear() {
		_list.clear();
	}

	/**
	 * Adds an object to the list.
	 * 
	 * @param object
	 *            new object to add.
	 */
	public void insert(Object object) {
		_list.add(object);
	}

	/**
	 * Sets a new value to the object in the index specified.
	 * 
	 * @param index
	 *            index to set.
	 * @param object
	 *            new value to set.
	 */
	public void setValueAt(int index, String object) {
		_list.set(index, object);
	}

	/**
	 * Sets a new value to the object given as a parameter.
	 * 
	 * @param object
	 *            new value to set.
	 */
	public void setValue(Object object) {

		if (_list.indexOf(object) != -1)
			_list.set(_list.indexOf(object), object);
	}
}
