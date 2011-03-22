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
package acide.configuration.lexicon.validExtensions;

import java.io.Serializable;
import java.util.StringTokenizer;

import acide.utils.ObjectList;

/**
 * ACIDE - A Configurable IDE lexicon configuration valid extensions manager.
 * 
 * @version 0.8
 * @see Serializable
 */
public class AcideValidExtensionsManager implements Serializable {

	/**
	 * ACIDE - A Configurable IDE configuration valid extensions manager
	 * class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE configuration valid extensions manager
	 * unique class instance.
	 */
	private static AcideValidExtensionsManager _instance;

	/**
	 * ACIDE - A Configurable IDE configuration valid extensions manager
	 * list.
	 */
	private ObjectList _list;

	/**
	 * Returns the ACIDE - A Configurable IDE configuration valid
	 * extensions manager unique class instance.
	 * 
	 * @return ACIDE - A Configurable IDE configuration valid extensions
	 *         manager unique class instance.
	 */
	public static AcideValidExtensionsManager getInstance() {

		if (_instance == null)
			_instance = new AcideValidExtensionsManager();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE configuration valid
	 * extensions manager.
	 */
	public AcideValidExtensionsManager() {
		super();
		_list = new ObjectList();
	}

	/**
	 * Returns the extension a the position <b>POS</b> in the list.
	 * 
	 * @param pos
	 *            position to get
	 * @return the extension a the position <b>POS</b> in the list.
	 */
	public Object getExtensionAt(int pos) {
		return _list.getObjectAt(pos);
	}

	/**
	 * Sets the new extension for the value at the position <b>POS</b> at the
	 * list.
	 * 
	 * @param element
	 *            new value to set.
	 */
	public void setExtensionAt(Object element) {
		_list.insert(_list.size(), element);
	}

	/**
	 * Checks if the extension given as a parameter is a valid or invalid
	 * extension, returning true in for the first case and false in another.
	 * 
	 * @param extension
	 *            extension to check.
	 * 
	 * @return true if it is valid extension and false in other case.
	 */
	public boolean isValidExtension(String extension) {

		for (int index = 0; index < _list.size(); index++)
			if (extension.endsWith((String) _list.getObjectAt(index)))
				return true;

		return false;
	}

	/**
	 * Converts the list of extensions into different tokens and save them into
	 * a string separated by commas.
	 * 
	 * @param string
	 *            string to save.
	 */
	public void tokenizeExtensions(String string) {

		StringTokenizer tokens = new StringTokenizer(string, ",");

		for (int token = 0; token < tokens.countTokens(); token++)
			setExtensionAt(tokens.nextToken());
	}
}
