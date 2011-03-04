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
package es.text;

import java.io.File;
import javax.swing.filechooser.*;

/**
 * ACIDE - A Configurable IDE file extension filter manager.
 * 
 * @version 0.8
 */
public class AcideFileExtensionFilterManager extends FileFilter {

	/**
	 * ACIDE - A Configurable IDE file extension filter manager valid extensions.
	 */
	private String[] _extensions;
	/**
	 * ACIDE - A Configurable IDE file extension filter manager extension
	 * description.
	 */
	private String _description;

	/**
	 * Creates a new ACIDE - A Configurable IDE file extension filter manager with an
	 * extension given as a parameter.
	 * 
	 * @param extension
	 *            new extension.
	 */
	public AcideFileExtensionFilterManager(String extension) {

		this(new String[] { extension }, null);
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE file extension filter manager with an
	 * extension and description given as parameters.
	 * 
	 * @param extensions
	 *            extensions to set.
	 * @param description
	 *            description to set.
	 */
	public AcideFileExtensionFilterManager(String[] extensions, String description) {
		_extensions = new String[extensions.length];
		for (int index = extensions.length - 1; index >= 0; index--) {
			_extensions[index] = extensions[index].toLowerCase();
		}
		_description = (description == null ? extensions[0] + " files"
				: description);
	}

	/**
	 * Returns true if the file contains a valid extension and false in other
	 * case.
	 * 
	 * @param file
	 *            file to check.
	 */
	public boolean accept(File file) {

		if (file.isDirectory()) {
			return true;
		}
		String name = file.getName().toLowerCase();

		for (int index = _extensions.length - 1; index >= 0; index--) {
			if (name.endsWith(_extensions[index])) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE extension filter manager extension
	 * description.
	 * 
	 * @return the ACIDE - A Configurable IDE extension filter manager extension
	 *         description.
	 */
	public String getDescription() {
		return _description;
	}
}
