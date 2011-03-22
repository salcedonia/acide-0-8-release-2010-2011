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
package acide.files.text;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.filechooser.FileFilter;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE text file extension filter manager.
 * 
 * @version 0.8
 * @see FileFilter
 */
public class AcideTextFileExtensionFilterManager extends FileFilter {

	/**
	 * ACIDE - A Configurable IDE text file extension filter manager extension
	 * description.
	 */
	private String _description;
	/**
	 * ACIDE - A Configurable IDE text file extension filter manager valid
	 * extensions.
	 */
	private List<String> _extensions;

	/**
	 * Creates a new ACIDE - A Configurable IDE text file extension filter
	 * manager with a new description given as a parameter.
	 * 
	 * @param description
	 *            new description for the text file filter.
	 */
	public AcideTextFileExtensionFilterManager(String description) {

		if (description == null)
			throw new NullPointerException(AcideLanguageManager.getInstance()
					.getLabels().getString("s312"));

		_description = description;
		_extensions = new ArrayList<String>();
	}

	/**
	 * Returns true if the file contains a valid extension and false in other
	 * case.
	 * 
	 * @param file
	 *            file to check.
	 */
	public boolean accept(File file) {
		if (file.isDirectory() || _extensions.size() == 0) {
			return true;
		}
		String fileName = file.getName().toLowerCase();
		for (String extension : _extensions) {
			if (fileName.endsWith(extension)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text file extension filter manager
	 * extension description.
	 * 
	 * @return the ACIDE - A Configurable IDE text file extension filter manager
	 *         extension description.
	 */
	public String getDescription() {

		StringBuffer buffer = new StringBuffer(_description);
		buffer.append(" (");

		for (String extension : _extensions)
			buffer.append(extension).append(" ");

		return buffer.append(")").toString();
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE text file extension
	 * filter manager extension description.
	 * 
	 * @param description
	 *            new value to set.
	 */
	public void setDescription(String description) {

		if (description == null)
			throw new NullPointerException(AcideLanguageManager.getInstance()
					.getLabels().getString("s313"));

		_description = description;
	}

	/**
	 * Adds a new extension to the ACIDE - A Configurable IDE text file
	 * extension filter manager valid extension list.
	 * 
	 * @param extension
	 *            new extension to add.
	 */
	public void addExtension(String extension) {

		if (extension == null)
			throw new NullPointerException(AcideLanguageManager.getInstance()
					.getLabels().getString("s314"));

		_extensions.add(extension.toLowerCase());
	}

	/**
	 * Removes an extension from the ACIDE - A Configurable IDE text file
	 * extension filter manager valid extension list.
	 * 
	 * @param extension
	 *            extension to remove.
	 */
	public void removeExtension(String extension) {
		_extensions.remove(extension);
	}

	/**
	 * Deletes all the ACIDE - A Configurable IDE text file extension filter
	 * manager valid extensions.
	 */
	public void clearExtensions() {
		_extensions.clear();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE text file extension filter manager
	 * valid extension list.
	 * 
	 * @return the ACIDE - A Configurable IDE text file extension filter manager
	 *         extension list.
	 */
	public List<String> getExtensions() {
		return _extensions;
	}
}
