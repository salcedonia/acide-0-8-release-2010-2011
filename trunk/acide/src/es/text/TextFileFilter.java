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
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import javax.swing.filechooser.FileFilter;

import operations.log.AcideLog;

import resources.AcideResourceManager;

import language.AcideLanguageManager;

/**																
 * Handle the text file filters of ACIDE - A Configurable IDE.											
 *					
 * @version 0.8	
 * @see FileFilter																													
 */
public class TextFileFilter extends FileFilter {

	/**
	 * File description.
	 */
	private String _description;
	/**
	 * File extensions.
	 */
	private List<String> _extensions;

	/**
	 * Creates a new text file filter with a new description given
	 * as a parameter.
	 * 
	 * @param description new description for the text file filter.
	 */
	public TextFileFilter(String description) {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		if (description == null)
			throw new NullPointerException(labels.getString("s312"));
		
		_description = description;
		_extensions = new ArrayList<String>();
	}

	/**
	 * Returns true if the file contains a valid extension and false in other case.
	 * 
	 * @param file file to check.
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
	 * Returns the description.
	 * 
	 * @return the description.
	 */
	public String getDescription() {
		
		StringBuffer buffer = new StringBuffer(_description);
		buffer.append(" (");
		
		for (String extension : _extensions)
			buffer.append(extension).append(" ");
		
		return buffer.append(")").toString();
	}

	/**
	 * Sets a new value for the description
	 * 
	 * @param description new value to set
	 */
	public void setDescription(String description) {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		if (description == null)
			throw new NullPointerException(labels.getString("s313"));
		
		_description = description;
	}

	/**
	 * Adds a new extension to the list.
	 * 
	 * @param extension new extension to add.
	 */
	public void addExtension(String extension) {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		if (extension == null)
			throw new NullPointerException(labels.getString("s314"));
	
		_extensions.add(extension.toLowerCase());
	}

	/** 
	 * Removes an extension from the list of available extensions.
	 * 
	 * @param extension extension to remove.
	 */
	public void removeExtension(String extension) {
		_extensions.remove(extension);
	}

	/**
	 * Deletes all the extensions.
	 */
	public void clearExtensions() {
		_extensions.clear();
	}

	/**
	 * Returns the list of extensions
	 * 
	 * @return the list of extensions
	 */
	public List<String> getExtensions() {
		return _extensions;
	}
}
