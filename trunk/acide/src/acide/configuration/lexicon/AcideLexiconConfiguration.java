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
package acide.configuration.lexicon;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import javax.swing.JOptionPane;

import acide.configuration.lexicon.delimiters.AcideLexiconDelimitersManager;
import acide.configuration.lexicon.remarks.AcideLexiconRemarksManager;
import acide.configuration.lexicon.tokens.AcideLexiconTokenTypeManager;
import acide.configuration.lexicon.validExtensions.AcideValidExtensionsManager;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * ACIDE - A Configurable IDE lexicon configuration.
 * 
 * @version 0.8
 */
public class AcideLexiconConfiguration {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration default path.
	 */
	public static final String DEFAULT_PATH = "./configuration/lexicon/";
	/**
	 * ACIDE - A Configurable IDE lexicon configuration default name.
	 */
	public static final String DEFAULT_NAME = "default.xml";
	/**
	 * ACIDE - A Configurable IDE lexicon configuration name.
	 */
	private String _name;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration path.
	 */
	private String _path;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration flag that indicates if
	 * it is compiled or interpreted.
	 */
	private boolean _isCompiledOrInterpreted;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type list.
	 */
	private AcideLexiconTokenTypeManager _tokenTypeManager;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration valid extensions.
	 */
	private AcideValidExtensionsManager _validExtensionsManager;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration delimiter manager.
	 */
	private AcideLexiconDelimitersManager _delimitersManager;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration remarks.
	 */
	private AcideLexiconRemarksManager _remarksManager;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration.
	 */
	public AcideLexiconConfiguration() {
		super();
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration.
	 * 
	 * @param path
	 *            lexicon configuration path.
	 */
	public void newLexicon(String path) {

		// Gets the name
		int index = path.lastIndexOf("\\");
		if (index == -1)
			index = path.lastIndexOf("/");

		// Sets the language name
		_name = path.substring(index + 1, path.length());

		// Sets the language path
		_path = path;

		// Creates the token type manager
		_tokenTypeManager = new AcideLexiconTokenTypeManager();

		// Creates the remarks manager
		_remarksManager = new AcideLexiconRemarksManager();

		// Creates the valid extensions manager
		_validExtensionsManager = new AcideValidExtensionsManager();

		// Creates the delimiters manager
		_delimitersManager = new AcideLexiconDelimitersManager();
		
		// Saves it
		save(_name, false);
	}

	/**
	 * Saves the lexicon configuration for a programming language in a XML file
	 * and returns true if the operation was succeed or false in other case.
	 * 
	 * @param name
	 *            name of the lexicon configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean save(String name, boolean isCompiledOrInterpreted) {

		// If the name is already set by the user
		if ((_name != null) && (!_name.trim().equalsIgnoreCase(""))) {

			_name = name;
			_isCompiledOrInterpreted = isCompiledOrInterpreted;

			// Creates the XStream object to write in the XML file
			XStream xStream = new XStream();
			try {

				// Creates the file output stream
				FileOutputStream fileOutputStream = new FileOutputStream(_path);

				// Parses it to XML
				xStream.toXML(this, fileOutputStream);

				// Closes the file output stream
				fileOutputStream.close();
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
				return false;
			}
		}
		return true;
	}

	/**
	 * Saves the lexicon configuration for a programming language in a XML file
	 * in a defined path given as a parameter, returning true if the operation
	 * was succeed or false in other case.
	 * 
	 * @param name
	 *            name of the lexicon configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted.
	 * @param path
	 *            path for the new file.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean saveAs(String name, boolean IsCompiledOrInterpreted,
			String path) {

		// If the name is already set by the user
		if ((_name != null) && (!_name.trim().equalsIgnoreCase(""))) {

			_name = name;
			_isCompiledOrInterpreted = IsCompiledOrInterpreted;
			
			// Creates the XStream object to write in the XML file
			XStream xStream = new XStream();

			try {

				// Creates the file output stream
				FileOutputStream fileOutputStream = new FileOutputStream(path);

				// Parses it to XML
				xStream.toXML(this, fileOutputStream);

				// Closes the file output stream
				fileOutputStream.close();
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
				return false;
			}
		}
		return true;
	}

	/**
	 * Saves the lexicon configuration of a programming language in a temporal
	 * XML file, returning true if the operation was succeed or false in other
	 * case.
	 * 
	 * @param name
	 *            name of the lexicon configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted.
	 * 
	 * @return True if the operation was succeed or false in other case.
	 */
	public String saveTemp(String name, boolean isCompiledOrInterpreted) {

		File temporalFile = null;

		// If the name is already set by the user
		if ((_name != null) && (!_name.trim().equalsIgnoreCase(""))) {

			_name = name;
			_isCompiledOrInterpreted = isCompiledOrInterpreted;
			
			// Creates the XStream object to write in the XML file
			XStream xStream = new XStream();

			try {

				// Creates the temporal file
				temporalFile = File.createTempFile("TMP", ".xml", new File(
						"./configuration/lexicon/temp/"));

				// Deletes it on exit
				temporalFile.deleteOnExit();

				// Creates the file output stream
				FileOutputStream fileOutputStream = new FileOutputStream(
						temporalFile);

				// Parses it to XML
				xStream.toXML(this, fileOutputStream);

				// Closes the file output stream
				fileOutputStream.close();

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
				return null;
			}

			return "./configuration/lexicon/temp/" + temporalFile.getName();
		}

		return "./configuration/lexicon/temp/NULL.xml";
	}

	/**
	 * Loads the lexicon configuration for a programming language in the
	 * application from a temporal XML file which is located in a path given as
	 * a parameter.
	 * 
	 * @param path
	 *            file path of the file to extract the configuration from.
	 */
	public void loadTemp(String path) {

		// If the name is already set by the user
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {

			try {

				// Creates the XStream object
				XStream x = new XStream();

				// Creates the file input stream
				FileInputStream fileInputStream = new FileInputStream(path);

				// Gets the lexicon configuration
				AcideLexiconConfiguration lexiconConfiguration = (AcideLexiconConfiguration) x
						.fromXML(fileInputStream);

				// NAME
				String name = lexiconConfiguration.getName();

				// IS COMPILED OR INTERPRETED
				Boolean isCompiledOrInterpreted = lexiconConfiguration
						.getIsCompiledOrInterpreted();

				// TOKEN TYPE MANAGER
				AcideLexiconTokenTypeManager tokenTypeManager = lexiconConfiguration
						.getTokenTypeManager();

				// VALID EXTENSIONS MANAGER
				AcideValidExtensionsManager validExtensionsManager = lexiconConfiguration
						.getValidExtensionsManager();

				// DELIMITERS MANAGER
				AcideLexiconDelimitersManager delimitersManager = lexiconConfiguration
						.getDelimitersManager();

				// REMARKS MANAGER
				AcideLexiconRemarksManager remarksManager = lexiconConfiguration
						.getRemarksManager();

				// Closes the file input stream
				fileInputStream.close();

				// NAME
				_name = name;

				// IS COMPILED OR INTERPRETED
				_isCompiledOrInterpreted = isCompiledOrInterpreted;

				// TOKEN TYPE MANAGER
				_tokenTypeManager = tokenTypeManager;

				// VALID EXTENSIONS MANAGER
				_validExtensionsManager = validExtensionsManager;

				// DELIMITERS MANAGER
				_delimitersManager = delimitersManager;

				// REMARKS MANAGER
				_remarksManager = remarksManager;

				// PATH
				_path = path;

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * Loads the lexicon configuration for a programming language in the
	 * application from a XML file which is located in a path given as a
	 * parameter.
	 * 
	 * @param path
	 *            File path of the file to extract the configuration from.
	 */
	public void load(String path) {

		// If the name is already set by the user
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {
			try {

				// Creates the XStream object
				XStream x = new XStream(new DomDriver());

				// Creates the file input stream
				FileInputStream fileInputStream = new FileInputStream(path);

				// Gets the lexicon configuration from the XML file
				AcideLexiconConfiguration lexiconConfiguration = (AcideLexiconConfiguration) x
						.fromXML(fileInputStream);

				// NAME
				String name = lexiconConfiguration.getName();

				// IS COMPILED OR INTERPRETED
				Boolean isCompiledOrInterpreted = lexiconConfiguration
						.getIsCompiledOrInterpreted();

				// TOKEN TYPE MANAGER
				AcideLexiconTokenTypeManager tokenTypeList = lexiconConfiguration
						.getTokenTypeManager();

				// VALID EXTENSIONS MANAGER
				AcideValidExtensionsManager validExtensionsManager = lexiconConfiguration
						.getValidExtensionsManager();

				// DELIMITERS MANAGER
				AcideLexiconDelimitersManager delimitersManager = lexiconConfiguration
						.getDelimitersManager();

				// REMARKS MANAGER
				AcideLexiconRemarksManager remarksManager = lexiconConfiguration
						.getRemarksManager();

				// Closes the file input stream
				fileInputStream.close();

				// NAME
				_name = name;
				
				// IS COMPILED OR INTERPRETED
				_isCompiledOrInterpreted = isCompiledOrInterpreted;
				
				// TOKEN TYPE MANAGER
				_tokenTypeManager = tokenTypeList;
				
				// VALID EXTENSIONS MANAGER
				_validExtensionsManager = validExtensionsManager;
				
				// DELIMITERS MANAGER
				_delimitersManager = delimitersManager;
				
				// REMARKS MANAGER
				_remarksManager = remarksManager;
				
				// PATH
				_path = path;

			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s968")
						+ path
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s957")
						+ DEFAULT_PATH + DEFAULT_NAME);

				// If the file does not exist, loads the default configuration
				load(DEFAULT_PATH + DEFAULT_NAME);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s201")
								+ " " + path);
			}
		}
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration remarks
	 * manager.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration remarks
	 *         manager.
	 */
	public AcideLexiconRemarksManager getRemarksManager() {
		return _remarksManager;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * remarks manager.
	 * 
	 * @param remarksManager
	 *            new value to set.
	 */
	public void setRemarksManager(AcideLexiconRemarksManager remarksManager) {
		_remarksManager = remarksManager;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration is compiled
	 * or interpreted flag.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration is compiled
	 *         or interpreted flag.
	 */
	public boolean getIsCompiledOrInterpreted() {
		return _isCompiledOrInterpreted;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * is compiled or interpreted flag.
	 * 
	 * @param isCompiledOrInterpreted
	 *            new value to set.
	 */
	public void setIsCompiledOrInterpreted(boolean isCompiledOrInterpreted) {
		_isCompiledOrInterpreted = isCompiledOrInterpreted;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration valid
	 * extensions manager.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration valid
	 *         extensions manager.
	 */
	public AcideValidExtensionsManager getValidExtensionsManager() {
		return _validExtensionsManager;
	}

	/**
	 * Sets the a value to the ACIDE - A Configurable IDE lexicon configuration
	 * valid extensions manager.
	 * 
	 * @param validExtensionsManager
	 *            new value to set.
	 */
	public void setValidExtensionsManager(
			AcideValidExtensionsManager validExtensionsManager) {
		_validExtensionsManager = validExtensionsManager;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * manager.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         manager.
	 */
	public AcideLexiconTokenTypeManager getTokenTypeManager() {
		return _tokenTypeManager;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * token type manager.
	 * 
	 * @param tokenTypeManager
	 *            new value to set.
	 */
	public void setTokenTypeManager(
			AcideLexiconTokenTypeManager tokenTypeManager) {
		_tokenTypeManager = tokenTypeManager;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration delimiters
	 * manager.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration delimiters
	 *         manager.
	 */
	public AcideLexiconDelimitersManager getDelimitersManager() {
		return _delimitersManager;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * delimiter manager.
	 * 
	 * @param delimitersManager
	 *            new value to set.
	 */
	public void setDelimitersManager(
			AcideLexiconDelimitersManager delimitersManager) {
		_delimitersManager = delimitersManager;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration path.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration path.
	 */
	public String getPath() {
		return _path;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * path.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setPath(String path) {
		_path = path;
	}
}
