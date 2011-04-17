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
package acide.configuration.lexiconAssigner;

import java.io.FileInputStream;
import java.io.FileOutputStream;

import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * <p>
 * ACIDE - A Configurable IDE lexicon assigner manager.
 * </p>
 * <p>
 * Handles the classes and methods for the automatic assignment of the lexicon
 * configuration for the file editors based on their file extension.
 * </p>
 * 
 * @version 0.8
 */
public class AcideLexiconAssignerConfiguration {

	/**
	 * ACIDE - A Configurable IDE lexicon assigner manager default path
	 * constant.
	 */
	public static final String DEFAULT_PATH = "./configuration/lexiconAssigner/configuration.xml";
	/**
	 * ACIDE - A Configurable IDE lexicon assigner manager unique class
	 * instance.
	 */
	private static AcideLexiconAssignerConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE lexicon assigner manager list.
	 */
	private AcideLexiconAssignerConfigurationList _list;

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner manager unique
	 * class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner manager unique
	 *         class instance.
	 */
	public static AcideLexiconAssignerConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideLexiconAssignerConfiguration();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon assigner manager.
	 */
	public AcideLexiconAssignerConfiguration() {

		// Creates the list
		_list = new AcideLexiconAssignerConfigurationList();
	}

	/**
	 * <p>
	 * Returns the associated ACIDE - A Configurable IDE lexicon configuration
	 * path to an extension given as a parameter.
	 * </p>
	 * <p>
	 * Checks the whole list of lexicon assigners for the lexicon configuration
	 * path and returns it in case of finding, and <b>null</b> in other case.
	 * </p>
	 * 
	 * @param extension
	 *            extension to check.
	 * 
	 * @return the associated ACIDE - A Configurable IDE lexicon configuration
	 *         if any list contains it and <b>null</b> in other case.
	 */
	public String getLexiconConfiguration(String extension) {

		String lexiconConfiguration = null;

		for (int index = 0; index < _list.getSize(); index++) {

			// If it belongs to the associated extension list
			if ((lexiconConfiguration = _list
					.getFileEditorPanelConfigurationAt(index)
					.getLexiconConfiguration(extension)) != null)

				// Returns it
				return lexiconConfiguration;
		}

		return null;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner manager list.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner manager list.
	 */
	public AcideLexiconAssignerConfigurationList getList() {
		return _list;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon assigner
	 * manager list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setList(AcideLexiconAssignerConfigurationList list) {
		_list = list;
	}

	/**
	 * Load the ACIDE - A Configurable IDE lexicon assigner manager
	 * configuration from an XML file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void load(String configurationFilePath) {

		// If the name is already set by the user
		if ((configurationFilePath != null)
				&& (!configurationFilePath.trim().equalsIgnoreCase(""))) {
			try {

				// Creates the XStream object to handle XML files
				XStream xStream = new XStream(new DomDriver());

				// Creates the file input stream
				FileInputStream fileInputStream = new FileInputStream(
						configurationFilePath);

				// Gets the lexicon assigner configuration
				AcideLexiconAssignerConfiguration lexiconAssignerConfiguration = (AcideLexiconAssignerConfiguration) xStream
						.fromXML(fileInputStream);

				// Gets the lexicon assigner configuration list
				AcideLexiconAssignerConfigurationList lexiconAssignerConfigurationList = lexiconAssignerConfiguration
						.getList();

				// Closes the file input stream
				fileInputStream.close();

				// Sets the lexicon assigner configuration list
				_list = lexiconAssignerConfigurationList;

				// Updates the ACIDE - A Configurable IDE lexicon assigner
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"lexiconAssignerConfiguration", configurationFilePath);

			} catch (Exception exception) {

				exception.printStackTrace();

				// Updates the ACIDE - A Configurable IDE lexicon assigner
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"lexiconAssignerConfiguration", configurationFilePath);
			}
		}
	}

	/**
	 * Saves the ACIDE - A Configurable IDE lexicon assigner manager
	 * configuration in a XML file and returns true if the operation succeed or
	 * false in other case.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean save() {

		// Creates the xStream object to handle XML files
		XStream xStream = new XStream();

		try {

			// Saves the file
			FileOutputStream fileOutputStream = new FileOutputStream(
					DEFAULT_PATH);

			// Saves the content into XML format
			xStream.toXML(this, fileOutputStream);

			// Closes the file output stream
			fileOutputStream.close();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
			return false;
		}

		// Updates the ACIDE - A Configurable IDE file editor configuration
		AcideResourceManager.getInstance().setProperty(
				"lexiconAssignerConfiguration", DEFAULT_PATH);

		return true;
	}

	/**
	 * <p>
	 * Returns the predefined ACIDE - A Configurable IDE lexicon configuration
	 * for a file path given as a parameter.
	 * </p>
	 * <p>
	 * Gets its file extension and looks for the extension into the ACIDE - A
	 * Configurable IDE lexicon assigner. If it does not find it, configures the
	 * returned ACIDE - A Configurable IDE with the <b>default</b>
	 * configuration.
	 * </p>
	 * 
	 * @param filePath
	 *            file path to check.
	 * 
	 * @return the predefined ACIDE - A Configurable IDE lexicon configuration
	 *         for a file path given as a parameter.
	 */
	public AcideLexiconConfiguration getPredifinedLexiconConfiguration(
			String filePath) {

		// Creates the lexicon configuration
		AcideLexiconConfiguration lexiconConfiguration = new AcideLexiconConfiguration();

		// Gets the last index of "." in the file path
		int lastIndexOfDot = filePath.lastIndexOf(".");

		// If it has extension
		if (lastIndexOfDot != -1) {

			// Gets the lexicon configuration path from the lexicon assigner
			String lexiconConfigurationPath = AcideLexiconAssignerConfiguration
					.getInstance().getLexiconConfiguration(
							filePath.substring(lastIndexOfDot + 1));

			// If it is defined
			if (lexiconConfigurationPath != null)

				// Loads the lexicon configuration from the lexicon assigner
				lexiconConfiguration.load(lexiconConfigurationPath);
			else
				// Loads the lexicon configuration by default
				lexiconConfiguration
						.load(AcideLexiconConfiguration.DEFAULT_PATH
								+ AcideLexiconConfiguration.DEFAULT_NAME);

		} else {

			// Loads the lexicon configuration by default
			lexiconConfiguration.load(AcideLexiconConfiguration.DEFAULT_PATH
					+ AcideLexiconConfiguration.DEFAULT_NAME);
		}
		return lexiconConfiguration;
	}
}
