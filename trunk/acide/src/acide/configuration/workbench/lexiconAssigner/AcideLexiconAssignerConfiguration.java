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
package acide.configuration.workbench.lexiconAssigner;

import acide.configuration.lexicon.AcideLexiconConfiguration;

/**
 * <p>
 * ACIDE - A Configurable IDE lexicon assigner configuration.
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
	 * ACIDE - A Configurable IDE lexicon assigner configuration default path
	 * constant.
	 */
	public static final String DEFAULT_PATH = "./configuration/lexiconAssigner/configuration.xml";
	/**
	 * ACIDE - A Configurable IDE lexicon assigner configuration list.
	 */
	private AcideLexiconAssignerConfigurationList _list;
	/**
	 * ACIDE - A Configurable IDE lexicon assigner configuration console lexicon
	 * configuration.
	 */
	private String _consoleLexiconConfiguration;
	/**
	 * ACIDE - A Configurable IDE lexicon assigner configuration apply lexicon
	 * to console flag.
	 */
	private boolean _applyLexiconToConsole;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon assigner configuration.
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
	 * Returns the ACIDE - A Configurable IDE lexicon assigner configuration list.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner configuration list.
	 */
	public AcideLexiconAssignerConfigurationList getList() {
		return _list;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon assigner
	 * configuration list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setList(AcideLexiconAssignerConfigurationList list) {
		_list = list;
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
			String lexiconConfigurationPath = getLexiconConfiguration(filePath
					.substring(lastIndexOfDot + 1));

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

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner configuration
	 * console lexicon configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner configuration
	 *         console lexicon configuration.
	 */
	public String getConsoleLexiconConfiguration() {
		return _consoleLexiconConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon assigner
	 * configuration console lexicon configuration.
	 * 
	 * @param consoleLexiconConfiguration
	 *            new value to set.
	 */
	public void setConsoleLexiconConfiguration(
			String consoleLexiconConfiguration) {
		_consoleLexiconConfiguration = consoleLexiconConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner configuration
	 * apply lexicon to console flag.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner configuration
	 *         apply lexicon to console flag
	 */
	public boolean getApplyLexiconToConsole() {
		return _applyLexiconToConsole;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon assigner
	 * configuration apply lexicon to console flag.
	 * 
	 * @param applyLexiconToConsole
	 *            new value to set.
	 */
	public void setApplyLexiconToConsole(boolean applyLexiconToConsole) {
		_applyLexiconToConsole = applyLexiconToConsole;
	}
}
