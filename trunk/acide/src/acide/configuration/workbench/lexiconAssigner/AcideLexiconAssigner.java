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

import java.util.ArrayList;

import acide.configuration.lexicon.AcideLexiconConfiguration;

/**
 * <p>
 * ACIDE - A Configurable IDE lexicon assigner.
 * </p>
 * <p>
 * Represents an instance of an automatic assignment of the lexicon
 * configuration for the file editors.
 * </p>
 * <p>
 * It is defined by three components: a description, a list of extensions
 * related and a lexicon configuration to apply.
 * </p>
 * 
 * @version 0.8
 */
public class AcideLexiconAssigner {

	/**
	 * ACIDE - A Configurable IDE lexicon assigner number of parameter constant.
	 */
	public static final int NUMBER_OF_PARAMETERS = 3;
	/**
	 * ACIDE - A Configurable IDE lexicon assigner description.
	 */
	private String _description;
	/**
	 * ACIDE - A Configurable IDE lexicon assigner extension list.
	 */
	private ArrayList<String> _extensionList;
	/**
	 * ACIDE - A Configurable IDE lexicon assigner associated lexicon
	 * configuration.
	 */
	private String _lexiconConfiguration;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon assigner.
	 */
	public AcideLexiconAssigner() {

		// Sets the description by default
		_description = "";

		// Creates the extension list
		_extensionList = new ArrayList<String>();

		// Sets the lexicon configuration
		_lexiconConfiguration = AcideLexiconConfiguration.DEFAULT_PATH
				+ AcideLexiconConfiguration.DEFAULT_NAME;
	}

	/**
	 * <p>
	 * Returns the ACIDE - A Configurable IDE lexicon configuration associated
	 * to an extension given as a parameter.
	 * </p>
	 * <p>
	 * If the extension list contains such extension, then it returns the
	 * associated lexicon configuration. If not it will return <b>null</b>.
	 * </p>
	 * 
	 * @param extension
	 *            extension to check.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration associated
	 *         to the extension if the list contains it and <b>null</b> in other
	 *         case.
	 */
	public String getLexiconConfiguration(String extension) {

		if (_extensionList.contains(extension))
			return _lexiconConfiguration;

		return null;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner description.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner description.
	 */
	public String getDescription() {
		return _description;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon assigner
	 * description.
	 * 
	 * @param description
	 *            new value to set.
	 */
	public void setDescription(String description) {
		_description = description;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner extension list.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner extension list.
	 */
	public ArrayList<String> getExtensionList() {
		return _extensionList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon assigner
	 * extension list.
	 * 
	 * @param extensionList
	 *            new value to set.
	 */
	public void setExtensionList(ArrayList<String> extensionList) {
		_extensionList = extensionList;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner associated
	 * lexicon configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner associated
	 *         lexicon configuration.
	 */
	public String getLexiconConfiguration() {
		return _lexiconConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon assigner
	 * associated lexicon configuration.
	 * 
	 * @param lexiconConfiguration
	 *            new value to set.
	 */
	public void setLexiconConfiguration(String lexiconConfiguration) {
		_lexiconConfiguration = lexiconConfiguration;
	}
}
