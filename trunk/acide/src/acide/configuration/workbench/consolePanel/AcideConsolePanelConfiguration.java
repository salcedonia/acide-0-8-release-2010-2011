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
package acide.configuration.workbench.consolePanel;

import acide.configuration.lexicon.AcideLexiconConfiguration;

/**
 * ACIDE - A Configurable IDE console panel configuration.
 * 
 * Stores the file editor configuration for an opened file in the file editor of
 * the application.
 * 
 * @version 0.8
 */
public class AcideConsolePanelConfiguration {

	/**
	 * ACIDE - A Configurable IDE console panel configuration lexicon
	 * configuration.
	 */
	private String _lexiconConfiguration;

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel configuration.
	 */
	public AcideConsolePanelConfiguration() {

		// Sets the default lexicon configuration
		_lexiconConfiguration = AcideLexiconConfiguration.DEFAULT_PATH
				+ AcideLexiconConfiguration.DEFAULT_NAME;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration
	 * lexicon configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration
	 *         lexicon configuration.
	 */
	public String getLexiconConfiguration() {
		return _lexiconConfiguration;
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE console panel
	 * configuration lexicon configuration.
	 * 
	 * @param lexiconConfiguration
	 *            new value to set.
	 */
	public void setLexiconConfiguration(String lexiconConfiguration) {
		_lexiconConfiguration = lexiconConfiguration;
	}
}
