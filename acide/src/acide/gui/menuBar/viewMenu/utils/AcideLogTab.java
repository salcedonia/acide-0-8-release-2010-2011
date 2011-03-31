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
package acide.gui.menuBar.viewMenu.utils;

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFileType;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE log tab to display in the file editor panel.
 * 
 * @version 0.8
 */
public class AcideLogTab {

	/**
	 * ACIDE - A Configurable IDE log file path.
	 */
	private static final String LOG_FILE_PATH = "./log/logfile.txt";
	/**
	 * ACIDE - A Configurable IDE log file content.
	 */
	private String _logFileContent = "";

	/**
	 * Creates a new ACIDE - A Configurable IDE log tab.
	 */
	public AcideLogTab() {

		// Gets the file content
		_logFileContent = AcideFileManager.getInstance().load(LOG_FILE_PATH);

		if (_logFileContent != null) {

			// TODO: Load the predefined extension

			// Creates the lexicon configuration
			AcideLexiconConfiguration lexiconConfiguration = new AcideLexiconConfiguration();

			// Loads the lexicon configuration
			lexiconConfiguration.load(AcideLexiconConfiguration.DEFAULT_PATH
					+ AcideLexiconConfiguration.DEFAULT_NAME);

			// TODO: Load the predefined extension

			// Creates the current grammar configuration
			AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

			// Sets the current grammar configuration path
			currentGrammarConfiguration
					.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

			// Creates the previous grammar configuration
			AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

			// Sets the previous grammar configuration path
			previousGrammarConfiguration
					.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

			// Updates the tabbed pane in the file editor manager
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.updatesTabbedPane("Log", _logFileContent, false,
							AcideProjectFileType.NORMAL, 0, 0, 1,
							lexiconConfiguration, currentGrammarConfiguration,
							previousGrammarConfiguration);
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE log tab text log.
	 * 
	 * @return the ACIDE - A Configurable IDE log tab text log.
	 */
	public String getLogFileContent() {
		return _logFileContent;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE log tab file content.
	 * 
	 * @param logFileContent
	 *            new value to set.
	 */
	public void setLogFileContent(String logFileContent) {
		_logFileContent = logFileContent;
	}
}
