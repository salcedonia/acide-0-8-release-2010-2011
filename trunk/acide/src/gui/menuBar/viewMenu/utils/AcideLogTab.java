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
package gui.menuBar.viewMenu.utils;

import es.project.AcideProjectFileType;
import es.text.AcideFileManager;
import gui.mainWindow.MainWindow;

/**
 * ACIDE - A Configurable IDE log tab to display in the file editor panel.
 * 
 * @version 0.8
 */
public class AcideLogTab {

	/**
	 * ACIDE - A Configurable IDE log file path.
	 */
	private static final String LOG_FILE_PATH = ".//log/logfile.txt";
	/**
	 * ACIDE - A Configurable IDE log file content.
	 */
	private String _logFileContent = "";

	/**
	 * Creates a new ACIDE - A Configurable IDE log tab.
	 */
	public AcideLogTab() {

		// Creates the file manager
		AcideFileManager fileManager = new AcideFileManager();

		// Gets the file content
		_logFileContent = fileManager.load(LOG_FILE_PATH);

		if (_logFileContent != null)
			
			// Updates the tabbed pane in the file editor manager
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.updatesTabbedPane("Log", _logFileContent, false,
							AcideProjectFileType.NORMAL, 0, 0);
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
