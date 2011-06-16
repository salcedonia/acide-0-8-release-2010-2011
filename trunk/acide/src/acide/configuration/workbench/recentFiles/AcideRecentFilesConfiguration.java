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
package acide.configuration.workbench.recentFiles;

import java.util.ArrayList;

import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE recent files configuration.
 * 
 * @version 0.8
 */
public class AcideRecentFilesConfiguration {

	/**
	 * ACIDE - A Configurable IDE recent files configuration list.
	 */
	private ArrayList<String> _list;

	/**
	 * Creates a new ACIDE - A Configurable IDE recent files configuration.
	 */
	public AcideRecentFilesConfiguration() {

		// Creates the recent files list
		_list = new ArrayList<String>();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE recent files configuration list.
	 * 
	 * @return the ACIDE - A Configurable IDE recent files configuration list.
	 */
	public ArrayList<String> getList() {
		return _list;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE recent files
	 * configuration list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setList(ArrayList<String> list) {
		_list = list;
	}

	/**
	 * Adds a new file path to the recent file list, avoiding duplicates.
	 * 
	 * @param filePath
	 *            new file path to add.
	 */
	public void addRecentFileToList(String filePath) {

		if (!_list.contains(filePath)) {

			// Adds the file to the recent file list
			_list.add(filePath);

			// Updates the menu
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getOpenRecentFilesMenu().build();
		}
	}
}
