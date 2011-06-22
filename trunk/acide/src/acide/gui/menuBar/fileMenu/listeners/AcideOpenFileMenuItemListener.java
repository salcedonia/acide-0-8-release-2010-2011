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
package acide.gui.menuBar.fileMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.files.AcideFileManager;
import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file menu open file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideOpenFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Ask the path to the user
		String[] absolutePaths = AcideFileManager.getInstance().askForFiles(
				AcideFileOperation.OPEN, AcideFileTarget.FILES,
				AcideFileType.FILE, "", null);

		if (absolutePaths != null) {

			// Opens all the selected files
			for (int index = 0; index < absolutePaths.length; index++) {

				// Opens the file
				AcideMainWindow.getInstance().getMenu().getFileMenu()
						.openFile(absolutePaths[index]);
			}
		} else {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s83"));
		}
	}
}