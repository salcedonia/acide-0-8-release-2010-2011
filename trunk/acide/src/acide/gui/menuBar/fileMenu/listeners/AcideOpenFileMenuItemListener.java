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
import java.io.File;

import javax.swing.JFileChooser;

import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.resources.exception.MissedPropertyException;

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

		// Creates and configures the file chooser
		JFileChooser fileChooser = new JFileChooser();

		File file = null;
		try {
			file = new File(AcideResourceManager.getInstance().getProperty(
					"defaultPath"));
		} catch (MissedPropertyException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Sets the current directory to the grammar configuration folder
		fileChooser.setCurrentDirectory(file);

		// Asks to the user
		int returnValue = fileChooser.showOpenDialog(fileChooser);

		// If it is OK
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			// Asks for the path to the user
			String filePath = fileChooser.getSelectedFile().getAbsolutePath();

			// Updates the ACIDE - A Configurable IDE default path
			AcideResourceManager.getInstance().setProperty("defaultPath",
					filePath);

			// If the file exists
			if (filePath != null) {

				// Opens the file
				AcideMainWindow.getInstance().getMenu().getFileMenu()
						.openFile(filePath);
			} else {

				// FILE DOESN'T EXISTS

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s83"));
			}
		} else {

			// If it is CANCEL
			if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancels selection
				fileChooser.cancelSelection();
			}
		}
	}
}