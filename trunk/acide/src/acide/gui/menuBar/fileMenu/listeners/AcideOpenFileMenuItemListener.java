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

		// Creates the file chooser
		JFileChooser _fileChooser = new JFileChooser();

		File[] selectedFiles = null;
		String absolutePaths[] = null;
		String lastPath = null;

		try {

			// Gets the ACIDE - A Configurable IDE last opened file
			// directory
			lastPath = AcideResourceManager.getInstance().getProperty(
					"lastOpenedFileDirectory");

			// Sets the title of the file chooser window
			_fileChooser.setDialogTitle(AcideLanguageManager.getInstance()
					.getLabels().getString("s9"));

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(new File(lastPath));

			// Clears the previous selected files
			_fileChooser.setSelectedFiles(new File[0]);

			// Enables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(true);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask for the file to the user
			int returnValue = _fileChooser.showOpenDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected files
				selectedFiles = _fileChooser.getSelectedFiles();

				// Creates the string array
				absolutePaths = new String[selectedFiles.length];

				// Opens all the selected files
				for (int index = 0; index < absolutePaths.length; index++) {

					// Gets the absolute path
					absolutePaths[index] = selectedFiles[index]
							.getAbsolutePath();

					// Opens the file
					AcideMainWindow.getInstance().getMenu().getFileMenu()
							.openFile(absolutePaths[index]);

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s300")
									+ absolutePaths[index]);

					// Updates the ACIDE - A Configurable IDE last opened
					// file
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedFileDirectory", absolutePaths[index]);
				}

			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancels the selection
				_fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s302"));
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}