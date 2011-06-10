/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
package acide.gui.menuBar.projectMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;

import acide.files.AcideFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE -A Configurable IDE project menu save project as menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveProjectAsMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Selects the extension for the project
		String[] extensions = new String[] { "acideProject" };

		File selectedFile = null;
		String absolutePath = null;
		String lastPath = null;

		// Creates the file chooser
		JFileChooser _fileChooser = new JFileChooser();

		try {

			// Gets the ACIDE - A Configurable IDE last opened project
			// directory
			lastPath = AcideResourceManager.getInstance().getProperty(
					"lastOpenedProjectDirectory");

			// Adds the filter to the file chooser
			_fileChooser
					.addChoosableFileFilter(new AcideFileExtensionFilterManager(
							extensions, AcideLanguageManager
									.getInstance().getLabels()
									.getString("s328")));

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(new File(lastPath));

			// Disables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(false);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask for the file to the user
			int returnValue = _fileChooser.showSaveDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected file
				selectedFile = _fileChooser.getSelectedFile();

				// Gets the absolute path
				absolutePath = selectedFile.getAbsolutePath();

				if (absolutePath != null) {

					// Add the extension if the name does not contain it
					if (!absolutePath.contains(".acideProject"))

						// Adds it
						absolutePath = absolutePath + ".acideProject";

					// Saves the project as
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.saveProjectAs(absolutePath);

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance()
									.getLabels().getString("s300")
									+ absolutePath);

					// Updates the ACIDE - A Configurable IDE last
					// opened
					// project
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedProjectDirectory", absolutePath);
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
