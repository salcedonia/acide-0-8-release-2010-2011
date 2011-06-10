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
package acide.gui.menuBar.configurationMenu.toolBarMenu.listeners;

import acide.files.AcideFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE tool bar menu load tool bar listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideLoadToolBarMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Selects the extension for the project
		String[] extensions = new String[] { "toolbarConfig" };

		File selectedFile = null;
		String absolutePath = null;

		// Creates the file chooser
		JFileChooser _fileChooser = new JFileChooser();

		try {

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(new File(
					"./configuration/toolbar/"));

			// Adds the filter to the file chooser
			_fileChooser
					.addChoosableFileFilter(new AcideFileExtensionFilterManager(
							extensions, AcideLanguageManager.getInstance()
									.getLabels().getString("s904")));

			// Disables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(false);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask for the file to the user
			int returnValue = _fileChooser.showOpenDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected file
				selectedFile = _fileChooser.getSelectedFile();

				// Gets the absolute path
				absolutePath = selectedFile.getAbsolutePath();

				if (absolutePath != null) {

					// Loads the tool bar configuration
					AcideMainWindow.getInstance().getToolBarPanel()
							.load(absolutePath);

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s300")
									+ absolutePath);

					// Updates the ACIDE - A Configurable IDE last opened
					// file
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedFileDirectory", absolutePath);
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
