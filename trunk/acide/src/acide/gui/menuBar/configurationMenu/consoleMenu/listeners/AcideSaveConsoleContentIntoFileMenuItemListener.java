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
package acide.gui.menuBar.configurationMenu.consoleMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.files.AcideFileManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE save console content into file menu item action.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveConsoleContentIntoFileMenuItemListener implements
		ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		String absoluteFilePath = null;
		String lastOpenedFileDirectory = null;

		// Creates the file chooser
		JFileChooser fileChooser = new JFileChooser();

		try {

			// Gets the ACIDE - A Configurable IDE last opened file
			// directory
			lastOpenedFileDirectory = AcideResourceManager.getInstance()
					.getProperty("lastOpenedFileDirectory");

			// Sets the current directory to the last opened file directory
			fileChooser.setCurrentDirectory(new File(lastOpenedFileDirectory));

			// Disables the multiple selection of files
			fileChooser.setMultiSelectionEnabled(false);

			// Sets only files
			fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask to the user for saving the changes
			int returnValueSaveFile = fileChooser.showSaveDialog(null);

			// Ask the user for saving it
			if (returnValueSaveFile == JFileChooser.APPROVE_OPTION) {

				// Gets the ACIDE - A Configurable IDE last opened
				// file
				// directory
				absoluteFilePath = fileChooser.getSelectedFile()
						.getAbsolutePath();

				// If the user selected something
				if (absoluteFilePath != null) {

					// Builds the file to check if it exists
					File file = new File(absoluteFilePath);

					// If the file exists
					if (file.exists()) {

						// Asks to the user if he wants to overwrite it
						int result = JOptionPane.showConfirmDialog(null,
								AcideLanguageManager.getInstance().getLabels()
										.getString("s954"),
								AcideLanguageManager.getInstance().getLabels()
										.getString("s953"),
								JOptionPane.YES_NO_OPTION);

						// If it overwrites the file
						if (result == JOptionPane.YES_OPTION) {
							// Save the file
							saveFile(absoluteFilePath);
						}
					} else {

						// Save the file
						saveFile(absoluteFilePath);
					}
				}
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Saves the file into the absolute file path given as a parameter.
	 * 
	 * @param absoluteFilePath
	 *            file to save.
	 */
	private void saveFile(String absoluteFilePath) {

		// Saves the file
		boolean result = AcideFileManager.getInstance().write(
				absoluteFilePath,
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getText());

		// If it could save it
		if (result) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s93")
							+ absoluteFilePath
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s94"));
		} else {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s95")
							+ absoluteFilePath);
		}
	}
}
