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
package acide.gui.menuBar.configurationMenu.menuMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.menu.AcideMenuItemInformation;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.AcideMenuConfigurationWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE menu menu load menu menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideLoadMenuMenuItemListener implements ActionListener {

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
		String[] extensions = new String[] { "menuConfig" };

		File selectedFile = null;
		String absolutePath = null;
	
		// Creates the file chooser
		JFileChooser _fileChooser = new JFileChooser();

		try {

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(new File("./configuration/menu/"));

			// Adds the filter to the file chooser
			_fileChooser
					.addChoosableFileFilter(
							new AcideFileExtensionFilterManager(extensions,
									AcideLanguageManager.getInstance()
											.getLabels().getString("s287"))); 
			
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

					// Creates the menu item list
					ArrayList<AcideMenuItemInformation> menuItemList = null;

					try {

						// Loads the menu item list
						menuItemList = AcideMenuConfiguration.getInstance()
								.loadMenuConfigurationFile(absolutePath);

						// Updates the the ACIDE - A Configurable IDE current menu
						// configuration
						AcideResourceManager.getInstance().setProperty(
								"currentMenuConfiguration", absolutePath);

						// Sets the new menu item list
						AcideMenuConfiguration.getInstance().setMenuElementList(
								menuItemList);

						// Builds the menu
						AcideMainWindow.getInstance().getMenu().updateComponentsVisibility();

						// Validates the changes in the main window
						AcideMainWindow.getInstance().validate();

						// Repaints the main window
						AcideMainWindow.getInstance().repaint();

						// Disables the save menu item
						AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
								.getMenuMenu().getSaveMenuMenuItem().setEnabled(false);

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s289"));

						// The changes are saved now
						AcideMenuConfigurationWindow.setChangesAreSaved(true);

						// If it is not the default project
						if (!AcideProjectConfiguration.getInstance().isDefaultProject())

							// The project has been modified
							AcideProjectConfiguration.getInstance().setIsModified(true);

					} catch (Exception exception) {

						// Displays an error message
						JOptionPane.showMessageDialog(null, AcideLanguageManager
								.getInstance().getLabels().getString("s288")
								+ " " + absolutePath, AcideLanguageManager
								.getInstance().getLabels().getString("289"),
								JOptionPane.ERROR_MESSAGE);

						// Updates the log
						AcideLog.getLog().error(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s288")
										+ " " + absolutePath);
					}

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