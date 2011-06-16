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
package acide.gui.menuBar.projectMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;

/**
 * ACIDE -A Configurable IDE project menu open project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideOpenProjectMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Asks for saving the project configuration
		if (AcideProjectConfiguration.getInstance()
				.askForSavingProjectConfiguration()) {

			// Saves the file editor configuration
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.askForSavingModifiedFiles()) {

				// Asks for the file path to the user
				String filePath = AcideFileManager.getInstance().askForFile(
						AcideFileOperation.OPEN,
						AcideFileTarget.PROJECTS,
						AcideFileType.FILE,
						"",
						new AcideFileExtensionFilterManager(
								new String[] { "acideProject" },
								AcideLanguageManager.getInstance().getLabels()
										.getString("s328")));

				// If the file content is not empty
				if (filePath != null) {

					// Gets the number of file editors
					int numberOfFileEditorPanels = AcideMainWindow
							.getInstance().getFileEditorManager()
							.getNumberOfFileEditorPanels();

					// Closes all the files
					for (int index = 0; index < numberOfFileEditorPanels; index++) {

						// Closes the tab defined by index at the tabbed pane
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().setSelectedIndex(0);

						// Closes the tab defined by index at the tabbed pane
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().remove(0);

						// Closes the tab defined by index at the tabbed pane
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().validate();
					}

					// Opens the project
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.openProject(filePath);
				}
			}
		}
	}
}
