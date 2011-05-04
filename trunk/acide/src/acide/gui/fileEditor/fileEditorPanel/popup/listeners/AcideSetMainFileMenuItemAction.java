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
package acide.gui.fileEditor.fileEditorPanel.popup.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;

import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu set main file
 * menu item action listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSetMainFileMenuItemAction implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
	 * ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// IF it is not MAIN FILE
		if (!AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().isMainFile()) {

			// Removes the previous MAIN FILE
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// Finds the previous MAIN FILE
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).isMainFile()) {

					// Sets MAIN FILE as false
					AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).setMainFile(false);

					// Sets COMPILER FILE as false
					AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index)
							.setCompilableFile(false);

					// Updates the status message in the status bar
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(index)
											.getAbsolutePath());

					// Removes the MAIN icon from the file editor manager
					// tab
					AcideMainWindow.getInstance().getFileEditorManager()
							.getTabbedPane().setIconAt(index, null);
				}
			}

			// Sets MAIN FILE as true
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setMainFile(true);

			// Sets COMPILER FILE as true
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setCompilableFile(true);

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Search for the file into the project configuration file
				// list
				for (int index1 = 0; index1 < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index1++) {

					// If exists
					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index1)
							.getAbsolutePath()
							.equals(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath())) {

						// The project has been modified
						AcideProjectConfiguration.getInstance()
								.setIsModified(true);

						// Removes the previous MAIN FILE from the project
						// configuration
						for (int index2 = 0; index2 < AcideProjectConfiguration
								.getInstance().getFileListSize(); index2++) {

							// MAIN FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index2).isMainFile()) {

								// Sets MAIN FILE to false
								AcideProjectConfiguration.getInstance()
										.getFileAt(index2)
										.setIsMainFile(false);

								// Sets COMPILABLE FILE to false
								AcideProjectConfiguration.getInstance()
										.getFileAt(index2)
										.setIsCompilableFile(false);
							}
						}

						// Sets it as MAIN FILE
						AcideProjectConfiguration.getInstance()
								.getFileAt(index1).setIsMainFile(true);

						// Sets it as COMPILABLE FILE
						AcideProjectConfiguration.getInstance()
								.getFileAt(index1)
								.setIsCompilableFile(true);

						// Puts the MAIN icon in the selected file editor
						// panel
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.setIconAt(
										AcideMainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex(),
										new ImageIcon(
												"./resources/icons/editor/main.png"));

						// Updates the status message in the status bar
						AcideMainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideMainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getAbsolutePath()
												+ " <MAIN>");

					}
				}
			} else {

				// DEFAULT CONFIGURATION

				// Puts the MAIN icon in the selected file editor panel
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.setIconAt(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex(),
								new ImageIcon(
										"./resources/icons/editor/main.png"));

				// Updates the status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath()
										+ " <MAIN>");
			}
		}
	}
}
