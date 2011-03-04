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
package gui.menuBar.fileMenu.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.text.AcideFileManager;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file menu close all files item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class CloseAllFilesMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the number of file editor panels
		int numberOfFileEditorPanels = MainWindow.getInstance()
		.getFileEditorManager().getNumberOfFileEditorPanels();
		
		// Checks the opened editors
		for (int index1 = numberOfFileEditorPanels - 1; index1 >= 0; index1--) {

			// Sets the selected editor at the current checked file editor,
			// starting from the last one
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index1);

			// It is a modified editor
			if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {

				// Ask the user for saving the file
				int resultValue = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s643"));

				// If yes
				if (resultValue == JOptionPane.OK_OPTION) {

					// If it is the new file
					if (MainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getAbsolutePath()
							.equals(AcideLanguageManager.getInstance()
									.getLabels().getString("s79"))) {

						// Asks for the path to the user
						String fileAbsolutePath = " ";
						fileAbsolutePath = AcideFileManager.getInstance()
								.askSavingFileEditorFile();

						if (fileAbsolutePath.equals(" ")) {

							// Updates the log
							AcideLog.getLog().info(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s92"));
						} else {

							// Saves the file
							boolean savingResult = AcideFileManager
									.getInstance()
									.write(fileAbsolutePath,
											MainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanel()
													.getTextEditionAreaContent());

							// If it could save it
							if (savingResult) {

								// Updates the log
								AcideLog.getLog().info(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s93")
												+ fileAbsolutePath
												+ AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s94"));

								// Sets green button
								MainWindow.getInstance().getFileEditorManager()
										.setGreenButton();

								// Sets the path
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(index1)
										.setAbsolutePath(fileAbsolutePath);

								// Sets the tool tip text
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(index1)
										.setToolTipText(fileAbsolutePath);

								// Gets the name
								int lastIndexOfSlash = fileAbsolutePath
										.lastIndexOf("\\");
								if (lastIndexOfSlash == -1)
									lastIndexOfSlash = fileAbsolutePath
											.lastIndexOf("/");
								String file = fileAbsolutePath.substring(
										lastIndexOfSlash + 1,
										fileAbsolutePath.length());
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(index1)
										.setName(file);

								// Creates the file
								File projectFile = new File(MainWindow
										.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath());

								// Sets the last change
								MainWindow
										.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.setLastChange(
												projectFile.lastModified());

								// Sets the last size
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.setLastSize(projectFile.length());

							} else {

								// Updates the log
								AcideLog.getLog().info(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s95")
												+ fileAbsolutePath);
							}
						}

					} else {

						// Enables the save menu item in the file menu
						MainWindow.getInstance().getMenu().getFile()
								.getSaveFile().setEnabled(true);

						// Does the save menu item action performed
						MainWindow.getInstance().getMenu().getFile()
								.getSaveFile().doClick();
					}

					// If it is not the default project
					if (!AcideProjectConfiguration.getInstance()
							.isDefaultProject())

						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

					// Sets the editors to closed in the project configuration
					for (int index2 = 0; index2 < AcideProjectConfiguration
							.getInstance().getFileListSize(); index2++) {

						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index2)
								.getAbsolutePath()
								.equals(MainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(index1)
										.getAbsolutePath())) {
							AcideProjectConfiguration.getInstance()
									.getFileAt(index2).setIsOpened(false);
						}
					}
				} else {
					if (resultValue == JOptionPane.CANCEL_OPTION)
						return;
				}
			}

			// Sets the editors to closed in the project configuration
			for (int index2 = 0; index2 < AcideProjectConfiguration
					.getInstance().getFileListSize(); index2++) {

				if (AcideProjectConfiguration
						.getInstance()
						.getFileAt(index2)
						.getAbsolutePath()
						.equals(MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index1).getAbsolutePath())) {

					// Sets the file as closed
					AcideProjectConfiguration.getInstance().getFileAt(index2)
							.setIsOpened(false);
				}
			}
		}

		// Removes all the tabs in the tabbed pane
		for (int index = 0; index < numberOfFileEditorPanels ; index++) {

			// Sets the selected file editor panel at 0
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(0);

			// Removes it
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.remove(0);

			// Validates the changes in the tabbed pane
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.validate();
		}

		// Disables the file menu
		MainWindow.getInstance().getMenu().getFile().disableMenu();

		// Disables the edit menu
		MainWindow.getInstance().getMenu().disableEditMenu();
	}
}
