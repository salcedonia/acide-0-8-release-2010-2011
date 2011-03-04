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
 * ACIDE - A Configurable IDE file menu close file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class CloseFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// If the editor has been modified
		if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {

			// Do you want to save the file?
			int resultValueSaving = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s643"));

			// If OK
			if (resultValueSaving == JOptionPane.OK_OPTION) {

				// If it is the NEW FILE
				if (MainWindow
						.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getAbsolutePath()
						.equals(AcideLanguageManager.getInstance().getLabels()
								.getString("s79"))) {

					// Asks for the path to the user
					String filePath = " ";
					filePath = AcideFileManager.getInstance()
							.askSavingFileEditorFile();

					if (!filePath.equals(" ")) {

						// Saves it
						boolean savingResult = AcideFileManager.getInstance()
								.write(filePath,
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getTextEditionAreaContent());

						// If it could save it
						if (savingResult) {

							// Updates the log
							AcideLog.getLog().info(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s93")
											+ filePath
											+ AcideLanguageManager
													.getInstance().getLabels()
													.getString("s94"));

							// Sets the green button
							MainWindow.getInstance().getFileEditorManager()
									.setGreenButton();

							// Sets the path
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(
											selectedFileEditorPanelIndex)
									.setAbsolutePath(filePath);

							// Sets the tool tip text
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(
											selectedFileEditorPanelIndex)
									.setToolTipText(filePath);

							// Gets the file name
							int index = filePath.lastIndexOf("\\");
							if (index == -1)
								index = filePath.lastIndexOf("/");
							String file = filePath.substring(index + 1,
									filePath.length());

							// Sets the file editor panel title
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(
											selectedFileEditorPanelIndex)
									.setName(file);

							// Saves the original file
							File projectFile = new File(MainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath());
							
							// Sets the last change
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.setLastChange(projectFile.lastModified());
							
							// Sets the last size
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.setLastSize(projectFile.length());

							// Updates the status message in the status bar
							MainWindow.getInstance().getStatusBar()
									.setStatusMessage(" ");
						} else
							// Updates the log
							AcideLog.getLog().info(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s92"));
					} else

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s95")
										+ filePath);
				} else {

					// Enables the save file menu item in the file menu
					MainWindow.getInstance().getMenu().getFile().getSaveFile()
							.setEnabled(true);
					
					// Performs the save file menu item action
					MainWindow.getInstance().getMenu().getFile().getSaveFile()
							.doClick();
					
					// Updates the status message in the status bar
					MainWindow.getInstance().getStatusBar()
							.setStatusMessage(" ");
				}

				// Updates the file state in the project configuration
				for (int index = 0; index < AcideProjectConfiguration.getInstance()
						.getFileListSize(); index++) {
					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(MainWindow
									.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(
											selectedFileEditorPanelIndex)
									.getAbsolutePath())) {
						AcideProjectConfiguration.getInstance().getFileAt(index)
								.setIsOpened(false);
					}
				}

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())

					// The project is modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

				// Removes the tab from the tabbed pane
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(selectedFileEditorPanelIndex);

			} else if (resultValueSaving == JOptionPane.NO_OPTION) {

				// Removes the tab from the tabbed pane
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(selectedFileEditorPanelIndex);

				// Updates the status message status bar
				MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
			}
		} else {

			// Updates the file state in the project configuration
			for (int index = 0; index < AcideProjectConfiguration.getInstance()
					.getFileListSize(); index++) {

				if (AcideProjectConfiguration
						.getInstance()
						.getFileAt(index)
						.getAbsolutePath()
						.equals(MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.getAbsolutePath())) {

					// Is not opened
					AcideProjectConfiguration.getInstance().getFileAt(index)
							.setIsOpened(false);
				}
			}

			// Not default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject())
				
				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);

			// Removes the tab from the tabbed pane
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.remove(selectedFileEditorPanelIndex);

			// Updates the status message in the status bar
			MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
		}

		// No more opened tabs
		if (MainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.getTabCount() == 0) {

			// Disables the file menu
			MainWindow.getInstance().getMenu().getFile().disableMenu();

			// Disables the edit menu
			MainWindow.getInstance().getMenu().disableEditMenu();
		}
	}
}
