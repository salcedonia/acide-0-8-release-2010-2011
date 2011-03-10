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

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import acide.language.AcideLanguageManager;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.project.workbench.AcideWorkbenchManager;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideUndoRedoManager;

/**
 * ACIDE - A Configurable IDE file menu open all files menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideOpenAllFilesMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		for (int index1 = 0; index1 < AcideProjectConfiguration.getInstance()
				.getNumberOfFilesFromList(); index1++) {

			// Checks if the file really exists
			File file = new File(AcideProjectConfiguration.getInstance()
					.getFileAt(index1).getAbsolutePath());

			// If the file is not a directory and exists
			if (!AcideProjectConfiguration.getInstance().getFileAt(index1)
					.isDirectory()
					&& file.exists()) {

				// Loads its content
				String fileContent = null;
				fileContent = AcideFileManager.getInstance().load(
						AcideProjectConfiguration.getInstance()
								.getFileAt(index1).getAbsolutePath());

				// Gets the absolute path of the current checked editor
				String filePath = AcideProjectConfiguration.getInstance()
						.getFileAt(index1).getAbsolutePath();

				// Checks if the file is already opened
				boolean isOpened = false;

				for (int index2 = 0; index2 < AcideMainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels(); index2++) {
					if (AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index2).getAbsolutePath()
							.equals(filePath)) {
						isOpened = true;
					}
				}

				// If it is not opened
				if (!isOpened) {

					// Updates the status message in the status bar
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideProjectConfiguration.getInstance()
											.getFileAt(index1)
											.getAbsolutePath());

					// Check if it is a MAIN or COMPILABLE FILE
					AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

					// COMPILABLE
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index1).isCompilableFile()) {

						fileType = AcideProjectFileType.COMPILABLE;

						// Adds the <COMPILABLE> tag in the status bar
						AcideMainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideProjectConfiguration.getInstance()
												.getFileAt(index1)
												.getAbsolutePath()
												+ " <COMPILABLE>");
					}

					// MAIN
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index1).isMainFile()) {

						fileType = AcideProjectFileType.MAIN;

						// Adds the <MAIN> tag in the status bar
						AcideMainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideProjectConfiguration.getInstance()
												.getFileAt(index1)
												.getAbsolutePath()
												+ " <MAIN>");
					}

					// Updates the tabbed pane in the file editor manager
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.updatesTabbedPane(filePath, fileContent, true,
									fileType, 0, 0, 1);

					// Checks if it is marked as a MAIN or COMPILABLE FILE
					for (int index2 = 0; index2 < AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getNumberOfFileEditorPanels(); index2++) {

						if (AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(index2)
								.getAbsolutePath()
								.equals(AcideProjectConfiguration.getInstance()
										.getFileAt(index1).getAbsolutePath())) {

							// IS COMPILABLE FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index1).isCompilableFile())
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(index2)
										.setCompilableFile(true);

							// IS MAIN FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index1).isMainFile())
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(index2)
										.setMainFile(true);
						}
					}

					// Enables the file menu
					AcideMainWindow.getInstance().getMenu().enableFileMenu();

					// Enables the edit menu
					AcideMainWindow.getInstance().getMenu().enableEditMenu();

					// Updates the undo manager
					AcideUndoRedoManager.getInstance().update();

					// The project configuration has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(false);

					// Adds the file to the recent files list
					AcideWorkbenchManager.getInstance().addRecentFileToList(
							filePath);

				} else {

					// Is already opened
				}
			} else {

				// If the file is not a directory
				if (!AcideProjectConfiguration.getInstance().getFileAt(index1)
						.isDirectory()) {

					// If the file does not exist
					if (!file.exists()) {

						// Error message
						JOptionPane.showMessageDialog(null,
								AcideLanguageManager.getInstance().getLabels()
										.getString("s970")
										+ AcideProjectConfiguration
												.getInstance()
												.getFileAt(index1)
												.getAbsolutePath()
										+ AcideLanguageManager.getInstance()
												.getLabels().getString("s971"),
								"Error", JOptionPane.ERROR_MESSAGE);

						// Removes the file from the project
						AcideProjectConfiguration.getInstance().removeFileAt(
								index1);

						// The project configuration has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);
					}
				}
			}
		}

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				// Sets the focus in the edition area
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea().requestFocusInWindow();

				// Sets the caret visible
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().setCaretVisible(true);

				// Selects the tree node
				AcideMainWindow.getInstance().getExplorerPanel()
						.selectTreeNodeFromFileEditor();

				// Updates the status bar with the selected editor
				AcideMainWindow.getInstance().getStatusBar()
						.updatesStatusBarFromFileEditor();
			}
		});
	}
}
