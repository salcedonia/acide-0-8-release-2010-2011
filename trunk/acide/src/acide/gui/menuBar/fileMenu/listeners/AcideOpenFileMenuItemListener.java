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

import javax.swing.SwingUtilities;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.project.workbench.AcideWorkbenchManager;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideUndoRedoManager;

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

		// Asks for the path to the user
		String filePath = " ";
		filePath = AcideFileManager.getInstance().askAbsolutePath();

		// If the file exists
		if (filePath != null) {

			boolean isOpened = false;

			// Checks if the file is already opened
			int fileIndex = -1;
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).getAbsolutePath()
						.equals(filePath)) {
					isOpened = true;
					fileIndex = index;
				}
			}

			// If it is not opened
			if (!isOpened) {

				String text = null;
				text = AcideFileManager.getInstance().load(filePath);

				// If the text is not empty
				if (text != null) {

					// Searches for the file into the project configuration file
					// list
					int fileProjectIndex = -1;
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index++) {
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(index).getAbsolutePath()
								.equals(filePath))
							fileProjectIndex = index;
					}

					AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

					// If belongs to the project
					if (fileProjectIndex > -1) {

						// Updates the status message in the status bar
						AcideMainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideProjectConfiguration.getInstance()
												.getFileAt(fileProjectIndex)
												.getAbsolutePath());

						// Is COMPILABLE FILE?
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(fileProjectIndex).isCompilableFile()) {

							// It is a compilable file
							fileType = AcideProjectFileType.COMPILABLE;

							// Updates the status message in the status bar
							AcideMainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration
													.getInstance()
													.getFileAt(fileProjectIndex)
													.getAbsolutePath()
													+ " <COMPILABLE>");
						}

						// Is MAIN FILE?
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(fileProjectIndex).isMainFile()) {

							// It is a main file
							fileType = AcideProjectFileType.MAIN;

							// Updates the status message in the status bar
							AcideMainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration
													.getInstance()
													.getFileAt(fileProjectIndex)
													.getAbsolutePath()
													+ " <MAIN>");
						}

					} else {

						// If it does not belong to the project

						// Updates the status message in the status bar
						AcideMainWindow.getInstance().getStatusBar()
								.setStatusMessage(filePath);

					}

					// Updates the tabbed pane in the file editor manager
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.updatesTabbedPane(filePath, text, true, fileType,
									0, 0, 1);

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s84")
									+ filePath);
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s85")
									+ filePath
									+ AcideLanguageManager.getInstance()
											.getLabels().getString("s86"));

					// Enables the file menu
					AcideMainWindow.getInstance().getMenu().enableFileMenu();

					// Enables the edit menu
					AcideMainWindow.getInstance().getMenu().enableEditMenu();

					// Updates the undo manager
					AcideUndoRedoManager.getInstance().update();

					// Sets the caret in the first position of the editor
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().setCaretPosition(0);

					// Sets the new file state to opened
					for (int filePosition = 0; filePosition < AcideProjectConfiguration
							.getInstance().getFileListSize(); filePosition++) {
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(filePosition).getAbsolutePath()
								.equals(filePath)) {
							AcideProjectConfiguration.getInstance()
									.getFileAt(filePosition).setIsOpened(true);
						}
					}

					// Sets the focus on the selected file at the editor
					for (int index = 0; index < AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getNumberOfFileEditorPanels(); index++) {

						final int editorIndex = index;

						if (AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index).getAbsolutePath()
								.equals(filePath)) {

							SwingUtilities.invokeLater(new Runnable() {
								/*
								 * (non-Javadoc)
								 * 
								 * @see java.lang.Runnable#run()
								 */
								@Override
								public void run() {

									// Sets the selected editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													editorIndex);

									// Sets the focus on the text area
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(editorIndex)
											.getActiveTextEditionArea()
											.requestFocusInWindow();

									// If it is not the default project
									if (!AcideProjectConfiguration
											.getInstance().isDefaultProject())

										// Selects the node in the tree that
										// matches with the clicked file
										// editor
										AcideMainWindow.getInstance()
												.getExplorerPanel()
												.selectTreeNodeFromFileEditor();

								}
							});
						}
					}
					// If it is not the default project
					if (!AcideProjectConfiguration.getInstance()
							.isDefaultProject())

						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

				} else {

					// EMPTY FILE

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s88"));
				}

			} else {

				// If it is already opened

				final int editorIndex = fileIndex;

				SwingUtilities.invokeLater(new Runnable() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {

						// Sets the selected editor
						AcideMainWindow.getInstance().getFileEditorManager()
								.setSelectedFileEditorPanelAt(editorIndex);

						// Sets the focus on the text component
						AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(editorIndex)
								.getActiveTextEditionArea()
								.requestFocusInWindow();

						// Selects the node in the tree that matches with the
						// clicked file
						// editor
						AcideMainWindow.getInstance().getExplorerPanel()
								.selectTreeNodeFromFileEditor();
					}
				});
			}

			// Adds the file to the recent files list
			AcideWorkbenchManager.getInstance().addRecentFileToList(filePath);

		} else

			// FILE DOESN'T EXISTS

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s83"));
	}
}