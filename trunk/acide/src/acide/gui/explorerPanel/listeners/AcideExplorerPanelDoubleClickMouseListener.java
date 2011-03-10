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
package acide.gui.explorerPanel.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.files.project.AcideProjectFileType;
import acide.gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * ACIDE - A Configurable IDE explorer panel double click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideExplorerPanelDoubleClickMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Double click
		if (mouseEvent.getClickCount() == 2) {

			// Gets the current explorer tree selection
			TreePath currentSelection = AcideMainWindow.getInstance()
					.getExplorerPanel().getTree()
					.getPathForLocation(mouseEvent.getX(), mouseEvent.getY());

			// If something is selected
			if (currentSelection != null) {

				// Updates status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								currentSelection.getLastPathComponent()
										.toString());

				// Gets the current node
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) currentSelection
						.getLastPathComponent();

				// Gets the current project file
				AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
						.getUserObject();

				// Searches for the file into the editor files
				int fileEditorPanelIndex = -1;
				for (int index = 0; index < AcideMainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

					if (AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).getAbsolutePath()
							.equals(currentProjectFile.getAbsolutePath())) {

						// Found it
						fileEditorPanelIndex = index;
					}
				}

				// If it is not opened
				if (fileEditorPanelIndex == -1) {

					// Not a directory
					if (!currentProjectFile.isDirectory()) {

						// Gets the file editor manager
						AcideFileEditorManager fileEditorManager = AcideMainWindow
								.getInstance().getFileEditorManager();

						// Gets its content
						String fileContent = "";
						fileContent = AcideFileManager.getInstance().load(
								currentProjectFile.getAbsolutePath());

						// Searches for the file into the project opened files
						// list
						fileEditorPanelIndex = -1;
						for (int index = 0; index < AcideProjectConfiguration
								.getInstance().getNumberOfFilesFromList(); index++) {
							if (AcideProjectConfiguration
									.getInstance()
									.getFileAt(index)
									.getAbsolutePath()
									.equals(currentProjectFile
											.getAbsolutePath()))

								// Found it
								fileEditorPanelIndex = index;
						}

						// Checks the type
						AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

						// COMPILABLE FILE?
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(fileEditorPanelIndex)
								.isCompilableFile())

							// MAIN FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(fileEditorPanelIndex)
									.isMainFile()) {

								fileType = AcideProjectFileType.MAIN;

								// Updates the status message in the status bar
								AcideMainWindow
										.getInstance()
										.getStatusBar()
										.setStatusMessage(
												AcideProjectConfiguration
														.getInstance()
														.getFileAt(
																fileEditorPanelIndex)
														.getAbsolutePath()
														+ " <MAIN>");
							} else {

								fileType = AcideProjectFileType.COMPILABLE;
								// Updates the status message in the status bar
								AcideMainWindow
										.getInstance()
										.getStatusBar()
										.setStatusMessage(
												AcideProjectConfiguration
														.getInstance()
														.getFileAt(
																fileEditorPanelIndex)
														.getAbsolutePath()
														+ " <COMPILABLE>");
							}
						else {

							fileType = AcideProjectFileType.NORMAL;

							// Updates the status message in the status bar
							AcideMainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration
													.getInstance()
													.getFileAt(
															fileEditorPanelIndex)
													.getAbsolutePath());
						}

						// Updates the tabbed pane in the file editor manager
						fileEditorManager.updatesTabbedPane(
								currentProjectFile.getAbsolutePath(),
								fileContent, true, fileType, 0, 0, 1);

						// Enables the file menu
						AcideMainWindow.getInstance().getMenu().enableFileMenu();

						// Enables the edit menu
						AcideMainWindow.getInstance().getMenu().enableEditMenu();

						// Updates the undo manager
						AcideUndoRedoManager.getInstance().update();

						// Sets the focus on the selected file at the editor
						for (int index = 0; index < AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getNumberOfFileEditorPanels(); index++) {

							final int editorIndex = index;

							if (AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(index)
									.getAbsolutePath()
									.equals(currentProjectFile
											.getAbsolutePath())) {
								// Puts the focus on the active text area
								AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										editorIndex).putFocusOnActiveTextArea();							}
						}

						// Validates the main window
						AcideMainWindow.getInstance().validate();

						// Repaints the main window
						AcideMainWindow.getInstance().repaint();

						// Sets the file status in the project configuration
						for (int index = 0; index < AcideProjectConfiguration
								.getInstance().getFileListSize(); index++) {
							if (AcideProjectConfiguration
									.getInstance()
									.getFileAt(index)
									.getAbsolutePath()
									.equals(currentProjectFile
											.getAbsolutePath())) {
								AcideProjectConfiguration.getInstance()
										.getFileAt(index).setIsOpened(true);
							}
						}

						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);
					}
				} else {

					// If it is already opened

					final int editorIndex = fileEditorPanelIndex;

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
						}
					});
				}
			}
		}
	}
}
