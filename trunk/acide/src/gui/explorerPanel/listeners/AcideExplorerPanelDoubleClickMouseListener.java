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
package gui.explorerPanel.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import es.project.AcideProjectFileType;
import es.text.TextFile;
import gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

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

			// Gets the selected node
			TreePath selectedNode = MainWindow.getInstance().getExplorerPanel()
					.getTree()
					.getPathForLocation(mouseEvent.getX(), mouseEvent.getY());

			// If something is selected
			if (selectedNode != null) {

				// Updates the status bar
				String filePath = selectedNode.getLastPathComponent()
						.toString();
				MainWindow.getInstance().getStatusBar().setStatusMessage(filePath);

				DefaultMutableTreeNode defaultMutableTreeNode = (DefaultMutableTreeNode) selectedNode
						.getLastPathComponent();
				Object node = defaultMutableTreeNode.getUserObject();
				AcideProjectFile projectFile = (AcideProjectFile) node;

				// Searches for the file into the editor files
				int fileEditorPanelIndex = -1;
				for (int index = 0; index < MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels(); index++) {
					if (MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).getAbsolutePath()
							.equals(projectFile.getAbsolutePath())) {
						fileEditorPanelIndex = index;
					}
				}

				// If it is not opened
				if (fileEditorPanelIndex == -1) {

					// Not a directory
					if (!projectFile.isDirectory()) {

						TextFile textFile = new TextFile();
						AcideFileEditorManager editorBuilder = MainWindow
								.getInstance().getFileEditorManager();

						String fileContent = "";
						fileContent = textFile.load(projectFile.getAbsolutePath());

						// Searches for the file into the project opened files
						// list
						fileEditorPanelIndex = -1;
						for (int index = 0; index < AcideProjectConfiguration.getInstance()
								.getNumFilesFromList(); index++) {
							if (AcideProjectConfiguration.getInstance().getFileAt(index)
									.getAbsolutePath().equals(projectFile.getAbsolutePath()))
								fileEditorPanelIndex = index;
						}

						// Checks the type
						AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

						// COMPILABLE FILE?
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(fileEditorPanelIndex).isCompilableFile())

							// MAIN FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(fileEditorPanelIndex).isMainFile()) {

								fileType = AcideProjectFileType.MAIN;

								// Updates the status message in the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setStatusMessage(
												AcideProjectConfiguration.getInstance().getFileAt(fileEditorPanelIndex)
														.getAbsolutePath()
														+ " <MAIN>");
							} else {

								fileType = AcideProjectFileType.COMPILABLE;
								// Updates the status message in the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setStatusMessage(
												AcideProjectConfiguration.getInstance().getFileAt(fileEditorPanelIndex)
														.getAbsolutePath()
														+ " <COMPILABLE>");
							}
						else {

							fileType = AcideProjectFileType.NORMAL;
							
							// Updates the status message in the status bar
							MainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration.getInstance().getFileAt(fileEditorPanelIndex)
													.getAbsolutePath());
						}

						// Opens a new tab in the editor
						editorBuilder.newTab(projectFile.getAbsolutePath(),
								projectFile.getAbsolutePath(), fileContent, true, fileType, 0);

						// Enables the file menu
						MainWindow.getInstance().getMenu().enableFileMenu();
						
						// Enables the edit menu
						MainWindow.getInstance().getMenu().enableEditMenu();
						
						// Updates the undo manager
						AcideUndoRedoManager.getInstance().update();

						// Sets the focus on the selected file at the editor
						for (int index = 0; index < MainWindow.getInstance()
								.getFileEditorManager()
								.getNumFileEditorPanels(); index++) {

							final int editorIndex = index;

							if (MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(index).getAbsolutePath()
									.equals(projectFile.getAbsolutePath())) {

								SwingUtilities.invokeLater(new Runnable() {
									/*
									 * (non-Javadoc)
									 * 
									 * @see java.lang.Runnable#run()
									 */
									@Override
									public void run() {

										// Sets the focus on the text area
										MainWindow.getInstance()
												.getFileEditorManager()
												.getFileEditorPanelAt(editorIndex)
												.getActiveTextEditionArea()
												.requestFocusInWindow();
									}
								});
							}
						}

						// Validates the main window
						MainWindow.getInstance().validate();
						
						// Repaints the main window
						MainWindow.getInstance().repaint();

						// Sets the file status in the project configuration
						for (int index = 0; index < AcideProjectConfiguration.getInstance().getFileListSize(); index++) {
							if (AcideProjectConfiguration.getInstance().getFileAt(index)
									.getAbsolutePath().equals(projectFile.getAbsolutePath())) {
								AcideProjectConfiguration.getInstance()
										.getFileAt(index).setIsOpened(true);
							}
						}

						// The project has been modified
						AcideProjectConfiguration.getInstance()
								.setIsModified(true);
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
							MainWindow.getInstance().getFileEditorManager()
									.setSelectedFileEditorPanelAt(editorIndex);

							// Sets the focus on the text component
							MainWindow.getInstance().getFileEditorManager()
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
