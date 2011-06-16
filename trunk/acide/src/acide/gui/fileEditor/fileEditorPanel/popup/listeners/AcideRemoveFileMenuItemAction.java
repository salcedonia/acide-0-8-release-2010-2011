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

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu remove file menu
 * item action listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideRemoveFileMenuItemAction implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
	 * ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Are you sure?
		int returnValueAreYouSure = JOptionPane.showConfirmDialog(
				null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s623"));

		// If OK
		if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

			AcideProjectFile projectFile = new AcideProjectFile();
			int currentProjectFileIndex = -1;

			// Gets the selected file editor panel index
			int selectedFileEditorPanelIndex = AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanelIndex();

			for (int index1 = 0; index1 < AcideProjectConfiguration
					.getInstance().getNumberOfFilesFromList(); index1++) {

				if (AcideProjectConfiguration
						.getInstance()
						.getFileAt(index1)
						.getAbsolutePath()
						.equals(AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getAbsolutePath())) {

					// Gets the project file from the project configuration
					projectFile = AcideProjectConfiguration.getInstance()
							.getFileAt(index1);

					for (int index2 = 0; index2 < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList() + 1; index2++) {

						if (AcideMainWindow.getInstance()
								.getExplorerPanel().getTree()
								.getPathForRow(index2)
								.getLastPathComponent().toString()
								.equals(projectFile.getLastPathComponent())) {

							currentProjectFileIndex = index2;
						}
					}
				}
			}

			// Gets the selected tree node
			TreePath currentSelection = AcideMainWindow.getInstance()
					.getExplorerPanel().getTree()
					.getPathForRow(currentProjectFileIndex);

			// Something selected
			if (currentSelection != null) {

				// Gets the current node
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());

				// Gets the current project file from the current node info
				AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
						.getUserObject();

				// Is not a directory
				if (!currentProjectFile.isDirectory()) {

					// Gets the parent
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());

					// Has parent
					if (parent != null) {

						// Removes the node from the parent
						AcideMainWindow.getInstance().getExplorerPanel()
								.getTreeModel()
								.removeNodeFromParent(currentNode);

						// Searches for the file into the project
						// configuration file list
						int fileIndex = -1;

						for (int index = 0; index < AcideProjectConfiguration
								.getInstance().getNumberOfFilesFromList(); index++) {

							if (AcideProjectConfiguration
									.getInstance()
									.getFileAt(index)
									.getAbsolutePath()
									.equals(currentProjectFile
											.getAbsolutePath())) {
								fileIndex = index;
							}
						}

						// Removes the file
						AcideProjectConfiguration.getInstance()
								.removeFileAt(fileIndex);

						// Updates the status message in the status bar
						AcideMainWindow.getInstance().getStatusBar()
								.setStatusMessage(" ");

						// The project configuration has been modified
						AcideProjectConfiguration.getInstance()
								.setIsModified(true);

						// If the file has been modified
						if (AcideMainWindow.getInstance()
								.getFileEditorManager()
								.isRedButton(selectedFileEditorPanelIndex)) {

							// Do you want to save it?
							returnValueAreYouSure = JOptionPane
									.showConfirmDialog(
											null,
											AcideLanguageManager
													.getInstance()
													.getLabels()
													.getString("s643"),
											AcideLanguageManager
													.getInstance()
													.getLabels()
													.getString("s953"),
											JOptionPane.YES_NO_OPTION);

							// If OK
							if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

								// Enables the save file menu item in the
								// file menu
								AcideMainWindow.getInstance().getMenu()
										.getFileMenu()
										.getSaveFileMenuItem()
										.setEnabled(true);

								// Calls to the file menu save file menu
								// item action performed
								AcideMainWindow.getInstance().getMenu()
										.getFileMenu()
										.getSaveFileMenuItem().doClick();
							}
						}

						// Closes the tab
						AcideMainWindow.getInstance()
								.getFileEditorManager().getTabbedPane()
								.remove(selectedFileEditorPanelIndex);

						// Validates the changes in the file editor manager
						// tabbed pane
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().validate();
												
						// Configures the file menu
						AcideMainWindow.getInstance().getMenu()
								.getFileMenu().configure();	
						
						// Configures the edit menu
						AcideMainWindow.getInstance().getMenu()
								.getEditMenu().configure();
					}
				}
			}

			// If there are more files in the project configuration
			if (AcideProjectConfiguration.getInstance()
					.getNumberOfFilesFromList() > 0) {

				// Updates the selected file editor index
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.updateRelatedComponentsAt(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());
				
				// Enables the remove file menu item in the explorer panel
				// popup menu
				AcideMainWindow.getInstance().getExplorerPanel()
						.getPopupMenu().getRemoveFileMenuItem()
						.setEnabled(true);

				// Enables the delete file menu item in the explorer panel
				// popup menu
				AcideMainWindow.getInstance().getExplorerPanel()
						.getPopupMenu().getDeleteFileMenuItem()
						.setEnabled(true);
			} else {

				// Disables the remove file menu item in the explorer panel
				// popup menu
				AcideMainWindow.getInstance().getExplorerPanel()
						.getPopupMenu().getRemoveFileMenuItem()
						.setEnabled(false);

				// Disables the delete file menu item in the explorer panel
				// popup menu
				AcideMainWindow.getInstance().getExplorerPanel()
						.getPopupMenu().getDeleteFileMenuItem()
						.setEnabled(false);
			}
		}
	}
}
