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
import java.io.File;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu delete file menu
 * item action listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideDeleteFileMenuItemAction implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
	 * ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Asks to the user
		int returnValueAreYouSure = JOptionPane.showConfirmDialog(
				null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s951"));

		// If OK
		if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

			// If it is the default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Deletes the file
				String fileRemove = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanel().getAbsolutePath();
				File file = new File(fileRemove);
				file.delete();

				// Removes the tab in the editor
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.remove(AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanelIndex());

				// Updates the status message in the status bar
				AcideMainWindow.getInstance().getStatusBar()
						.setStatusMessage(" ");

			} else {

				// Not default project
				AcideProjectFile explorerFile = new AcideProjectFile();

				int fileIndex = -1;

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

						explorerFile = AcideProjectConfiguration
								.getInstance().getFileAt(index1);

						for (int index2 = 0; index2 < AcideProjectConfiguration
								.getInstance().getNumberOfFilesFromList() + 1; index2++) {

							if (AcideMainWindow
									.getInstance()
									.getExplorerPanel()
									.getTree()
									.getPathForRow(index2)
									.getLastPathComponent()
									.toString()
									.equals(explorerFile
											.getLastPathComponent())) {

								fileIndex = index2;
							}
						}
					}
				}

				// Gets the selected noede
				TreePath currentSelection = AcideMainWindow.getInstance()
						.getExplorerPanel().getTree()
						.getPathForRow(fileIndex);

				// Belongs the the project
				if (currentSelection != null) {

					// Gets the current node
					DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
							.getLastPathComponent());

					// Gets the current project file from the current node
					// info
					AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
							.getUserObject();

					// Is not a directory
					if (!currentProjectFile.isDirectory()) {

						// Gets the parent
						MutableTreeNode parent = (MutableTreeNode) (currentNode
								.getParent());

						// Has parent
						if (parent != null) {

							// Removes the parent
							AcideMainWindow.getInstance()
									.getExplorerPanel().getTreeModel()
									.removeNodeFromParent(currentNode);

							// Looks for the file in the project
							// configuration
							fileIndex = -1;
							for (int index = 0; index < AcideProjectConfiguration
									.getInstance()
									.getNumberOfFilesFromList(); index++)
								if (AcideProjectConfiguration
										.getInstance()
										.getFileAt(index)
										.getAbsolutePath()
										.equals(currentProjectFile
												.getAbsolutePath()))
									fileIndex = index;

							// Gets the file to be removed
							AcideProjectFile f = AcideProjectConfiguration
									.getInstance().getFileAt(fileIndex);
							// Gets the file to be removed path
							String fileToBeRemovedPath = f
									.getAbsolutePath();

							// Removes the file from the project
							// configuration
							AcideProjectConfiguration.getInstance()
									.removeFileAt(fileIndex);

							File file = new File(fileToBeRemovedPath);
							file.delete();

							// Updates the status message in the status bar
							AcideMainWindow.getInstance().getStatusBar()
									.setStatusMessage(" ");

							// The project has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);

							// Removes the tab in the file editor
							AcideMainWindow.getInstance()
									.getFileEditorManager().getTabbedPane()
									.remove(selectedFileEditorPanelIndex);

							return;
						}
					}
				} else {

					// Not belongs to the project
					String filePath = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath();

					// Deletes the file
					File file = new File(filePath);
					file.delete();

					// Closes the tab
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.remove(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanelIndex());

					// Updates the status message in the status bar
					AcideMainWindow.getInstance().getStatusBar()
							.setStatusMessage(" ");
				}

				// If there are more files in the project configuration
				if (AcideProjectConfiguration.getInstance()
						.getNumberOfFilesFromList() > 0) {

					// Enables the remove file menu item in the explorer
					// panel popup menu
					AcideMainWindow.getInstance().getExplorerPanel()
							.getPopupMenu().getRemoveFileMenuItem()
							.setEnabled(true);
					// Enables the delete file menu item in the explorer
					// panel popup menu
					AcideMainWindow.getInstance().getExplorerPanel()
							.getPopupMenu().getDeleteFileMenuItem()
							.setEnabled(true);
				} else {

					// Disables the remove file menu item in the explorer
					// panel popup menu
					AcideMainWindow.getInstance().getExplorerPanel()
							.getPopupMenu().getRemoveFileMenuItem()
							.setEnabled(false);
					// Disables the delete file menu item in the explorer
					// panel popup menu
					AcideMainWindow.getInstance().getExplorerPanel()
							.getPopupMenu().getDeleteFileMenuItem()
							.setEnabled(false);
				}
			}
		}
	}
}
