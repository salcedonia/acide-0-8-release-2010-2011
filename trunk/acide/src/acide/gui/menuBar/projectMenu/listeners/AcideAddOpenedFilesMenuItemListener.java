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

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE project menu add opened files menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideAddOpenedFilesMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			for (int index1 = 0; index1 < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index1++) {
				
				// Gets the current file absolute path
				String filePath = "";
				filePath = AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								index1)
						.getAbsolutePath();

				if (filePath != null) {

					// Gets the current selection
					TreePath currentSelection = AcideMainWindow.getInstance()
							.getExplorerPanel().getTree().getSelectionPath();

					// Gets the current node
					DefaultMutableTreeNode currentNode;

					// Gets the current project file
					AcideProjectFile currentProjectFile;

					// If there is something selected
					if (currentSelection != null) {

						// Gets the current node
						currentNode = (DefaultMutableTreeNode) currentSelection
								.getLastPathComponent();

						// Gets the current project file from the current node
						// info
						currentProjectFile = (AcideProjectFile) currentNode
								.getUserObject();

						// If a file is selected
						if (!currentProjectFile.isDirectory()) {

							// The current node is the node root
							currentNode = AcideMainWindow.getInstance()
									.getExplorerPanel().getRoot().getNextNode();

							// Gets the current project file from the current
							// node info
							currentProjectFile = (AcideProjectFile) currentNode
									.getUserObject();
						}

					} else {

						// The current node is the node root
						currentNode = AcideMainWindow.getInstance()
								.getExplorerPanel().getRoot().getNextNode();

						// Gets the current project file from the current node
						// info
						currentProjectFile = (AcideProjectFile) currentNode
								.getUserObject();
					}

					// Gets the file name
					String fileName = "";
					int lastIndexOfSlash = filePath.lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = filePath.lastIndexOf("/");
					fileName = filePath.substring(lastIndexOfSlash + 1,
							filePath.length());

					// Creates and configures the new project file
					AcideProjectFile newProjectFile = new AcideProjectFile();

					// Sets its absolute path
					newProjectFile.setAbsolutePath(filePath);

					// Sets its name
					newProjectFile.setName(fileName);

					// Checks if it is already added to the project
					boolean isAdded = false;
					for (int index2 = 0; index2 < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index2++) {
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(index2).getAbsolutePath()
								.equals(newProjectFile.getAbsolutePath())) {
							isAdded = true;
						}
					}

					// If is not
					if (!isAdded) {

						// Puts the file as the root folder child
						newProjectFile.setParent(AcideProjectConfiguration
								.getInstance().getName());

						// Adds the file to the project configuration
						AcideProjectConfiguration.getInstance().addFile(
								newProjectFile);

						// Sets the file as opened
						AcideProjectConfiguration
								.getInstance()
								.getFileAt(
										AcideProjectConfiguration.getInstance()
												.getNumberOfFilesFromList() - 1)
								.setIsOpened(true);

						// Creates the node to be added to the explorer tree
						DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
								newProjectFile);

						// Children are not allowed
						newNode.setAllowsChildren(false);

						// Adds the node to the explorer tree
						currentNode.add(newNode);

						// Updates the explorer tree
						AcideMainWindow.getInstance().getExplorerPanel()
								.getTreeModel().reload();

						// Repaint the explorer tree
						AcideMainWindow.getInstance().getExplorerPanel()
								.expandTree();

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

						// The project configuration has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

						// Updates the focus in the explorer panel, caret, etc.
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.updateRelatedComponentsAt(
										AcideMainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex());
					}
				}
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
