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

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import acide.language.AcideLanguageManager;

/**
 * ACIDE -A Configurable IDE project menu delete file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideDeleteFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Are you sure?
		int returnValue = JOptionPane.showConfirmDialog(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s951"));

		// If yes
		if (returnValue == JOptionPane.OK_OPTION) {

			// Gets the selection in the explorer tree
			TreePath currentSelection = AcideMainWindow.getInstance()
					.getExplorerPanel().getTree().getSelectionPath();

			// If there is something selected
			if (currentSelection != null) {

				// Gets the selected node in the explorer tree
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());

				// Transforms it into a project file
				AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
						.getUserObject();

				// If it is a file
				if (!currentProjectFile.isDirectory()) {

					// Gets the parent
					MutableTreeNode parentNode = (MutableTreeNode) (currentNode
							.getParent());

					// If it has parent
					if (parentNode != null) {

						// Removes it the node from its parent
						AcideMainWindow.getInstance().getExplorerPanel()
								.getTreeModel()
								.removeNodeFromParent(currentNode);

						// Searches for the file into the project configuration
						int fileIndex = -1;
						for (int index = 0; index < AcideProjectConfiguration
								.getInstance().getNumberOfFilesFromList(); index++) {

							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index).getAbsolutePath()
									.equals(currentProjectFile.getAbsolutePath())) {
								
								// Found it
								fileIndex = index;
							}
						}

						// Gets the file from the project configuration file
						// list
						AcideProjectFile configurationFile = AcideProjectConfiguration
								.getInstance().getFileAt(fileIndex);

						// Gets its absolute path
						String absolutePath = configurationFile.getAbsolutePath();

						// Removes the file from the project configuration
						AcideProjectConfiguration.getInstance().removeFileAt(
								fileIndex);

						// Deletes this file
						File physicalFile = new File(absolutePath);
						physicalFile.delete();

						// Updates the status message in the status bar
						AcideMainWindow.getInstance().getStatusBar()
								.setStatusMessage(" ");

						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

						return;
					}
				}
			}

			// If there are more files in the project
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
				
				// Enables the remove file menu item in the explorer panel popup
				// menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getRemoveFileMenuItem().setEnabled(true);

				// Enables the delete file menu item in the explorer panel popup
				// menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getDeleteFileMenuItem().setEnabled(true);
			} else {

				// Disables the remove file menu item in the explorer panel
				// popup menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getRemoveFileMenuItem().setEnabled(false);

				// Disables the delete file menu item in the explorer panel
				// popup menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getDeleteFileMenuItem().setEnabled(false);
			}
		}
	}
}
