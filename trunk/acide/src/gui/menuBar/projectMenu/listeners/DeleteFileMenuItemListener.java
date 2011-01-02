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
package gui.menuBar.projectMenu.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE -A Configurable IDE project menu delete file menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class DeleteFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		// Are you sure?
		int chosenOption = JOptionPane.showConfirmDialog(null, labels
				.getString("s951"));
		
		// If yes
		if (chosenOption == JOptionPane.OK_OPTION) {

			// Gets the selection in the explorer tree
			TreePath currentSelection = MainWindow.getInstance().getExplorerPanel().getTree()
					.getSelectionPath();

			// If there is something selected
			if (currentSelection != null) {
				
				// Gets the selected node in the explorer tree
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());

				// Transforms it into a project file
				AcideProjectFile projectFile = (AcideProjectFile) currentNode.getUserObject();
				
				// If it is not a directory but a file
				if (!projectFile.isDirectory()) {
					
					// Gets the parent
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					
					// If it has parent
					if (parent != null) {
						
						// Removes its parent
						MainWindow.getInstance().getExplorerPanel().getTreeModel()
								.removeNodeFromParent(currentNode);
						
						// Searches for the file into the project configuration
						int posExplorer = -1;
						for (int position = 0; position < AcideProjectConfiguration.getInstance()
								.getNumFilesFromList(); position++) {
							
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(position).getAbsolutePath().equals(
											projectFile.getAbsolutePath())) {
								posExplorer = position;
							}
						}

						// Gets the file from the project configuration file list
						AcideProjectFile configurationFile = AcideProjectConfiguration.getInstance().getFileAt(posExplorer);
						
						String fileRemove = configurationFile.getAbsolutePath();
						
						// Removes the file from the project configuration
						AcideProjectConfiguration.getInstance().removeFileAt(
								posExplorer);

						// Deletes this file
						File physicalFile = new File(fileRemove);
						physicalFile.delete();

						// Updates the status bar
						MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
						
						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

						return;
					}
				}
			}

			// If there are more files in the project
			if (AcideProjectConfiguration.getInstance().getNumFilesFromList() > 0) {
				
				// Enables the remove file menu item in the explorer panel popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu().getRemoveFile()
						.setEnabled(true);
				
				// Enables the delete file menu item in the explorer panel popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu().getDeleteFile()
						.setEnabled(true);
			} else {
				
				// Disables the remove file menu item in the explorer panel popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu().getRemoveFile()
						.setEnabled(false);
				
				// Disables the delete file menu item in the explorer panel popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu().getDeleteFile()
						.setEnabled(false);
			}
		}
	}
}
