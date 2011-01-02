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
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE -A Configurable IDE project menu add folder menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class AddFolderMenuItemListener implements ActionListener {

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
		final ResourceBundle labels = language.getLabels();

		// Asks for the folder name to the user
		String newFolder = JOptionPane.showInputDialog(null, labels
				.getString("s656"));

		// If it is a valid folder
		if (newFolder != null && !newFolder.matches("")) {
			
			// Gets the selected node in the explorer tree
			TreePath path = MainWindow.getInstance().getExplorerPanel().getTree()
					.getSelectionPath();

			// Creates the explorer folder
			DefaultMutableTreeNode folderPath;
			AcideProjectFile folder;

			// If folder selected
			if (path != null) {
				
				// Gets the selected node in the explorer tree
				folderPath = (DefaultMutableTreeNode) path
						.getLastPathComponent();
				
				// Transforms the node into a project file
				folder = (AcideProjectFile) folderPath.getUserObject();

				// If it is a file and not a directory
				if (!folder.isDirectory()) {
					folderPath = MainWindow.getInstance().getExplorerPanel().getRoot()
							.getNextNode();
					folder = (AcideProjectFile) folderPath.getUserObject();
				}

			} else {
				
				// File selected 
				
				folderPath = MainWindow.getInstance().getExplorerPanel().getRoot().getNextNode();
				folder = (AcideProjectFile) folderPath.getUserObject();
			}

			// Builds the project file
			AcideProjectFile projectFile = new AcideProjectFile();
			projectFile.setAbsolutePath(newFolder);
			projectFile.setName(newFolder);
			projectFile.setParent(folder.getName());
			projectFile.setIsDirectory(true);
			
			// Adds the folder to the configuration
			AcideProjectConfiguration.getInstance().addFile(projectFile);
			
			// Updates the explorer tree with the new folder
			DefaultMutableTreeNode node = new DefaultMutableTreeNode(
					projectFile);
			node.setAllowsChildren(true);
			folderPath.add(node);
			folderPath.setAllowsChildren(true);
			MainWindow.getInstance().getExplorerPanel().getTreeModel().reload();
			MainWindow.getInstance().getExplorerPanel().expandTree();
			
			// The project configuration has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);
		}
	}
}

