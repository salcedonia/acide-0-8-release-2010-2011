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
import java.util.Enumeration;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import acide.language.AcideLanguageManager;

/**
 * ACIDE -A Configurable IDE project menu add folder menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideAddFolderMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Asks for the folder name to the user
		String newFolderPath = JOptionPane.showInputDialog(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s656"));

		// If it is a valid folder
		if (newFolderPath != null && !newFolderPath.matches("")) {

			// Gets the selected explorer node tree path
			TreePath currentSelection = AcideMainWindow.getInstance()
					.getExplorerPanel().getTree().getSelectionPath();

			// Creates the explorer folder
			DefaultMutableTreeNode currentNode;
			AcideProjectFile currentFolder;

			// If a node is selected
			if (currentSelection != null) {

				// Gets the selected node last path component in the explorer
				// tree
				currentNode = (DefaultMutableTreeNode) currentSelection
						.getLastPathComponent();

				// Transforms the node into a project file
				currentFolder = (AcideProjectFile) currentNode
						.getUserObject();

				// If it is a file and not a directory
				if (!currentFolder.isDirectory()) {

					// Gets the root node
					currentNode = AcideMainWindow.getInstance()
							.getExplorerPanel().getRoot().getNextNode();

					// Transforms the node into a project file
					currentFolder = (AcideProjectFile) currentNode
							.getUserObject();
				}

			} else {

				// Gets the root node
				currentNode = AcideMainWindow.getInstance()
						.getExplorerPanel().getRoot().getNextNode();

				// Transforms the node into a project file
				currentFolder = (AcideProjectFile) currentNode
						.getUserObject();
			}

			// Checks if the folder already exists in the same level at the
			// explorer tree
			boolean exists = false;

			for (@SuppressWarnings("unchecked")
			Enumeration<DefaultMutableTreeNode> children = currentNode
					.children(); children.hasMoreElements();) {

				// Gets the next child
				DefaultMutableTreeNode node = children.nextElement();
				
				// Transforms into a project file
				AcideProjectFile file = (AcideProjectFile) node.getUserObject();

				// Found it
				if (file.getAbsolutePath().matches(newFolderPath))
					exists = true;
			}

			if (!exists) {
				// Builds the new folder to be added
				AcideProjectFile newFolder = new AcideProjectFile();

				// Sets the absolute path
				newFolder.setAbsolutePath(newFolderPath);

				// Sets the name
				newFolder.setName(newFolderPath);

				// Sets the parent
				newFolder.setParent(currentFolder.getName());

				// Sets is directory
				newFolder.setIsDirectory(true);

				// Adds the folder to the configuration
				AcideProjectConfiguration.getInstance().addFile(newFolder);

				// Updates the explorer tree with the new folder
				DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
						newFolder);

				// Children are allowed
				newNode.setAllowsChildren(true);

				// Adds the node
				currentNode.add(newNode);

				// Children are allowed
				currentNode.setAllowsChildren(true);

				// Updates the explorer tree model
				AcideMainWindow.getInstance().getExplorerPanel().getTreeModel()
						.reload();

				// Validates the changes in the explorer tree
				AcideMainWindow.getInstance().getExplorerPanel().expandTree();

				// The project configuration has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
				
			} else {

				// Warning message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s1019"),
						"Warning", JOptionPane.WARNING_MESSAGE);
			}
		}
	}
}
