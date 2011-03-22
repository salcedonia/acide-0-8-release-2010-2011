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
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.tree.DefaultMutableTreeNode;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.resources.exception.MissedPropertyException;

/**
 * ACIDE -A Configurable IDE project menu save project as menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveProjectAsMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			// Selects the project extension
			String[] acideProjectExtension = new String[] { "acidePrj" };
			AcideFileManager.getInstance().getFileChooser().addChoosableFileFilter(
					new AcideFileExtensionFilterManager(acideProjectExtension,
							AcideLanguageManager.getInstance().getLabels()
									.getString("s328")));

			// Asks for the file to the user
			String filePath = AcideFileManager.getInstance().askSavingFileEditorFile(false);

			// Sets the ACIDE - A Configurable IDE language configuration
			AcideProjectConfiguration.getInstance().setLanguageConfiguration(
					AcideResourceManager.getInstance().getProperty("language"));

			// Sets the ACIDE - A Configurable current menu configuration
			AcideProjectConfiguration.getInstance().setMenuConfiguration(
					AcideResourceManager.getInstance().getProperty(
							"currentMenuConfiguration"));

			// Sets the ACIDE - A Configurable IDE current tool bar configuration
			AcideProjectConfiguration.getInstance().setToolBarConfiguration(
					AcideResourceManager.getInstance().getProperty(
							"currentToolBarConfiguration"));

			// Sets the ACIDE - Configurable IDE file editor configuration
			AcideProjectConfiguration.getInstance().setFileEditorConfiguration(
					AcideResourceManager.getInstance().getProperty(
							"fileEditorConfiguration"));

			// Add the extension if the name does not contain it
			if (!filePath.contains(".acidePrj"))
				
				// Adds it
				filePath = filePath + ".acidePrj";

			// Gets the new project name
			int lastIndexOfSlash = filePath.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = filePath.lastIndexOf("/");
			String newProjectName = filePath.substring(lastIndexOfSlash + 1,
					filePath.lastIndexOf("."));

			// Gets the current project name
			String currentProjectName = AcideProjectConfiguration.getInstance()
					.getName();

			// Sets the name
			AcideProjectConfiguration.getInstance().setName(newProjectName);

			// Sets the path
			AcideProjectConfiguration.getInstance().setPath(filePath);

			// Saves the file
			String fileContent = AcideProjectConfiguration.getInstance().save();
			AcideFileManager.getInstance().write(AcideProjectConfiguration.getInstance()
					.getProjectPath(), fileContent);

			// Updates the explorer tree
			updateExplorerTree(currentProjectName, newProjectName);

			// Updates the main window title
			AcideMainWindow.getInstance()
					.setTitle(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s425")
									+ " - "
									+ AcideProjectConfiguration.getInstance()
											.getName());

			// Validates the changes in the main window
			AcideMainWindow.getInstance().validate();

			// Repaint the main window with the new changes
			AcideMainWindow.getInstance().repaint();

			// Is the first time that the project has been saved
			AcideProjectConfiguration.getInstance().setFirstSave(true);

			// Updates the ACIDE - A Configurable IDE project configuration
			AcideResourceManager.getInstance().setProperty(
					"projectConfiguration", filePath);
			
			// Updates the ACIDE - A Configurable IDE current default path
			AcideResourceManager.getInstance().setProperty("defaultPath",
					filePath);

			// The project has not been modified yet
			AcideProjectConfiguration.getInstance().setIsModified(false);

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Updates the explorer tree with the new project name.
	 * 
	 * First of all, it is mandatory to update all the references to the
	 * previous node parent in the project configuration file list. If the
	 * parent was the previous project, then now becomes the new project name
	 * given as a parameter.
	 * 
	 * @param currentName
	 *            old project name to be replaced.
	 * @param newProjectName
	 *            new project name to be set.
	 */
	private void updateExplorerTree(String currentName, String newProjectName) {

		// Updates the parent files in the project configuration
		for (int index = 0; index < AcideProjectConfiguration.getInstance()
				.getNumberOfFilesFromList(); index++)
			if (AcideProjectConfiguration.getInstance().getFileAt(index)
					.getParent().matches(currentName))
				AcideProjectConfiguration.getInstance().getFileAt(index)
						.setParent(newProjectName);

		// Changes the root node in the explorer tree
		rebuildExplorerTree(newProjectName);
	}

	/**
	 * Rebuilds the explorer tree.
	 * 
	 * @param newProjectName
	 *            new project name.
	 */
	private void rebuildExplorerTree(String newProjectName) {

		// Creates the new project file
		AcideProjectFile projectFile = new AcideProjectFile();
		projectFile.setAbsolutePath(newProjectName);
		projectFile.setName(newProjectName);
		projectFile.setParent(null);
		projectFile.setIsDirectory(true);

		// Enables add file menu item in the explorer popup menu
		AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu().getAddFileMenuItem()
				.setEnabled(true);

		// Enables the remove file menu item in the explorer popup menu
		AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getRemoveFileMenuItem().setEnabled(true);

		// Builds the EXPLORER TREE with all the associated files
		AcideMainWindow.getInstance().getExplorerPanel().getRoot()
				.removeAllChildren();

		// Creates the new explorer node
		DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
				projectFile);

		// Adds the new node to the explorer tree root
		AcideMainWindow.getInstance().getExplorerPanel().getRoot()
				.add(defaultMutableTreeNode);

		// Creates the directory list
		ArrayList<DefaultMutableTreeNode> directoryList = new ArrayList<DefaultMutableTreeNode>();

		for (int index = 0; index < AcideProjectConfiguration.getInstance()
				.getNumberOfFilesFromList(); index++) {

			// Gets the node
			DefaultMutableTreeNode node = new DefaultMutableTreeNode(
					AcideProjectConfiguration.getInstance().getFileAt(index));

			// Checks if the file really exists
			File file = new File(AcideProjectConfiguration.getInstance()
					.getFileAt(index).getAbsolutePath());

			// If exists
			if (file.exists()) {

				// Directory?
				if (AcideProjectConfiguration.getInstance().getFileAt(index)
						.isDirectory()) {

					// Allows children in the tree
					node.setAllowsChildren(true);

					// Adds the node
					directoryList.add(node);
				} else
					// No children are allowed
					node.setAllowsChildren(false);

				// If the file already exists in the level above
				if (AcideProjectConfiguration
						.getInstance()
						.getFileAt(index)
						.getParent()
						.equals(AcideProjectConfiguration.getInstance()
								.getName())) {

					// Adds the new node
					defaultMutableTreeNode.add(node);
				} else {

					// Searches for the node
					DefaultMutableTreeNode defaultMutableTreeNode1 = AcideMainWindow
							.getInstance()
							.getExplorerPanel()
							.searchDirectoryList(
									directoryList,
									AcideProjectConfiguration.getInstance()
											.getFileAt(index).getParent());

					// Adds the new node
					defaultMutableTreeNode1.add(node);
				}
			}
		}

		// Updates the explorer tree
		AcideMainWindow.getInstance().getExplorerPanel().getTreeModel().reload();

		// Repaint the explorer tree
		AcideMainWindow.getInstance().getExplorerPanel().expandTree();

		// Enables the add file menu item in the popup menu
		AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu().getAddFileMenuItem()
				.setEnabled(true);

		// Enables the save project menu item in the popup menu
		AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getSaveProjectMenuItem().setEnabled(true);

		// If it has more than 0 files associated
		if (AcideProjectConfiguration.getInstance().getNumberOfFilesFromList() > 0)

			// Allows to remove files in the EXPLORER menu
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFileMenuItem().setEnabled(true);
		else
			// Removing files in the EXPLORER menu is not allowed
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFileMenuItem().setEnabled(false);

		// Saves the default configuration
		AcideProjectConfiguration.getInstance().setFirstSave(true);

		try {
			// Updates the ACIDE - A Configurable IDE project configuration
			AcideProjectConfiguration.getInstance().setPath(
					AcideResourceManager.getInstance().getProperty(
							"projectConfiguration"));
		} catch (MissedPropertyException exception) {
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
