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
import es.text.ExtensionFilter;
import es.text.AcideTextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.tree.DefaultMutableTreeNode;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import resources.exception.MissedPropertyException;

/**
 * ACIDE -A Configurable IDE project menu save project as menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class SaveProjectAsMenuItemListener implements ActionListener {

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

			// Gets the labels
			ResourceBundle labels = AcideLanguageManager.getInstance()
					.getLabels();

			// Creates the text file which is going to be the project file
			AcideTextFile projectFile = AcideIOFactory.getInstance().buildFile();

			// Selects the project extension
			String[] acideProjectExtension = new String[] { "acidePrj" };
			projectFile.getFileChooser().addChoosableFileFilter(
					new ExtensionFilter(acideProjectExtension, labels
							.getString("s328")));

			String filePath = projectFile.askSavingFileEditorFile();

			// Sets the language configuration
			AcideProjectConfiguration.getInstance().setLanguageConfiguration(
					AcideResourceManager.getInstance().getProperty("language"));

			// Sets the menu configuration
			AcideProjectConfiguration.getInstance().setMenuConfiguration(
					AcideResourceManager.getInstance().getProperty(
							"currentMenuConfiguration"));

			// Sets the tool bar configuration
			AcideProjectConfiguration.getInstance().setToolBarConfiguration(
					AcideResourceManager.getInstance().getProperty(
							"currentToolBarConfiguration"));

			// Sets the file editor configuration
			AcideProjectConfiguration.getInstance().setFileEditorConfiguration(
					AcideResourceManager.getInstance().getProperty(
							"fileEditorConfiguration"));

			// Add the extension if the name does not contain it
			if (!filePath.contains(".acidePrj"))
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
			projectFile.write(AcideProjectConfiguration.getInstance()
					.getProjectPath(), fileContent);

			// Updates the explorer tree
			updateExplorerTree(currentProjectName, newProjectName);

			// Updates the main window title
			MainWindow.getInstance()
					.setTitle(
							labels.getString("s425")
									+ " - "
									+ AcideProjectConfiguration.getInstance()
											.getName());

			// Validates the changes in the main window
			MainWindow.getInstance().validate();

			// Repaint the main window with the new changes
			MainWindow.getInstance().repaint();

			// Is the first time that the project has been saved
			AcideProjectConfiguration.getInstance().setFirstSave(true);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty(
					"defaultAcideProject", filePath);
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
	 * previous node parent in the project configuration file list. If the parent
	 * was the previous project, then now becomes the new project name given as
	 * a parameter.
	 * 
	 * @param currentName old project name to be replaced.
	 * @param newProjectName new project name to be set.
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
		MainWindow.getInstance().getExplorerPanel().getPopupMenu().getAddFile()
				.setEnabled(true);

		// Enables the remove file menu item in the explorer popup menu
		MainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getRemoveFile().setEnabled(true);

		// Builds the EXPLORER TREE with all the associated files
		MainWindow.getInstance().getExplorerPanel().getRoot()
				.removeAllChildren();

		// Creates the new explorer node
		DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
				projectFile);

		// Adds the new node to the explorer tree root
		MainWindow.getInstance().getExplorerPanel().getRoot()
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
					DefaultMutableTreeNode defaultMutableTreeNode1 = MainWindow
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
		MainWindow.getInstance().getExplorerPanel().getTreeModel().reload();

		// Repaint the explorer tree
		MainWindow.getInstance().getExplorerPanel().expandTree();

		// Enables the add file menu item in the popup menu
		MainWindow.getInstance().getExplorerPanel().getPopupMenu().getAddFile()
				.setEnabled(true);

		// Enables the save project menu item in the popup menu
		MainWindow.getInstance().getExplorerPanel().getPopupMenu()
				.getSaveProject().setEnabled(true);

		// If it has more than 0 files associated
		if (AcideProjectConfiguration.getInstance().getNumberOfFilesFromList() > 0)

			// Allows to remove files in the EXPLORER menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFile().setEnabled(true);
		else
			// Removing files in the EXPLORER menu is not allowed
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFile().setEnabled(false);

		// Saves the default configuration
		AcideProjectConfiguration.getInstance().setFirstSave(true);

		try {
			// Updates the project configuration
			AcideProjectConfiguration.getInstance().setPath(
					AcideResourceManager.getInstance().getProperty(
							"defaultAcideProject"));
		} catch (MissedPropertyException exception) {
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
