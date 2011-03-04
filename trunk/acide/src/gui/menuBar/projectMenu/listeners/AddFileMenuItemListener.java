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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;

import operations.log.AcideLog;
import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import es.project.AcideProjectFileType;
import es.text.AcideFileExtensionFilterManager;
import es.text.AcideFileManager;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

/**
 * ACIDE - A Configurable IDE project menu add file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AddFileMenuItemListener implements ActionListener {

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

			// Adds the extension for any type of files
			String[] askExtension = new String[] { "*" };
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.addChoosableFileFilter(
							new AcideFileExtensionFilterManager(
									askExtension, AcideLanguageManager
											.getInstance().getLabels()
											.getString("s1036")));
			
			// Ask to the user for the file
			String[] filePaths = AcideFileManager.getInstance().askMultipleAbsolutePath();

			// If the user selected any
			if (filePaths != null) {

				// Gets the selection path from the explorer panel tree
				TreePath currentSelection = MainWindow.getInstance()
						.getExplorerPanel().getTree().getSelectionPath();

				// Creates the explorer node
				DefaultMutableTreeNode currentNode;

				// Creates the selected project file
				AcideProjectFile currentFile;

				if (currentSelection != null) {

					// Gets the selected node last path component
					currentNode = (DefaultMutableTreeNode) currentSelection
							.getLastPathComponent();

					// Transforms the selected node into a project file
					currentFile = (AcideProjectFile) currentNode
							.getUserObject();

					// If it is not a directory
					if (!currentFile.isDirectory()) {

						// Gets the selected node last path component
						currentNode = MainWindow.getInstance()
								.getExplorerPanel().getRoot().getNextNode();

						// Transforms the selected node into a project file
						currentFile = (AcideProjectFile) currentNode
								.getUserObject();
					}

				} else {

					// Gets the info from the next node
					currentNode = MainWindow.getInstance().getExplorerPanel()
							.getRoot().getNextNode();

					// Gets the info from the next node for the project file
					currentFile = (AcideProjectFile) currentNode
							.getUserObject();
				}

				// Adds the files to the project
				for (int index = 0; index < filePaths.length; index++) {
					
					// Gets the file name
					String fileName = "";
					int lastIndexOfSlash = filePaths[index].lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = filePaths[index].lastIndexOf("/");	
					fileName = filePaths[index].substring(lastIndexOfSlash + 1,
							filePaths[index].length());

					// Builds the new project file
					AcideProjectFile newProjectFile = new AcideProjectFile();

					// Set the path
					newProjectFile.setAbsolutePath(filePaths[index]);

					// Sets the name
					newProjectFile.setName(fileName);

					// Sets the parent
					newProjectFile.setParent(currentFile.getName());

					// Sets is directory
					newProjectFile.setIsDirectory(false);

					// Adds it to the project configuration
					addToExplorerTree(currentNode, newProjectFile);

					// Adds it to the file editor
					addToFileEditor(newProjectFile);
				}
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Checks if the new file to be added is already opened in the file editor.
	 * 
	 * If it was not added, creates the tab with the content on it.
	 * 
	 * @param projectFile
	 *            file to be added.
	 */
	public void addToFileEditor(AcideProjectFile projectFile) {

		// Checks if the file is opened in the editor
		boolean isOpened = false;
		for (int index = 0; index < MainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

			// If it is the wanted file
			if (MainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).getAbsolutePath()
					.equals(projectFile.getAbsolutePath())) {

				// Found it
				isOpened = true;
			}
		}

		// If it is not opened in the editor
		if (!isOpened) {

			// Creates the file manager
			AcideFileManager fileManager = new AcideFileManager();
			
			// Creates a new NORMAL file
			AcideProjectFileType projectFileType = AcideProjectFileType.NORMAL;

			// Updates the status message in the status bar
			MainWindow.getInstance().getStatusBar()
					.setStatusMessage(projectFile.getAbsolutePath());

			// Updates the tabbed pane in the file editor manager
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.updatesTabbedPane(projectFile.getAbsolutePath(),
							fileManager.load(projectFile.getAbsolutePath()), true,
							projectFileType, 0, 0);

			// Enables the file menu
			MainWindow.getInstance().getMenu().enableFileMenu();

			// Enables the edit menu
			MainWindow.getInstance().getMenu().enableEditMenu();

			// Updates the undo manager
			AcideUndoRedoManager.getInstance().update();

			SwingUtilities.invokeLater(new Runnable() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see java.lang.Runnable#run()
				 */
				@Override
				public void run() {

					// Sets the focus on the text area
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(
									MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanelIndex())
							.getActiveTextEditionArea().requestFocusInWindow();
				}
			});
		}
	}

	/**
	 * Checks if the file has been added to the project configuration already.
	 * 
	 * If it was not added, adds the node to the explorer tree.
	 * 
	 * @param currentNode
	 *            selected explorer tree node.
	 * @param projectFile
	 *            file to be added.
	 */
	public void addToExplorerTree(DefaultMutableTreeNode currentNode,
			AcideProjectFile projectFile) {

		// Checks if it is added already to the project
		boolean isAdded = false;
		for (int index = 0; index < AcideProjectConfiguration.getInstance()
				.getNumberOfFilesFromList(); index++) {

			// If it is the wanted file
			if (AcideProjectConfiguration.getInstance().getFileAt(index)
					.getAbsolutePath().equals(projectFile.getAbsolutePath())) {

				// Found it
				isAdded = true;
			}
		}

		// If it is not added
		if (!isAdded) {

			// Adds the file to the project file list
			AcideProjectConfiguration.getInstance().addFile(projectFile);

			// Sets the file as opened in the project configuration
			AcideProjectConfiguration
					.getInstance()
					.getFileAt(
							AcideProjectConfiguration.getInstance()
									.getNumberOfFilesFromList() - 1)
					.setIsOpened(true);

			// Sets the file as opened in the node
			projectFile.setIsOpened(true);

			// Creates the node in the explorer panel tree
			DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
					projectFile);

			// No children are allowed
			newNode.setAllowsChildren(false);

			// Adds the file
			currentNode.add(newNode);

			// Validates the main window
			MainWindow.getInstance().validate();

			// Repaints the main window
			MainWindow.getInstance().repaint();

			// Updates the tree model
			MainWindow.getInstance().getExplorerPanel().getTreeModel().reload();

			// Expands the explorer panel tree
			MainWindow.getInstance().getExplorerPanel().expandTree();

			// Enables the remove file menu item in the explorer panel popup
			// menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFile().setEnabled(true);

			// Enables the delete file menu item in the explorer panel popup
			// menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFile().setEnabled(true);

			// The project has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);
		}
	}
}