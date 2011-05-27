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
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.files.project.AcideProjectFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE project menu add file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideAddFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		File[] selectedFiles = null;
		String lastPath = null;

		// Creates the file chooser
		JFileChooser _fileChooser = new JFileChooser();

		try {

			// Gets the ACIDE - A Configurable IDE last opened file
			// directory
			lastPath = AcideResourceManager.getInstance().getProperty(
					"lastOpenedFileDirectory");

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(new File(lastPath));

			// Enables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(true);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask for the file to the user
			int returnValue = _fileChooser.showOpenDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected files
				selectedFiles = _fileChooser.getSelectedFiles();

				// Gets the selection path from the explorer panel tree
				TreePath currentSelection = AcideMainWindow.getInstance()
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

						// Gets the parent node
						currentNode = (DefaultMutableTreeNode) ((MutableTreeNode) (currentNode
								.getParent()));

						// Transforms the selected node into a project file
						currentFile = (AcideProjectFile) currentNode
								.getUserObject();
					}

				} else {

					// Gets the info from the project root folder
					currentNode = AcideMainWindow.getInstance()
							.getExplorerPanel().getRoot().getNextNode();

					// Gets the info from the next node for the project file
					currentFile = (AcideProjectFile) currentNode
							.getUserObject();
				}

				// Adds the files to the project
				for (int index = 0; index < selectedFiles.length; index++) {

					// Gets the absolute path of the current checked file
					String absolutePath = selectedFiles[index]
							.getAbsolutePath();

					// Gets the file name
					String fileName = "";
					int lastIndexOfSlash = absolutePath.lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = absolutePath.lastIndexOf("/");
					fileName = absolutePath.substring(lastIndexOfSlash + 1,
							absolutePath.length());

					// Builds the new project file
					AcideProjectFile newProjectFile = new AcideProjectFile();

					// Set the path
					newProjectFile.setAbsolutePath(absolutePath);

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

					// Adds the file to the recent files list
					AcideWorkbenchConfiguration.getInstance()
							.getRecentFilesConfiguration()
							.addRecentFileToList(absolutePath);
				}

				// Stores the selected absolute paths in the string array
				for (int index = 0; index < selectedFiles.length; index++) {

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s300")
									+ selectedFiles[index].getAbsolutePath());

					// Updates the ACIDE - A Configurable IDE last opened
					// file
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedFileDirectory",
							selectedFiles[index].getAbsolutePath());
				}

			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancels the selection
				_fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s302"));
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
		int fileEditorPanelIndex = -1;
		for (int index = 0; index < AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

			// If it is the wanted file
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).getAbsolutePath()
					.equals(projectFile.getAbsolutePath())) {

				// Found it
				fileEditorPanelIndex = index;
			}
		}

		// If it is not opened in the editor
		if (fileEditorPanelIndex == -1) {

			// Gets the file content
			String fileContent = null;
			fileContent = AcideFileManager.getInstance().load(
					projectFile.getAbsolutePath());

			// If it is not empty
			if (fileContent != null) {

				// Creates a new NORMAL file
				AcideProjectFileType projectFileType = AcideProjectFileType.NORMAL;

				// Updates the status message in the status bar
				AcideMainWindow.getInstance().getStatusBar()
						.setStatusMessage(projectFile.getAbsolutePath());

				// Gets the predefined lexicon configuration
				AcideLexiconConfiguration lexiconConfiguration = AcideWorkbenchConfiguration
						.getInstance()
						.getLexiconAssignerConfiguration()
						.getPredifinedLexiconConfiguration(
								projectFile.getAbsolutePath());

				// Creates the current grammar configuration
				AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

				// Sets the current grammar configuration path
				currentGrammarConfiguration
						.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

				// Creates the previous grammar configuration
				AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

				// Sets the previous grammar configuration path
				previousGrammarConfiguration
						.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

				// Updates the tabbed pane in the file editor manager
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.updateTabbedPane(projectFile.getAbsolutePath(),
								fileContent, true, projectFileType, 0, 0, 1,
								lexiconConfiguration,
								currentGrammarConfiguration,
								previousGrammarConfiguration);
			}
		} else {

			// Updates the selected file editor index
			AcideMainWindow.getInstance().getFileEditorManager()
					.updateRelatedComponentsAt(fileEditorPanelIndex);
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

			// Updates the tree model
			AcideMainWindow.getInstance().getExplorerPanel().getTreeModel()
					.reload();

			// Expands the explorer panel tree
			AcideMainWindow.getInstance().getExplorerPanel().expandTree();

			// Enables the remove file menu item in the explorer panel popup
			// menu
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFileMenuItem().setEnabled(true);

			// Enables the delete file menu item in the explorer panel popup
			// menu
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFileMenuItem().setEnabled(true);

			// The project has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);
		}
	}
}