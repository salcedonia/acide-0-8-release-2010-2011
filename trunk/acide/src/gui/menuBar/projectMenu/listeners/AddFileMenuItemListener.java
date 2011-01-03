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

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import es.project.AcideProjectFileType;
import es.text.TextFile;
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

		// Builds the text file
		TextFile textFile = AcideIOFactory.getInstance().buildFile();

		try {

			// Builds the file content
			String fileContent = "";
			fileContent = textFile.read();

			// If the file content is not empty
			if (fileContent != null) {

				// Gets the selection path from the explorer panel tree
				TreePath path = MainWindow.getInstance().getExplorerPanel()
						.getTree().getSelectionPath();
				DefaultMutableTreeNode filePath;
				AcideProjectFile projectFile;

				if (path != null) {
					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					projectFile = (AcideProjectFile) filePath.getUserObject();

					if (!projectFile.isDirectory()) {
						filePath = MainWindow.getInstance().getExplorerPanel()
								.getRoot().getNextNode();
						projectFile = (AcideProjectFile) filePath.getUserObject();
					}

				} else {
					filePath = MainWindow.getInstance().getExplorerPanel()
							.getRoot().getNextNode();
					projectFile = (AcideProjectFile) filePath.getUserObject();
				}

				// Gets the file name
				String name = "";
				int lastIndexOfSlash = fileContent.lastIndexOf("\\");
				if (lastIndexOfSlash == -1)
					lastIndexOfSlash = fileContent.lastIndexOf("/");
				name = fileContent.substring(lastIndexOfSlash + 1, fileContent.length());

				// Builds the project file
				projectFile = new AcideProjectFile();
				projectFile.setAbsolutePath(fileContent);
				projectFile.setName(name);
				projectFile.setParent(projectFile.getName());

				// Checks if it is added already to the project
				boolean isAdded = false;
				for (int index = 0; index < AcideProjectConfiguration.getInstance().getNumFilesFromList(); index++) {
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index).getAbsolutePath()
							.equals(projectFile.getAbsolutePath())) {
						isAdded = true;
					}
				}

				// If it is not added
				if (!isAdded) {

					// Adds the file to the project file list
					AcideProjectConfiguration.getInstance()
							.addFile(projectFile);
					
					// Sets the file as opened
					AcideProjectConfiguration.getInstance()
							.getFileAt(
									AcideProjectConfiguration.getInstance()
											.getNumFilesFromList() - 1)
							.setIsOpened(true);

					// Creates the node in the explorer panel tree
					DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
							projectFile);
					defaultMutableTreeNode.setAllowsChildren(false);
					filePath.add(defaultMutableTreeNode);
					
					// Validates the MAIN WINDOW
					MainWindow.getInstance().validate();
					
					// Repaints the MAIN WINDOW
					MainWindow.getInstance().repaint();
					
					// Updates the tree model
					MainWindow.getInstance().getExplorerPanel().getTreeModel()
							.reload();
					
					// Expands the explorer panel tree
					MainWindow.getInstance().getExplorerPanel().expandTree();
					
					// Enables the remove file menu item in the explorer panel popup menu
					MainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getRemoveFile().setEnabled(true);
					
					// Enables the delete file menu item in the explorer panel popup menu
					MainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getDeleteFile().setEnabled(true);
					
					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);
				}

				// Checks if the file is opened in the editor
				boolean isOpened = false;
				for (int index = 0; index < MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels(); index++) {
					if (MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).getAbsolutePath()
							.equals(projectFile.getAbsolutePath())) {
						isOpened = true;
					}
				}

				// If it is not opened in the editor
				if (!isOpened) {

					// Creates a new NORMAL file
					TextFile textFile1 = new TextFile();
					AcideProjectFileType type = AcideProjectFileType.NORMAL;

					// Updates the status bar
					MainWindow.getInstance().getStatusBar()
							.setStatusMessage(projectFile.getAbsolutePath());

					// Opens a new tab in the editor
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.newTab(projectFile.getAbsolutePath(),
									projectFile.getAbsolutePath(),
									textFile1.load(projectFile.getAbsolutePath()), true, type, 0);

					// Enables the file menu
					MainWindow.getInstance().getMenu().enableFileMenu();

					// Enables the edit menu
					MainWindow.getInstance().getMenu().enableEditMenu();
					
					// Updates the undo manager
					AcideUndoRedoManager.getInstance().update();
					
					// Sets the caret in the first position of the selected file
					// editor panel
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().setCaretPosition(0);
				}
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}