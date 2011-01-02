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
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE -A Configurable IDE project menu remove file menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class RemoveFileMenuItemListener implements ActionListener {

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
				.getString("s623"));

		// If yes
		if (chosenOption == JOptionPane.OK_OPTION) {

			// Gets the selection over the explorer tree
			TreePath currentSelection = MainWindow.getInstance().getExplorerPanel().getTree()
					.getSelectionPath();

			// If something has been selected
			if (currentSelection != null) {
				
				// Gets the select node in the explorer tree
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());

				// Transform the node into a project file
				AcideProjectFile projectFile = (AcideProjectFile) currentNode.getUserObject();
				
				// If it is a file and not a directory
				if (!projectFile.isDirectory()) {
					
					// Gets the node parent
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					
					// If it has parent
					if (parent != null) {
						
						// Removes the node
						MainWindow.getInstance().getExplorerPanel().getTreeModel()
								.removeNodeFromParent(currentNode);
						
						// Searches for the file in the explorer
						int posFile = -1;
						for (int position = 0; position < AcideProjectConfiguration.getInstance()
								.getNumFilesFromList(); position++) {
							
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(position).getAbsolutePath().equals(
											projectFile.getAbsolutePath())) {
								
								posFile = position;
							}
						}

						// Removes the file from the project configuration
						AcideProjectConfiguration.getInstance().removeFileAt(
								posFile);
						
						// Updates the status bar
						MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
						
						// Searches for the file in the editor
						int posEditor = -1;
						for (int position = 0; position < MainWindow.getInstance().getFileEditorManager()
								.getNumFileEditorPanels(); position++) {
							if (MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(position).getAbsolutePath()
									.equals(projectFile.getAbsolutePath()))
								posEditor = position;
						}
						
						// If it exists
						if (posEditor != -1) {

							// Is the file modified?
							if (MainWindow.getInstance().getFileEditorManager().isRedButton(
									posEditor)) {

								// Do you want to save it?
								chosenOption = JOptionPane.showConfirmDialog(
										null, labels.getString("s643"),
										labels.getString("s953"),
										JOptionPane.YES_NO_OPTION);

								// If yes
								if (chosenOption == JOptionPane.OK_OPTION) {
	
									// Creates the external file
									TextFile textFile = AcideIOFactory
									.getInstance().buildFile();

									// Saves the file
									boolean result = textFile.save(MainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(posEditor)
											.getAbsolutePath(), MainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(posEditor).getTextEditionAreaContent());
									
									// If it could save it
									if (result) {
										
										// Sets the green button
										MainWindow.getInstance()
												.getFileEditorManager()
												.setGreenButtonAt(posEditor);
									}
								}
							}
						}

						// Closes the editor tab
						MainWindow.getInstance().getFileEditorManager().getTabbedPane().remove(
								posEditor);

						// If there are no more opened tabs
						if (MainWindow.getInstance().getFileEditorManager().getTabbedPane()
								.getTabCount() == 0) {
							
							// Disables the FILE and EDIT menu
							MainWindow.getInstance().getMenu().disableFileMenu();
							MainWindow.getInstance().getMenu().disableEditMenu();
						}
					}

					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);
					return;
				}
			}
		}

		// If there are more opened files 
		if (AcideProjectConfiguration.getInstance().getNumFilesFromList() > 0) {
			
			// Updates the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getRemoveFile()
					.setEnabled(true);
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getDeleteFile()
					.setEnabled(true);
		} else {
			
			// Updates the explorer popup menu
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getRemoveFile()
					.setEnabled(false);
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getDeleteFile()
					.setEnabled(false);
		}
	}
}
