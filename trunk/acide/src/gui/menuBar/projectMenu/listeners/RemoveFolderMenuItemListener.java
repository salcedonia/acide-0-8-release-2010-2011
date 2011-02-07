/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
import es.text.AcideTextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;
import resources.AcideResourceManager;

/**																
 * ACIDE -A Configurable IDE project menu remove folder menu item listener.										
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class RemoveFolderMenuItemListener implements ActionListener {

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
		int chosenOption = JOptionPane.showConfirmDialog(null, AcideLanguageManager.getInstance().getLabels()
				.getString("s654"));
		
		// If yes
		if (chosenOption == JOptionPane.OK_OPTION) {

			// The project has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);
			
			// Gets the selection in the explorer
			TreePath currentSelection = MainWindow.getInstance().getExplorerPanel().getTree()
					.getSelectionPath();
			
			// Something selected
			if (currentSelection != null) {
				
				// Gets the selected node in the explorer tree 
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());
				
				// Transforms the node into a project file
				AcideProjectFile currentFolder = (AcideProjectFile) currentNode.getUserObject();
				
				// If it is a directory
				if (currentFolder.isDirectory()) {
					
					// Gets the parent node
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					
					// If it has parent
					if (parent != null) {
						
						// Removes the node from the parent
						MainWindow.getInstance().getExplorerPanel().getTreeModel()
								.removeNodeFromParent(currentNode);
						
						// Creates the removed file list
						ArrayList<String> removedFileList = new ArrayList<String>();
						
						if ((currentNode.getDepth() <= 2)
								&& (currentFolder.getName().equals(AcideProjectConfiguration.getInstance()
										.getName()))) {
							
							// Disables the add file menu item in the explorer panel popup menu 
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getAddFile().setEnabled(false);
							
							// Disables the save project menu item in the explorer panel popup menu 
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getSaveProject().setEnabled(false);
							
							// Disables the remove file menu item in the explorer panel popup menu 
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getRemoveFile().setEnabled(false);
							
							// Disables the delete file menu item in the explorer panel popup menu 
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getDeleteFile().setEnabled(false);
							
							// Sets the title to the default project
							MainWindow.getInstance().setTitle(AcideLanguageManager.getInstance().getLabels().getString("s425")
									+ " - <empty>");
							
							// Creates a new project file by default
							AcideTextFile f = new AcideTextFile();
							f.write("./configuration/file_acidePrj",
									"<EMPTY>");
							
							// Validates the changes in the main window
							MainWindow.getInstance().validate();
							
							// Repaints the main window
							MainWindow.getInstance().repaint();
							
							// Sets the name in the project configuration
							AcideProjectConfiguration.getInstance()
									.setName("");
							
							// Updates the RESOURCE MANAGER
							AcideResourceManager.getInstance().setProperty(
									"defaultAcideProject",
									"./configuration/project/default.acidePrj");
						}

						// Searches for the file in the project configuration
						int fileIndex = -1;
						for (int index = 0; index < AcideProjectConfiguration.getInstance()
								.getNumberOfFilesFromList(); index++) {
							
							if (!currentFolder.getName().equals(
									AcideProjectConfiguration.getInstance()
											.getName())) {
								if (AcideProjectConfiguration.getInstance()
										.getFileAt(index).getName().equals(
												currentFolder.getName())) {
									
									// Found it
									fileIndex = index;

								} else if (AcideProjectConfiguration.getInstance()
										.getFileAt(index).getParent().equals(
												currentFolder.getName())) {
									
									// If it is not a directory
									if (!AcideProjectConfiguration.getInstance()
											.getFileAt(index).isDirectory()) {
										
										// Adds the file to the removed file list
										removedFileList.add(AcideProjectConfiguration.getInstance()
												.getFileAt(index).getAbsolutePath());
										
										// If it is not the last file in the project configuration
										if (AcideProjectConfiguration.getInstance()
												.getNumberOfFilesFromList() != 1)
											
											// Removes it
											AcideProjectConfiguration.getInstance()
													.removeFileAt(index);
										else
											// Removes it
											AcideProjectConfiguration.getInstance()
													.removeFileAt(0);
									} else {
										
										// Gets the folder name
										String folderName = AcideProjectConfiguration.getInstance()
												.getFileAt(index).getName();
										
										for (int index2 = index + 1; index2 < AcideProjectConfiguration.getInstance()
												.getNumberOfFilesFromList(); index2++) {
											if (AcideProjectConfiguration.getInstance()
													.getFileAt(index)
													.getParent()
													.equals(folderName)) {
												
												// Adds the file to the removed file list
												removedFileList
														.add(AcideProjectConfiguration.getInstance()
																.getFileAt(
																		index2)
																.getAbsolutePath());

												// If it is not the last file in the project configuration
												if (AcideProjectConfiguration.getInstance()
														.getNumberOfFilesFromList() != 1)
													
													// Removes it
													AcideProjectConfiguration.getInstance()
															.removeFileAt(index2);
												else
													// Removes it
													AcideProjectConfiguration.getInstance()
															.removeFileAt(0);
											}
										}
									}
								}
							}
						}
						
						// If it exists
						if (fileIndex != -1)
							
							// If it is not the last file in the project 
							if (AcideProjectConfiguration.getInstance()
									.getNumberOfFilesFromList() != 1)
								
								// Removes it from the project configuration
								AcideProjectConfiguration.getInstance()
										.removeFileAt(fileIndex);
							else
								
								// Removes it from the project configuration
								AcideProjectConfiguration.getInstance()
										.removeFileAt(0);
						
						// Are you sure?
						chosenOption = JOptionPane.showConfirmDialog(null, AcideLanguageManager.getInstance().getLabels()
								.getString("s655"));
						
						// If so
						if (chosenOption == JOptionPane.OK_OPTION) {

							// Deletes the file
							for (int index = 0; index < removedFileList.size(); index++) {
								File file = new File(removedFileList.get(index));
								
								// If it is a file
								if (file.isFile())
									
									// Deletes it
									file.delete();
							}

						} else
							// Updates the status message in the status bar
							MainWindow.getInstance().getStatusBar().setStatusMessage(
									AcideLanguageManager.getInstance().getLabels().getString("s107"));
						return;

					}
				}
			}
		}

		// If there are more files in the project
		if (AcideProjectConfiguration.getInstance().getNumberOfFilesFromList() > 0) {
			
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