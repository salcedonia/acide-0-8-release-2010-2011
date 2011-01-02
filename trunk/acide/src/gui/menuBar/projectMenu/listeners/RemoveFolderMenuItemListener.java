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
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;
import operations.log.AcideLog;
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
				
				// Transforms the node into a explorer file
				AcideProjectFile folder = (AcideProjectFile) currentNode.getUserObject();
				
				// If it is a directory
				if (folder.isDirectory()) {
					
					// Gets the node parent
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					
					// If it has parent
					if (parent != null) {
						
						// Removes the node
						MainWindow.getInstance().getExplorerPanel().getTreeModel()
								.removeNodeFromParent(currentNode);
						
						ArrayList<String> contRemove = new ArrayList<String>();
						
						if ((currentNode.getDepth() <= 2)
								&& (folder.getName().equals(AcideProjectConfiguration.getInstance()
										.getName()))) {
							
							// Updates the explorer popup menu 
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getAddFile().setEnabled(false);
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getSaveProject().setEnabled(false);
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getRemoveFile().setEnabled(false);
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getDeleteFile().setEnabled(false);
							
							MainWindow.getInstance().setTitle(labels.getString("s425")
									+ " - <empty>");
							TextFile f = new TextFile();

							f.save("./configuration/file_acidePrj",
									"<EMPTY>");
							MainWindow.getInstance().validate();
							MainWindow.getInstance().repaint();
							AcideProjectConfiguration.getInstance()
									.setName("");
							
							// Updates the RESOURCE MANAGER
							AcideResourceManager.getInstance().setProperty(
									"defaultAcideProject",
									"./configuration/project/default.acidePrj");
						}

						// Searches for the file in the explorer
						int posExplorer = -1;
						for (int position = 0; position < AcideProjectConfiguration.getInstance()
								.getNumFilesFromList(); position++) {
							
							if (!folder.getName().equals(
									AcideProjectConfiguration.getInstance()
											.getName())) {
								if (AcideProjectConfiguration.getInstance()
										.getFileAt(position).getName().equals(
												folder.getName())) {
									posExplorer = position;

								} else if (AcideProjectConfiguration.getInstance()
										.getFileAt(position).getParent().equals(
												folder.getName())) {
									if (!AcideProjectConfiguration.getInstance()
											.getFileAt(position).isDirectory()) {
										
										contRemove.add(AcideProjectConfiguration.getInstance()
												.getFileAt(position).getAbsolutePath());
										
										if (AcideProjectConfiguration.getInstance()
												.getNumFilesFromList() != 1)
											AcideProjectConfiguration.getInstance()
													.removeFileAt(position);
										else
											AcideProjectConfiguration.getInstance()
													.removeFileAt(0);
									} else {
										String dir = AcideProjectConfiguration.getInstance()
												.getFileAt(position).getName();
										
										for (int k = position + 1; k < AcideProjectConfiguration.getInstance()
												.getNumFilesFromList(); k++) {
											if (AcideProjectConfiguration.getInstance()
													.getFileAt(position)
													.getParent()
													.equals(dir)) {
												contRemove
														.add(AcideProjectConfiguration.getInstance()
																.getFileAt(
																		k)
																.getAbsolutePath());

												if (AcideProjectConfiguration.getInstance()
														.getNumFilesFromList() != 1)
													AcideProjectConfiguration.getInstance()
															.removeFileAt(k);
												else
													AcideProjectConfiguration.getInstance()
															.removeFileAt(0);
											}
										}
									}
								}
							}
						}
						
						// If it exists
						if (posExplorer != -1)
							
							// If it is not the last file in the project 
							if (AcideProjectConfiguration.getInstance()
									.getNumFilesFromList() != 1)
								AcideProjectConfiguration.getInstance()
										.removeFileAt(posExplorer);
							else
								
								// Last file in the project
								AcideProjectConfiguration.getInstance()
										.removeFileAt(0);
						
						// Are you sure?
						chosenOption = JOptionPane.showConfirmDialog(null, labels
								.getString("s655"));
						
						// If so
						if (chosenOption == JOptionPane.OK_OPTION) {

							// Deletes the file
							for (int j = 0; j < contRemove.size(); j++) {
								File fi = new File(contRemove.get(j));
								if (fi.isFile())
									fi.delete();
							}

						} else
							// Updates the status bar
							MainWindow.getInstance().getStatusBar().setStatusMessage(
									"Option cancel");
						return;

					}
				}
			}
		}

		// UPDATES THE EXPLORER POPUP MENU
		if (AcideProjectConfiguration.getInstance().getNumFilesFromList() > 0) {
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getRemoveFile()
					.setEnabled(true);
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getDeleteFile()
					.setEnabled(true);
		} else {
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getRemoveFile()
					.setEnabled(false);
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getDeleteFile()
					.setEnabled(false);
		}
	}
}