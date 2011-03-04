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

import javax.swing.ImageIcon;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**																
 * ACIDE -A Configurable IDE project menu set main file menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class SetMainFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the selection in the explorer tree
		TreePath currentSelection = MainWindow.getInstance().getExplorerPanel().getTree()
				.getSelectionPath();
		
		// Current node
		DefaultMutableTreeNode currentNode;
		
		// Current project file
		AcideProjectFile currentProjectFile;

		// If something is selected
		if (currentSelection != null) {

			// Gets the current node
			currentNode = (DefaultMutableTreeNode) currentSelection.getLastPathComponent();
			
			// Gets the current project file
			currentProjectFile = (AcideProjectFile) currentNode.getUserObject();

			// Is not MAIN FILE
			if (!currentProjectFile.isMainFile()) {

				// Is file and not a directory
				if (!currentProjectFile.isDirectory()) {

					for (int index1 = 0; index1 < AcideProjectConfiguration.getInstance().getFileListSize(); index1++) {

						// Quits previous MAIN
						if (AcideProjectConfiguration.getInstance().getFileAt(index1)
								.isMainFile()) {
							
							AcideProjectConfiguration.getInstance().getFileAt(index1)
									.setIsMainFile(false);
							AcideProjectConfiguration.getInstance().getFileAt(index1)
									.setIsCompilableFile(false);

							// Quits the icon tab
							for (int index2 = 0; index2 < MainWindow.getInstance()
									.getFileEditorManager().getNumberOfFileEditorPanels(); index2++) {

								if (MainWindow
										.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(index2)
										.getAbsolutePath()
										.equals(
												AcideProjectConfiguration.getInstance()
														.getFileAt(index1)
														.getAbsolutePath()))
									MainWindow.getInstance()
											.getFileEditorManager().getTabbedPane()
											.setIconAt(index2, null);
							}
						}
					}

					// Is a MAIN FILE
					currentProjectFile.setIsMainFile(true);
					
					// Is a COMPILABLE FILE
					currentProjectFile.setIsCompilableFile(true);

					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);

					// Updates the File Editor
					for (int index = 0; index < MainWindow.getInstance()
							.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {
						
						if (MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index).getAbsolutePath().equals(
										currentProjectFile.getAbsolutePath())) {
							
							// Creates the MAIN image icon to set
							ImageIcon imageIcon = new ImageIcon(
							"./resources/icons/editor/main.png");
							
							// It is a COMPILABLE file
							MainWindow
							.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index).setCompilableFile(true);
							
							// It is a MAIN file
							MainWindow
							.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index).setMainFile(true);
							
							// Updates the file editor panel image icon
							MainWindow
							.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index).setIcon(imageIcon);
							
							// Sets the MAIN icon in the selected file editor panel 
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setIconAt(
											index,
											imageIcon);
							
							// Updates the status message in the status bar
							MainWindow.getInstance().getStatusBar().setStatusMessage(
									MainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(index)
									.getAbsolutePath() + " <MAIN>");
						}
					}
				}
			}
		}
	}
}
