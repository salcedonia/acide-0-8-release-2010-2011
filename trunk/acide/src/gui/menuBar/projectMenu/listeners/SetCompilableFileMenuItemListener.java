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
 * ACIDE -A Configurable IDE project menu set compilable menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class SetCompilableFileMenuItemListener implements ActionListener {

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

			// Gets the selected node in the explorer tree
			currentNode = (DefaultMutableTreeNode) currentSelection.getLastPathComponent();
			
			// Transforms it into a explorer file
			currentProjectFile = (AcideProjectFile) currentNode.getUserObject();

			// If is not COMPILABLE FILE or COMPILABLE and MAIN FILE
			if (!currentProjectFile.isCompilableFile()
					|| (currentProjectFile.isCompilableFile() && currentProjectFile
							.isMainFile())) {

				// Is a file and not a directory
				if (!currentProjectFile.isDirectory()) {

					if (currentProjectFile.isMainFile())
						currentProjectFile.setIsMainFile(false);
					currentProjectFile.setIsCompilableFile(true);

					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);

					// Updates the file editor
					for (int index = 0; index < MainWindow.getInstance()
							.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

						if (MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index).getAbsolutePath().equals(
										currentProjectFile.getAbsolutePath())) {

							// Creates the image icon
							ImageIcon imageIcon = new ImageIcon(
							"./resources/icons/editor/compilable.png");
							
							// Updates the file editor configuration
							MainWindow
							.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index).setCompilableFile(true);
							MainWindow
							.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index).setMainFile(false);
							
							// Updates the file editor panel image icon
							MainWindow
							.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index).setIcon(imageIcon);
							
							// Sets the icon in the file editor tab
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setIconAt(
											index,
											imageIcon);

							// Updates the status message in the status bar
							MainWindow.getInstance().getStatusBar()
									.setStatusMessage(
											MainWindow.getInstance()
													.getFileEditorManager()
													.getFileEditorPanelAt(index)
													.getAbsolutePath()
													+ " <COMPILABLE>");
						}
					}
				}
			}
		} else {

			// Default project
			if (AcideProjectConfiguration.getInstance()
					.isDefaultProject())

				// If there are opened editors
				if (MainWindow.getInstance().getFileEditorManager()
						.getNumberOfFileEditorPanels() > 0)

					// Sets the COMPILABLE FILE
					MainWindow.getInstance().getFileEditorManager()
							.setCompilableFile();
		}
	}
}
