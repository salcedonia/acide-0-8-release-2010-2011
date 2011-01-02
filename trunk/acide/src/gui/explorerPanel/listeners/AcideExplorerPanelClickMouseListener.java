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
package gui.explorerPanel.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**																
 * ACIDE - A Configurable IDE explorer panel mouse click listener.
 *					
 * @version 0.8	
 * @see MouseAdapter																													
 */
public class AcideExplorerPanelClickMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Gets the selected node from the explorer tree
		TreePath selectedNode = MainWindow.getInstance().getExplorerPanel().getTree().getPathForLocation(mouseEvent.getX(),
				mouseEvent.getY());

		if (selectedNode != null) {

			// Updates the status bar
			String filePath = selectedNode.getLastPathComponent()
					.toString();
			
			// Updates the status message in the status bar
			MainWindow.getInstance().getStatusBar().setStatusMessage(filePath);

			// Gets the file form the tree node
			DefaultMutableTreeNode defaultMutableTreeNode = (DefaultMutableTreeNode) selectedNode
					.getLastPathComponent();
			Object node = defaultMutableTreeNode.getUserObject();
			AcideProjectFile file = (AcideProjectFile) node;
			file.getAbsolutePath();

			// Searches the explorer file into the editor files
			for (int fileIndex = 0; fileIndex < AcideProjectConfiguration.getInstance().getNumFilesFromList(); fileIndex++) {

				// If it is the searched file
				if (AcideProjectConfiguration.getInstance()
						.getFileAt(fileIndex).getAbsolutePath()
						.equals(file.getAbsolutePath()))

					// Is not a directory
					if (!AcideProjectConfiguration.getInstance()
							.getFileAt(fileIndex).isDirectory()) {

						// IS COMPILABLE FILE?
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(fileIndex).isCompilableFile())

							// IS MAIN FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(fileIndex).isMainFile())

								// Updates status message in the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setStatusMessage(
												AcideProjectConfiguration.getInstance().getFileAt(fileIndex)
														.getAbsolutePath()
														+ " <MAIN>");
							else
								// Updates status message in the status bar
								MainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration.getInstance().getFileAt(fileIndex)
													.getAbsolutePath()
													+ " <COMPILABLE>");
						else
							// Updates status message in the status bar
							MainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideProjectConfiguration.getInstance().getFileAt(fileIndex)
												.getAbsolutePath());	
					}
			}

			// Sets the focus on the selected file at the file editor text edition area
			for (int index = 0; index < MainWindow.getInstance().getFileEditorManager()
					.getNumFileEditorPanels(); index++) {
				
				// If this is the file editor panel
				if (MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).getAbsolutePath()
						.equals(file.getAbsolutePath())) {
					
					// Sets the selected file editor panel
					MainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(index);
				}
			}

			// Validates the changes in the MAIN WINDOW
			MainWindow.getInstance().validate();
			
			// Repaints the MAIN WINDOW
			MainWindow.getInstance().repaint();
		}
	}
}
