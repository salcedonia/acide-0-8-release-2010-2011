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
package acide.gui.explorerPanel.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**																
 * ACIDE - A Configurable IDE explorer panel keyboard listener.
 *					
 * @version 0.8	
 * @see KeyAdapter																													
 */
public class AcideExplorerPanelKeyboardListener extends KeyAdapter {

	/**
	 * Sets the focus into the editor file determined by the selected node
	 * given as a parameter.
	 * 
	 * @param selectedNode
	 *            Selected node in the explorer tree.
	 */
	private void setFocusEditorFile(TreePath selectedNode) {
		
		// If there is a selected node
		if (selectedNode != null) {

			// Gets the file path from the selected node in the explorer tree
			String filePath = selectedNode.getLastPathComponent()
					.toString();
			
			// Updates the status message in the status bar
			AcideMainWindow.getInstance().getStatusBar().setStatusMessage(filePath);

			// Builds the project file from the explorer tree node
			DefaultMutableTreeNode defaultMutableTreeNode = (DefaultMutableTreeNode) selectedNode
					.getLastPathComponent();
			Object node = defaultMutableTreeNode.getUserObject();
			AcideProjectFile file = (AcideProjectFile) node;
			file.getAbsolutePath();

			// Searches for the explorer file into the editor files
			for (int fileIndex = 0; fileIndex < AcideProjectConfiguration.getInstance().getNumberOfFilesFromList(); fileIndex++) {

				// If exists
				if (AcideProjectConfiguration.getInstance()
						.getFileAt(fileIndex).getAbsolutePath()
						.equals(file.getAbsolutePath()))

					// Is not a directory
					if (!AcideProjectConfiguration.getInstance()
							.getFileAt(fileIndex).isDirectory()) {

						// COMPILABLE FILE?
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(fileIndex).isCompilableFile())

							// MAIN FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(fileIndex).isMainFile())

								// Updates the status message in the status bar
								AcideMainWindow
										.getInstance()
										.getStatusBar()
										.setStatusMessage(
												AcideProjectConfiguration.getInstance().getFileAt(fileIndex)
														.getAbsolutePath()
														+ " <MAIN>");
							else
								// Updates the status message in the status bar
								AcideMainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration.getInstance().getFileAt(fileIndex)
													.getAbsolutePath()
													+ " <COMPILABLE>");
						else
							// Updates the status message in the status bar
							AcideMainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideProjectConfiguration.getInstance().getFileAt(fileIndex)
												.getAbsolutePath());	
					}
			}

			// Sets the focus on the selected file at the editor
			for (int index = 0; index < AcideMainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels(); index++) {
				
				// If it is the file editor panel
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).getAbsolutePath()
						.equals(file.getAbsolutePath())) {
					
					// Sets the selected file editor panel on it
					AcideMainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(index);
				}
			}
			
			// Validates the MAIN WINDOW
			AcideMainWindow.getInstance().validate();
			
			// Repaint the MAIN WINDOW
			AcideMainWindow.getInstance().repaint();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		switch (keyEvent.getKeyCode()) {

		// NOW THE SELECTED ELEMENT IS THE PREVIOUS IN THE TREE
		case KeyEvent.VK_UP:
			setFocusEditorFile(AcideMainWindow.getInstance().getExplorerPanel().getTree().getPathForRow(AcideMainWindow.getInstance().getExplorerPanel().getTree()
					.getLeadSelectionRow() - 1));
			break;

		// NOW THE SELECTED ELEMENT IS THE NEXT IN THE TREE
		case KeyEvent.VK_DOWN:
			setFocusEditorFile(AcideMainWindow.getInstance().getExplorerPanel().getTree().getPathForRow(AcideMainWindow.getInstance().getExplorerPanel().getTree()
					.getLeadSelectionRow() + 1));
			break;

		// THE SELECTED ELEMENT IS THE ACTUAL
		case KeyEvent.VK_ENTER:
			setFocusEditorFile(AcideMainWindow.getInstance().getExplorerPanel().getTree().getSelectionPath());
			break;
		}
	}
}
