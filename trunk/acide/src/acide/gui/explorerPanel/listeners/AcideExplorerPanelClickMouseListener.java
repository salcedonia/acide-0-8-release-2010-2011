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

import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

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
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Gets the selected node from the explorer tree
		TreePath currentSelection = AcideMainWindow.getInstance()
				.getExplorerPanel().getTree()
				.getPathForLocation(mouseEvent.getX(), mouseEvent.getY());

		if (currentSelection != null) {

			// Updates the status bar
			String filePath = currentSelection.getLastPathComponent().toString();

			// Updates the status message in the status bar
			AcideMainWindow.getInstance().getStatusBar()
					.setStatusMessage(filePath);

			// Gets the current node from the tree
			DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) currentSelection
					.getLastPathComponent();
			
			// Gets the current project file
			AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode.getUserObject();

			// Sets selected tab at the tabbed pane
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// If this is the file editor panel
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).getAbsolutePath()
						.equals(currentProjectFile.getAbsolutePath())) {

					// Sets the selected file editor panel
					AcideMainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(index);
				}
			}
			
			// Validates the changes in the main window
			AcideMainWindow.getInstance().validate();

			// Repaints the main window
			AcideMainWindow.getInstance().repaint();
		}
	}
}
