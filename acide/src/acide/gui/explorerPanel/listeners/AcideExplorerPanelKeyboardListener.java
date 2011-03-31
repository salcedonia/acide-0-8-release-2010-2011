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
	 * Sets the focus into the editor file determined by the selected node given
	 * as a parameter.
	 * 
	 * @param currentSelection
	 *            Selected node in the explorer tree.
	 */
	private void setFocusEditorFile(TreePath currentSelection) {

		// If there is a selected node
		if (currentSelection != null) {

			// Gets the file path from the selected node in the explorer tree
			String filePath = currentSelection.getLastPathComponent()
					.toString();

			// Updates the status message in the status bar
			AcideMainWindow.getInstance().getStatusBar()
					.setStatusMessage(filePath);

			// Builds the project file from the explorer tree node
			DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) currentSelection
					.getLastPathComponent();

			// Gets the project file from the selected node in the explorer tree
			AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
					.getUserObject();

			// Selects the file editor
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// If it is the file editor panel
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).getAbsolutePath()
						.equals(currentProjectFile.getAbsolutePath())) {

					// Sets the selected file editor panel on it
					AcideMainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(index);
				}
			}
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

		// The next selected element will be the previous node in the tree
		case KeyEvent.VK_UP:
			setFocusEditorFile(AcideMainWindow
					.getInstance()
					.getExplorerPanel()
					.getTree()
					.getPathForRow(
							AcideMainWindow.getInstance().getExplorerPanel()
									.getTree().getLeadSelectionRow() - 1));
			break;

		// The next selected element will be the next node in the tree
		case KeyEvent.VK_DOWN:
			setFocusEditorFile(AcideMainWindow
					.getInstance()
					.getExplorerPanel()
					.getTree()
					.getPathForRow(
							AcideMainWindow.getInstance().getExplorerPanel()
									.getTree().getLeadSelectionRow() + 1));
			break;

		// The next selected element is the current one
		case KeyEvent.VK_ENTER:
			setFocusEditorFile(AcideMainWindow.getInstance().getExplorerPanel()
					.getTree().getSelectionPath());
			break;
		}
	}
}
