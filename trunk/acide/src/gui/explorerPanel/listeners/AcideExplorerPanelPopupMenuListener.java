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
import gui.explorerPanel.AcideExplorerPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * ACIDE - A Configurable IDE explorer panel popup menu listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideExplorerPanelPopupMenuListener extends MouseAdapter{

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/**
	 * Shows the popup menu
	 * 
	 * @param mouseEvent
	 *            mouse event
	 */
	private void maybeShowPopup(MouseEvent mouseEvent) {

		if (mouseEvent.isPopupTrigger()) {

			// Gets the explorer panel
			AcideExplorerPanel explorerPanel = MainWindow.getInstance().getExplorerPanel();
			
			// Default project
			if (AcideProjectConfiguration.getInstance()
					.isDefaultProject()) {
		
				// Disables the save project menu item
				explorerPanel.getPopupMenu().getSaveProject().setEnabled(false);
				
				// Disables the new file menu item
				explorerPanel.getPopupMenu().getNewFile().setEnabled(false);
				
				// Disables the add file menu item
				explorerPanel.getPopupMenu().getAddFile().setEnabled(false);
				
				// Disables the remove file menu item
				explorerPanel.getPopupMenu().getRemoveFile().setEnabled(false);
				
				// Disables the delete file menu item
				explorerPanel.getPopupMenu().getDeleteFile().setEnabled(false);
				
				// Disables the set main file menu item
				explorerPanel.getPopupMenu().getSetMainFile().setEnabled(false);
				
				// Disables the unset main file menu item
				explorerPanel.getPopupMenu().getUnsetMainFile().setEnabled(false);
				
				// Disables the set compilable file menu item
				explorerPanel.getPopupMenu().getSetCompilableFile().setEnabled(false);
				
				// Disables the unset compilable file menu item
				explorerPanel.getPopupMenu().getUnsetCompilableFile().setEnabled(false);
				
				// Disables the add folder menu item
				explorerPanel.getPopupMenu().getAddFolder().setEnabled(false);
				
				// Disables the remove folder menu item
				explorerPanel.getPopupMenu().getRemoveFolder().setEnabled(false);
			} else {
				
				// Not default project
				
				// Disables the save project menu item
				explorerPanel.getPopupMenu().getSaveProject().setEnabled(false);
				
				// Enables the new file menu item
				explorerPanel.getPopupMenu().getNewFile().setEnabled(true);
				
				// Enables the add file menu item
				explorerPanel.getPopupMenu().getAddFile().setEnabled(true);
				
				// Disables the remove file menu item
				explorerPanel.getPopupMenu().getRemoveFile().setEnabled(false);
				
				// Disables the delete file menu item
				explorerPanel.getPopupMenu().getDeleteFile().setEnabled(false);
				
				// Disables the set main file menu item
				explorerPanel.getPopupMenu().getSetMainFile().setEnabled(false);
				
				// Disables the unset main file menu item
				explorerPanel.getPopupMenu().getUnsetMainFile().setEnabled(false);
				
				// Disables the set compilable file menu item
				explorerPanel.getPopupMenu().getSetCompilableFile().setEnabled(false);
				
				// Disables the unset compilable file menu item
				explorerPanel.getPopupMenu().getUnsetCompilableFile().setEnabled(false);
				
				// Enables the add folder menu item
				explorerPanel.getPopupMenu().getAddFolder().setEnabled(true);
				
				// Disables the remove folder menu item
				explorerPanel.getPopupMenu().getRemoveFolder().setEnabled(false);

				// If the project configuration has been modified
				if (AcideProjectConfiguration.getInstance()
						.isModified())
					
					// Enables the save project menu item
					explorerPanel.getPopupMenu().getSaveProject().setEnabled(true);

				// Gets the tree path
				TreePath path = MainWindow.getInstance().getExplorerPanel()
						.getTree().getSelectionPath();
				DefaultMutableTreeNode filePath;
				AcideProjectFile file;

				if (path != null) {

					// Gets the file path from the explorer tree
					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					
					// Buils the projec file
					file = (AcideProjectFile) filePath.getUserObject();

					// If it is not a directory
					if (!file.isDirectory()) {
						
						// Enables the remove file menu item
						explorerPanel.getPopupMenu().getRemoveFile().setEnabled(true);
						
						// Enables the delete file menu item
						explorerPanel.getPopupMenu().getDeleteFile().setEnabled(true);
						
						// If it is not a MAIN FILE
						if (!file.isMainFile())
							
							// Enables the set main file menu item
							explorerPanel.getPopupMenu().getSetMainFile().setEnabled(true);
						else
							// Enables the unset main file menu item
							explorerPanel.getPopupMenu().getUnsetMainFile().setEnabled(true);
						
						// If it is not a COMPILABLE FILE or is COMPILABLE and a MAIN FILE
						if (!file.isCompilableFile()
								|| (file.isCompilableFile() && file
										.isMainFile()))
							
							// Enables the set compilable file menu item
							explorerPanel.getPopupMenu().getSetCompilableFile().setEnabled(true);
						
						// If it is COMPILABLE FILE and it is not a MAIN FILE
						if (file.isCompilableFile() && !file.isMainFile())
							
							// Enables the unset compilable file menu item
							explorerPanel.getPopupMenu().getUnsetCompilableFile().setEnabled(true);
					} else {
						
						// Enables the remove folder menu item
						explorerPanel.getPopupMenu().getRemoveFolder().setEnabled(true);
					}
				}
			}
			
			// Shows the popup menu
			explorerPanel.getPopupMenu().show(mouseEvent.getComponent(), mouseEvent.getX(), mouseEvent.getY());
		}
	}
}
