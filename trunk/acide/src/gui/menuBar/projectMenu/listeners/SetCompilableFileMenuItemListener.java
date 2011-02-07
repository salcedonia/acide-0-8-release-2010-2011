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
		TreePath explorerSelection = MainWindow.getInstance().getExplorerPanel().getTree()
				.getSelectionPath();
		DefaultMutableTreeNode selectedNode;
		AcideProjectFile projectFile;

		// If something is selected
		if (explorerSelection != null) {

			// Gets the selected node in the explorer tree
			selectedNode = (DefaultMutableTreeNode) explorerSelection.getLastPathComponent();
			
			// Transforms it into a explorer file
			projectFile = (AcideProjectFile) selectedNode.getUserObject();

			// If is not COMPILABLE FILE or COMPILABLE and MAIN FILE
			if (!projectFile.isCompilableFile()
					|| (projectFile.isCompilableFile() && projectFile
							.isMainFile())) {

				// Is a file and not a directory
				if (!projectFile.isDirectory()) {

					if (projectFile.isMainFile())
						projectFile.setIsMainFile(false);

					projectFile.setIsCompilableFile(true);

					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);

					// Puts the icon in the editor
					for (int index = 0; index < MainWindow.getInstance()
							.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

						if (MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index).getAbsolutePath().equals(
										projectFile.getAbsolutePath())) {

							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setIconAt(
											index,
											new ImageIcon(
													"./resources/icons/editor/compilable.PNG"));

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
