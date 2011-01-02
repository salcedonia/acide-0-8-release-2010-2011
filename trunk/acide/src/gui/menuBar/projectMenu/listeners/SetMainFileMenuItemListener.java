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
		TreePath explorerSelection = MainWindow.getInstance().getExplorerPanel().getTree()
				.getSelectionPath();
		DefaultMutableTreeNode filePath;
		AcideProjectFile projectFile;

		// If something is selected
		if (explorerSelection != null) {

			filePath = (DefaultMutableTreeNode) explorerSelection.getLastPathComponent();
			projectFile = (AcideProjectFile) filePath.getUserObject();

			// Is not MAIN FILE
			if (!projectFile.isMainFile()) {

				// Is file and not a directory
				if (!projectFile.isDirectory()) {

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
									.getFileEditorManager().getNumFileEditorPanels(); index2++) {

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
					projectFile.setIsMainFile(true);
					
					// Is a COMPILABLE FILE
					projectFile.setIsCompilableFile(true);

					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);

					// Updates the status message in the status bar
					MainWindow.getInstance().getStatusBar().setStatusMessage(
							projectFile.getAbsolutePath() + " <MAIN>");

					// Puts the icon 
					for (int index = 0; index < MainWindow.getInstance()
							.getFileEditorManager().getNumFileEditorPanels(); index++) {
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
													"./resources/icons/editor/main.PNG"));
						}
					}
				}
			}
		} else {

			// Default configuration
			if (AcideProjectConfiguration.getInstance()
					.isDefaultProject()) {

				// If there are more opened editor
				if (MainWindow.getInstance().getFileEditorManager()
						.getNumFileEditorPanels() > 0)

					// Sets the MAIN FILE
					MainWindow.getInstance().getFileEditorManager()
							.setMainFile();
			}
		}
	}
}
