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

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * ACIDE - A Configurable IDE explorer panel double click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideExplorerPanelDoubleClickMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Double click
		if (mouseEvent.getClickCount() == 2) {

			// Gets the current explorer tree selection
			TreePath currentSelection = AcideMainWindow.getInstance()
					.getExplorerPanel().getTree()
					.getPathForLocation(mouseEvent.getX(), mouseEvent.getY());

			// If something is selected
			if (currentSelection != null) {

				// Gets the current node
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) currentSelection
						.getLastPathComponent();

				// Gets the current project file
				AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
						.getUserObject();

				// Searches for the file into the editor files
				int fileEditorPanelIndex = AcideMainWindow.getInstance()
				.getFileEditorManager().getIndexOfFileEditorPanelByName(currentProjectFile.getName());
				
				// If it is not opened
				if (fileEditorPanelIndex == -1) {

					// Not a directory
					if (!currentProjectFile.isDirectory()) {

						// Gets the file content
						String fileContent = null;
						fileContent = AcideFileManager.getInstance().load(
								currentProjectFile.getAbsolutePath());

						// If it could get the file content
						if (fileContent != null) {
							
							// Searches for the file into the project files list
							int fileProjectIndex = AcideProjectConfiguration
									.getInstance().getIndexOfFile(
											currentProjectFile
													.getAbsolutePath());

							// TODO: Load the predefined extension

							// Creates the lexicon configuration
							AcideLexiconConfiguration lexiconConfiguration = new AcideLexiconConfiguration();

							// Loads the lexicon configuration
							lexiconConfiguration
									.load(AcideLexiconConfiguration.DEFAULT_PATH
											+ AcideLexiconConfiguration.DEFAULT_NAME);

							// TODO: Load the predefined extension

							// Creates the grammar configuration
							AcideGrammarConfiguration grammarConfiguration = new AcideGrammarConfiguration();

							// Sets the grammar configuration path
							grammarConfiguration
									.setPath(AcideGrammarConfiguration.DEFAULT_PATH);

							// Updates the tabbed pane in the file editor
							// manager
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.updatesTabbedPane(
											currentProjectFile
													.getAbsolutePath(),
											fileContent,
											true,
											AcideProjectConfiguration
													.getInstance()
													.getFileAt(fileProjectIndex)
													.getType(), 0, 0, 1,
											lexiconConfiguration,
											grammarConfiguration);

							// Sets the file as opened in the project
							// configuration
							AcideProjectConfiguration.getInstance()
									.getFileAt(fileProjectIndex)
									.setIsOpened(true);

							// The project has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);
						}
					}
				} else {

					// IT IS ALREADY OPENED

					// Sets the selected file editor panel at it
					AcideMainWindow.getInstance().getFileEditorManager()
							.updatesFileEditorAt(fileEditorPanelIndex);
				}
			}
		}
	}
}
