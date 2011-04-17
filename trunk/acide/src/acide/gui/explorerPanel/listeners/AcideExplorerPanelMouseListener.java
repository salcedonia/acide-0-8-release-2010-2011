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
import acide.configuration.lexiconAssigner.AcideLexiconAssignerConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * ACIDE - A Configurable IDE explorer panel mouse listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideExplorerPanelMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Gets the current explorer tree selection
		TreePath currentSelection = AcideMainWindow.getInstance()
				.getExplorerPanel().getTree()
				.getPathForLocation(mouseEvent.getX(), mouseEvent.getY());

		// If there are more than 1 clicks
		if (mouseEvent.getClickCount() > 1)

			// Performs the double click
			doubleClick(currentSelection);
		else
			// Performs the single click
			singleClick(currentSelection);
	}

	/**
	 * Performs the single click mouse event.
	 * 
	 * @param currentSelection
	 *            current tree selection.
	 */
	private void singleClick(TreePath currentSelection) {

		if (currentSelection != null) {

			// Updates the status bar
			String filePath = currentSelection.getLastPathComponent()
					.toString();

			// Updates the status message in the status bar
			AcideMainWindow.getInstance().getStatusBar()
					.setStatusMessage(filePath);

			// Gets the current node from the tree
			DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) currentSelection
					.getLastPathComponent();

			// Gets the current project file
			AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
					.getUserObject();

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
		}
	}

	/**
	 * Performs the double click mouse event.
	 * 
	 * @param currentSelection
	 *            current tree selection.
	 */
	private void doubleClick(TreePath currentSelection) {

		// If something is selected
		if (currentSelection != null) {

			// Gets the current node
			DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) currentSelection
					.getLastPathComponent();

			// Gets the current project file
			AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
					.getUserObject();

			int fileEditorPanelIndex = -1;

			// Searches for the file into the editor files
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).getAbsolutePath()
						.equals(currentProjectFile.getAbsolutePath()))
					fileEditorPanelIndex = index;
			}

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
								.getInstance().getIndexOfFile(currentProjectFile.getAbsolutePath());

						// Gets the predefined lexicon configuration
						AcideLexiconConfiguration lexiconConfiguration = AcideLexiconAssignerConfiguration
								.getInstance()
								.getPredifinedLexiconConfiguration(currentProjectFile.getAbsolutePath());

						// Creates the current grammar configuration
						AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

						// Sets the current grammar configuration path
						currentGrammarConfiguration
								.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

						// Creates the previous grammar configuration
						AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

						// Sets the previous grammar configuration path
						previousGrammarConfiguration
								.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

						// Updates the tabbed pane in the file editor
						// manager
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.updateTabbedPane(
										currentProjectFile.getAbsolutePath(),
										fileContent,
										true,
										AcideProjectConfiguration.getInstance()
												.getFileAt(fileProjectIndex)
												.getType(), 0, 0, 1,
										lexiconConfiguration,
										currentGrammarConfiguration,
										previousGrammarConfiguration);

						// Sets the file as opened in the project
						// configuration
						AcideProjectConfiguration.getInstance()
								.getFileAt(fileProjectIndex).setIsOpened(true);

						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);
					}
				}
			} else {

				// The file editor is already opened

				// Updates the selected file editor index
				AcideMainWindow.getInstance().getFileEditorManager()
						.updateRelatedComponentsAt(fileEditorPanelIndex);
			}
		}
	}
}
