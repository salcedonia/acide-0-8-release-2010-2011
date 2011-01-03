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
package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import gui.fileEditor.fileEditorManager.utils.logic.MatchingBraces;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.text.BadLocationException;
import javax.swing.tree.TreePath;

import operations.log.AcideLog;

/**																
 * ACIDE - A Configurable IDE file editor text edition area double click listener.
 *					
 * @version 0.8	
 * @see MouseAdapter																													
 */
public class AcideFileEditorTextEditionAreaMouseDoubleClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Gets the selected file editor panel index
		AcideFileEditorPanel selectedFileEditorPanelIndex = MainWindow.getInstance()
		.getFileEditorManager().getSelectedFileEditorPanel();
		
		// Double click
		if (mouseEvent.getClickCount() > 1) {
			
			try {
				
				// Selects the word which is over the caret position in
				// the active editor
				int start = selectedFileEditorPanelIndex.getActiveTextEditionArea().getCaretPosition();
				int end = MatchingBraces.findMatchingBracket(selectedFileEditorPanelIndex.getActiveTextEditionArea()
						.getDocument(), start - 1);
				
				if (end > -1) {
					if (end > start)
						selectedFileEditorPanelIndex.selectText(start - 1, end - start + 2);
					if (end < start)
						selectedFileEditorPanelIndex.selectText(end, start - end);
				}
			} catch (BadLocationException exception) {
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

		}

		// Not default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// Creates the project file
			AcideProjectFile projectFile = new AcideProjectFile();
			
			int index = -1;
			for (int position = 0; position < AcideProjectConfiguration.getInstance().getNumFilesFromList(); position++) {

				if (AcideProjectConfiguration.getInstance()
						.getFileAt(position).getAbsolutePath().equals(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath())) {

					// Gets the file from the project configuration
					projectFile = AcideProjectConfiguration.getInstance().getFileAt(position);

					for (int positionProject = 0; positionProject < AcideProjectConfiguration.getInstance()
							.getNumFilesFromList() + 1; positionProject++) {

						if (MainWindow
								.getInstance()
								.getExplorerPanel()
								.getTree()
								.getPathForRow(positionProject)
								.getLastPathComponent()
								.toString()
								.equals(projectFile.getLastPathComponent())) {

							index = positionProject;
						}
					}
				}
			}

			// Selects the file in the explorer tree
			TreePath currentSelection = MainWindow.getInstance()
					.getExplorerPanel().getTree().getPathForRow(index);
			MainWindow.getInstance().getExplorerPanel().getTree()
					.setSelectionPath(currentSelection);
		}
	}
}
