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
import gui.fileEditor.fileEditorManager.utils.logic.ElementMatcher;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.tree.TreePath;

import operations.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file editor text edition area mouse listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideFileEditorMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Highlights the matching elements
		highlightMatchingElements();

		// Selects the explorer tree node if any
		selectExplorerTreeNode();

		// Paints the caret in both text areas and puts the focus on the active
		// one.
		paintCaretAndPutFocus();
	}

	/**
	 * Highlights the matching elements.
	 */
	public void highlightMatchingElements() {

		// Gets the selected file editor panel index
		AcideFileEditorPanel selectedFileEditorPanelIndex = MainWindow
				.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel();

		// Selects the matching elements
		try {

			// Selects the word which is over the caret position in
			// the active editor
			int start = selectedFileEditorPanelIndex.getActiveTextEditionArea()
					.getCaretPosition();
			int end = ElementMatcher.findMatchingBracket(
					selectedFileEditorPanelIndex.getActiveTextEditionArea()
							.getDocument(), start - 1);

			if (end > -1) {
				if (end > start)
					selectedFileEditorPanelIndex.selectText(start - 1, end
							- start + 2);
				if (end < start)
					selectedFileEditorPanelIndex.selectText(end, start - end);
			}
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Paints the caret in both text areas and puts the focus on the active one.
	 */
	public void paintCaretAndPutFocus() {

		// Gets the active text pane
		final AcideFileEditorPanel selectedFileEditorPanel = MainWindow
				.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel();

		SwingUtilities.invokeLater(new Runnable() {

			@Override
			public void run() {

				// Sets the caret position at its position
				selectedFileEditorPanel
						.setCaretPosition(selectedFileEditorPanel
								.getActiveTextEditionArea().getCaretPosition());
			}
		});

		// Sets the caret visible
		selectedFileEditorPanel.setCaretVisible(true);

		// Sets the focus on the active text component
		selectedFileEditorPanel.putFocusOnActiveTextArea();
	}

	/**
	 * Searches for the node in the explorer tree and selects it if exists on
	 * it.
	 */
	public void selectExplorerTreeNode() {

		// If it is not the default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// Creates the project file
			AcideProjectFile projectFile = new AcideProjectFile();

			int fileIndex = -1;
			for (int index = 0; index < AcideProjectConfiguration.getInstance()
					.getNumberOfFilesFromList(); index++) {

				if (AcideProjectConfiguration
						.getInstance()
						.getFileAt(index)
						.getAbsolutePath()
						.equals(MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath())) {

					// Gets the file from the project configuration
					projectFile = AcideProjectConfiguration.getInstance()
							.getFileAt(index);

					for (int projectIndex = 0; projectIndex < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList() + 1; projectIndex++) {

						if (MainWindow.getInstance().getExplorerPanel()
								.getTree().getPathForRow(projectIndex)
								.getLastPathComponent().toString()
								.equals(projectFile.getLastPathComponent())) {

							fileIndex = projectIndex;
						}
					}
				}
			}

			// Gets the current selection in the explorer for the current file
			TreePath currentSelection = MainWindow.getInstance()
					.getExplorerPanel().getTree().getPathForRow(fileIndex);

			// Selects the file in the explorer tree
			MainWindow.getInstance().getExplorerPanel().getTree()
					.setSelectionPath(currentSelection);
		}
	}
}
