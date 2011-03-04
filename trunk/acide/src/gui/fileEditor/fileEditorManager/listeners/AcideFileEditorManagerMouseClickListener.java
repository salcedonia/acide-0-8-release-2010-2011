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
package gui.fileEditor.fileEditorManager.listeners;

import es.text.AcideFileManager;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor manager mouse click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideFileEditorManagerMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		dispatchEvent(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		dispatchEvent(mouseEvent);
	}

	/**
	 * Dispatches the mouse event.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void dispatchEvent(MouseEvent mouseEvent) {

		// If there are opened editors
		if (MainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.getComponentCount() != 0) {

			// Gets the selected editor path
			String selectedEditorPath = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getAbsolutePath();

			// If there is a valid selected file editor panel index
			if (MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanelIndex() != -1) {

				// Gets the active text pane
				AcideFileEditorPanel selectedFileEditorPanel = MainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel();

				// Sets the caret position at its position
				selectedFileEditorPanel
						.setCaretPosition(selectedFileEditorPanel
								.getActiveTextEditionArea().getCaretPosition());

				// Sets the caret visible
				selectedFileEditorPanel.setCaretVisible(true);

				// Sets the focus on the active text component
				selectedFileEditorPanel.putFocusOnActiveTextArea();
			}

			// If has path
			if (selectedEditorPath != null) {

				// Builds the file to check its last modification and size
				// properties
				File file = new File(selectedEditorPath);

				if ((file.lastModified() != MainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getLastChange())
						|| (file.length() != MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getLastSize())) {

					// Ask to the user for saving
					int resultValue = JOptionPane.showConfirmDialog(null,
							AcideLanguageManager.getInstance().getLabels()
									.getString("s65"));

					// OK OPTION
					if (resultValue == JOptionPane.OK_OPTION) {

						// Creates the file manager
						AcideFileManager fileManager = new AcideFileManager();

						// Load the file content
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setFileContent(
										fileManager.load(selectedEditorPath));

						// Sets last change
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastChange(file.lastModified());

						// Sets last size
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastSize(file.length());
					} else {

						// NO OPTION

						// Sets last change
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastChange(file.lastModified());

						// Sets last size
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastSize(file.length());
					}
				}
			}
		}

		// Updates the status bar with the type of file selected in the file
		// editor
		MainWindow.getInstance().getStatusBar()
				.updatesStatusBarFromFileEditor();

		// Selects the node in the tree that matches with the clicked file
		// editor
		MainWindow.getInstance().getExplorerPanel()
				.selectTreeNodeFromFileEditor();

		// Updates the button icons
		MainWindow.getInstance().getFileEditorManager().updatesButtonIcons();
	}
}
