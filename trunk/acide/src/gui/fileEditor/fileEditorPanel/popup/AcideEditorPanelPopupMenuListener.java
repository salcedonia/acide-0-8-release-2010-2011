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
package gui.fileEditor.fileEditorPanel.popup;

import es.configuration.project.AcideProjectConfiguration;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * ACIDE - A Configurable IDE editor panel popup menu listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideEditorPanelPopupMenuListener extends MouseAdapter {

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
	 * Shows the popup menu.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void maybeShowPopup(MouseEvent mouseEvent) {

		// If it has to display the popup menu
		if (mouseEvent.isPopupTrigger()) {

			// Gets the selected file editor panel
			AcideFileEditorPanel selectedFileEditorPanel = MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel();
			
			// Disables the copy menu item
			selectedFileEditorPanel.getPopupMenu().getCopy().setEnabled(false);
			
			// Disables the cut menu item
			selectedFileEditorPanel.getPopupMenu().getCut().setEnabled(false);
			
			// Disables the paste menu item
			selectedFileEditorPanel.getPopupMenu().getPaste().setEnabled(false);
			
			// Disables the add file menu item
			selectedFileEditorPanel.getPopupMenu().getAddFile().setEnabled(false);
			
			// Disables the remove file menu item
			selectedFileEditorPanel.getPopupMenu().getRemoveFile().setEnabled(false);
			
			// Disables the set compilable menu item
			selectedFileEditorPanel.getPopupMenu().getSetCompilable().setEnabled(false);
			
			// Disables the unset compilable menu item
			selectedFileEditorPanel.getPopupMenu().getUnsetCompilable().setEnabled(false);
			
			// Disables the set main menu item
			selectedFileEditorPanel.getPopupMenu().getSetMain().setEnabled(false);
			
			// Disables the unset main menu item
			selectedFileEditorPanel.getPopupMenu().getUnsetMain().setEnabled(false);

			// Check the system clipboard
			if (Toolkit.getDefaultToolkit().getSystemClipboard()
					.getContents(null) != null)
				// Enables the paste menu item in the popup menu
				selectedFileEditorPanel.getPopupMenu().getPaste().setEnabled(true);

			// Checks the selected editor
			if (selectedFileEditorPanel.getActiveTextEditionArea().getSelectedText() != null) {

				// Enables the copy menu item
				selectedFileEditorPanel.getPopupMenu().getCopy().setEnabled(true);
				
				// Enables the cut menu item
				selectedFileEditorPanel.getPopupMenu().getCut().setEnabled(true);
			}

			// Is it default project?
			if (AcideProjectConfiguration.getInstance()
					.isDefaultProject()) {

				// Disables the add file menu item
				selectedFileEditorPanel.getPopupMenu().getAddFile().setEnabled(false);
				
				// Disables the remove file menu item
				selectedFileEditorPanel.getPopupMenu().getRemoveFile().setEnabled(false);

				// Checks the types
				if (!selectedFileEditorPanel.isMainFile())
					// Enables the set main menu item
					selectedFileEditorPanel.getPopupMenu().getSetMain().setEnabled(true);
				if (selectedFileEditorPanel.isMainFile())
					// Enables the unset main menu item
					selectedFileEditorPanel.getPopupMenu().getUnsetMain().setEnabled(true);
				if (!selectedFileEditorPanel.isCompilableFile()
						|| (selectedFileEditorPanel.isCompilableFile() && selectedFileEditorPanel.isMainFile()))
					// Enables the set compilable menu item
					selectedFileEditorPanel.getPopupMenu().getSetCompilable().setEnabled(true);
				if (selectedFileEditorPanel.isCompilableFile() && !selectedFileEditorPanel.isMainFile())
					// Enables the unset compilable menu item
					selectedFileEditorPanel.getPopupMenu().getUnsetCompilable().setEnabled(true);
			} else {

				// Searches for the file in the project list
				String file = MainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getAbsolutePath();

				boolean exists = false;

				for (int index = 0; index < AcideProjectConfiguration.getInstance()
						.getNumberOfFilesFromList(); index++) {
					if (AcideProjectConfiguration.getInstance().getFileAt(index)
							.getAbsolutePath().equals(file)) {
						exists = true;
					}
				}
				
				if (exists) {
					
					// Enables the remove file menu item
					selectedFileEditorPanel.getPopupMenu().getRemoveFile().setEnabled(true);
					
					// Enables the add file menu item
					selectedFileEditorPanel.getPopupMenu().getAddFile().setEnabled(false);

					if (!selectedFileEditorPanel.isMainFile())
						// Enables the set main menu item
						selectedFileEditorPanel.getPopupMenu().getSetMain().setEnabled(true);
					if (selectedFileEditorPanel.isMainFile())
						// Enables the unset main menu item
						selectedFileEditorPanel.getPopupMenu().getUnsetMain().setEnabled(true);
					if (!selectedFileEditorPanel.isCompilableFile()
							|| (selectedFileEditorPanel.isCompilableFile() && selectedFileEditorPanel.isMainFile()))
						// Enables the set compilable menu item
						selectedFileEditorPanel.getPopupMenu().getSetCompilable().setEnabled(true);
					if (selectedFileEditorPanel.isCompilableFile() && !selectedFileEditorPanel.isMainFile())
						// Enables the unset compilable menu item
						selectedFileEditorPanel.getPopupMenu().getUnsetCompilable()
								.setEnabled(true);

				} else {
					
					// Disables the remove file menu item
					selectedFileEditorPanel.getPopupMenu().getRemoveFile().setEnabled(false);
					
					// Disables the add file menu item
					selectedFileEditorPanel.getPopupMenu().getAddFile().setEnabled(true);
				}
			}

			// Shows the popup menu
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getPopupMenu()
					.show(mouseEvent.getComponent(), mouseEvent.getX(),
							mouseEvent.getY());
		}
	}
}
