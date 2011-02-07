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
 * Editor panel popup menu listener.
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

		if (mouseEvent.isPopupTrigger()) {

			AcideFileEditorPanel selectedEditor = MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel();
			
			selectedEditor.getPopupMenu().getCopy().setEnabled(false);
			selectedEditor.getPopupMenu().getCut().setEnabled(false);
			selectedEditor.getPopupMenu().getPaste().setEnabled(false);
			selectedEditor.getPopupMenu().getAddFile().setEnabled(false);
			selectedEditor.getPopupMenu().getRemoveFile().setEnabled(false);
			selectedEditor.getPopupMenu().getSetCompilable().setEnabled(false);
			selectedEditor.getPopupMenu().getUnsetCompilable().setEnabled(false);
			selectedEditor.getPopupMenu().getSetMain().setEnabled(false);
			selectedEditor.getPopupMenu().getUnsetMain().setEnabled(false);

			// Check the systeme clipboard
			if (Toolkit.getDefaultToolkit().getSystemClipboard()
					.getContents(null) != null)
				// Enables the paste menu item in the popup menu
				selectedEditor.getPopupMenu().getPaste().setEnabled(true);

			// Checks the selected editor
			if (selectedEditor.getActiveTextEditionArea().getSelectedText() != null) {

				// Enable the options
				selectedEditor.getPopupMenu().getCopy().setEnabled(true);
				selectedEditor.getPopupMenu().getCut().setEnabled(true);
			}

			// Is it default project?
			if (AcideProjectConfiguration.getInstance()
					.isDefaultProject()) {

				// Disables the add and remove file menu item in the popup menu
				selectedEditor.getPopupMenu().getAddFile().setEnabled(false);
				selectedEditor.getPopupMenu().getRemoveFile().setEnabled(false);

				// Checks the types
				if (!selectedEditor.isMainFile())
					selectedEditor.getPopupMenu().getSetMain().setEnabled(true);
				if (selectedEditor.isMainFile())
					selectedEditor.getPopupMenu().getUnsetMain().setEnabled(true);
				if (!selectedEditor.isCompilableFile()
						|| (selectedEditor.isCompilableFile() && selectedEditor.isMainFile()))
					selectedEditor.getPopupMenu().getSetCompilable().setEnabled(true);
				if (selectedEditor.isCompilableFile() && !selectedEditor.isMainFile())
					selectedEditor.getPopupMenu().getUnsetCompilable().setEnabled(true);
			} else {

				// Searches for the file in the project list
				String file = MainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getAbsolutePath();

				boolean exists = false;

				for (int i = 0; i < AcideProjectConfiguration.getInstance()
						.getNumberOfFilesFromList(); i++) {
					if (AcideProjectConfiguration.getInstance().getFileAt(i)
							.getAbsolutePath().equals(file)) {
						exists = true;
					}
				}
				if (exists) {
					selectedEditor.getPopupMenu().getRemoveFile().setEnabled(true);
					selectedEditor.getPopupMenu().getAddFile().setEnabled(false);

					if (!selectedEditor.isMainFile())
						selectedEditor.getPopupMenu().getSetMain().setEnabled(true);
					if (selectedEditor.isMainFile())
						selectedEditor.getPopupMenu().getUnsetMain().setEnabled(true);
					if (!selectedEditor.isCompilableFile()
							|| (selectedEditor.isCompilableFile() && selectedEditor.isMainFile()))
						selectedEditor.getPopupMenu().getSetCompilable().setEnabled(true);
					if (selectedEditor.isCompilableFile() && !selectedEditor.isMainFile())
						selectedEditor.getPopupMenu().getUnsetCompilable()
								.setEnabled(true);

				} else {
					selectedEditor.getPopupMenu().getRemoveFile().setEnabled(false);
					selectedEditor.getPopupMenu().getAddFile().setEnabled(true);
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
