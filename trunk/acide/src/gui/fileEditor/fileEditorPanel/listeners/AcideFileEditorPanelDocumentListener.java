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
package gui.fileEditor.fileEditorPanel.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.configuration.project.workbench.AcideWorkbenchManager;
import gui.mainWindow.MainWindow;

import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * Editor panel document listener.
 * 
 * @version 0.8
 * @see DocumentListener
 */
public class AcideFileEditorPanelDocumentListener implements DocumentListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void insertUpdate(DocumentEvent documentEvent) {
		updatesEditorAndProjectState(documentEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void removeUpdate(DocumentEvent documentEvent) {
		updatesEditorAndProjectState(documentEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void changedUpdate(DocumentEvent documentEvent) {
		updatesEditorAndProjectState(documentEvent);
	}

	/**
	 * Updates the close button and the modification project state.
	 */
	public void updatesEditorAndProjectState(final DocumentEvent documentEvent) {

		// If the workbench configuration has been loaded
		if (AcideWorkbenchManager.getInstance().isWorkbenchLoaded()) {

			// Gets the selected file editor panel index
			final int selectedFileEditorPanelIndex = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			SwingUtilities.invokeLater(new Runnable() {
				
				@Override
				public void run() {

					if (selectedFileEditorPanelIndex != -1) {

						// Gets the current content
						String fileContent = MainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedFileEditorPanelIndex)
								.getTextEditionAreaContent();

						// If has been changes in the file
						if (!MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(selectedFileEditorPanelIndex)
								.isEqualToFileDiskCopy(fileContent)) {

							// Sets the red icon to the close button
							MainWindow.getInstance().getFileEditorManager()
									.getTestPlaf()
									.getCloseButtonAt(selectedFileEditorPanelIndex)
									.setRedCloseButton();

							// Enables the save as menu item
							MainWindow.getInstance().getMenu().getFile()
									.getSaveFileAs().setEnabled(true);

							// The project configuration has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);
						} else {

							// Sets the red icon to the close button
							MainWindow.getInstance().getFileEditorManager()
									.getTestPlaf()
									.getCloseButtonAt(selectedFileEditorPanelIndex)
									.setGreenCloseButton();

							// Enables the save as menu item
							MainWindow.getInstance().getMenu().getFile()
									.getSaveFileAs().setEnabled(false);

							// The project configuration has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(false);
						}
					}		
				}
			});		
		}
	}
}
