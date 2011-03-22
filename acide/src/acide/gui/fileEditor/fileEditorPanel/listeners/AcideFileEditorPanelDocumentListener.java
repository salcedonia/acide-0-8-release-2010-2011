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
package acide.gui.fileEditor.fileEditorPanel.listeners;

import acide.configuration.project.workbench.AcideWorkbenchManager;
import acide.gui.fileEditor.fileEditorManager.utils.logic.AcideStyledDocument;
import acide.gui.mainWindow.AcideMainWindow;

import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * ACIDE - A Configurable IDE file editor panel document listener.
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
		dispatchEvent(documentEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void removeUpdate(DocumentEvent documentEvent) {
		dispatchEvent(documentEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event
	 * .DocumentEvent)
	 */
	@Override
	public void changedUpdate(DocumentEvent documentEvent) {
		dispatchEvent(documentEvent);
	}

	/**
	 * Updates the close button and the modification project state.
	 */
	public void dispatchEvent(final DocumentEvent documentEvent) {

		// If the workbench configuration has been loaded
		if (AcideWorkbenchManager.getInstance().isWorkbenchLoaded()) {

			// Gets the syntax document which contains
			AcideStyledDocument document = (AcideStyledDocument) documentEvent
					.getDocument();

			// Gets the file editor panel index which has to be on focus
			final int fileEditorPanelIndexOnFocus = AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getIndexOfFileEditorPanelByName(
							document.getFileEditorPanelName());

			if (fileEditorPanelIndexOnFocus != -1) {

				// Puts the focus on the tab which provokes the change
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.setSelectedFileEditorPanelAt(
								fileEditorPanelIndexOnFocus);

				// Updates the focus, caret and so on..
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.updatesFileEditorAt(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());

				// Gets the current content
				String fileContent = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(fileEditorPanelIndexOnFocus)
						.getTextEditionAreaContent();

				// If has been changes in the file
				if (!AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(fileEditorPanelIndexOnFocus)
						.isEqualToFileDiskCopy(fileContent)) {

					SwingUtilities.invokeLater(new Runnable() {
						/*
						 * (non-Javadoc)
						 * 
						 * @see java.lang.Runnable#run()
						 */
						@Override
						public void run() {
							// Sets the red icon to the close button
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPaneUI()
									.getCloseButtonAt(
											fileEditorPanelIndexOnFocus)
									.setRedCloseButton();
						}
					});

					// Enables the save menu item
					AcideMainWindow.getInstance().getMenu().getFileMenu()
							.getSaveFileMenuItem().setEnabled(true);
					
					// The file editor manager has been modified
					AcideMainWindow.getInstance().getFileEditorManager()
							.setIsModified(true);
				} else {

					SwingUtilities.invokeLater(new Runnable() {
						/*
						 * (non-Javadoc)
						 * 
						 * @see java.lang.Runnable#run()
						 */
						@Override
						public void run() {
							// Sets the red icon to the close button
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPaneUI()
									.getCloseButtonAt(
											fileEditorPanelIndexOnFocus)
									.setGreenCloseButton();
						}
					});

					// Disables the save menu item
					AcideMainWindow.getInstance().getMenu().getFileMenu()
							.getSaveFileMenuItem().setEnabled(false);

					// The file editor manager has not been modified
					AcideMainWindow.getInstance().getFileEditorManager()
							.setIsModified(false);
				}
			}
			
			// Validates the changes in the main window
			AcideMainWindow.getInstance().validate();
			
			// Updates the main window
			AcideMainWindow.getInstance().repaint();
		}
	}
}
