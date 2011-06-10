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

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.fileEditor.fileEditorPanel.AcideStyledDocument;
import acide.gui.mainWindow.AcideMainWindow;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * <p>
 * ACIDE - A Configurable IDE file editor panel document listener.
 * </p>
 * <p>
 * Checks the name of the file editor panel which provokes the document event,
 * selects it in the tabbed pane (if any) and updates its closing button
 * depending on the state of its file content. The document event can be
 * provoked by an undoable edit event, not only from the document currently in
 * use. With this we get the effect on changing between tabs when the undo or
 * redo events affect to another opened tab without the focus.
 * </p>
 * <p>
 * To determine if a file has been modified, the current text is compared with
 * the disk copy of the file.
 * </p>
 * <p>
 * Additionally, the static tool bar is also updated, enabling or disabling the
 * save file and save all files button, depending on the new state of the
 * closing button in the selected file editor panel.
 * </p>
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

		// Gets the syntax document which contains
		AcideStyledDocument document = (AcideStyledDocument) documentEvent
				.getDocument();

		// If the workbench configuration has been loaded
		if (AcideWorkbenchConfiguration.getInstance().isWorkbenchLoaded()) {

			// Gets the file editor panel index which has to be on focus
			int indexOnFocus = AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt((String) document.getProperty("name"));

			// If there is any
			if (indexOnFocus != -1) {

				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanelIndex() != indexOnFocus)

					// Sets the selected file editor
					AcideMainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(indexOnFocus);

				// Selects the tree node
				AcideMainWindow.getInstance().getExplorerPanel()
						.selectTreeNodeFromFileEditor();

				// Gets the current content
				String fileContent = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(indexOnFocus)
						.getTextEditionAreaContent();

				// If has been changes in the file
				if (!AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(indexOnFocus)
						.isEqualToFileDiskCopy(fileContent)) {

					// Sets the red icon to the close button
					AcideMainWindow.getInstance().getFileEditorManager()
							.setRedButtonAt(indexOnFocus);

				} else {

					// Sets the green icon to the close button
					AcideMainWindow.getInstance().getFileEditorManager()
							.setGreenButtonAt(indexOnFocus);
				}

				// Updates the save project in the menu bar tool bar
				AcideMainWindow.getInstance().getToolBarPanel()
						.getMenuBarToolBar().updateStateOfFileButtons();
			}
		}
	}
}
