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
package acide.gui.listeners;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.gui.AcideSearchReplaceWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.SwingUtilities;

/**
 * ACIDE - A Configurable IDE search/replace window keyboard listener.
 * 
 * @version 0.8
 * @see KeyAdapter
 */
public class AcideSearchReplaceWindowKeyboardListener extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(final KeyEvent keyEvent) {

		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				// If there are opened file editor panels
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getNumberOfFileEditorPanels() > 0) {

					// Gets the selected text in the active editor
					String selectedText = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().getSelectedText();

					// Initializes the variables in the search/replace window
					AcideSearchReplaceWindow.getInstance()
							.initializeVariables();

					// If the F3 key is pressed down -> FORWARD SEARCH
					if (keyEvent.getKeyCode() == KeyEvent.VK_F3) {

						// If there is something selected
						if (selectedText != null) {

							// Updates the selected text in the search/replace
							// window
							AcideSearchReplaceWindow.getInstance()
									.getSearchTextField().setText(selectedText);

							// Selects the current document radio button in the
							// search/replace
							// window
							AcideSearchReplaceWindow.getInstance()
									.getCurrentDocumentRadioButton()
									.setSelected(true);

							// If the shift key + F3 key are pressed down ->
							// BACKWARD
							// SEARCH
							if (keyEvent.isShiftDown())

								// Enables the backward radio button in the
								// search/replace
								// window
								AcideSearchReplaceWindow.getInstance()
										.getBackwardRadioButton()
										.setSelected(true);
							else
								// Enables the forward radio button in the
								// search/replace
								// window
								AcideSearchReplaceWindow.getInstance()
										.getForwardRadioButton()
										.setSelected(true);

							// Does the search in the selected direction in the
							// search/replace window
							AcideSearchReplaceWindow.getInstance()
									.getSearchButton().doClick();
						}
					}

					// If there is selected text
					if (selectedText != null) {

						// Sets the selected text radio button in the
						// search/replace
						// window as selected
						AcideSearchReplaceWindow.getInstance()
								.getSelectedTextRadioButton().setSelected(true);

						// Enables the selected text radio button in the
						// search/replace
						// window
						AcideSearchReplaceWindow.getInstance()
								.getSelectedTextRadioButton()
								.setEnabled(true);
						
						// Disables the all radio button in the search/replace
						// window
						AcideSearchReplaceWindow.getInstance()
								.getBothDirectionsRadioButton()
								.setEnabled(false);

						// Updates the search text field with the selected text
						// in
						// the
						// search/replace window
						AcideSearchReplaceWindow.getInstance()
								.getSearchTextField().setText(selectedText);
					} else {

						// Sets the current document radio button in the
						// search/replace
						// window as selected
						AcideSearchReplaceWindow.getInstance()
								.getCurrentDocumentRadioButton()
								.setSelected(true);

						// Disables the selected text radio button in the
						// search/replace
						// window
						AcideSearchReplaceWindow.getInstance()
								.getSelectedTextRadioButton()
								.setEnabled(false);
						
						// Enables the all radio button in the search/replace
						// window
						AcideSearchReplaceWindow.getInstance()
								.getBothDirectionsRadioButton()
								.setEnabled(true);

						// Sets the replace text field to empty in the
						// search/replace
						// window
						AcideSearchReplaceWindow.getInstance()
								.getSearchTextField().setText("");
					}
				}
			}
		});
	}
}