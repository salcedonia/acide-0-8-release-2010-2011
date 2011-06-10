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

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.gui.AcideReplaceWindow;
import acide.gui.menuBar.editMenu.gui.AcideSearchWindow;

/**
 * ACIDE - A Configurable IDE search and replace windows keyboard listener.
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
	public void keyPressed(KeyEvent keyEvent) {

		String selectedText = null;

		// Gets the selected file editor panel index
		int selectedFileEditorPanel = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// If it is the ESCAPE key
		if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {

			// If the ACIDE - A Configurable IDE search window is focused
			if (AcideSearchWindow.getInstance().isFocused())

				// Closes it
				AcideSearchWindow.getInstance().dispose();

			// If the ACIDE - A Configurable IDE replace window is focused
			if (AcideReplaceWindow.getInstance().isFocused())

				// Closes it
				AcideReplaceWindow.getInstance().dispose();
		}

		// If it is the ENTER key
		if (keyEvent.getKeyCode() == KeyEvent.VK_ENTER) {

			// If the ACIDE - A Configurable IDE search window is focused
			if (AcideSearchWindow.getInstance().isFocused())

				// Performs its search button action
				AcideSearchWindow.getInstance().getSearchButton().doClick();

			// If the ACIDE - A Configurable IDE replace window is focused
			if (AcideReplaceWindow.getInstance().isFocused())

				// Performs its search button action
				AcideReplaceWindow.getInstance().getSearchButton().doClick();
		}

		// If it is the F3 key
		if (keyEvent.getKeyCode() == KeyEvent.VK_F3) {

			// Gets the selected text
			selectedText = AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(selectedFileEditorPanel)
					.getActiveTextEditionArea().getSelectedText();

			// If there is selected text
			if (selectedText != null) {

				// Updates the search text field
				AcideSearchWindow.getInstance().getSearchTextField()
						.setText(selectedText);

				// Selects the current document radio button
				AcideSearchWindow.getInstance().setCurrentDocumentRadioButton(
						true);

				// If the shift is pressed down
				if (keyEvent.isShiftDown()) {

					// BACKWARDS search
					AcideSearchWindow.getInstance().getBackwardRadioButton()
							.setSelected(true);

				} else {

					// FORWARDS search
					AcideSearchWindow.getInstance().getForwardRadioButton()
							.setSelected(true);
				}

				// Performs the search button action
				AcideSearchWindow.getInstance().getSearchButton().doClick();
			}
		}

		// Updates the ACIDE - A Configurable IDE search window variables
		AcideSearchWindow.getInstance().setCurrentPosition(-2);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsEnd(false);
		AcideSearchWindow.getInstance().getSearchEngine()
				.setTemporalPosition(-2);
		AcideSearchWindow.getInstance().getSearchEngine().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setSelectedText(null);
		AcideSearchWindow.setIsFirst(true);

		// If there is selected text
		if (selectedText != null) {

			// Selects the selected text radio button in the ACIDE - A
			// Configurable IDE search window
			AcideSearchWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Deselects the both directions radio button in the ACIDE - A
			// Configurable IDE search window
			AcideSearchWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(false);
		} else {

			// Selects the current document radio button in the ACIDE - A
			// Configurable IDE search window
			AcideSearchWindow.getInstance().getCurrentDocumentRadioButton()
					.setSelected(true);

			// Selects the both directions radio button in the ACIDE - A
			// Configurable IDE search window
			AcideSearchWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(true);
		}

		// Updates the ACIDE - A Configurable IDE search window variables
		AcideReplaceWindow.getInstance().setCurrentPosition(-2);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsEnd(false);
		AcideReplaceWindow.getInstance().getSearchEngine()
				.setTemporalPosition(-2);
		AcideReplaceWindow.getInstance().getSearchEngine().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setSelectedText(null);
		AcideReplaceWindow.setIsFirst(true);
		AcideReplaceWindow.setIsFirstReplacement(true);

		// If there is selected text
		if (selectedText != null) {

			// Selects the selected text radio button in the ACIDE - A
			// Configurable IDE replace window
			AcideReplaceWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Deselects the both directions radio button in the ACIDE - A
			// Configurable IDE replace window
			AcideReplaceWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(false);
		} else {

			// Deselects the current document radio button in the ACIDE - A
			// Configurable IDE replace window
			AcideReplaceWindow.getInstance().getCurrentDocumentRadioButton()
					.setSelected(false);

			// Selects the both directions radio button in the ACIDE - A
			// Configurable IDE replace window
			AcideReplaceWindow.getInstance().getBothDirectionsRadioButton()
					.setEnabled(true);
		}
	}
}