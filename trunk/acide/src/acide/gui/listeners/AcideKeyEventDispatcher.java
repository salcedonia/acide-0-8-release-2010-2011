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

import java.awt.KeyEventDispatcher;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.gui.AcideReplaceWindow;
import acide.gui.menuBar.editMenu.gui.AcideSearchWindow;
import acide.utils.AcideOSChecker;

/**
 * <p>
 * ACIDE - A Configurable IDE key event dispatcher.
 * </p>
 * <p>
 * Dispatches all the key events in ACIDE - A Configurable IDE, avoiding 
 * to add key listeners everywhere in the code.
 * </p>
 * 
 * @version 0.8
 * @see KeyEventDispatcher
 */
public class AcideKeyEventDispatcher implements KeyEventDispatcher {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.KeyEventDispatcher#dispatchKeyEvent(java.awt.event.KeyEvent)
	 */
	@Override
	public boolean dispatchKeyEvent(KeyEvent keyEvent) {

		// Dispatches the lock keys
		dispatchLockKeys();

		// Dispatches the search and replace windows keys
		dispatchSearchReplaceKeys(keyEvent);
		
		return false;
	}

	/**
	 * Dispatches the search and replace windows keys.
	 * 
	 * @param keyEvent
	 *            key event.
	 */
	private void dispatchSearchReplaceKeys(KeyEvent keyEvent) {

		String selectedText = null;

		// Gets the selected file editor panel index
		int selectedFileEditorPanel = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// If it is the ESCAPE key
		if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {

			// If the ACIDE - A Configurable IDE search window is focused
			if (AcideSearchWindow.getInstance().isFocused())

				// Closes the ACIDE - A Configurable IDE search window
				AcideSearchWindow.getInstance().dispose();

			// If the ACIDE - A Configurable IDE replace window is focused
			if (AcideReplaceWindow.getInstance().isFocused())

				// Closes the ACIDE - A Configurable IDE replace window
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
		AcideSearchWindow.getInstance().initializeVariables();

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
		AcideReplaceWindow.getInstance().initializeVariables();

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

	/**
	 * Dispatches the system block keys events, it means, SCROLL, NUM and CAPS
	 * LOCK keys.
	 */
	private void dispatchLockKeys() {

		Toolkit toolkit = Toolkit.getDefaultToolkit();

		// CAPS LOCK only valid in WINDOWS
		if (AcideOSChecker.isWindows()) {

			// Gets the SCROLL LOCK state
			boolean isScrollLockOn = toolkit
					.getLockingKeyState(KeyEvent.VK_SCROLL_LOCK);

			// Gets the NUM LOCK state
			boolean isNumLockOn = toolkit
					.getLockingKeyState(KeyEvent.VK_NUM_LOCK);

			// Gets the CAPS LOCK state
			boolean isCapsLockOn = toolkit
					.getLockingKeyState(KeyEvent.VK_CAPS_LOCK);

			// Updates the ACIDE - A Configurable IDE status bar scroll lock message
			if (isScrollLockOn)
				AcideMainWindow.getInstance().getStatusBar()
						.setScrollLockMessage("SCROLL");
			else
				AcideMainWindow.getInstance().getStatusBar()
						.setScrollLockMessage("      ");

			// Updates the ACIDE - A Configurable IDE status bar num lock message
			if (isNumLockOn)
				AcideMainWindow.getInstance().getStatusBar()
						.setNumLockMessage("NUM");
			else
				AcideMainWindow.getInstance().getStatusBar()
						.setNumLockMessage("   ");

			// Updates the ACIDE - A Configurable IDE status bar caps lock message
			if (isCapsLockOn)
				AcideMainWindow.getInstance().getStatusBar()
						.setCapsLockMessage("CAPS");
			else
				AcideMainWindow.getInstance().getStatusBar()
						.setCapsLockMessage("    ");

		} else {
			AcideMainWindow.getInstance().getStatusBar()
					.setScrollLockMessage("      ");
		}
	}
}
