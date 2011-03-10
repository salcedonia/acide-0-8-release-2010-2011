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
import acide.gui.menuBar.editMenu.gui.replace.AcideReplaceWindow;
import acide.gui.menuBar.editMenu.gui.search.AcideSearchWindow;

import java.awt.Toolkit;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Locale;

import acide.utils.OSValidator;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE keyboard listener.
 * 
 * @version 0.8
 * @see KeyAdapter
 */
public class AcideKeyboardListener extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		String selectedText = null;

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// ESCAPE
		if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {

			// If the Search window is focused
			if (AcideSearchWindow.getInstance().isFocused())

				// Closes it
				AcideSearchWindow.getInstance().dispose();

			// If the Replace window is focused
			if (AcideReplaceWindow.getInstance().isFocused())

				// Closes it
				AcideReplaceWindow.getInstance().dispose();
		}

		// ENTER
		if (keyEvent.getKeyCode() == KeyEvent.VK_ENTER) {

			// If the Search window is focused
			if (AcideSearchWindow.getInstance().isFocused())

				// Does the search button action in the search window
				AcideSearchWindow.getInstance().getSearchButton().doClick();

			// If the Search window is focused
			if (AcideReplaceWindow.getInstance().isFocused())

				// Does the search button action in the replace window
				AcideReplaceWindow.getInstance().getSearchButton().doClick();
		}

		// F3 -> FORWARD SEARCH
		if (keyEvent.getKeyCode() == KeyEvent.VK_F3) {

			// Gets the selected in the active editor
			selectedText = AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(selectedFileEditorPanelIndex)
					.getActiveTextEditionArea().getSelectedText();

			// If there is something selected
			if (selectedText != null) {

				// Updates the selected text
				AcideSearchWindow.getInstance().setSearchTextFieldText(
						selectedText);

				// Enables the current document radio button in the search
				// window
				AcideSearchWindow.getInstance().setCurrentDocumentRadioButton(
						true);

				// SHIFT + F3 -> BACKWARD SEARCH
				if (keyEvent.isShiftDown())

					// Enables the backward radio button
					AcideSearchWindow.getInstance()
							.setBackwardRadioButton(true);
				else
					// Enables the forward radio button
					AcideSearchWindow.getInstance().setForwardRadioButton(true);

				// Does the search in the selected direction
				AcideSearchWindow.getInstance().getSearchButton().doClick();
			}
		}

		// SEARCH
		AcideSearchWindow.getInstance().setCurrentPosition(-2);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsEnd(false);
		AcideSearchWindow.getInstance().getSearch().setTemporalPosition(-2);
		AcideSearchWindow.getInstance().getSearch().setIsCycle(false);
		AcideSearchWindow.getInstance().setIsCycle(false);
		AcideSearchWindow.getInstance().setSelectedText(null);
		AcideSearchWindow.setIsFirst(true);

		// If there is selected text
		if (selectedText != null) {

			// Enables the selected text radio button in the search window
			AcideSearchWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Disables the all radio button in the search window
			AcideSearchWindow.getInstance().getAllRadioButton()
					.setEnabled(false);
		} else {

			// Enables the current document radio button in the search window
			AcideSearchWindow.getInstance().getCurrentDocumentRadioButton()
					.setSelected(true);

			// Enables the all radio button in the search window
			AcideSearchWindow.getInstance().getAllRadioButton()
					.setEnabled(true);
		}

		// If the user press CTRL + F in English or CTRL + B in Spanish
		if (((AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN"))) && (keyEvent.isControlDown() && keyEvent
				.getKeyCode() == KeyEvent.VK_F))
				|| ((AcideLanguageManager.getInstance().getCurrentLocale()
						.equals(new Locale("es", "ES"))) && (keyEvent
						.isControlDown() && keyEvent.getKeyCode() == KeyEvent.VK_B))) {

			// Gets the selected text
			selectedText = AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(selectedFileEditorPanelIndex)
					.getActiveTextEditionArea().getSelectedText();

			// If the search window is visible
			if (AcideSearchWindow.getInstance().isVisible())

				// Closes it
				AcideSearchWindow.getInstance().dispose();

			// If there is selected text
			if (selectedText != null) {

				// Enables the selected text radio button in the search window
				AcideSearchWindow.getInstance().getSelectedTextRadioButton()
						.setSelected(true);

				// Disables the all radio button in the search window
				AcideSearchWindow.getInstance().getAllRadioButton()
						.setEnabled(false);

				// Sets the search text field to empty in the search window
				AcideSearchWindow.getInstance().setSearchTextFieldText("");

			} else {

				// Disables the current document radio button in the search
				// window
				AcideSearchWindow.getInstance().getCurrentDocumentRadioButton()
						.setSelected(false);

				// Enables the all radio button in the search window
				AcideSearchWindow.getInstance().getAllRadioButton()
						.setEnabled(true);

				// Sets the search text field to empty in the search window
				AcideSearchWindow.getInstance().setSearchTextFieldText("");
			}
		}

		// REPLACE
		AcideReplaceWindow.getInstance().setCurrentPosition(-2);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsEnd(false);
		AcideReplaceWindow.getInstance().getSearch().setTemporalPosition(-2);
		AcideReplaceWindow.getInstance().getSearch().setIsCycle(false);
		AcideReplaceWindow.getInstance().setIsCycle(false);
		AcideReplaceWindow.getInstance().setSelectedText(null);
		AcideReplaceWindow.setIsFirstSearch(true);
		AcideReplaceWindow.setIsFirstReplacement(true);

		// If there is selected text
		if (selectedText != null) {

			// Enables the selected text radio button in the replace window
			AcideReplaceWindow.getInstance().getSelectedTextRadioButton()
					.setSelected(true);

			// Disables the all radio button in the replace window
			AcideReplaceWindow.getInstance().getAllRadioButton()
					.setEnabled(false);
		} else {

			// Disables the current document radio button in the replace window
			AcideReplaceWindow.getInstance().getCurrentDocumentRadioButton()
					.setSelected(false);

			// Enables the all radio button in the replace window
			AcideReplaceWindow.getInstance().getAllRadioButton()
					.setEnabled(true);
		}

		// If the user press CTRL + L in English or CTRL + R in Spanish
		if (((AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN"))) && (keyEvent.isControlDown() && keyEvent
				.getKeyCode() == KeyEvent.VK_L))
				|| ((AcideLanguageManager.getInstance().getCurrentLocale()
						.equals(new Locale("es", "ES"))) && (keyEvent
						.isControlDown() && keyEvent.getKeyCode() == KeyEvent.VK_R))) {

			// If the replace window is visible
			if (AcideReplaceWindow.getInstance().isVisible())

				// Closes it
				AcideReplaceWindow.getInstance().dispose();

			// Initializes the selected text
			selectedText = null;

			// Gets the selected file editor panel index
			selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Updates the selected text
			selectedText = AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(selectedFileEditorPanelIndex)
					.getActiveTextEditionArea().getSelectedText();

			// If there is selected text
			if (selectedText != null) {

				// Enables the selected text radio button in the replace window
				AcideReplaceWindow.getInstance().getSelectedTextRadioButton()
						.setSelected(true);

				// Disables the all radio button in the replace window
				AcideReplaceWindow.getInstance().getAllRadioButton()
						.setEnabled(false);

				// Sets the replace text field to empty in the replace window
				AcideReplaceWindow.getInstance().setReplaceTextField("");
			} else {

				// Enables the current document radio button in the replace
				// window
				AcideReplaceWindow.getInstance()
						.getCurrentDocumentRadioButton().setSelected(true);

				// Enables the all radio button in the replace window
				AcideReplaceWindow.getInstance().getAllRadioButton()
						.setEnabled(true);

				// Sets the replace text field to empty in the replace window
				AcideReplaceWindow.getInstance().setReplaceTextField("");
			}
		}

		// CAPS LOCK
		if (keyEvent.getKeyCode() == KeyEvent.VK_CAPS_LOCK) {

			// CAPS LOCK only valid in WINDOWS
			if (OSValidator.isWindows()) {

				// Gets the state
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());

				// Always the opposite
				if (!state)
					AcideMainWindow.getInstance().getStatusBar()
							.setCapsLock("CAPS");
				else
					AcideMainWindow.getInstance().getStatusBar()
							.setCapsLock("    ");
			} else
				AcideMainWindow.getInstance().getStatusBar()
						.setCapsLock("    ");
		}

		// NUM LOCK
		if (keyEvent.getKeyCode() == KeyEvent.VK_NUM_LOCK) {

			// NUM LOCK only valid WINDOWS
			if (OSValidator.isWindows()) {

				// Gets the state
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());

				// Always the opposite
				if (!state)
					AcideMainWindow.getInstance().getStatusBar()
							.setNumLockMessage("NUM");
				else
					AcideMainWindow.getInstance().getStatusBar()
							.setNumLockMessage("   ");
			} else
				AcideMainWindow.getInstance().getStatusBar()
						.setNumLockMessage("   ");
		}

		// SCROLL LOCK
		if (keyEvent.getKeyCode() == KeyEvent.VK_SCROLL_LOCK) {

			// SCROLL LOCK only valid in WINDOWS
			if (OSValidator.isWindows()) {

				// Gets the state
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());

				// Always the opposite
				if (!state)
					AcideMainWindow.getInstance().getStatusBar()
							.setScrollLockMessage("SCROLL");
				else
					AcideMainWindow.getInstance().getStatusBar()
							.setScrollLockMessage("     ");
			} else
				AcideMainWindow.getInstance().getStatusBar()
						.setScrollLockMessage("     ");
		}

		// INSERT
		if (keyEvent.isControlDown() && keyEvent.getKeyCode() == KeyEvent.VK_0) {

			if (AcideMainWindow.getInstance().getStatusBar()
					.getEditionModeMessage().equals("INS")) {

				// Updates the insert message in the status bar
				AcideMainWindow.getInstance().getStatusBar()
						.setEditionModeMessage("   ");

			} else {

				// Updates the insert message in the status bar
				AcideMainWindow.getInstance().getStatusBar()
						.setEditionModeMessage("INS");
			}

			// Sets the edition mode to overwrite in all the opened file editor
			// panels
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++){
				
				AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(index).setEditionMode(
						!AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getEditionMode());
			}
		}

		// Validates the changes in the main window
		AcideMainWindow.getInstance().validate();

		// Repaints the changes in the main window
		AcideMainWindow.getInstance().repaint();
	}
}