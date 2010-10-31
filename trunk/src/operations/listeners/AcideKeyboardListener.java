package operations.listeners;

import gui.MainWindow;
import gui.menu.edit.ReplaceGUI;
import gui.menu.edit.SearchGUI;

import java.awt.Toolkit;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Locale;

import language.Language;
import main.OSValidator;

/**
 * Listener for the Keyboard events in the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class AcideKeyboardListener extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	public void keyPressed(KeyEvent evt) {

		SearchGUI searchGUI = SearchGUI.getInstance();
		ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
		MainWindow mainWindow = MainWindow.getInstance();
		Language language = Language.getInstance();

		String selectedText = null;
		int numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();

		if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
			if (searchGUI.isFocused() == true)
				searchGUI.dispose();
			if (replaceGUI.isFocused() == true)
				replaceGUI.dispose();
		}

		if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
			if (searchGUI.isFocused() == true)
				searchGUI.getSearchButton().doClick();
			if (replaceGUI.isFocused() == true)
				replaceGUI.getSearchButton().doClick();
		}

		if (evt.getKeyCode() == KeyEvent.VK_F3) {

			selectedText = mainWindow.getEditorBuilder().getEditorAt(numEditor)
					.getEditor().getSelectedText();
			if (selectedText != null) {
				searchGUI.setSearchTextFieldText(selectedText);
				searchGUI.setCurrentDocumentRadioButton(true);
				searchGUI.setForwardRadioButton(true);
				searchGUI.getSearchButton().doClick();
			}
		}

		// SEARCH
		searchGUI.setCurrentPosition(-2);
		searchGUI.setCycle(false);
		searchGUI.setEnd(false);
		searchGUI.getSearch().setTemporalPosition(-2);
		searchGUI.getSearch().setCycle(false);
		searchGUI.setCycle(false);
		searchGUI.setSelectedText(null);
		SearchGUI.setFirst(true);
		if (selectedText != null) {
			searchGUI.getSelectedRadioButton().setSelected(true);
			searchGUI.getAllRadioButton().setEnabled(false);
		} else {
			searchGUI.getCurrentDocumentRadioButton().setSelected(true);
			searchGUI.getAllRadioButton().setEnabled(true);
		}
		// REPLACE
		replaceGUI.setCurrentPosition(-2);
		replaceGUI.setCycle(false);
		replaceGUI.setEnd(false);
		replaceGUI.getSearch().setTemporalPosition(-2);
		replaceGUI.getSearch().setCycle(false);
		replaceGUI.setCycle(false);
		replaceGUI.setSelectedText(null);
		ReplaceGUI.setIsFirstSearch(true);
		ReplaceGUI.setIsFirstReplacement(true);
		if (selectedText != null) {
			replaceGUI.getSelectedRadioButton().setSelected(true);
			replaceGUI.getAllRadioButton().setEnabled(false);
		} else {
			replaceGUI.getCurrentDocumentRadioButton().setSelected(false);
			replaceGUI.getAllRadioButton().setEnabled(true);
		}

		// SEARCH
		if (((language.getCurrentLocale().equals(new Locale("en", "EN"))) && (evt
				.isControlDown() && evt.getKeyCode() == KeyEvent.VK_F))
				|| ((language.getCurrentLocale().equals(new Locale("es", "ES"))) && (evt
						.isControlDown() && evt.getKeyCode() == KeyEvent.VK_B))) {
			selectedText = mainWindow.getEditorBuilder().getEditorAt(numEditor)
					.getEditor().getSelectedText();
			if (searchGUI.isVisible())
				searchGUI.dispose();
			if (selectedText != null) {
				searchGUI.getSelectedRadioButton().setSelected(true);
				searchGUI.getAllRadioButton().setEnabled(false);
				searchGUI.setSearchTextFieldText("");

			} else {
				searchGUI.getCurrentDocumentRadioButton().setSelected(false);
				searchGUI.getAllRadioButton().setEnabled(true);
				searchGUI.setSearchTextFieldText("");
			}
		}

		// REPLACE
		if (((language.getCurrentLocale().equals(new Locale("en", "EN"))) && (evt
				.isControlDown() && evt.getKeyCode() == KeyEvent.VK_L))
				|| ((language.getCurrentLocale().equals(new Locale("es", "ES"))) && (evt
						.isControlDown() && evt.getKeyCode() == KeyEvent.VK_R))) {
			if (replaceGUI.isVisible())
				replaceGUI.dispose();
			selectedText = null;
			numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
			selectedText = mainWindow.getEditorBuilder().getEditorAt(numEditor)
					.getEditor().getSelectedText();
			if (selectedText != null) {
				replaceGUI.getSelectedRadioButton().setSelected(true);
				replaceGUI.getAllRadioButton().setEnabled(false);
				replaceGUI.setReplaceTextField("");
			} else {
				replaceGUI.getCurrentDocumentRadioButton().setSelected(true);
				replaceGUI.getAllRadioButton().setEnabled(true);
				replaceGUI.setReplaceTextField("");
			}
		}

		// CAPS LOCK
		if (evt.getKeyCode() == KeyEvent.VK_CAPS_LOCK) {

			// CAPS LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						evt.getKeyCode());
				if (state)
					mainWindow.getStatusBar().setCapsLock("CAPS");
				else
					mainWindow.getStatusBar().setCapsLock("");
			} else
				mainWindow.getStatusBar().setCapsLock("");

			mainWindow.validate();
			mainWindow.repaint();

		}

		// NUM LOCK
		if (evt.getKeyCode() == KeyEvent.VK_NUM_LOCK) {

			// NUM LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {

				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						evt.getKeyCode());
				if (state)
					mainWindow.getStatusBar().setNumLock("NUM");
				else
					mainWindow.getStatusBar().setNumLock("");
			} else
				mainWindow.getStatusBar().setNumLock("");

			mainWindow.validate();
			mainWindow.repaint();
		}

		// SCROLL LOCK
		if (evt.getKeyCode() == KeyEvent.VK_SCROLL_LOCK) {

			// SCROLL LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						evt.getKeyCode());
				if (state)
					mainWindow.getStatusBar().setScrollLock("SCROLL");
				else
					mainWindow.getStatusBar().setScrollLock("");
			} else
				mainWindow.getStatusBar().setScrollLock("");

			mainWindow.validate();
			mainWindow.repaint();
		}
	}
}