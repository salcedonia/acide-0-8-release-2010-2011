package operations.listeners;

import gui.MainWindow;
import gui.menu.edit.ReplaceGUI;
import gui.menu.edit.SearchGUI;

import java.awt.Toolkit;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Locale;

import utils.OSValidator;

import language.Language;

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

		Language language = Language.getInstance();

		String selectedText = null;
		int numEditor = MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex();

		if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
			if (SearchGUI.getInstance().isFocused())
				SearchGUI.getInstance().dispose();
			if (ReplaceGUI.getInstance().isFocused())
				ReplaceGUI.getInstance().dispose();
		}

		if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
			if (SearchGUI.getInstance().isFocused())
				SearchGUI.getInstance().getSearchButton().doClick();
			if (ReplaceGUI.getInstance().isFocused())
				ReplaceGUI.getInstance().getSearchButton().doClick();
		}

		if (evt.getKeyCode() == KeyEvent.VK_F3) {

			selectedText = MainWindow.getInstance().getEditorBuilder().getEditorAt(numEditor)
					.getEditor().getSelectedText();
			if (selectedText != null) {
				SearchGUI.getInstance().setSearchTextFieldText(selectedText);
				SearchGUI.getInstance().setCurrentDocumentRadioButton(true);
				SearchGUI.getInstance().setForwardRadioButton(true);
				SearchGUI.getInstance().getSearchButton().doClick();
			}
		}

		// SEARCH
		SearchGUI.getInstance().setCurrentPosition(-2);
		SearchGUI.getInstance().setCycle(false);
		SearchGUI.getInstance().setEnd(false);
		SearchGUI.getInstance().getSearch().setTemporalPosition(-2);
		SearchGUI.getInstance().getSearch().setCycle(false);
		SearchGUI.getInstance().setCycle(false);
		SearchGUI.getInstance().setSelectedText(null);
		SearchGUI.setFirst(true);
		if (selectedText != null) {
			SearchGUI.getInstance().getSelectedRadioButton().setSelected(true);
			SearchGUI.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			SearchGUI.getInstance().getCurrentDocumentRadioButton().setSelected(true);
			SearchGUI.getInstance().getAllRadioButton().setEnabled(true);
		}
		// REPLACE
		ReplaceGUI.getInstance().setCurrentPosition(-2);
		ReplaceGUI.getInstance().setCycle(false);
		ReplaceGUI.getInstance().setEnd(false);
		ReplaceGUI.getInstance().getSearch().setTemporalPosition(-2);
		ReplaceGUI.getInstance().getSearch().setCycle(false);
		ReplaceGUI.getInstance().setCycle(false);
		ReplaceGUI.getInstance().setSelectedText(null);
		ReplaceGUI.setIsFirstSearch(true);
		ReplaceGUI.setIsFirstReplacement(true);
		if (selectedText != null) {
			ReplaceGUI.getInstance().getSelectedRadioButton().setSelected(true);
			ReplaceGUI.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			ReplaceGUI.getInstance().getCurrentDocumentRadioButton().setSelected(false);
			ReplaceGUI.getInstance().getAllRadioButton().setEnabled(true);
		}

		// SEARCH
		if (((language.getCurrentLocale().equals(new Locale("en", "EN"))) && (evt
				.isControlDown() && evt.getKeyCode() == KeyEvent.VK_F))
				|| ((language.getCurrentLocale().equals(new Locale("es", "ES"))) && (evt
						.isControlDown() && evt.getKeyCode() == KeyEvent.VK_B))) {
			selectedText = MainWindow.getInstance().getEditorBuilder().getEditorAt(numEditor)
					.getEditor().getSelectedText();
			if (SearchGUI.getInstance().isVisible())
				SearchGUI.getInstance().dispose();
			if (selectedText != null) {
				SearchGUI.getInstance().getSelectedRadioButton().setSelected(true);
				SearchGUI.getInstance().getAllRadioButton().setEnabled(false);
				SearchGUI.getInstance().setSearchTextFieldText("");

			} else {
				SearchGUI.getInstance().getCurrentDocumentRadioButton().setSelected(false);
				SearchGUI.getInstance().getAllRadioButton().setEnabled(true);
				SearchGUI.getInstance().setSearchTextFieldText("");
			}
		}

		// REPLACE
		if (((language.getCurrentLocale().equals(new Locale("en", "EN"))) && (evt
				.isControlDown() && evt.getKeyCode() == KeyEvent.VK_L))
				|| ((language.getCurrentLocale().equals(new Locale("es", "ES"))) && (evt
						.isControlDown() && evt.getKeyCode() == KeyEvent.VK_R))) {
			if (ReplaceGUI.getInstance().isVisible())
				ReplaceGUI.getInstance().dispose();
			selectedText = null;
			numEditor = MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex();
			selectedText = MainWindow.getInstance().getEditorBuilder().getEditorAt(numEditor)
					.getEditor().getSelectedText();
			if (selectedText != null) {
				ReplaceGUI.getInstance().getSelectedRadioButton().setSelected(true);
				ReplaceGUI.getInstance().getAllRadioButton().setEnabled(false);
				ReplaceGUI.getInstance().setReplaceTextField("");
			} else {
				ReplaceGUI.getInstance().getCurrentDocumentRadioButton().setSelected(true);
				ReplaceGUI.getInstance().getAllRadioButton().setEnabled(true);
				ReplaceGUI.getInstance().setReplaceTextField("");
			}
		}

		// CAPS LOCK
		if (evt.getKeyCode() == KeyEvent.VK_CAPS_LOCK) {

			// CAPS LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						evt.getKeyCode());
				if (state)
					MainWindow.getInstance().getStatusBar().setCapsLock("CAPS");
				else
					MainWindow.getInstance().getStatusBar().setCapsLock("");
			} else
				MainWindow.getInstance().getStatusBar().setCapsLock("");

			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();

		}

		// NUM LOCK
		if (evt.getKeyCode() == KeyEvent.VK_NUM_LOCK) {

			// NUM LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {

				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						evt.getKeyCode());
				if (state)
					MainWindow.getInstance().getStatusBar().setNumLock("NUM");
				else
					MainWindow.getInstance().getStatusBar().setNumLock("");
			} else
				MainWindow.getInstance().getStatusBar().setNumLock("");

			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();
		}

		// SCROLL LOCK
		if (evt.getKeyCode() == KeyEvent.VK_SCROLL_LOCK) {

			// SCROLL LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						evt.getKeyCode());
				if (state)
					MainWindow.getInstance().getStatusBar().setScrollLock("SCROLL");
				else
					MainWindow.getInstance().getStatusBar().setScrollLock("");
			} else
				MainWindow.getInstance().getStatusBar().setScrollLock("");

			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();
		}
	}
}