package operations.listeners;

import gui.MainWindow;
import gui.menu.edit.ReplaceGUI;
import gui.menu.edit.SearchGUI;

import java.awt.Toolkit;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Locale;

import language.Language;

/**
 * 
 */
public class AcideKeyboardListener extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@SuppressWarnings("static-access")
	public void keyPressed(KeyEvent evt) {

		SearchGUI searchGUI = SearchGUI.getInstance();
		ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
		MainWindow mainWindow = MainWindow.getInstance();
		Language language = Language.getInstance();
		Toolkit toolkit = Toolkit.getDefaultToolkit();
		String t = null;
		int numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();

		if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
			if (searchGUI.isFocused() == true)
				searchGUI.dispose();
			if (replaceGUI.isFocused() == true)
				replaceGUI.dispose();
		}

		if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
			if (searchGUI.isFocused() == true)
				searchGUI.getBtnSearch().doClick();
			if (replaceGUI.isFocused() == true)
				replaceGUI.getBuscar().doClick();
		}

		if (evt.getKeyCode() == KeyEvent.VK_F3) {

			t = mainWindow.getEditorBuilder().getEditorAt(numEditor).getEditor()
					.getSelectedText();
			if (t != null) {
				searchGUI.setT1(t);
				searchGUI.setRdBtnCurrentDocument(true);
				searchGUI.setRdBtnForward(true);
				searchGUI.getBtnSearch().doClick();
			}
		}

		// Search
		searchGUI.setCurrentPosition(-2);
		searchGUI.setCycle(false);
		searchGUI.setEnd(false);
		searchGUI.getSearch().setTemporalPosition(-2);
		searchGUI.getSearch().setCycle(false);
		searchGUI.setCycle(false);
		searchGUI.setSelectedText(null);
		SearchGUI.setFirst(true);
		if (t != null) {
			searchGUI.getRdBtnSelected().setSelected(true);
			searchGUI.getRdBtnAll().setEnabled(false);
		} else {
			searchGUI.getRdBtnCurrentDocument().setSelected(true);
			searchGUI.getRdBtnAll().setEnabled(true);
		}
		// Replace
		replaceGUI.setCurrentPosition(-2);
		replaceGUI.setCycle(false);
		replaceGUI.setEnd(false);
		replaceGUI.getB().setTemporalPosition(-2);
		replaceGUI.getB().setCycle(false);
		replaceGUI.setCycle(false);
		replaceGUI.setSelectedText(null);
		ReplaceGUI.setFirst(true);
		ReplaceGUI.setFirstReplacement(true);
		if (t != null) {
			replaceGUI.getSelected().setSelected(true);
			replaceGUI.getRadioTodo().setEnabled(false);
		} else {
			replaceGUI.getCurrentDocument().setSelected(false);
			replaceGUI.getRadioTodo().setEnabled(true);
		}

		if (((language.getCurrentLocale().equals(new Locale("en", "EN"))) && (evt
				.isControlDown() && evt.getKeyCode() == KeyEvent.VK_F))
				|| ((language.getCurrentLocale().equals(new Locale("es", "ES"))) && (evt
						.isControlDown() && evt.getKeyCode() == KeyEvent.VK_B))) {
			t = mainWindow.getEditorBuilder().getEditorAt(numEditor).getEditor()
					.getSelectedText();
			if (searchGUI.isVisible())
				searchGUI.dispose();
			if (t != null) {
				// Search
				searchGUI.getRdBtnSelected().setSelected(true);
				searchGUI.getRdBtnAll().setEnabled(false);
				searchGUI.setT1("");

			} else {
				searchGUI.getRdBtnCurrentDocument().setSelected(false);
				searchGUI.getRdBtnAll().setEnabled(true);
				searchGUI.setT1("");

			}
		}

		if (((language.getCurrentLocale().equals(new Locale("en", "EN"))) && (evt
				.isControlDown() && evt.getKeyCode() == KeyEvent.VK_L))
				|| ((language.getCurrentLocale().equals(new Locale("es", "ES"))) && (evt
						.isControlDown() && evt.getKeyCode() == KeyEvent.VK_R))) {
			if (replaceGUI.isVisible())
				replaceGUI.dispose();
			t = null;
			numEditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
			t = mainWindow.getEditorBuilder().getEditorAt(numEditor).getEditor()
					.getSelectedText();
			if (t != null) {
				// Replace
				replaceGUI.getSelected().setSelected(true);
				replaceGUI.getRadioTodo().setEnabled(false);
				replaceGUI.setT1("");
			} else {
				// Replace
				replaceGUI.getCurrentDocument().setSelected(true);
				replaceGUI.getRadioTodo().setEnabled(true);
				replaceGUI.setT1("");
			}

		}

		if (evt.getKeyCode() == KeyEvent.VK_CAPS_LOCK) {
			boolean state = toolkit.getLockingKeyState(evt.getKeyCode());
			if (state)
				mainWindow.getStatusBar().setCapsLock("CAPS");
			else
				mainWindow.getStatusBar().setCapsLock("");

			mainWindow.validate();
			mainWindow.repaint();

		}

		if (evt.getKeyCode() == KeyEvent.VK_NUM_LOCK) {
			boolean state = toolkit.getLockingKeyState(evt.getKeyCode());
			if (state)
				mainWindow.getStatusBar().setNumLock("NUM");
			else
				mainWindow.getStatusBar().setNumLock("");

			mainWindow.validate();
			mainWindow.repaint();

		}

		if (evt.getKeyCode() == KeyEvent.VK_SCROLL_LOCK) {
			boolean state = toolkit.getLockingKeyState(evt.getKeyCode());
			if (state)
				mainWindow.getStatusBar().setScrollLock("SCROLL");
			else
				mainWindow.getStatusBar().setScrollLock("");

			mainWindow.validate();
			mainWindow.repaint();
		}
	}
}