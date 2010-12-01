package operations.listeners;

import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.gui.replace.ReplaceWindow;
import gui.menuBar.editMenu.gui.search.SearchWindow;

import java.awt.Toolkit;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Locale;

import utils.OSValidator;

import language.AcideLanguage;

/************************************************************************																
 * ACIDE - A Configurable IDE keyboard listener.
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see KeyAdapter																													
 ***********************************************************************/
public class AcideKeyboardListener extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		String selectedText = null;
		int numEditor = MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanelIndex();

		// ESCAPE
		if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {
			if (SearchWindow.getInstance().isFocused())
				SearchWindow.getInstance().dispose();
			if (ReplaceWindow.getInstance().isFocused())
				ReplaceWindow.getInstance().dispose();
		}

		// ENTER
		if (keyEvent.getKeyCode() == KeyEvent.VK_ENTER) {
			if (SearchWindow.getInstance().isFocused())
				SearchWindow.getInstance().getSearchButton().doClick();
			if (ReplaceWindow.getInstance().isFocused())
				ReplaceWindow.getInstance().getSearchButton().doClick();
		}

		// F3 -> FORWARD SEARCH
		if (keyEvent.getKeyCode() == KeyEvent.VK_F3) {

			selectedText = MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(numEditor)
					.getActiveTextEditionArea().getSelectedText();
			if (selectedText != null) {
				SearchWindow.getInstance().setSearchTextFieldText(selectedText);
				SearchWindow.getInstance().setCurrentDocumentRadioButton(true);
				SearchWindow.getInstance().setForwardRadioButton(true);
				SearchWindow.getInstance().getSearchButton().doClick();
			}
		}

		// SHIFT + F3 -> BACKWARD SEARCH
		if (keyEvent.isShiftDown() && keyEvent.getKeyCode() == KeyEvent.VK_F3) {

			selectedText = MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(numEditor)
					.getActiveTextEditionArea().getSelectedText();
			if (selectedText != null) {
				SearchWindow.getInstance().setSearchTextFieldText(selectedText);
				SearchWindow.getInstance().setCurrentDocumentRadioButton(true);
				SearchWindow.getInstance().setBackwardRadioButton(true);
				SearchWindow.getInstance().getSearchButton().doClick();
			}
		}
		
		// SEARCH
		SearchWindow.getInstance().setCurrentPosition(-2);
		SearchWindow.getInstance().setIsCycle(false);
		SearchWindow.getInstance().setIsEnd(false);
		SearchWindow.getInstance().getSearch().setTemporalPosition(-2);
		SearchWindow.getInstance().getSearch().setIsCycle(false);
		SearchWindow.getInstance().setIsCycle(false);
		SearchWindow.getInstance().setSelectedText(null);
		SearchWindow.setIsFirst(true);
		if (selectedText != null) {
			SearchWindow.getInstance().getSelectedTextRadioButton().setSelected(true);
			SearchWindow.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			SearchWindow.getInstance().getCurrentDocumentRadioButton().setSelected(true);
			SearchWindow.getInstance().getAllRadioButton().setEnabled(true);
		}
		// REPLACE
		ReplaceWindow.getInstance().setCurrentPosition(-2);
		ReplaceWindow.getInstance().setIsCycle(false);
		ReplaceWindow.getInstance().setIsEnd(false);
		ReplaceWindow.getInstance().getSearch().setTemporalPosition(-2);
		ReplaceWindow.getInstance().getSearch().setIsCycle(false);
		ReplaceWindow.getInstance().setIsCycle(false);
		ReplaceWindow.getInstance().setSelectedText(null);
		ReplaceWindow.setIsFirstSearch(true);
		ReplaceWindow.setIsFirstReplacement(true);
		if (selectedText != null) {
			ReplaceWindow.getInstance().getSelectedTextRadioButton().setSelected(true);
			ReplaceWindow.getInstance().getAllRadioButton().setEnabled(false);
		} else {
			ReplaceWindow.getInstance().getCurrentDocumentRadioButton().setSelected(false);
			ReplaceWindow.getInstance().getAllRadioButton().setEnabled(true);
		}

		// SEARCH
		if (((language.getCurrentLocale().equals(new Locale("en", "EN"))) && (keyEvent
				.isControlDown() && keyEvent.getKeyCode() == KeyEvent.VK_F))
				|| ((language.getCurrentLocale().equals(new Locale("es", "ES"))) && (keyEvent
						.isControlDown() && keyEvent.getKeyCode() == KeyEvent.VK_B))) {
			selectedText = MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(numEditor)
					.getActiveTextEditionArea().getSelectedText();
			if (SearchWindow.getInstance().isVisible())
				SearchWindow.getInstance().dispose();
			if (selectedText != null) {
				SearchWindow.getInstance().getSelectedTextRadioButton().setSelected(true);
				SearchWindow.getInstance().getAllRadioButton().setEnabled(false);
				SearchWindow.getInstance().setSearchTextFieldText("");

			} else {
				SearchWindow.getInstance().getCurrentDocumentRadioButton().setSelected(false);
				SearchWindow.getInstance().getAllRadioButton().setEnabled(true);
				SearchWindow.getInstance().setSearchTextFieldText("");
			}
		}

		// REPLACE
		if (((language.getCurrentLocale().equals(new Locale("en", "EN"))) && (keyEvent
				.isControlDown() && keyEvent.getKeyCode() == KeyEvent.VK_L))
				|| ((language.getCurrentLocale().equals(new Locale("es", "ES"))) && (keyEvent
						.isControlDown() && keyEvent.getKeyCode() == KeyEvent.VK_R))) {
			if (ReplaceWindow.getInstance().isVisible())
				ReplaceWindow.getInstance().dispose();
			selectedText = null;
			numEditor = MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanelIndex();
			selectedText = MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(numEditor)
					.getActiveTextEditionArea().getSelectedText();
			if (selectedText != null) {
				ReplaceWindow.getInstance().getSelectedTextRadioButton().setSelected(true);
				ReplaceWindow.getInstance().getAllRadioButton().setEnabled(false);
				ReplaceWindow.getInstance().setReplaceTextField("");
			} else {
				ReplaceWindow.getInstance().getCurrentDocumentRadioButton().setSelected(true);
				ReplaceWindow.getInstance().getAllRadioButton().setEnabled(true);
				ReplaceWindow.getInstance().setReplaceTextField("");
			}
		}

		// CAPS LOCK
		if (keyEvent.getKeyCode() == KeyEvent.VK_CAPS_LOCK) {

			// CAPS LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());
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
		if (keyEvent.getKeyCode() == KeyEvent.VK_NUM_LOCK) {

			// NUM LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {

				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());
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
		if (keyEvent.getKeyCode() == KeyEvent.VK_SCROLL_LOCK) {

			// SCROLL LOCK ONLY VALID IN WINDOWS
			if (OSValidator.isWindows()) {
				boolean state = Toolkit.getDefaultToolkit().getLockingKeyState(
						keyEvent.getKeyCode());
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