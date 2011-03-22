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
package acide.gui.fileEditor.fileEditorManager.listeners;

import acide.gui.mainWindow.AcideMainWindow;

import javax.swing.JTabbedPane;
import javax.swing.JTextPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.Utilities;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file editor manager change listener.
 * 
 * Implements the actions taken when the active tab has changed in the tabbed
 * pane.
 * 
 * @version 0.8
 * @see ChangeListener
 */
public class AcideFileEditorManagerChangeListener implements ChangeListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent
	 * )
	 */
	@Override
	public void stateChanged(ChangeEvent changeEvent) {

		// gets the tabbed pane
		JTabbedPane tabbedPane = AcideMainWindow.getInstance()
				.getFileEditorManager().getTabbedPane();

		// makes sure that the tabbed pane is valid
		if (tabbedPane == null)
			return;

		// Updates the menu bar
		updateMenuBar(tabbedPane.getSelectedIndex());

		// Updates the status bar with the new info
		updateStatusBar(tabbedPane.getSelectedIndex());
	}

	/**
	 * Updates the menu bar depending on the number of opened file editors.
	 * 
	 * @param selectedIndex
	 *            selected tab index in the tabbed pane.
	 */
	private void updateMenuBar(int selectedIndex) {

		if (selectedIndex == -1) {

			// Disables the file menu
			AcideMainWindow.getInstance().getMenu().disableFileMenu();

			// Disables the edit menu
			AcideMainWindow.getInstance().getMenu().disableEditMenu();

			// Disables the lexicon menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLexiconMenu().setEnabled(false);

			// Disables the grammar menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().setEnabled(false);

		} else {

			// Enables the file menu
			AcideMainWindow.getInstance().getMenu().enableFileMenu();

			// Enables the edit menu
			AcideMainWindow.getInstance().getMenu().enableEditMenu();

			// Enables the lexicon menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLexiconMenu().setEnabled(true);

			// Enables the grammar menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().setEnabled(true);

			// If the current grammar configuration is lastModified or newGrammar
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(selectedIndex)
					.getCurrentGrammarConfiguration().getName()
					.matches("newGrammar")
					|| AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedIndex)
							.getCurrentGrammarConfiguration().getName()
							.matches("lastModified"))

				// Enables the save grammar menu
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getGrammarMenu().getSaveGrammarMenuItem().setEnabled(true);
			else
				// Disables the save grammar menu
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getGrammarMenu().getSaveGrammarMenuItem().setEnabled(false);
		}
	}

	/**
	 * Updates the number of lines and the line and column message in the status
	 * bar.
	 * 
	 * @param selectedTabIndex
	 *            selected tab index in the tabbed pane.
	 */
	public void updateStatusBar(int selectedTabIndex) {

		if (selectedTabIndex != -1) {

			// Gets the active text edition area
			JTextPane activeTextPane = AcideMainWindow.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(selectedTabIndex)
					.getActiveTextEditionArea();

			// Gets the line of the caret position
			int caretLine = getCaretLine(activeTextPane.getCaretPosition(),
					activeTextPane);

			// Gets the column of the caret position
			int caretColumn = getCaretColumn(activeTextPane.getCaretPosition(),
					activeTextPane);

			// Gets the root element from the styled document
			Element rootElement = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getStyledDocument().getDefaultRootElement();

			// Gets its number of lines
			int numLines = rootElement.getElementCount();

			// Updates the line and column message in the status
			// bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setLineAndColumnMessage(
							(caretLine + 1) + ":" + (caretColumn + 1));

			// Updates the number of lines message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setNumberOfLinesMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s1001")
									+ numLines);

			// Updates the lexicon message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setLexiconMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s449")
									+ " "
									+ AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(
													selectedTabIndex)
											.getLexiconConfiguration()
											.getName());

			// Updates the grammar message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s248")
									+ " "
									+ AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(
													selectedTabIndex)
											.getCurrentGrammarConfiguration()
											.getName());

		} else {

			// Updates the line and column message in the status
			// bar
			AcideMainWindow.getInstance().getStatusBar()
					.setLineAndColumnMessage(" ");

			// Updates the number of lines message in the status bar
			AcideMainWindow.getInstance().getStatusBar()
					.setNumberOfLinesMessage(" ");

			// Updates the lexicon message in the status bar
			AcideMainWindow.getInstance().getStatusBar().setLexiconMessage(" ");

			// Updates the grammar message in the status bar
			AcideMainWindow.getInstance().getStatusBar().setGrammarMessage(" ");
		}

		// Updates the status message in the status bar
		AcideMainWindow.getInstance().getStatusBar()
				.updateStatusMessageFromFileEditor();
	}

	/**
	 * Returns the caret line in the selected file editor text area.
	 * 
	 * @param caretPosition
	 *            current caret position.
	 * @param textPane
	 *            file editor text area.
	 * 
	 * @return the caret line in the selected file editor text area.
	 */
	public static int getCaretLine(int caretPosition, JTextComponent textPane) {
		int line = (caretPosition == 0) ? 1 : 0;

		try {
			int offset = caretPosition;
			while (offset > 0) {
				offset = Utilities.getRowStart(textPane, offset) - 1;
				line++;
			}
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
		return line - 1;
	}

	/**
	 * Returns the caret column in the selected file editor text area.
	 * 
	 * @param caretPosition
	 *            current caret position.
	 * @param textPane
	 *            file editor text area.
	 * 
	 * @return the caret column in the selected file editor text area.
	 */
	public static int getCaretColumn(int caretPosition, JTextComponent textPane) {

		try {
			return caretPosition
					- Utilities.getRowStart(textPane, caretPosition);
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
		return -1;
	}
}
