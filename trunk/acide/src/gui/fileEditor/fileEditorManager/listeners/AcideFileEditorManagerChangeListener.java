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
package gui.fileEditor.fileEditorManager.listeners;

import java.util.ResourceBundle;

import gui.mainWindow.MainWindow;

import javax.swing.JTabbedPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.Utilities;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file editor manager change listener.
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

		// Gets the tabbed pane
		JTabbedPane tabbedPane = MainWindow.getInstance()
				.getFileEditorManager().getTabbedPane();

		// make sure that the tabbed Pane is valid
		if (tabbedPane == null)
			return;

		final int index = tabbedPane.getSelectedIndex();

		// i returns -1 if nothing selected
		if (index < 0)
			return;

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		final int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		SwingUtilities.invokeLater(new Runnable() {

			@Override
			public void run() {

				if (selectedFileEditorPanelIndex != -1) {

					if (MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index) != null) {
						
						// Gets the active text edition area
						final JTextPane activeTextPane = MainWindow
								.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index)
								.getActiveTextEditionArea();

						// Gets the line of the caret position
						int line = getCaretLine(
								activeTextPane.getCaretPosition(),
								activeTextPane);

						// Gets the column of the caret position
						int col = getCaretColumn(
								activeTextPane.getCaretPosition(),
								activeTextPane);

						// Updates the number of lines message in the status Bar
						Element rootElement = MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getStyledDocument().getDefaultRootElement();
						int numLines = rootElement.getElementCount();

						// Updates the the line and column message in the status
						// bar
						MainWindow
								.getInstance()
								.getStatusBar()
								.setLineAndColumnMessage(
										(line + 1) + ":" + (col + 1));

						// Updates the number of lines message in the status bar
						MainWindow
								.getInstance()
								.getStatusBar()
								.setNumberOfLinesMessage(
										labels.getString("s1001") + numLines);
					}
				}
			}
		});
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
			exception.printStackTrace();
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
			exception.printStackTrace();
		}
		return -1;
	}
}
