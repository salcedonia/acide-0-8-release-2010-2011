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
package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import java.util.ResourceBundle;

import gui.fileEditor.fileEditorManager.utils.logic.MatchingBraces;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

import language.AcideLanguageManager;

import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file editor text edition area caret listener.
 * 
 * @version 0.8
 * @see CaretListener
 */
public class AcideFileEditorTextEditionAreaCaretListener implements
		CaretListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent
	 * )
	 */
	@Override
	public void caretUpdate(CaretEvent caretEvent) {
	
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
		ResourceBundle labels = language.getLabels();

		// Get selected editor
		AcideFileEditorPanel selectedEditor = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel();

		// If the editor1 is focused
		if (selectedEditor.getTextEditionPanelList().get(0).getTextPane()
				.isFocusOwner())

			// The active editor is the editor1
			selectedEditor.setActiveEditor(0);

		// If the editor2 is focused
		if (selectedEditor.getTextEditionPanelList().get(1).getTextPane()
				.isFocusOwner())

			// The active editor is the editor2
			selectedEditor.setActiveEditor(1);

		// Gets the root element
		Element rootElement = selectedEditor.getStyledDocument()
				.getDefaultRootElement();
		
		// Gets the dot
		int dot = caretEvent.getDot();
		
		// Gets the line
		int line = rootElement.getElementIndex(dot);
		
		// Gets the column
		int column = dot - rootElement.getElement(line).getStartOffset();

		// Updates the line and columns message in the status bar
		MainWindow.getInstance().getStatusBar()
				.setLineAndColumnMessage((line + 1) + ":" + (column + 1));

		// Updates the number of lines message in the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setNumberOfLinesMessage(
						labels.getString("s1001")
								+ rootElement.getElementCount());

		// Updates the braces
		try {
			if (selectedEditor.getTextEditionPanelList().get(0)
					.getBraceMatcher() != -1) {
				selectedEditor.getStyledDocument().removeHighlightBrace(
						selectedEditor.getTextEditionPanelList().get(0)
								.getBraceMatcher());
				selectedEditor.getTextEditionPanelList().get(0)
						.setBraceMatcher(-1);
			}
			if (selectedEditor.getTextEditionPanelList().get(1)
					.getBraceMatcher() != -1) {
				selectedEditor.getStyledDocument().removeHighlightBrace(
						selectedEditor.getTextEditionPanelList().get(1)
								.getBraceMatcher());
				selectedEditor.getTextEditionPanelList().get(1)
						.setBraceMatcher(-1);
			}

			int start = selectedEditor.getActiveTextEditionArea()
					.getCaretPosition();
			int end;

			// Selects the end
			if (start == 0)
				end = MatchingBraces.findMatchingBracket(selectedEditor
						.getActiveTextEditionArea().getDocument(), start);
			else
				end = MatchingBraces.findMatchingBracket(selectedEditor
						.getActiveTextEditionArea().getDocument(), start - 1);

			if (((start > 0) && (start <= selectedEditor.getStyledDocument()
					.getLength()))
					&& ((end >= 0) && (end <= selectedEditor
							.getStyledDocument().getLength()))) {

				if (end > -1) {

					if (end > start) {

						selectedEditor.getTextEditionPanelList().get(0)
								.setBraceMatcher(start - 1);
						selectedEditor.getTextEditionPanelList().get(1)
								.setBraceMatcher(end);
						selectedEditor.getStyledDocument().addHighlightBrace(
								selectedEditor.getTextEditionPanelList().get(0)
										.getBraceMatcher());
						selectedEditor.getStyledDocument().addHighlightBrace(
								selectedEditor.getTextEditionPanelList().get(1)
										.getBraceMatcher());
					} else if (end < start) {
						selectedEditor.getTextEditionPanelList().get(0)
								.setBraceMatcher(end);
						selectedEditor.getTextEditionPanelList().get(1)
								.setBraceMatcher(start - 1);
						selectedEditor.getStyledDocument().addHighlightBrace(
								selectedEditor.getTextEditionPanelList().get(0)
										.getBraceMatcher());
						selectedEditor.getStyledDocument().addHighlightBrace(
								selectedEditor.getTextEditionPanelList().get(1)
										.getBraceMatcher());
					}
				}
			}
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
