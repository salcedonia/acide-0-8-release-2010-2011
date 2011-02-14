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

import gui.fileEditor.fileEditorManager.utils.logic.ElementMatcher;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

import operations.log.AcideLog;

import language.AcideLanguageManager;

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

		// Get selected file editor panel
		final AcideFileEditorPanel selectedFileEditorPanel = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel();

		// If the first text editor is focused
		if (selectedFileEditorPanel.getTextEditionPanelList().get(0).getTextPane()
				.isFocusOwner())
			// The active editor is the first text editor
			selectedFileEditorPanel.setActiveEditor(0);

		// If the second editor is focused
		if (selectedFileEditorPanel.getTextEditionPanelList().get(1).getTextPane()
				.isFocusOwner())
			// The active editor is the second text editor
			selectedFileEditorPanel.setActiveEditor(1);

		// Gets the root element
		Element rootElement = selectedFileEditorPanel.getStyledDocument()
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
						AcideLanguageManager.getInstance().getLabels()
								.getString("s1001")
								+ rootElement.getElementCount());

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				// Updates the matching braces
				updatesMatchingBraces(selectedFileEditorPanel);
			}
		});
	}

	/**
	 * Updates the matching braces in the text edition area. All the matching
	 * braces are removed and added afterwards.
	 * 
	 * @param selectedFileEditorPanel
	 *            current selected file editor panel.
	 */
	public void updatesMatchingBraces(
			final AcideFileEditorPanel selectedFileEditorPanel) {

		// If the first edition text area has a matching brace
		if (selectedFileEditorPanel.getTextEditionPanelList().get(0)
				.getMatchingBracePosition() != -1) {
			
			// Removes the highlighting of the matching brace
			selectedFileEditorPanel.getStyledDocument().removeHighlightBrace(
					selectedFileEditorPanel.getTextEditionPanelList().get(0)
							.getMatchingBracePosition());

			// Sets the matching brace position as -1
			selectedFileEditorPanel.getTextEditionPanelList().get(0)
					.setMatchingBracePosition(-1);
		}

		// If the second edition text area has a matching brace
		if (selectedFileEditorPanel.getTextEditionPanelList().get(1)
				.getMatchingBracePosition() != -1) {

			// Removes the highlighting of the matching brace
			selectedFileEditorPanel.getStyledDocument().removeHighlightBrace(
					selectedFileEditorPanel.getTextEditionPanelList().get(1)
							.getMatchingBracePosition());

			// Sets the matching brace position as -1
			selectedFileEditorPanel.getTextEditionPanelList().get(1)
					.setMatchingBracePosition(-1);
		}

		try {

			// Gets the selected brace position in the text
			int start = selectedFileEditorPanel.getActiveTextEditionArea()
					.getCaretPosition();
			int end;

			// Selects the matching end position avoiding to reach out of the
			// bounds
			if (start == 0)
				end = ElementMatcher.findMatchingBracket(
						selectedFileEditorPanel.getActiveTextEditionArea()
								.getDocument(), start);
			else
				end = ElementMatcher.findMatchingBracket(
						selectedFileEditorPanel.getActiveTextEditionArea()
								.getDocument(), start - 1);

			// If are inside the bounds
			if (((start > 0) && (start <= selectedFileEditorPanel
					.getStyledDocument().getLength()))
					&& ((end >= 0) && (end <= selectedFileEditorPanel
							.getStyledDocument().getLength()))) {

				if (end != start) {

					// If the matching brace is below the selected brace in
					// the text
					if (end > start) {

						// Set the matching brace position in the text
						// edition area
						selectedFileEditorPanel.getTextEditionPanelList()
								.get(0).setMatchingBracePosition(start - 1);
						// Set the matching brace position in the text
						// edition area
						selectedFileEditorPanel.getTextEditionPanelList()
								.get(1).setMatchingBracePosition(end);
					}
					// If the matching brace is above the selected brace in
					// the text
					else if (end < start) {

						// Set the matching brace position in the text
						// edition area
						selectedFileEditorPanel.getTextEditionPanelList()
								.get(0).setMatchingBracePosition(end);
						// Set the matching brace position in the text
						// edition area
						selectedFileEditorPanel.getTextEditionPanelList()
								.get(1).setMatchingBracePosition(start - 1);					
					}
					
					// Highlights the matching brace in the first text
					// edition panel
					selectedFileEditorPanel.getStyledDocument()
							.addHighlightBrace(
									selectedFileEditorPanel
											.getTextEditionPanelList()
											.get(0)
											.getMatchingBracePosition());

					// Highlights the matching brace in the second text
					// edition panel
					selectedFileEditorPanel.getStyledDocument()
							.addHighlightBrace(
									selectedFileEditorPanel
											.getTextEditionPanelList()
											.get(1)
											.getMatchingBracePosition());
				}
			}
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
