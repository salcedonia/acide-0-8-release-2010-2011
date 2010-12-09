package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import java.util.ResourceBundle;

import gui.fileEditor.fileEditorManager.utils.logic.MatchingBraces;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

import language.AcideLanguage;

import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Editor panel caret listener.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
 * @see CaretListener
 ***********************************************************************/
public class AcideTextEditorTextEditionAreaCaretListener implements CaretListener {

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
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
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

		Element rootElement = selectedEditor.getSyntaxDocument()
				.getDefaultRootElement();
		int dot = caretEvent.getDot();
		int line = rootElement.getElementIndex(dot);
		int col = dot - rootElement.getElement(line).getStartOffset();
		int numLines = rootElement.getElementCount();

		String lineColMessage = (line + 1) + ":" + (col + 1);
		
		// Updates the status bar
		MainWindow.getInstance().getStatusBar().getLineColMessage()
		.setColumns(lineColMessage.length());

		MainWindow.getInstance().getStatusBar().getLineColMessage()
				.setText(lineColMessage);

		String numLinesMessage = labels.getString("s1001") + numLines;

		// Updates the status bar
		MainWindow.getInstance().getStatusBar().getNumLinesMessage()
				.setColumns(numLinesMessage.length());
		MainWindow.getInstance().getStatusBar().getNumLinesMessage()
				.setText(numLinesMessage);
		
		try {
			if (selectedEditor.getTextEditionPanelList().get(0)
					.getBraceMatcher() != -1) {
				selectedEditor.getSyntaxDocument().removeBrace(
						selectedEditor.getTextEditionPanelList().get(0)
								.getBraceMatcher());
				selectedEditor.getTextEditionPanelList().get(0).setBraceMatcher(-1);
			}
			if (selectedEditor.getTextEditionPanelList().get(1)
					.getBraceMatcher() != -1) {
				selectedEditor.getSyntaxDocument().removeBrace(
						selectedEditor.getTextEditionPanelList().get(1)
								.getBraceMatcher());
				selectedEditor.getTextEditionPanelList().get(1).setBraceMatcher(-1);
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

			if (((start > 0) && (start <= selectedEditor.getSyntaxDocument()
					.getLength()))
					&& ((end >= 0) && (end <= selectedEditor
							.getSyntaxDocument().getLength()))) {

				if (end > -1) {

					if (end > start) {
						
						selectedEditor.getTextEditionPanelList().get(0).setBraceMatcher(start - 1);
						selectedEditor.getTextEditionPanelList().get(1).setBraceMatcher(end);
						selectedEditor.getSyntaxDocument().setBrace(
								selectedEditor.getTextEditionPanelList().get(0).getBraceMatcher());
						selectedEditor.getSyntaxDocument().setBrace(
								selectedEditor.getTextEditionPanelList().get(1).getBraceMatcher());
					} else if (end < start) {
						selectedEditor.getTextEditionPanelList().get(0).setBraceMatcher(end);
						selectedEditor.getTextEditionPanelList().get(1).setBraceMatcher(start - 1);
						selectedEditor.getSyntaxDocument().setBrace(
								selectedEditor.getTextEditionPanelList().get(0).getBraceMatcher());
						selectedEditor.getSyntaxDocument().setBrace(
								selectedEditor.getTextEditionPanelList().get(1).getBraceMatcher());
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
