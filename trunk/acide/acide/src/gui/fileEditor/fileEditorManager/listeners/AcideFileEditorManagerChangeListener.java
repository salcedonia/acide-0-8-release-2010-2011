package gui.fileEditor.fileEditorManager.listeners;

import java.util.ResourceBundle;

import gui.mainWindow.MainWindow;

import javax.swing.JTabbedPane;
import javax.swing.JTextPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.Utilities;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * ACIDE - A Configurable IDE file editor manager change listener.
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
 * @see ChangeListener
 ***********************************************************************/
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

		final JTextPane activeTextPane = MainWindow.getInstance()
				.getFileEditorManager().getFileEditorPanelAt(index)
				.getActiveTextEditionArea();
		
		// Gets the line of the caret position
		int line = getCaretLine(activeTextPane.getCaretPosition(),
				activeTextPane);

		// Gets the column of the caret position
		int col = getCaretColumn(activeTextPane.getCaretPosition(),
				activeTextPane);

		// Updates the Status Bar
		Element rootElement = MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getSyntaxDocument()
				.getDefaultRootElement();
		int numLines = rootElement.getElementCount();

		// Updates the status bar
		MainWindow.getInstance().getStatusBar().getLineColMessage()
				.setText((line + 1) + ":" + (col + 1));

		String numLinesMessage = labels.getString("s1001") + numLines;

		// Updates the status bar
		MainWindow.getInstance().getStatusBar().getNumLinesMessage()
				.setColumns(numLinesMessage.length() + 10);
		MainWindow.getInstance().getStatusBar().getNumLinesMessage()
				.setText(numLinesMessage);
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
