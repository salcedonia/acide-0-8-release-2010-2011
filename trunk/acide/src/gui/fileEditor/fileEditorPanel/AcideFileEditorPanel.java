package gui.fileEditor.fileEditorPanel;

import gui.fileEditor.fileEditorManager.utils.logic.SyntaxDocument;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.AcideFileEditorTextEditionArea;
import gui.fileEditor.fileEditorPanel.listeners.AcideFileEditorPanelDocumentListener;
import gui.fileEditor.fileEditorPanel.popup.AcideEditorPopupMenu;
import gui.mainWindow.MainWindow;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.undo.UndoManager;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * File editor panel of ACIDE - A Configurable IDE.
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
 * @see JPanel
 ***********************************************************************/
public class AcideFileEditorPanel extends JPanel {

	/**
	 * File editor panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Constant that indicates the number of text editors for the file editor
	 * panel.
	 */
	private static final int NUM_TEXT_EDITORS = 2;
	/**
	 * File editor panel text edition area.
	 */
	private ArrayList<AcideFileEditorTextEditionArea> _textEditionAreaList;
	/**
	 * File editor panel file path.
	 */
	private String _path;
	/**
	 * File editor panel last change in the editor.
	 */
	private long _lastChange;
	/**
	 * File editor panel last size of the editor.
	 */
	private long _lastSize;
	/**
	 * File editor panel editor panel popup menu.
	 */
	private AcideEditorPopupMenu _popup;
	/**
	 * File editor panel syntax document for the files.
	 */
	private SyntaxDocument _syntaxDocument;
	/**
	 * File editor panel editor document listener.
	 */
	private AcideFileEditorPanelDocumentListener _documentListener;
	/**
	 * Flag that indicates the active text edition area.
	 */
	private int _activeTextEditionArea;
	/**
	 * File editor panel horizontal split panel.
	 */
	private JSplitPane _horizontalSplitPane;
	/**
	 * Flag that indicates if the file is main.
	 */
	private boolean _isMainFile;
	/**
	 * Flag that indicates if the file is compiled.
	 */
	private boolean _isCompiledFile;
	/**
	 * File editor panel icon.
	 */
	private Icon _icon;
	/**
	 * File editor panel undo manager.
	 */
	private UndoManager _undoManager = new UndoManager();

	/**
	 * Creates a new file editor panel.
	 */
	public AcideFileEditorPanel() {

		super();

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

		try {

			// Initializes the variables
			_activeTextEditionArea = 1;

			// Updates the log
			AcideLog.getLog().info(labels.getString("s317"));

			// Creates the text edition panel list
			_textEditionAreaList = new ArrayList<AcideFileEditorTextEditionArea>();

			// Creates the syntax document
			_syntaxDocument = new SyntaxDocument();

			// Builds the popup menu
			buildPopupMenu();

			// Sets the layout
			setLayout(new BorderLayout());

			// Creates the number of text editors
			for (int index = 0; index < NUM_TEXT_EDITORS; index++) {

				AcideFileEditorTextEditionArea textEditionPanel = new AcideFileEditorTextEditionArea(
						_syntaxDocument);
				_textEditionAreaList.add(textEditionPanel);
			}

			// Creates the split horizontal pane
			_horizontalSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
					_textEditionAreaList.get(0).getScrollPane(),
					_textEditionAreaList.get(1).getScrollPane());
			_horizontalSplitPane.setDividerLocation(0);
			_horizontalSplitPane.setContinuousLayout(true);

			add(_horizontalSplitPane);

			// Updates the log
			AcideLog.getLog().info(labels.getString("s318"));

			_path = null;
			_lastChange = 0;
			_lastSize = 0;

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(labels.getString("s319"));
			exception.printStackTrace();
		}
	}

	/**
	 * Builds the file editor panel popup menu.
	 */
	public void buildPopupMenu() {
		_popup = new AcideEditorPopupMenu();
	}

	/**
	 * Returns the active text edition panel.
	 * 
	 * @return the active text edition panel.
	 */
	public JTextPane getActiveTextEditionArea() {
		return _textEditionAreaList.get(_activeTextEditionArea).getTextPane();
	}

	/**
	 * Loads the text into both editors.
	 * 
	 * @param text
	 *            text to load.
	 */
	public void loadText(String text) {

		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.getDocument().removeDocumentListener(_documentListener);
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setText(text);

		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setCaretPosition(_syntaxDocument.getLength());
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.getDocument().addDocumentListener(_documentListener);

		revalidate();
		repaint();
	}

	/**
	 * Selects a text in both editors.
	 * 
	 * @param start
	 *            selection start.
	 * @param length
	 *            selection length.
	 */
	public void selectText(int start, int length) {

		// Gets the selection
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setSelectionStart(start);
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setSelectionEnd(start + length);

		// Sets the focus in the active text edition panel index
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.requestFocusInWindow();
	}

	/**
	 * Resets the document in a different thread.
	 */
	public void resetDocument() {

		String textContent = "";
		try {
			textContent = _syntaxDocument.getText(0,
					_syntaxDocument.getLength());
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		SyntaxDocument syntaxDocument = new SyntaxDocument();
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setStyledDocument(syntaxDocument);
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setText(textContent);
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setCaretPosition(_syntaxDocument.getLength());
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.getDocument().addDocumentListener(_documentListener);

		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
	}

	/**
	 * Sets the caret at the start of a line.
	 * 
	 * @param line
	 *            line to put the caret on.
	 */
	public void goToLine(int line) {

		JTextPane component = getActiveTextEditionArea();
		Element root = component.getDocument().getDefaultRootElement();
		line = Math.max(line, 1);
		line = Math.min(line, root.getElementCount());
		component.setCaretPosition(root.getElement(line - 1).getStartOffset());
	}

	/**
	 * Returns the text edition area content.
	 * 
	 * @return the text edition area content.
	 */
	public String getTextEditionAreaContent() {
		return _textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.getText();
	}

	/**
	 * Sets a new value to the file path.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setAbsolutePath(String path) {
		_path = path;
	}

	/**
	 * Returns the absolute file path.
	 * 
	 * @return the absolute file path.
	 */
	public String getAbsolutePath() {
		return _path;
	}

	/**
	 * Returns the relative file path.
	 * 
	 * @return the relative file path.
	 */
	public String getFilePath() {

		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");

		// If this is the new or the log tab returns empty path
		// in order to avoid exceptions
		if (index == -1)
			return "";
		return _path.substring(0, index);
	}

	/**
	 * Returns the file extension.
	 * 
	 * @return the file extension.
	 */
	public String getFileExtension() {
		return _path.substring(_path.lastIndexOf(".") + 1);
	}

	/**
	 * Returns the file name.
	 * 
	 * @return the file name.
	 */
	public String getFileName() {
		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");

		// If the file doesn't have an extension
		if (_path.lastIndexOf(".") == -1)
			return _path.substring(index + 1, _path.length());
		return _path.substring(index + 1, _path.lastIndexOf("."));
	}

	/**
	 * Returns the last change.
	 * 
	 * @return the last change.
	 */
	public long getLastChange() {
		return _lastChange;
	}

	/**
	 * Sets a new value to the last change.
	 * 
	 * @param lastChange
	 *            New value to set.
	 */
	public void setLastChange(long lastChange) {
		_lastChange = lastChange;
	}

	/**
	 * Returns the last size.
	 * 
	 * @return the last size.
	 */
	public long getLastSize() {
		return _lastSize;
	}

	/**
	 * Sets the modifiable feature of the editors to false or true.
	 * 
	 * @param editable
	 *            new value to set.
	 */
	public void setEditable(boolean editable) {
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setEditable(editable);
	}

	/**
	 * Sets a new value to the last size.
	 * 
	 * @param lastSize
	 *            new value to set.
	 */
	public void setLastSize(long lastSize) {
		_lastSize = lastSize;
	}

	/**
	 * Returns the is compiler file flag.
	 * 
	 * @return the is compiler file flag.
	 */
	public boolean isCompilerFile() {
		return _isCompiledFile;
	}

	/**
	 * Sets a new value to the is compiler flag.
	 * 
	 * @param compilerFile
	 *            new value to set.
	 */
	public void setCompilerFile(boolean compilerFile) {
		_isCompiledFile = compilerFile;
	}

	/**
	 * Returns the is main file flag.
	 * 
	 * @return the is main file flag.
	 */
	public boolean isMainFile() {
		return _isMainFile;
	}

	/**
	 * Sets a new value to the is main file flag.
	 * 
	 * @param mainFile
	 *            new value to set.
	 */
	public void setMainFile(boolean mainFile) {
		_isMainFile = mainFile;
	}

	/**
	 * Returns the icon.
	 * 
	 * @return the icon.
	 */
	public Icon getIcon() {
		return _icon;
	}

	/**
	 * Sets a new value to the icon.
	 * 
	 * @param icon
	 *            new value to set.
	 */
	public void setIcon(Icon icon) {
		_icon = icon;
	}

	/**
	 * Returns the syntax document.
	 * 
	 * @return the syntax document.
	 */
	public SyntaxDocument getSyntaxDocument() {
		return _syntaxDocument;
	}

	/**
	 * Returns the active editor index.
	 * 
	 * @return the active editor index.
	 */
	public int getActiveEditorIndex() {
		return _activeTextEditionArea;
	}

	/**
	 * Returns the popup menu.
	 * 
	 * @return the popup menu.
	 */
	public AcideEditorPopupMenu getPopupMenu() {
		return _popup;
	}

	/**
	 * Returns true if the opened file in the editor is the new file and false
	 * in other case.
	 * 
	 * @return true if the opened file in the editor is the new file and false
	 *         in other case.
	 */
	public boolean isNewFile() {

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

		return _path.equals(labels.getString("s79"));
	}

	/**
	 * Returns true if the opened file in the editor is the log tab and false in
	 * other case.
	 * 
	 * @return true if the opened file in the editor is the log tab and false in
	 *         other case.
	 */
	public boolean isLogFile() {
		return _path.equals("Log");
	}

	/**
	 * Sets a new value to the active editor.
	 * 
	 * @param activeEditor
	 *            new value to set.
	 */
	public void setActiveEditor(int activeEditor) {
		_activeTextEditionArea = activeEditor;
	}

	/**
	 * Returns the text edition panel list.
	 * 
	 * @return the text edition panel list.
	 */
	public ArrayList<AcideFileEditorTextEditionArea> getTextEditionPanelList() {
		return _textEditionAreaList;
	}

	/**
	 * Returns the undo-redo manager.
	 * 
	 * @return the undo-redo manager.
	 * @see UndoManager
	 */
	public UndoManager getUndoManager() {
		return _undoManager;
	}

	/**
	 * Sets a new value to the undo-redo manager.
	 * 
	 * @param undoManager
	 *            new value to set.
	 * @see UndoManager
	 */
	public void setUndoManager(UndoManager undoManager) {
		_undoManager = undoManager;
	}
}