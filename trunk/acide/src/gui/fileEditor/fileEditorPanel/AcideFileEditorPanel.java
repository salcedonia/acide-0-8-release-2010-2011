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
package gui.fileEditor.fileEditorPanel;

import gui.fileEditor.fileEditorManager.utils.logic.AcideStyledDocument;
import gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.AcideFileEditorTextEditionArea;
import gui.fileEditor.fileEditorPanel.listeners.AcideFileEditorPanelDocumentListener;
import gui.fileEditor.fileEditorPanel.popup.AcideFileEditorPopupMenu;
import gui.mainWindow.MainWindow;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file editor panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideFileEditorPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE file editor panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE file editor panel constant that indicates the
	 * number of text editors for the file editor panel.
	 */
	private static final int NUM_TEXT_EDITORS = 2;
	/**
	 * ACIDE - A Configurable IDE file editor panel text edition area.
	 */
	private ArrayList<AcideFileEditorTextEditionArea> _textEditionAreaList;
	/**
	 * ACIDE - A Configurable IDE file editor panel file path.
	 */
	private String _path;
	/**
	 * ACIDE - A Configurable IDE file editor panel last change in the editor.
	 */
	private long _lastChange;
	/**
	 * ACIDE - A Configurable IDE file editor panel last size of the editor.
	 */
	private long _lastSize;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu.
	 */
	private AcideFileEditorPopupMenu _popup;
	/**
	 * ACIDE - A Configurable IDE file editor panel styled document for the
	 * files.
	 */
	private AcideStyledDocument _styledDocument;
	/**
	 * ACIDE - A Configurable IDE file editor panel document listener.
	 */
	private AcideFileEditorPanelDocumentListener _documentListener;
	/**
	 * ACIDE - A Configurable IDE file editor panel flag that indicates the
	 * active text edition area.
	 */
	private int _activeTextEditionArea;
	/**
	 * ACIDE - A Configurable IDE file editor panel horizontal split panel.
	 */
	private JSplitPane _horizontalSplitPane;
	/**
	 * ACIDE - A Configurable IDE file editor panel flag that indicates if the
	 * file is main.
	 */
	private boolean _isMainFile;
	/**
	 * ACIDE - A Configurable IDE file editor panel flag that indicates if the
	 * file is compilable.
	 */
	private boolean _isCompilableFile;
	/**
	 * ACIDE - A Configurable IDE file editor panel icon.
	 */
	private ImageIcon _icon;
	/**
	 * ACIDE - A Configurable IDE file editor panel file disk copy.
	 * 
	 * Used to determine the color of the close button.
	 */
	private String _fileDiskCopy;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor panel.
	 */
	public AcideFileEditorPanel() {

		super();

		try {

			// Initializes the variables
			_activeTextEditionArea = 1;

			// Updates the log
			AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s317"));

			// Creates the text edition panel list
			_textEditionAreaList = new ArrayList<AcideFileEditorTextEditionArea>();

			// Creates the syntax document
			_styledDocument = new AcideStyledDocument();
			_styledDocument.addDocumentListener(_documentListener);
			
			// Builds the popup menu
			buildPopupMenu();

			// Sets the layout
			setLayout(new BorderLayout());

			// Creates the number of text editors
			for (int index = 0; index < NUM_TEXT_EDITORS; index++) {

				AcideFileEditorTextEditionArea textEditionPanel = new AcideFileEditorTextEditionArea(
						_styledDocument);
				_textEditionAreaList.add(textEditionPanel);
			}

			// Creates the split horizontal pane
			_horizontalSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
					_textEditionAreaList.get(0).getScrollPane(),
					_textEditionAreaList.get(1).getScrollPane());
			_horizontalSplitPane.setDividerLocation(0);
			_horizontalSplitPane.setContinuousLayout(true);

			add(_horizontalSplitPane);

			_path = null;
			_lastChange = 0;
			_lastSize = 0;

			// Updates the log
			AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s318"));

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s319"));
			exception.printStackTrace();
		}
	}

	/**
	 * Builds the file ACIDE - A Configurable IDE file editor panel popup menu.
	 */
	public void buildPopupMenu() {
		_popup = new AcideFileEditorPopupMenu();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel active text
	 * edition panel.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel active text
	 *         edition panel.
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
				.setCaretPosition(_styledDocument.getLength());
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.getDocument().addDocumentListener(_documentListener);

		// Updates the file disk copy with the new content
		setFileDiskCopy(text);

		// Updates the MAIN WINDOW
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
			textContent = _styledDocument.getText(0,
					_styledDocument.getLength());
		} catch (BadLocationException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		AcideStyledDocument syntaxDocument = new AcideStyledDocument();
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setStyledDocument(syntaxDocument);
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setText(textContent);
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setCaretPosition(_styledDocument.getLength());
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.getDocument().addDocumentListener(_documentListener);

		// Updates the file disk copy with the new content
		setFileDiskCopy(textContent);

		// Updates the MAIN WINDOW
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
	 * Returns the ACIDE - A Configurable IDE file editor panel text edition
	 * area content.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel text edition
	 *         area content.
	 */
	public String getTextEditionAreaContent() {
		return _textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.getText();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor panel file
	 * path.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setAbsolutePath(String path) {

		// Updates the path
		_path = path;

		// Once we know the path we can inform to the syntax document
		// in order to get the file editor panel which provokes the
		// change in the document listener
		_styledDocument.setFileEditorPanelName(getFileName());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel absolute file
	 * path.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel absolute file
	 *         path.
	 */
	public String getAbsolutePath() {
		return _path;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel relative file
	 * path.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel relative file
	 *         path.
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
	 * Returns the ACIDE - A Configurable IDE file editor panel file extension.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel file extension.
	 */
	public String getFileExtension() {
		return _path.substring(_path.lastIndexOf(".") + 1);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel file name.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel file name.
	 */
	public String getFileName() {

		if (_path != null) {
			int index = _path.lastIndexOf("\\");
			if (index == -1)
				index = _path.lastIndexOf("/");

			// If the file doesn't have an extension
			if (_path.lastIndexOf(".") == -1)
				return _path.substring(index + 1, _path.length());
			return _path.substring(index + 1, _path.lastIndexOf("."));
		}
		return "";
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel file name, extension included.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel file name, extension included.
	 */
	public String getFileNameWithExtension() {

		if (_path != null) {
			int index = _path.lastIndexOf("\\");
			if (index == -1)
				index = _path.lastIndexOf("/");

			return _path.substring(index + 1, _path.length());
		}
		return "";
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel last change.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel last change.
	 */
	public long getLastChange() {
		return _lastChange;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor panel last
	 * change.
	 * 
	 * @param lastChange
	 *            New value to set.
	 */
	public void setLastChange(long lastChange) {
		_lastChange = lastChange;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel last size.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel last size.
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
	 * Returns the ACIDE - A Configurable IDE file editor panel is compilable
	 * file flag.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel is compilable
	 *         file flag.
	 */
	public boolean isCompilableFile() {
		return _isCompilableFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor panel is
	 * compilable flag.
	 * 
	 * @param compilableFile
	 *            new value to set.
	 */
	public void setCompilableFile(boolean compilableFile) {
		_isCompilableFile = compilableFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel is main file
	 * flag.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel is main file
	 *         flag.
	 */
	public boolean isMainFile() {
		return _isMainFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor panel is
	 * main file flag.
	 * 
	 * @param mainFile
	 *            new value to set.
	 */
	public void setMainFile(boolean mainFile) {
		_isMainFile = mainFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel icon.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel icon.
	 */
	public ImageIcon getIcon() {
		return _icon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor panel
	 * icon.
	 * 
	 * @param icon
	 *            new value to set.
	 */
	public void setIcon(ImageIcon icon) {
		_icon = icon;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel styled document.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel styled document.
	 */
	public AcideStyledDocument getStyledDocument() {
		return _styledDocument;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel active editor
	 * index.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel active editor
	 *         index.
	 */
	public int getActiveEditorIndex() {
		return _activeTextEditionArea;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu.
	 */
	public AcideFileEditorPopupMenu getPopupMenu() {
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
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
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
	 * Returns the ACIDE - A Configurable IDE file editor panel text edition
	 * panel list.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel text edition
	 *         panel list.
	 */
	public ArrayList<AcideFileEditorTextEditionArea> getTextEditionPanelList() {
		return _textEditionAreaList;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel file disk copy.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel file disk copy.
	 */
	public String getFileDiskCopy() {
		return _fileDiskCopy;
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE file editor panel
	 * file disk copy.
	 * 
	 * @param fileDiskCopy
	 *            new value to set.
	 */
	public void setFileDiskCopy(String fileDiskCopy) {
		_fileDiskCopy = "";
		_fileDiskCopy = _fileDiskCopy + fileDiskCopy;
	}

	/**
	 * Checks if the file content given as a parameter is equal to the file copy
	 * disk. This method is used to determine if the file has been modified in
	 * order to set the green button or the red button to an opened file in the
	 * file editor.
	 * 
	 * @param fileContent
	 *            text to compare within.
	 * 
	 * @return true if the text given as a parameter is equal to the file disk
	 *         copy and false in other case.
	 */
	public boolean isEqualToFileDiskCopy(String fileContent) {

		if (_fileDiskCopy != null)
			return _fileDiskCopy.equals(fileContent);
		return true;
	}
}