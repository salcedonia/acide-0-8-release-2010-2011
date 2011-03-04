/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.Element;

import language.AcideLanguageManager;
import operations.log.AcideLog;

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
	private AcideFileEditorPopupMenu _popupMenu;
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

			// Sets the layout
			setLayout(new BorderLayout());

			// Initializes the variables
			_activeTextEditionArea = 1;
			_path = null;
			_lastChange = 0;
			_lastSize = 0;

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s317"));

			// Creates the text edition panel list
			_textEditionAreaList = new ArrayList<AcideFileEditorTextEditionArea>();

			// Creates the document listener
			_documentListener = new AcideFileEditorPanelDocumentListener();

			// Creates the syntax document
			_styledDocument = new AcideStyledDocument();
			_styledDocument.addDocumentListener(_documentListener);

			// Builds the popup menu
			buildPopupMenu();

			// Creates the number of text editors
			for (int index = 0; index < NUM_TEXT_EDITORS; index++) {

				_textEditionAreaList.add(new AcideFileEditorTextEditionArea(
						_styledDocument));
			}

			// Creates the split horizontal pane
			_horizontalSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
					_textEditionAreaList.get(0).getScrollPane(),
					_textEditionAreaList.get(1).getScrollPane());
			_horizontalSplitPane.setDividerLocation(0);
			_horizontalSplitPane.setContinuousLayout(true);

			add(_horizontalSplitPane);

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s318"));

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s319"));
			exception.printStackTrace();
		}
	}

	/**
	 * Builds the file ACIDE - A Configurable IDE file editor panel popup menu.
	 */
	public void buildPopupMenu() {
		_popupMenu = new AcideFileEditorPopupMenu();
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
	 * Sets the file content given as a parameter into all the text edition
	 * areas of the file editor panel.
	 * 
	 * @param fileContent
	 *            file content to set.
	 */
	public void setFileContent(String fileContent) {

		for (int index = 0; index < NUM_TEXT_EDITORS; index++) {

			// Removes the document listener
			_textEditionAreaList.get(index).getTextPane().getDocument()
					.removeDocumentListener(_documentListener);

			// Sets the text content
			_textEditionAreaList.get(index).getTextPane().setText(fileContent);

			// Sets the caret position at the end of the file
			_textEditionAreaList.get(index).getTextPane()
					.setCaretPosition(_styledDocument.getLength());

			// Adds the document listener
			_textEditionAreaList.get(index).getTextPane().getDocument()
					.addDocumentListener(_documentListener);
		}

		// Updates the file disk copy with the new content
		setFileDiskCopy(fileContent);

		// Validates the changes in the main window
		revalidate();

		// Repaints the main window
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

		// Sets the selection start
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setSelectionStart(start);

		// Sets the selection end
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.setSelectionEnd(start + length);

		// Sets the focus in the active text edition panel index
		_textEditionAreaList.get(_activeTextEditionArea).getTextPane()
				.requestFocusInWindow();
	}

	/**
	 * Resets the styled document of the text edition areas in the file editor
	 * panel.
	 */
	public void resetStyledDocument(final int caretPosition) {

		// Initializes
		_styledDocument.init();

		// Validates the changes in the main window
		MainWindow.getInstance().validate();

		// Repaints the main window
		MainWindow.getInstance().repaint();

		/*
		 * Note: After the repainting it is mandatory to update the caret
		 * position because it goes automatically to the first position in order
		 * to apply the changes in the text edition areas.
		 */

		// Sets the caret position
		setCaretPosition(caretPosition);

		// Sets the caret as visible
		setCaretVisible(true);

		// Puts the focus on it
		putFocusOnActiveTextArea();
	}

	/**
	 * Sets the caret visible in both text edition areas.
	 * 
	 * @param isVisible
	 *            indicates if the caret has to be hidden or shown.
	 */
	public void setCaretVisible(final boolean isVisible) {

		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				getActiveTextEditionArea().getCaret().setVisible(isVisible);
			}
		});
	}

	/**
	 * Sets the caret position in both text edition areas.
	 * 
	 * @param caretPosition
	 *            new value to set.
	 */
	public void setCaretPosition(final int caretPosition) {

		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				getActiveTextEditionArea().setCaretPosition(caretPosition);
			}
		});
	}

	/**
	 * Puts the focus on the active text area.
	 */
	public void putFocusOnActiveTextArea() {

		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				getActiveTextEditionArea().requestFocusInWindow();
			}
		});
	}

	/**
	 * Sets the caret at the start of a line.
	 * 
	 * @param line
	 *            line to put the caret on.
	 */
	public void goToLine(int line) {

		// Gets the active text edition area
		JTextPane activeTextEditionArea = getActiveTextEditionArea();

		// Calculates the line
		Element root = activeTextEditionArea.getDocument()
				.getDefaultRootElement();
		line = Math.max(line, 1);
		line = Math.min(line, root.getElementCount());

		// Puts the caret at the beginning of the line
		activeTextEditionArea.setCaretPosition(root.getElement(line - 1)
				.getStartOffset());
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
		// in order to get the file editor panel which fires the
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
	 * Returns the ACIDE - A Configurable IDE file editor panel file name,
	 * extension included.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel file name,
	 *         extension included.
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
		return _popupMenu;
	}

	/**
	 * Returns true if the opened file in the editor is the new file and false
	 * in other case.
	 * 
	 * @return true if the opened file in the editor is the new file and false
	 *         in other case.
	 */
	public boolean isNewFile() {
		return _path.equals(AcideLanguageManager.getInstance().getLabels()
				.getString("s79"));
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
	 * Returns the ACIDE - A Configurable IDE file editor panel horizontal split
	 * panel.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel horizontal split
	 *         panel.
	 */
	public JSplitPane getHorizontalSplitPane() {
		return _horizontalSplitPane;
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

	/**
	 * Applies the display options to the file editor in both text edition
	 * areas.
	 * 
	 * @param font
	 *            new font to set.
	 * @param backgroundColor
	 *            background color to set.
	 * @param foregroundColor
	 *            foreground color to set.
	 */
	public void setDisplayOptions(Font font, Color backgroundColor,
			Color foregroundColor) {

		for (int index = 0; index < NUM_TEXT_EDITORS; index++) {

			// Updates the line number panel font
			_textEditionAreaList.get(index).getLineNumberPanel().setFont(font);

			// Sets the font
			_textEditionAreaList.get(index).getTextPane().setFont(font);

			// Sets the foreground color
			_textEditionAreaList.get(index).getTextPane()
					.setForeground(foregroundColor);

			// Sets the background color
			_textEditionAreaList.get(index).getTextPane()
					.setBackground(backgroundColor);

			// Sets the caret color
			_textEditionAreaList.get(index).getTextPane()
					.setCaretColor(foregroundColor);
		}
	}
}