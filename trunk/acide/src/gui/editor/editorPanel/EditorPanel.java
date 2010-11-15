package gui.editor.editorPanel;

import gui.editor.editorManager.utils.gui.LineNumber;
import gui.editor.editorManager.utils.logic.SyntaxDocument;
import gui.editor.editorPanel.listeners.Editor1MouseClickListener;
import gui.editor.editorPanel.listeners.Editor2MouseClickListener;
import gui.editor.editorPanel.listeners.EditorPanelAdjustmentListener;
import gui.editor.editorPanel.listeners.EditorPanelCaretListener;
import gui.editor.editorPanel.listeners.EditorPanelDocumentListener;
import gui.editor.editorPanel.listeners.EditorPanelDoubleClickMouseListener;
import gui.editor.editorPanel.listeners.EditorPanelKeyboardListener;
import gui.editor.editorPanel.popup.EditorPanelPopupMenuListener;
import gui.editor.editorPanel.popup.EditorPopupMenu;
import gui.mainWindow.MainWindow;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextPane;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;

import language.Language;
import operations.listeners.*;
import operations.log.Log;

import properties.PropertiesManager;

/************************************************************************																
 * Editor panel of ACIDE - A Configurable IDE										
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
 * @see JPanel																													
 ***********************************************************************/
public class EditorPanel extends JPanel {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * First editor
	 */
	private JTextPane _editor1;
	/**
	 * Second editor
	 */
	private JTextPane _editor2;
	/**
	 * Scroll panel for the first editor
	 */
	private JScrollPane _scrollPane1;
	/**
	 * Scroll panel for the second editor
	 */
	private JScrollPane _scrollPane2;
	/**
	 * Vertical value for the first editor
	 */
	private int _verticalValue1;
	/**
	 * Vertical value for the second editor
	 */
	private int _verticalValue2;
	/**
	 * Horizontal value for the first editor
	 */
	private int _horizontalValue1;
	/**
	 * Horizontal value for the second editor
	 */
	private int _horizontalValue2;
	/**
	 * File path
	 */
	private String _path;
	/**
	 * Last change in the editor
	 */
	private long _lastChange;
	/**
	 * Last size of the editor
	 */
	private long _lastSize;
	/**
	 * Editor panel popup menu
	 */
	private EditorPopupMenu _popup;
	/**
	 * Syntax document for the editors
	 */
	private SyntaxDocument _syntaxisDocument;
	/**
	 * Editor document listener
	 */
	private EditorPanelDocumentListener _documentListener;
	/**
	 * Editor caret listener
	 */
	private EditorPanelCaretListener _caretListener;
	/**
	 * Editor adjustment listener
	 */
	private EditorPanelAdjustmentListener _adjustmentListener;
	/**
	 * Editor1 mouse click listener
	 */
	private Editor1MouseClickListener _mouseClickListener1;
	/**
	 * Editor2 mouse click listener
	 */
	private Editor2MouseClickListener _mouseClickListener2;
	/**
	 * Flag that indicates the active editor
	 */
	private int _activeEditor;
	/**
	 * Line number panel 1
	 */
	private LineNumber _lineNumber;
	/**
	 * Line number panel 2
	 */
	private LineNumber _lineNumber2;
	/**
	 * Horizontal split panel
	 */
	private JSplitPane _splitPaneHorizontal;
	/**
	 * Object used for the creation of both editors
	 */
	private JTextPane _textArea;
	/**
	 * Flag that indicates if the file is main
	 */
	private boolean _isMainFile;
	/**
	 * Flag that indicates if the file is compiled
	 */
	private boolean _isCompiledFile;
	/**
	 * Brace count for the first editor
	 */
	private static int _brace1;
	/**
	 * Brace count for the second editor
	 */
	private static int _brace2;
	/**
	 * Icon of the editor
	 */
	private Icon _icon;

	/**
	 * Class constructor.
	 */
	public EditorPanel() {

		super();

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		try {

			_brace1 = -1;
			_brace2 = -1;
			_verticalValue1 = 0;
			_verticalValue2 = 0;
			_horizontalValue1 = 0;
			_horizontalValue2 = 0;
			_activeEditor = 1;
			
			// Updates the log
			Log.getLog().info(labels.getString("s317"));
			
			_syntaxisDocument = new SyntaxDocument();
			_editor1 = buildEditor();
			_editor1.setCaretPosition(0);
			_editor2 = buildEditor();
			_editor2.setCaretPosition(0);
			_editor1.addKeyListener(new EditorPanelKeyboardListener());
			_editor2.addKeyListener(new EditorPanelKeyboardListener());

			// POPUP
			buildPopupMenu();

			_editor1.addMouseListener(new EditorPanelPopupMenuListener());
			_editor2.addMouseListener(new EditorPanelPopupMenuListener());
			_editor1.addMouseListener(new EditorPanelDoubleClickMouseListener());
			_editor2.addMouseListener(new EditorPanelDoubleClickMouseListener());
			setLayout(new BorderLayout());
			_scrollPane1 = new JScrollPane(_editor1);
			_scrollPane2 = new JScrollPane(_editor2);
			_lineNumber = new LineNumber(_editor1);
			_lineNumber2 = new LineNumber(_editor2);
			_scrollPane1.setRowHeaderView(_lineNumber);
			_scrollPane2.setRowHeaderView(_lineNumber2);

			_mouseClickListener1 = new Editor1MouseClickListener();
			_mouseClickListener2 = new Editor2MouseClickListener();

			_scrollPane1.getVerticalScrollBar().addMouseListener(
					_mouseClickListener1);
			_scrollPane1.getHorizontalScrollBar().addMouseListener(
					_mouseClickListener1);
			_scrollPane2.getVerticalScrollBar().addMouseListener(
					_mouseClickListener2);
			_scrollPane2.getHorizontalScrollBar().addMouseListener(
					_mouseClickListener2);
			_scrollPane1.addMouseListener(_mouseClickListener1);
			_scrollPane2.addMouseListener(_mouseClickListener2);

			Component comp[] = _scrollPane1.getVerticalScrollBar()
					.getComponents();
			for (int i = 0; i < comp.length; i++) {
				if (comp[i] instanceof BasicArrowButton) {
					comp[i].addMouseListener(_mouseClickListener1);
				}
			}

			Component comph[] = _scrollPane1.getHorizontalScrollBar()
					.getComponents();
			for (int i = 0; i < comph.length; i++) {
				if (comph[i] instanceof BasicArrowButton) {
					comph[i].addMouseListener(_mouseClickListener1);
				}
			}

			Component comp2[] = _scrollPane2.getVerticalScrollBar()
					.getComponents();
			for (int i = 0; i < comp2.length; i++) {
				if (comp2[i] instanceof BasicArrowButton) {
					comp2[i].addMouseListener(_mouseClickListener2);
				}
			}

			Component comph2[] = _scrollPane1.getHorizontalScrollBar()
					.getComponents();
			for (int i = 0; i < comph2.length; i++) {
				if (comph2[i] instanceof BasicArrowButton) {
					comph2[i].addMouseListener(_mouseClickListener2);
				}
			}

			_documentListener = new EditorPanelDocumentListener();
			_editor1.getDocument().addDocumentListener(_documentListener);
			_editor2.getDocument().addDocumentListener(_documentListener);
			_caretListener = new EditorPanelCaretListener();
			_editor1.addCaretListener(_caretListener);
			_editor2.addCaretListener(_caretListener);
			_scrollPane1.setMinimumSize(new Dimension(0, 0));
			_scrollPane2.setMinimumSize(new Dimension(0, 0));
			_splitPaneHorizontal = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
					_scrollPane1, _scrollPane2);
			_splitPaneHorizontal.setDividerLocation(0);
			_splitPaneHorizontal.setContinuousLayout(true);
			_adjustmentListener = new EditorPanelAdjustmentListener();
			_scrollPane1.getVerticalScrollBar().addAdjustmentListener(
					_adjustmentListener);
			_scrollPane2.getVerticalScrollBar().addAdjustmentListener(
					_adjustmentListener);
			add(_splitPaneHorizontal);
			
			// Updates the log
			Log.getLog().info(labels.getString("s318"));
			
			_path = null;
			_lastChange = 0;
			_lastSize = 0;

			// Listeners
			_editor1.addKeyListener(new AcideKeyboardListener());
			_editor2.addKeyListener(new AcideKeyboardListener());
			_editor1.addMouseListener(new AcideMouseListener());
			_editor2.addMouseListener(new AcideMouseListener());
			_editor1.addKeyListener(new AcideKeyboardListenerForMenus());
			_editor2.addKeyListener(new AcideKeyboardListenerForMenus());

		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().info(labels.getString("s319"));
			exception.printStackTrace();
		}
	}

	/**
	 * Builds the popup menu
	 */
	public void buildPopupMenu() {
		_popup = new EditorPopupMenu();
	}

	/**
	 * Builds the editor text component
	 * 
	 * @return the build editor text component
	 */
	protected JTextPane buildEditor() {

		_textArea = new JTextPane(_syntaxisDocument) {

			/**
			 * Class serial version UID
			 */
			private static final long serialVersionUID = 1L;

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.Component#setSize(java.awt.Dimension)
			 */
			@Override
			public void setSize(Dimension d) {
				if (d.width < getParent().getSize().width)
					d.width = getParent().getSize().width;
				super.setSize(d);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see javax.swing.JEditorPane#getScrollableTracksViewportWidth()
			 */
			@Override
			public boolean getScrollableTracksViewportWidth() {
				return false;
			}
		};

		_textArea.setFont(new Font("monospaced", Font.PLAIN, 12));
		return _textArea;
	}

	/**
	 * Returns the active editor text component
	 * 
	 * @return the active editor text component
	 */
	public JTextComponent getEditor() {

		if (_activeEditor == 2)
			return _editor2;
		else
			return _editor1;
	}

	/**
	 * Loads the text into both editors
	 * 
	 * @param text
	 *            text to load
	 */
	public void loadText(String text) {

		_editor2.getDocument().removeDocumentListener(_documentListener);
		_editor1.getDocument().removeDocumentListener(_documentListener);
		_editor1.setText(text);

		String sold = MainWindow.getInstance().getEditorManager()
				.getTabbedPane().getTitleAt(
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditorIndex());

		sold = sold.replace(" *", "");

		MainWindow.getInstance().getEditorManager().getTabbedPane().setTitleAt(
				MainWindow.getInstance().getEditorManager()
						.getSelectedEditorIndex(), sold);

		_editor1.setCaretPosition(0);
		_editor2.setCaretPosition(0);
		_editor2.getDocument().addDocumentListener(_documentListener);
		_editor1.getDocument().addDocumentListener(_documentListener);

		revalidate();
	}

	/**
	 * Selects a text in both editors
	 * 
	 * @param start
	 *            selection start
	 * @param length
	 *            selection length
	 */
	public void selectText(int start, int length) {
		_editor1.setSelectionStart(start);
		_editor1.setSelectionEnd(start + length);
		_editor2.setSelectionStart(start);
		_editor2.setSelectionEnd(start + length);
		if (_activeEditor == 1)
			_editor1.requestFocus();
		else
			_editor2.requestFocus();
	}

	/**
	 * Resets the document in a different thread
	 */
	public void resetDocument() {

		Thread t = new Thread(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			public void run() {

				String textContent = "";
				try {
					textContent = _syntaxisDocument.getText(0, _syntaxisDocument
							.getLength());
				} catch (BadLocationException exception) {
					
					// Updates the log
					Log.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
				SyntaxDocument syntaxDocument = new SyntaxDocument();
				_editor1.setStyledDocument(syntaxDocument);
				_editor1.setText(textContent);
				_editor2.setStyledDocument(syntaxDocument);
				_editor1.setCaretPosition(0);
				_editor2.setCaretPosition(0);
				_editor1.getDocument().addDocumentListener(_documentListener);
				_editor2.getDocument().addDocumentListener(_documentListener);

			}
		});
		t.start();
	}

	/**
	 * Sets the caret at the start of a line
	 * 
	 * @param line
	 *            line to put the caret on
	 */
	public void goToLine(int line) {
		JTextComponent component = getEditor();
		Element root = component.getDocument().getDefaultRootElement();
		line = Math.max(line, 1);
		line = Math.min(line, root.getElementCount());
		component.setCaretPosition(root.getElement(line - 1).getStartOffset());
	}
	
	/**
	 * Returns the text of the editors
	 * 
	 * @return the text of the editors
	 */
	public String getText() {
		return _editor1.getText();
	}

	/**
	 * Sets a new value to the file path
	 * 
	 * @param path
	 *            new value to set
	 */
	public void setAbsolutePath(String path) {
		_path = path;
	}

	/**
	 * Returns the absolute file path
	 * 
	 * @return the absolute file path
	 */
	public String getAbsolutePath() {
		return _path;
	}

	/**
	 * Returns the relative file path
	 * 
	 * @return the relative file path
	 */
	public String getFilePath() {

		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");
		return _path.substring(0, index);
	}

	/**
	 * Returns the file extension
	 * 
	 * @return the file extension
	 */
	public String getFileExtension() {
		return _path.substring(_path.lastIndexOf(".") + 1);
	}

	/**
	 * Returns the file name
	 * 
	 * @return the file name
	 */
	public CharSequence getFileName() {
		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");
		return _path.substring(index + 1, _path.lastIndexOf("."));
	}

	/**
	 * Returns the last change
	 * 
	 * @return the last change
	 */
	public long getLastChange() {
		return _lastChange;
	}

	/**
	 * Sets a new value to the last change
	 * 
	 * @param lastChange
	 *            New value to set
	 */
	public void setLastChange(long lastChange) {
		_lastChange = lastChange;
	}

	/**
	 * Returns the last size
	 * 
	 * @return the last size
	 */
	public long getLastSize() {
		return _lastSize;
	}

	/**
	 * Sets the modifiable feature of the editors to false or true
	 * 
	 * @param editable
	 *            new value to set
	 */
	public void setEditable(boolean editable) {
		_editor1.setEditable(editable);
		_editor2.setEditable(editable);
	}

	/**
	 * Sets a new value to the last size
	 * 
	 * @param lastSize
	 *            new value to set
	 */
	public void setLastSize(long lastSize) {
		_lastSize = lastSize;
	}

	/**
	 * Returns the is compiler file flag
	 * 
	 * @return the is compiler file flag
	 */
	public boolean isCompilerFile() {
		return _isCompiledFile;
	}

	/**
	 * Sets a new value to the is compiler flag
	 * 
	 * @param compilerFile
	 *            new value to set
	 */
	public void setCompilerFile(boolean compilerFile) {
		_isCompiledFile = compilerFile;
	}

	/**
	 * Returns the is main file flag
	 * 
	 * @return the is main file flag
	 */
	public boolean isMainFile() {
		return _isMainFile;
	}

	/**
	 * Sets a new value to the is main file flag
	 * 
	 * @param mainFile
	 *            new value to set
	 */
	public void setMainFile(boolean mainFile) {
		_isMainFile = mainFile;
	}

	/**
	 * Returns the icon
	 * 
	 * @return the icon
	 */
	public Icon getIcon() {
		return _icon;
	}

	/**
	 * Sets a new value to the icon
	 * 
	 * @param icon
	 *            new value to set
	 */
	public void setIcon(Icon icon) {
		_icon = icon;
	}

	/**
	 * Returns the syntax document
	 * 
	 * @return the syntax document
	 */
	public SyntaxDocument getSyntaxDocument() {
		return _syntaxisDocument;
	}

	/**
	 * Returns the first editor
	 * 
	 * @return the first editor
	 */
	public JTextComponent getEditor1() {
		return _editor1;
	}

	/**
	 * Returns the second editor
	 * 
	 * @return the second editor
	 */
	public JTextComponent getEditor2() {
		return _editor2;
	}

	/**
	 * Returns the active editor
	 * 
	 * @return the active editor
	 */
	public int getActiveEditor() {
		return _activeEditor;
	}
	
	/**
	 * Returns the popup menu
	 * 
	 * @return the popup menu
	 */
	public EditorPopupMenu getPopupMenu() {
		return _popup;
	}
	
	/**
	 * Returns true if the opened file in the editor is the new file and false
	 * in other case
	 * 
	 * @return true if the opened file in the editor is the new file and false
	 * in other case
	 */
	public boolean isNewFile() {
		
		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		return _path.equals(labels.getString("s79"));
	}

	/**
	 * Returns true if the opened file in the editor is the log tab and false
	 * in other case
	 * 
	 * @return true if the opened file in the editor is the log tab and false
	 * in other case
	 */
	public boolean isLogFile() {
		return  _path.equals("Log");
	}
	
	/**
	 * Sets a new value to the active editor
	 * 
	 * @param activeEditor new value to set
	 */
	public void setActiveEditor(int activeEditor) {
		_activeEditor = activeEditor;
	}

	/**
	 * Returns the brace1
	 * 
	 * @return the brace1
	 */
	public int getBrace1() {	
		return _brace1;
	}

	/**
	 * Sets a new value to the brace1
	 * 
	 * @param brace1 new value to set
	 */
	public void setBrace1(int brace1) {
		_brace1 = brace1;
	}
	
	/**
	 * Returns the brace2
	 * 
	 * @return the brace2
	 */
	public int getBrace2() {	
		return _brace2;
	}

	/**
	 * Sets a new value to the brace2
	 * 
	 * @param brace2 new value to set
	 */
	public void setBrace2(int brace2) {
		_brace2 = brace2;
	}
	
	/**
	 * Returns the horizontal value1
	 * 
	 * @return the horizontal value1
	 */
	public int getHorizontalValue1(){
		return _horizontalValue1;
	}
	
	/**
	 * Returns the horizontal value2
	 * 
	 * @return the horizontal value2
	 */
	public int getHorizontalValue2(){
		return _horizontalValue2;
	}
	
	/**
	 * Sets a new value to the horizontal value1
	 * 
	 * @param horizontalValue1 new value to set
	 */
	public void setHorizontalValue1(int horizontalValue1){
		_horizontalValue1 = horizontalValue1;
	}
	
	/**
	 * Sets a new value to the horizontal value2
	 * 
	 * @param horizontalValue2 new value to set
	 */
	public void setHorizontalValue2(int horizontalValue2){
		_horizontalValue2 = horizontalValue2;
	}
	
	/**
	 * Returns the vertical value1
	 * 
	 * @return the vertical value1
	 */
	public int getVerticalValue1(){
		return _verticalValue1;
	}
	
	/**
	 * Returns the vertical value2
	 * 
	 * @return the vertical value2
	 */
	public int getVerticalValue2(){
		return _verticalValue2;
	}
	
	/**
	 * Sets a new value to the vertical value1
	 * 
	 * @param verticalValue1 new value to set
	 */
	public void setVerticalValue1(int verticalValue1){
		_verticalValue1 = verticalValue1;
	}
	
	/**
	 * Sets a new value to the vertical value2
	 * 
	 * @param verticalValue2 new value to set
	 */
	public void setVerticalValue2(int verticalValue2){
		_verticalValue2 = verticalValue2;
	}

	/**
	 * Returns the scroll panel1
	 * 
	 * @return the scroll panel1
	 */
	public JScrollPane getScrollPane1() {	
		return _scrollPane1;
	}
	
	/**
	 * Returns the scroll panel2
	 * 
	 * @return the scroll panel2
	 */
	public JScrollPane getScrollPane2() {	
		return _scrollPane2;
	}
}