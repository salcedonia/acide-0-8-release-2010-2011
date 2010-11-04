package gui.editor;

import es.explorer.ExplorerFile;
import gui.MainWindow;
import gui.editor.utils.LineNumber;
import gui.editor.utils.MatchingBraces;
import gui.editor.utils.SyntaxDocument;

import java.awt.Adjustable;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextPane;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.tree.TreePath;

import language.Language;
import operations.listeners.*;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * Editor panel of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class EditorPanel extends JPanel {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * First editor.
	 */
	private JTextPane _editor1;
	/**
	 * Second editor.
	 */
	private JTextPane _editor2;
	/**
	 * Scroll panel for the first editor.
	 */
	private JScrollPane _scrollPane1;
	/**
	 * Scroll panel for the second editor.
	 */
	private JScrollPane _scrollPane2;
	/**
	 * Vertical value for the first editor.
	 */
	private int _verticalValue1;
	/**
	 * Vertical value for the second editor.
	 */
	private int _verticalValue2;
	/**
	 * Horizontal value for the first editor.
	 */
	private int _horizontalValue1;
	/**
	 * Horizontal value for the second editor.
	 */
	private int _horizontalValue2;
	/**
	 * File path.
	 */
	private String _path;
	/**
	 * Last change in the editor.
	 */
	private long _lastChange;
	/**
	 * Last size of the editor.
	 */
	private long _lastSize;
	/**
	 * Editor panel popup menu.
	 */
	private EditorPopupMenu _popup;
	/**
	 * Syntax document for the editors.
	 */
	private SyntaxDocument _syntaxisDoc;
	/**
	 * Editor document listener.
	 */
	private EditorDocumentListener _documentListener;
	/**
	 * Editor caret listener.
	 */
	private EditorCaretListener _caretListener;
	/**
	 * Editor adjustment listener.
	 */
	private EditorAdjustmentListener _adjustmentListener;
	/**
	 * Editor1 mouse click listener.
	 */
	private Editor1MouseClickListener _mouseClickListener1;
	/**
	 * Editor2 mouse click listener.
	 */
	private Editor2MouseClickListener _mouseClickListener2;
	/**
	 * Flag that indicates the active editor.
	 */
	private int _activeEditor;
	/**
	 * Line number panel 1.
	 */
	private LineNumber _lineNumber;
	/**
	 * Line number panel 2.
	 */
	private LineNumber _lineNumber2;
	/**
	 * Horizontal split panel.
	 */
	private JSplitPane _splitPaneHorizontal;
	/**
	 * Object used for the creation of both editors.
	 */
	private JTextPane _textArea;
	/**
	 * Flag that indicates if the file is main.
	 */
	private boolean _isMainFile;
	/**
	 * Flag that indicates if the file is compiled.
	 */
	private boolean _isCompiledFile;
	/**
	 * Brace count for the first editor.
	 */
	private static int _brace1;
	/**
	 * Brace count for the second editor.
	 */
	private static int _brace2;
	/**
	 * Icon of the editor.
	 */
	private Icon _icon;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public EditorPanel() {

		super();

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}

		// GET THE LABELS
		ResourceBundle labels = language.getLabels();

		try {

			_brace1 = -1;
			_brace2 = -1;
			_verticalValue1 = 0;
			_verticalValue2 = 0;
			_horizontalValue1 = 0;
			_horizontalValue2 = 0;
			_activeEditor = 1;
			_logger.info(labels.getString("s317"));
			_syntaxisDoc = new SyntaxDocument();
			_editor1 = buildEditor();
			_editor1.setCaretPosition(0);
			_editor2 = buildEditor();
			_editor2.setCaretPosition(0);
			_editor1.addKeyListener(new EditorKeyListener());
			_editor2.addKeyListener(new EditorKeyListener());

			// POPUP
			_popup = new EditorPopupMenu();

			_editor1.addMouseListener(new EditorPopupListener());
			_editor2.addMouseListener(new EditorPopupListener());
			_editor1.addMouseListener(new EditorDoubleClick());
			_editor2.addMouseListener(new EditorDoubleClick());
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

			_documentListener = new EditorDocumentListener();
			_editor1.getDocument().addDocumentListener(_documentListener);
			_editor2.getDocument().addDocumentListener(_documentListener);
			_caretListener = new EditorCaretListener();
			_editor1.addCaretListener(_caretListener);
			_editor2.addCaretListener(_caretListener);
			_scrollPane1.setMinimumSize(new Dimension(0, 0));
			_scrollPane2.setMinimumSize(new Dimension(0, 0));
			_splitPaneHorizontal = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
					_scrollPane1, _scrollPane2);
			_splitPaneHorizontal.setDividerLocation(0);
			_splitPaneHorizontal.setContinuousLayout(true);
			_adjustmentListener = new EditorAdjustmentListener();
			_scrollPane1.getVerticalScrollBar().addAdjustmentListener(
					_adjustmentListener);
			_scrollPane2.getVerticalScrollBar().addAdjustmentListener(
					_adjustmentListener);
			add(_splitPaneHorizontal);
			_logger.info(labels.getString("s318"));
			_path = null;
			_lastChange = 0;
			_lastSize = 0;

			// LISTENERS
			_editor1.addKeyListener(new AcideKeyboardListener());
			_editor2.addKeyListener(new AcideKeyboardListener());
			_editor1.addMouseListener(new AcideMouseListener());
			_editor2.addMouseListener(new AcideMouseListener());
			_editor1.addKeyListener(new AcideKeyboardListenerForMenus());
			_editor2.addKeyListener(new AcideKeyboardListenerForMenus());

		} catch (Exception e) {
			_logger.info(labels.getString("s319"));
			e.printStackTrace();
		}
	}

	/**
	 * Builds the editor text component.
	 * 
	 * @return The build editor text component.
	 */
	protected JTextPane buildEditor() {

		_textArea = new JTextPane(_syntaxisDoc) {

			/**
			 * serialVersionUID
			 */
			private static final long serialVersionUID = 1L;

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.Component#setSize(java.awt.Dimension)
			 */
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
			public boolean getScrollableTracksViewportWidth() {
				return false;
			}
		};

		_textArea.setFont(new Font("monospaced", Font.PLAIN, 12));
		return _textArea;
	}

	/**
	 * Return the active editor text component.
	 * 
	 * @return The active editor text component.
	 */
	public JTextComponent getEditor() {

		if (_activeEditor == 2)
			return _editor2;
		else
			return _editor1;
	}

	/**
	 * Load the text into both editors.
	 * 
	 * @param text
	 *            Text to load.
	 */
	public void loadText(String text) {

		_editor2.getDocument().removeDocumentListener(_documentListener);
		_editor1.getDocument().removeDocumentListener(_documentListener);
		_editor1.setText(text);

		String sold = MainWindow.getInstance().getEditorBuilder()
				.getTabbedPane().getTitleAt(
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditorIndex());

		sold = sold.replace(" *", "");

		MainWindow.getInstance().getEditorBuilder().getTabbedPane().setTitleAt(
				MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditorIndex(), sold);

		_editor1.setCaretPosition(0);
		_editor2.setCaretPosition(0);
		_editor2.getDocument().addDocumentListener(_documentListener);
		_editor1.getDocument().addDocumentListener(_documentListener);

		revalidate();
	}

	/**
	 * Select a text in the editors.
	 * 
	 * @param start
	 *            Selection start.
	 * @param length
	 *            Selection length.
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
	 * Returns the text of the editors.
	 * 
	 * @return The text of the editors.
	 */
	public String getText() {
		return _editor1.getText();
	}

	/**
	 * Set a new value to the file path.
	 * 
	 * @param path
	 *            New value to set.
	 */
	public void setAbsolutePath(String path) {
		_path = path;
	}

	/**
	 * Returns the absolute file path.
	 * 
	 * @return The absolute file path.
	 */
	public String getAbsolutePath() {
		return _path;
	}

	/**
	 * Returns the relative file path.
	 * 
	 * @return The relative file path.
	 */
	public String getFilePath() {

		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");
		return _path.substring(0, index);
	}

	/**
	 * Returns the file extension.
	 * 
	 * @return The file extension.
	 */
	public String getFileExtension() {
		return _path.substring(_path.lastIndexOf(".") + 1);
	}

	/**
	 * Returns the file name.
	 * 
	 * @return The file name.
	 */
	public CharSequence getFileName() {
		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");
		return _path.substring(index + 1, _path.lastIndexOf("."));
	}

	/**
	 * Returns the last change.
	 * 
	 * @return The last change.
	 */
	public long getLastChange() {
		return _lastChange;
	}

	/**
	 * Set a new value to the last change.
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
	 * @return The last size.
	 */
	public long getLastSize() {
		return _lastSize;
	}

	/**
	 * Set the editable feature of the editors to false or true.
	 * 
	 * @param b
	 *            Value to set.
	 */
	public void setEditable(boolean b) {
		_editor1.setEditable(b);
		_editor2.setEditable(b);
	}

	/**
	 * Set a new value to the last size.
	 * 
	 * @param lastSize
	 *            New value to set.
	 */
	public void setLastSize(long lastSize) {
		_lastSize = lastSize;
	}

	/**
	 * Returns the is compiler file flag.
	 * 
	 * @return The is compiler file flag.
	 */
	public boolean isCompilerFile() {
		return _isCompiledFile;
	}

	/**
	 * Set a new value to the is compiler flag.
	 * 
	 * @param compilerFile
	 *            New value to set.
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
	 * Set a new value to the is main file flag.
	 * 
	 * @param mainFile
	 *            New value to set.
	 */
	public void setMainFile(boolean mainFile) {
		_isMainFile = mainFile;
	}

	/**
	 * Returns the icon.
	 * 
	 * @return The icon.
	 */
	public Icon getIcon() {
		return _icon;
	}

	/**
	 * Set a new value to the icon.
	 * 
	 * @param icon
	 *            New value to set.
	 */
	public void setIcon(Icon icon) {
		_icon = icon;
	}

	/**
	 * Return the syntax document.
	 * 
	 * @return The syntax document.
	 */
	public SyntaxDocument getSyntaxDocument() {
		return _syntaxisDoc;
	}

	/**
	 * Returns the first editor.
	 * 
	 * @return The first editor.
	 */
	public JTextComponent getEditor1() {
		return _editor1;
	}

	/**
	 * Returns the second editor.
	 * 
	 * @return The second editor.
	 */
	public JTextComponent getEditor2() {
		return _editor2;
	}

	/**
	 * Returns the active editor.
	 * 
	 * @return The active editor.
	 */
	public int getActiveEditor() {
		return _activeEditor;
	}
	
	/**
	 * Returns the popup menu.
	 * 
	 * @return The popup menu.
	 */
	public EditorPopupMenu getPopupMenu() {
		return _popup;
	}
	
	/**
	 * Reset the document in a different thread.
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
					textContent = _syntaxisDoc.getText(0, _syntaxisDoc
							.getLength());
				} catch (BadLocationException e) {
					e.printStackTrace();
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
	 * Position the caret at the start of a line.
	 * 
	 * @param line
	 *            Line to put the caret on.
	 */
	public void goToLine(int line) {
		JTextComponent component = getEditor();
		Element root = component.getDocument().getDefaultRootElement();
		line = Math.max(line, 1);
		line = Math.min(line, root.getElementCount());
		component.setCaretPosition(root.getElement(line - 1).getStartOffset());
	}

	/**
	 * Editor popup listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class EditorPopupListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent e) {
			maybeShowPopup(e);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
		 */
		public void mouseReleased(MouseEvent e) {
			maybeShowPopup(e);
		}

		/**
		 * Shows the popup menu.
		 * 
		 * @param e
		 *            Mouse event.
		 */
		private void maybeShowPopup(MouseEvent e) {

			if (e.isPopupTrigger()) {

				MainWindow mainWindow = MainWindow.getInstance();
				EditorPanel edit = mainWindow.getEditorBuilder()
						.getSelectedEditor();
				edit.getPopupMenu().getCopy().setEnabled(false);
				edit.getPopupMenu().getCut().setEnabled(false);
				edit.getPopupMenu().getPaste().setEnabled(false);
				edit.getPopupMenu().getAddFile().setEnabled(false);
				edit.getPopupMenu().getRemoveFile().setEnabled(false);
				edit.getPopupMenu().getSetCompilable().setEnabled(false);
				edit.getPopupMenu().getUnsetCompilable().setEnabled(false);
				edit.getPopupMenu().getSetMain().setEnabled(false);
				edit.getPopupMenu().getUnsetMain().setEnabled(false);

				// CHECK THE SYSTEM CLIPBOARD
				if (Toolkit.getDefaultToolkit().getSystemClipboard()
						.getContents(null) != null)
					// ENABLE THE PASTE OPTION IN THE POPUP MENU
					edit.getPopupMenu().getPaste().setEnabled(true);

				// CHECK THE SELECTED TEXT
				if (edit.getEditor().getSelectedText() != null) {
					
					// ENABLE THE OPTIONS
					edit.getPopupMenu().getCopy().setEnabled(true);
					edit.getPopupMenu().getCut().setEnabled(true);
				}

				// DEFAULT PROJECT
				if (MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
					
					// DISABLE THE POPUP OPTIONS
					edit.getPopupMenu().getAddFile().setEnabled(false);
					edit.getPopupMenu().getRemoveFile().setEnabled(false);

					// CHECK THE TYPE
					if (!edit.isMainFile())
						edit.getPopupMenu().getSetMain().setEnabled(true);
					if (edit.isMainFile())
						edit.getPopupMenu().getUnsetMain().setEnabled(true);
					if (!edit.isCompilerFile()
							|| (edit.isCompilerFile() && edit.isMainFile()))
						edit.getPopupMenu().getSetCompilable().setEnabled(true);
					if (edit.isCompilerFile() && !edit.isMainFile())
						edit.getPopupMenu().getUnsetCompilable().setEnabled(
								true);
				} else {

					// SEARCH FOR THE FILE IN THE PROJECT
					String file = mainWindow.getEditorBuilder().getEditorAt(
							mainWindow.getEditorBuilder()
									.getSelectedEditorIndex())
							.getAbsolutePath();

					boolean exists = false;

					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration().getFileAt(i)
								.getPath().equals(file)) {
							exists = true;
						}
					}
					if (exists) {
						edit.getPopupMenu().getRemoveFile().setEnabled(true);
						edit.getPopupMenu().getAddFile().setEnabled(false);

						if (!edit.isMainFile())
							edit.getPopupMenu().getSetMain().setEnabled(true);
						if (edit.isMainFile())
							edit.getPopupMenu().getUnsetMain().setEnabled(true);
						if (!edit.isCompilerFile()
								|| (edit.isCompilerFile() && edit.isMainFile()))
							edit.getPopupMenu().getSetCompilable().setEnabled(
									true);
						if (edit.isCompilerFile() && !edit.isMainFile())
							edit.getPopupMenu().getUnsetCompilable()
									.setEnabled(true);

					} else {
						edit.getPopupMenu().getRemoveFile().setEnabled(false);
						edit.getPopupMenu().getAddFile().setEnabled(true);
					}
				}

				_popup.show(e.getComponent(), e.getX(), e.getY());
			}
		}
	}

	/**
	 * Editor document listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class EditorDocumentListener implements DocumentListener {
		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * javax.swing.event.DocumentListener#insertUpdate(javax.swing.event
		 * .DocumentEvent)
		 */
		public void insertUpdate(DocumentEvent e) {
			MainWindow.getInstance().getEditorBuilder().getTestPlaf()
					.getCloseButtonAt(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).setRedButton();

			MainWindow.getInstance().getMenu().getFile().getSaveFileAs()
					.setEnabled(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * javax.swing.event.DocumentListener#removeUpdate(javax.swing.event
		 * .DocumentEvent)
		 */
		public void removeUpdate(DocumentEvent e) {
			MainWindow.getInstance().getEditorBuilder().getTestPlaf()
					.getCloseButtonAt(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).setRedButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs()
					.setEnabled(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * javax.swing.event.DocumentListener#changedUpdate(javax.swing.event
		 * .DocumentEvent)
		 */
		public void changedUpdate(DocumentEvent e) {
			MainWindow.getInstance().getEditorBuilder().getTestPlaf()
					.getCloseButtonAt(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).setRedButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs()
					.setEnabled(true);
		}
	}

	/**
	 * Editor caret listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class EditorCaretListener implements CaretListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent
		 * )
		 */
		public void caretUpdate(CaretEvent e) {

			if (_editor1.isFocusOwner())
				_activeEditor = 1;

			if (_editor2.isFocusOwner())
				_activeEditor = 2;

			Element root = _syntaxisDoc.getDefaultRootElement();
			int dot = e.getDot();
			int line = root.getElementIndex(dot);
			int col = dot - root.getElement(line).getStartOffset();
			MainWindow.getInstance().getStatusBar().getMessageLineCol()
					.setText((line + 1) + ":" + (col + 1));

			try {
				if (_brace1 != -1) {
					_syntaxisDoc.removeBrace(_brace1);
					_brace1 = -1;
				}
				if (_brace2 != -1) {
					_syntaxisDoc.removeBrace(_brace2);
					_brace2 = -1;
				}
				int ini = getEditor().getCaretPosition();
				int n = MatchingBraces.findMatchingBracket(getEditor()
						.getDocument(), ini - 1);
				if (((ini > 0) && (ini <= _syntaxisDoc.getLength()))
						&& ((n >= 0) && (n <= _syntaxisDoc.getLength()))) {
					if (n > -1) {
						if (n > ini) {
							_brace1 = ini - 1;
							_brace2 = n;
							_syntaxisDoc.setBrace(_brace1);
							_syntaxisDoc.setBrace(_brace2);
						} else if (n < ini) {
							_brace1 = n;
							_brace2 = ini - 1;
							_syntaxisDoc.setBrace(_brace1);
							_syntaxisDoc.setBrace(_brace2);
						}
					}
				}
			} catch (BadLocationException ex) {

			}
		}
	}

	/**
	 * Editor adjustment listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class EditorAdjustmentListener implements AdjustmentListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.AdjustmentListener#adjustmentValueChanged(java.awt
		 * .event.AdjustmentEvent)
		 */
		public void adjustmentValueChanged(AdjustmentEvent evt) {
			Adjustable source = evt.getAdjustable();
			if (evt.getValueIsAdjusting()) {
				_verticalValue1 = _scrollPane1.getVerticalScrollBar()
						.getValue();
				_horizontalValue1 = _scrollPane1.getHorizontalScrollBar()
						.getValue();
				_verticalValue2 = _scrollPane2.getVerticalScrollBar()
						.getValue();
				_horizontalValue2 = _scrollPane2.getHorizontalScrollBar()
						.getValue();
				return;
			}
			int orient = source.getOrientation();
			if (orient == Adjustable.HORIZONTAL) {
			} else {
			}
			int type = evt.getAdjustmentType();
			switch (type) {
			case AdjustmentEvent.UNIT_INCREMENT:
				break;
			case AdjustmentEvent.UNIT_DECREMENT:
				break;
			case AdjustmentEvent.BLOCK_INCREMENT:
				break;
			case AdjustmentEvent.BLOCK_DECREMENT:
				break;
			case AdjustmentEvent.TRACK:
				break;
			}
			if (_editor1.isFocusOwner()
					|| _scrollPane1.getVerticalScrollBar().isFocusOwner()
					|| _scrollPane1.getHorizontalScrollBar().isFocusOwner()) {
				_scrollPane2.getVerticalScrollBar().setValue(_verticalValue2);
				_scrollPane2.getHorizontalScrollBar().setValue(
						_horizontalValue2);
				_verticalValue1 = _scrollPane1.getVerticalScrollBar()
						.getValue();
				_horizontalValue1 = _scrollPane1.getHorizontalScrollBar()
						.getValue();
			}
			if (_editor2.isFocusOwner()
					|| _scrollPane2.getVerticalScrollBar().isFocusOwner()
					|| _scrollPane2.getHorizontalScrollBar().isFocusOwner()) {
				_scrollPane1.getVerticalScrollBar().setValue(_verticalValue1);
				_scrollPane1.getHorizontalScrollBar().setValue(
						_horizontalValue1);
				_verticalValue2 = _scrollPane2.getVerticalScrollBar()
						.getValue();
				_horizontalValue2 = _scrollPane2.getHorizontalScrollBar()
						.getValue();
			}
		}
	}

	/**
	 * Editor1 mouse click listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class Editor1MouseClickListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {
			_editor1.requestFocus();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			_editor1.requestFocus();
		}
	}

	/**
	 * Editor2 mouse click listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class Editor2MouseClickListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {
			_editor2.requestFocus();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			_editor2.requestFocus();
		}
	}

	/**
	 * Editor double click listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class EditorDoubleClick extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent evt) {

			if (evt.getClickCount() > 1) {
				try {
					int ini = getEditor().getCaretPosition();
					int n = MatchingBraces.findMatchingBracket(getEditor()
							.getDocument(), ini - 1);
					if (n > -1) {
						if (n > ini)
							selectText(ini - 1, n - ini + 2);
						if (n < ini)
							selectText(n, ini - n);
					}
				} catch (BadLocationException e) {
					e.printStackTrace();
				}

			}

			// NOT DEFAULT PROJECT
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {

				ExplorerFile explorerFile = new ExplorerFile();
				int y = -1;
				for (int j = 0; j < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); j++) {

					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(j).getPath().equals(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath())) {

						explorerFile = MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(j);

						for (int m = 0; m < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList() + 1; m++) {

							if (MainWindow
									.getInstance()
									.getExplorer()
									.getTree()
									.getPathForRow(m)
									.getLastPathComponent()
									.toString()
									.equals(explorerFile.getLastPathComponent())) {

								y = m;
							}
						}
					}
				}

				// SELECTS THE FILE IN THE TREE
				TreePath currentSelection = MainWindow.getInstance()
						.getExplorer().getTree().getPathForRow(y);
				MainWindow.getInstance().getExplorer().getTree()
						.setSelectionPath(currentSelection);
			}
		}
	}

	/**
	 * Editor key listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class EditorKeyListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
		 */
		public void keyTyped(KeyEvent e) {

		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
		 */
		public void keyPressed(KeyEvent e) {
			if (_brace1 != -1) {
				_syntaxisDoc.removeBrace(_brace1);
				_brace1 = -1;
			}
			if (_brace2 != -1) {
				_syntaxisDoc.removeBrace(_brace2);
				_brace2 = -1;
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
		 */
		public void keyReleased(KeyEvent e) {

		};
	}

	/**
	 * Return true if the opened file in the editor is the new file and false
	 * in other case.
	 * 
	 * @return True if the opened file in the editor is the new file and false
	 * in other case.
	 */
	public boolean isNewFile() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		return _path.equals(labels.getString("s79"));
	}

	/**
	 * Return true if the opened file in the editor is the log tab and false
	 * in other case.
	 * 
	 * @return True if the opened file in the editor is the log tab and false
	 * in other case.
	 */
	public boolean isLogFile() {
		return  _path.equals("Log");
	}
}