package gui.editor;

import gui.MainWindow;
import java.awt.Adjustable;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
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
import operations.configuration.ExplorerFile;
import operations.listeners.*;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * 
 */
public class Editor extends JPanel {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private JTextPane _editor;
	/**
	 * 
	 */
	private JTextPane _editor2;
	/**
	 * 
	 */
	private JScrollPane _scrollPane;
	/**
	 * 
	 */
	private JScrollPane _scrollPane2;
	/**
	 * 
	 */
	private int _verticalValue;
	/**
	 * 
	 */
	private int _verticalValue2;
	/**
	 * 
	 */
	private int _horizontalValue;
	/**
	 * 
	 */
	private int _horizontalValue2;
	/**
	 * 
	 */
	private String _path;
	/**
	 * 
	 */
	private long _lastChange;
	/**
	 * 
	 */
	private long _lastSize;
	/**
	 * 
	 */
	private JPopupMenu _popup;
	/**
	 * 
	 */
	private JPopupMenu _popup2;
	/**
	 * 
	 */
	private SyntaxisDoc _syntaxisDoc;
	/**
	 * 
	 */
	private MyDocumentListener _editor1Listener;
	/**
	 * 
	 */
	private MyDocumentListener2 _editor2Listener;
	/**
	 * 
	 */
	private MyCaretListener1 _caretListener1;
	/**
	 * 
	 */
	private MyCaretListener2 _caretListener2;
	/**
	 * 
	 */
	private MyAdjustmentListener1 _adjustmentListener1;
	/**
	 * 
	 */
	private MyAdjustmentListener2 _adjustmentListener2;
	/**
	 * 
	 */
	private EditorDoubleClick _doubleClick;
	/**
	 * 
	 */
	private EditorMouseClick1 _click1;
	/**
	 * 
	 */
	private EditorMouseClick2 _click2;
	/**
	 * 
	 */
	private EditorButtonClick1 _buttonClick1;
	/**
	 * 
	 */
	private EditorButtonClick2 _buttonClick2;
	/**
	 * 
	 */
	private int _activeEditor;
	/**
	 * 
	 */
	private LineNumber _lineNumber;
	/**
	 * 
	 */
	private LineNumber _lineNumber2;
	/**
	 * 
	 */
	private JSplitPane _splitPaneHorizontal;
	/**
	 *
	 */
	private JTextPane _textArea;
	/**
	 * 
	 */
	private boolean _isMainFile;
	/**
	 * 
	 */
	private boolean _isCompiledFile;
	/**
	 * 
	 */
	private static int _brace1;
	/**
	 * 
	 */
	private static int _brace2;
	/**
	 * 
	 */
	private JMenuItem _addFileMenu;
	/**
	 * 
	 */
	private JMenuItem _deleteFileMenu;
	/**
	 * 
	 */
	private JMenuItem _deleteFileMenu2;
	/**
	 * 
	 */
	private JMenuItem _removeFileMenu;
	/**
	 * 
	 */
	private JMenuItem _removeFileMenu2;
	/**
	 * 
	 */
	private JMenuItem _addFileMenu2;
	/**
	 * 
	 */
	private JMenuItem _selectAllMenu;
	/**
	 * 
	 */
	private JMenuItem _selectAllMenu2;
	/**
	 * 
	 */
	private JMenuItem _copyMenu;
	/**
	 * 
	 */
	private JMenuItem _copyMenu2;
	/**
	 * 
	 */
	private JMenuItem _pasteMenu;
	/**
	 * 
	 */
	private JMenuItem _pasteMenu2;
	/**
	 * 
	 */
	private JMenuItem _cutMenu;
	/**
	 * 
	 */
	private JMenuItem _cutMenu2;
	/**
	 * 
	 */
	private JMenuItem _setCompilable;
	/**
	 * 
	 */
	private JMenuItem _setCompilable2;
	/**
	 * 
	 */
	private JMenuItem _unsetCompilable;
	/**
	 * 
	 */
	private JMenuItem _unsetCompilable2;
	/**
	 * 
	 */
	private JMenuItem _setMain;
	/**
	 * 
	 */
	private JMenuItem _setMain2;
	/**
	 * 
	 */
	private JMenuItem _unsetMain;
	/**
	 * 
	 */
	private JMenuItem _unsetMain2;
	/**
	 * 
	 */
	private JMenuItem _printMenu;
	/**
	 * 
	 */
	private JMenuItem _printMenu2;
	/**
	 * 
	 */
	private Icon _icon;

	/**
	 * Constructor of the class.
	 */
	public Editor() {

		super();

		ResourceBundle labels = Language.getInstance().getLabels();

		try {

			_brace1 = -1;
			_brace2 = -1;
			_verticalValue = 0;
			_verticalValue2 = 0;
			_horizontalValue = 0;
			_horizontalValue2 = 0;
			_activeEditor = 1;
			_logger.info(labels.getString("s317"));
			_syntaxisDoc = new SyntaxisDoc();
			_editor = buildEditor();
			_editor.setCaretPosition(0);
			_editor2 = buildEditor();
			_editor2.setCaretPosition(0);
			_editor.addKeyListener(new KeyListener() {

				/*
				 * (non-Javadoc)
				 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
				 */
				public void keyTyped(KeyEvent e) {

				}

				/*
				 * (non-Javadoc)
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
				 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
				 */
				public void keyReleased(KeyEvent e) {

				};
			});

			_editor2.addKeyListener(new KeyListener() {

				/*
				 * (non-Javadoc)
				 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
				 */
				public void keyTyped(KeyEvent e) {

				}

				/*
				 * (non-Javadoc)
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
				 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
				 */
				public void keyReleased(KeyEvent e) {

				};
			});

			_popup = new JPopupMenu();
			_copyMenu = new JMenuItem(labels.getString("s187"));
			_copyMenu.addActionListener(new ActionListener() {

				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@SuppressWarnings("static-access")
				public void actionPerformed(ActionEvent e) {

					MainWindow mainWindow = MainWindow.getInstance();
					Language language = Language.getInstance();
					try {
						language.getLanguage(Integer.parseInt(PropertiesManager
								.getProperty("language")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = language.getLabels();
					_logger.info(labels.getString("s99"));
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().copy();
					mainWindow.getOutput().setText(labels.getString("s99"));
				}
			});
			_popup.add(_copyMenu);
			_cutMenu = new JMenuItem(labels.getString("s188"));
			_cutMenu.addActionListener(new ActionListener() {
				
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@SuppressWarnings("static-access")
				public void actionPerformed(ActionEvent e) {

					MainWindow mainWindow = MainWindow.getInstance();
					Language language = Language.getInstance();
					try {
						language.getLanguage(Integer.parseInt(PropertiesManager
								.getProperty("language")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = language.getLabels();
					_logger.info(labels.getString("s97"));
					mainWindow.getOutput().setText(labels.getString("s97"));
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().cut();
				}
			});
			_popup.add(_cutMenu);
			_pasteMenu = new JMenuItem(labels.getString("s189"));
			_pasteMenu.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@SuppressWarnings("static-access")
				public void actionPerformed(ActionEvent e) {
					
					MainWindow mainWindow = MainWindow.getInstance();
					Language language = Language.getInstance();
					try {
						language.getLanguage(Integer.parseInt(PropertiesManager
								.getProperty("language")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = language.getLabels();
					_logger.info(labels.getString("s98"));
					mainWindow.getOutput().setText(labels.getString("s98"));
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().paste();
				}
			});
			_popup.add(_pasteMenu);
			_selectAllMenu = new JMenuItem(labels.getString("s191"));
			_selectAllMenu.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@SuppressWarnings("static-access")
				public void actionPerformed(ActionEvent e) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().setCaretPosition(0);
					int length = mainWindow
							.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().getText().length();
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().setSelectionEnd(length);
				}
			});
			_popup.add(_selectAllMenu);
			_popup.addSeparator();
			
			_addFileMenu = new JMenuItem(labels.getString("s17"));
			_addFileMenu.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getAnadirFichero2().doClick();
				}
			});
			_removeFileMenu = new JMenuItem(labels.getString("s618"));
			_removeFileMenu.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getRemoveFile2().doClick();
				}
			});
			_popup.add(_removeFileMenu);
			
			_deleteFileMenu = new JMenuItem(labels.getString("s950"));
			_deleteFileMenu.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getDeleteFile2().doClick();
				}
			});
			_popup.add(_deleteFileMenu);
			_popup.addSeparator();
			
			_setCompilable = new JMenuItem(labels.getString("s254"));
			_setCompilable.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getSetCompilable().setEnabled(true);
					mainWindow.getMenu().getFile().getSetCompilable().doClick();
				}
			});
			_popup.add(_setCompilable);

			_unsetCompilable = new JMenuItem(labels.getString("s255"));
			_unsetCompilable.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getUnsetCompilable().setEnabled(true);
					mainWindow.getMenu().getFile().getUnsetCompilable().doClick();
				}
			});
			_popup.add(_unsetCompilable);

			_setMain = new JMenuItem(labels.getString("s256"));
			_setMain.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getSetMain().setEnabled(true);
					mainWindow.getMenu().getFile().getSetMain().doClick();
				}
			});
			_popup.add(_setMain);

			_unsetMain = new JMenuItem(labels.getString("s952"));
			_unsetMain.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getUnsetMain().setEnabled(true);
					mainWindow.getMenu().getFile().getUnsetMain().doClick();
				}
			});
			_popup.add(_unsetMain);
			_popup.addSeparator();
			
			_printMenu = new JMenuItem(labels.getString("s624"));
			_printMenu.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent e) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getPrintFile().setEnabled(true);
					mainWindow.getMenu().getFile().getPrintFile().doClick();
				}
			});
			_popup.add(_printMenu);
			
			// Editor2 
			_popup2 = new JPopupMenu();
			_copyMenu2 = new JMenuItem(labels.getString("s187"));
			_copyMenu2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@SuppressWarnings("static-access")
				public void actionPerformed(ActionEvent e) {
					
					MainWindow mainWindow = MainWindow.getInstance();
					Language language = Language.getInstance();
					try {
						language.getLanguage(Integer.parseInt(PropertiesManager
								.getProperty("language")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = language.getLabels();
					_logger.info(labels.getString("s99"));
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().copy();
					mainWindow.getOutput().setText(labels.getString("s99"));
				}
			});
			_popup2.add(_copyMenu2);
			
			_cutMenu2 = new JMenuItem(labels.getString("s188"));
			_cutMenu2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@SuppressWarnings("static-access")
				public void actionPerformed(ActionEvent e) {

					MainWindow mainWindow = MainWindow.getInstance();
					Language language = Language.getInstance();
					try {
						language.getLanguage(Integer.parseInt(PropertiesManager
								.getProperty("language")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = language.getLabels();
					_logger.info(labels.getString("s97"));
					mainWindow.getOutput().setText(labels.getString("s97"));
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().cut();
				}
			});
			_popup2.add(_cutMenu2);
			
			_pasteMenu2 = new JMenuItem(labels.getString("s189"));
			_pasteMenu2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@SuppressWarnings("static-access")
				public void actionPerformed(ActionEvent e) {
					
					MainWindow mainWindow = MainWindow.getInstance();
					Language language = Language.getInstance();
					try {
						language.getLanguage(Integer.parseInt(PropertiesManager
								.getProperty("language")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = language.getLabels();
					_logger.info(labels.getString("s98"));
					mainWindow.getOutput().setText(labels.getString("s98"));
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().paste();
				}
			});
			_popup2.add(_pasteMenu2);
			
			_selectAllMenu2 = new JMenuItem(labels.getString("s191"));
			_selectAllMenu2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@SuppressWarnings("static-access")
				public void actionPerformed(ActionEvent e) {
					
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().setCaretPosition(0);
					int length = mainWindow
							.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().getText().length();
					mainWindow.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getEditor().setSelectionEnd(length);
				}
			});
			_popup2.add(_selectAllMenu2);
			_popup2.addSeparator();

			_addFileMenu2 = new JMenuItem(labels.getString("s17"));
			_addFileMenu2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getAnadirFichero2().doClick();
				}
			});
			_popup2.add(_addFileMenu2);
			
			_removeFileMenu2 = new JMenuItem(labels.getString("s618"));
			_removeFileMenu2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getRemoveFile2().doClick();
				}
			});
			_popup2.add(_removeFileMenu2);
			
			_deleteFileMenu2 = new JMenuItem(labels.getString("s950"));
			_deleteFileMenu2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getDeleteFile2().doClick();
				}
			});
			_popup2.add(_deleteFileMenu2);
			_popup2.addSeparator();

			_setCompilable2 = new JMenuItem(labels.getString("s254"));
			_setCompilable2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getSetCompilable().setEnabled(true);
					mainWindow.getMenu().getFile().getSetCompilable().doClick();
				}
			});
			_popup2.add(_setCompilable2);

			_unsetCompilable2 = new JMenuItem(labels.getString("s255"));
			_unsetCompilable2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getUnsetCompilable().setEnabled(true);
					mainWindow.getMenu().getFile().getUnsetCompilable().doClick();
				}
			});
			_popup2.add(_unsetCompilable2);

			_setMain2 = new JMenuItem(labels.getString("s256"));
			_setMain2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getSetMain().setEnabled(true);
					mainWindow.getMenu().getFile().getSetMain().doClick();
				}
			});
			_popup2.add(_setMain2);

			_unsetMain2 = new JMenuItem(labels.getString("s952"));
			_unsetMain2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent arg0) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getUnsetMain().setEnabled(true);
					mainWindow.getMenu().getFile().getUnsetMain().doClick();
				}
			});
			_popup2.add(_unsetMain2);
			_popup2.addSeparator();

			_printMenu2 = new JMenuItem(labels.getString("s624"));
			_printMenu2.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent e) {
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getFile().getPrintFile().setEnabled(true);
					mainWindow.getMenu().getFile().getPrintFile().doClick();
				}
			});
			_doubleClick = new EditorDoubleClick();
			_popup2.add(_printMenu2);
			
			_editor.addMouseListener(new PopupListener());
			_editor2.addMouseListener(new PopupListener2());
			_editor.addMouseListener(_doubleClick);
			_editor2.addMouseListener(_doubleClick);
			setLayout(new BorderLayout());
			_scrollPane = new JScrollPane(_editor);
			_scrollPane2 = new JScrollPane(_editor2);
			_lineNumber = new LineNumber(_editor);
			_lineNumber2 = new LineNumber(_editor2);
			_scrollPane.setRowHeaderView(_lineNumber);
			_scrollPane2.setRowHeaderView(_lineNumber2);
			_click1 = new EditorMouseClick1();
			_click2 = new EditorMouseClick2();
			_scrollPane.getVerticalScrollBar().addMouseListener(_click1);
			_scrollPane.getHorizontalScrollBar().addMouseListener(_click1);
			_scrollPane2.getVerticalScrollBar().addMouseListener(_click2);
			_scrollPane2.getHorizontalScrollBar().addMouseListener(_click2);
			_scrollPane.addMouseListener(_click1);
			_scrollPane2.addMouseListener(_click2);

			_buttonClick1 = new EditorButtonClick1();
			_buttonClick2 = new EditorButtonClick2();
			Component comp[] = _scrollPane.getVerticalScrollBar().getComponents();
			for (int i = 0; i < comp.length; i++) {
				if (comp[i] instanceof BasicArrowButton) {
					comp[i].addMouseListener(_buttonClick1);
				}
			}

			Component comph[] = _scrollPane.getHorizontalScrollBar()
					.getComponents();
			for (int i = 0; i < comph.length; i++) {
				if (comph[i] instanceof BasicArrowButton) {
					comph[i].addMouseListener(_buttonClick1);
				}
			}

			Component comp2[] = _scrollPane2.getVerticalScrollBar().getComponents();
			for (int i = 0; i < comp2.length; i++) {
				if (comp2[i] instanceof BasicArrowButton) {
					comp2[i].addMouseListener(_buttonClick2);
				}
			}

			Component comph2[] = _scrollPane.getHorizontalScrollBar()
					.getComponents();
			for (int i = 0; i < comph2.length; i++) {
				if (comph2[i] instanceof BasicArrowButton) {
					comph2[i].addMouseListener(_buttonClick2);
				}
			}

			_editor1Listener = new MyDocumentListener();
			_editor.getDocument().addDocumentListener(_editor1Listener);
			_editor2Listener = new MyDocumentListener2();
			_editor2.getDocument().addDocumentListener(_editor2Listener);
			_caretListener1 = new MyCaretListener1();
			_editor.addCaretListener(_caretListener1);
			_caretListener2 = new MyCaretListener2();
			_editor2.addCaretListener(_caretListener2);
			_scrollPane.setMinimumSize(new Dimension(0, 0));
			_scrollPane2.setMinimumSize(new Dimension(0, 0));
			_splitPaneHorizontal = new JSplitPane(JSplitPane.VERTICAL_SPLIT, _scrollPane,
					_scrollPane2);
			_splitPaneHorizontal.setDividerLocation(0);
			_splitPaneHorizontal.setContinuousLayout(true);
			_adjustmentListener1 = new MyAdjustmentListener1();
			_adjustmentListener2 = new MyAdjustmentListener2();
			_scrollPane.getVerticalScrollBar().addAdjustmentListener(_adjustmentListener1);
			_scrollPane2.getVerticalScrollBar().addAdjustmentListener(_adjustmentListener2);
			add(_splitPaneHorizontal);
			_logger.info(labels.getString("s318"));
			_path = null;
			_lastChange = 0;
			_lastSize = 0;
			
			AcideKeyboardListener teclado = new AcideKeyboardListener();
			_editor.addKeyListener(teclado);
			_editor2.addKeyListener(teclado);
			AcideMouseListener mouse = new AcideMouseListener();
			_editor.addMouseListener(mouse);
			_editor2.addMouseListener(mouse);
			AcideKeyboardListener2 teclado2 = new AcideKeyboardListener2();
			_editor.addKeyListener(teclado2);
			_editor2.addKeyListener(teclado2);

		} catch (Exception e) {
			_logger.info(labels.getString("s319"));
			e.printStackTrace();
		}
	}

	/**
	 * 
	 * @return
	 */
	protected JTextPane buildEditor() {
		
		_textArea = new JTextPane(_syntaxisDoc) {

			/**
			 * serialVersionUID
			 */
			private static final long serialVersionUID = 1L;

			/*
			 * (non-Javadoc)
			 * @see java.awt.Component#setSize(java.awt.Dimension)
			 */
			public void setSize(Dimension d) {
				if (d.width < getParent().getSize().width)
					d.width = getParent().getSize().width;
				super.setSize(d);
			}

			/*
			 * (non-Javadoc)
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
	 * 
	 * @return 
	 */
	public static JTextComponent getEditor() {

		int pos = MainWindow.getInstance().getEditorBuilder()
				.getSelectedEditorIndex();
		int active = MainWindow.getInstance().getEditorBuilder()
				.getEditorAt(pos)._activeEditor;
		if (active == 2)
			return MainWindow.getInstance().getEditorBuilder().getEditorAt(pos)._editor2;
		else
			return MainWindow.getInstance().getEditorBuilder().getEditorAt(pos)._editor;

	}

	/**
	 * 
	 * @param text
	 */
	public void loadText(String text) {
		
		_editor2.getDocument().removeDocumentListener(_editor2Listener);
		_editor.getDocument().removeDocumentListener(_editor1Listener);
		_editor.setText(text);
		
		String sold = MainWindow.getInstance().getEditorBuilder().getTabbedPane()
				.getTitleAt(MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditorIndex());
		
		sold = sold.replace(" *", "");
		
		MainWindow.getInstance().getEditorBuilder().getTabbedPane().setTitleAt(MainWindow
				.getInstance().getEditorBuilder().getSelectedEditorIndex(), sold);
		
		_editor.setCaretPosition(0);
		_editor2.setCaretPosition(0);
		_editor2.getDocument().addDocumentListener(_editor2Listener);
		_editor.getDocument().addDocumentListener(_editor1Listener);
		
		revalidate();
	}

	/**
	 * 
	 * @param start
	 * @param length
	 */
	public void selectText(int start, int length) {
		_editor.setSelectionStart(start);
		_editor.setSelectionEnd(start + length);
		_editor2.setSelectionStart(start);
		_editor2.setSelectionEnd(start + length);
		if (_activeEditor == 1)
			_editor.requestFocus();
		else
			_editor2.requestFocus();
	}

	/**
	 * 
	 * 
	 * @return
	 */
	public String getText() {
		return _editor.getText();
	}

	/**
	 * 
	 * @param path
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * 
	 * @return
	 */
	public String getPath() {
		return _path;
	}

	/**
	 * 
	 * @return
	 */
	public String getFilePath() {
		return _path.substring(0, _path.lastIndexOf("\\"));
	}

	/**
	 * 
	 * @return
	 */
	public String getFileExtension() {
		return _path.substring(_path.lastIndexOf(".") + 1);
	}

	/**
	 * 
	 * @return
	 */
	public CharSequence getFileName() {

		return _path.substring(_path.lastIndexOf("\\") + 1,
				_path.lastIndexOf("."));
	}

	/**
	 * 
	 * @return
	 */
	public long getLastChange() {
		return _lastChange;
	}

	/**
	 * 
	 * @param lastChange
	 */
	public void setLastChange(long lastChange) {
		_lastChange = lastChange;
	}

	/**
	 * 
	 * @return
	 */
	public long getLastSize() {
		return _lastSize;
	}

	/**
	 * 
	 * @param b
	 */
	public void setEditable(boolean b) {
		_editor.setEditable(b);
		_editor2.setEditable(b);
	}

	/**
	 * 
	 * @param lastSize
	 */
	public void setLastSize(long lastSize) {
		_lastSize = lastSize;
	}

	/**
	 * 
	 */
	class PopupListener extends MouseAdapter {

		/**
		 * 
		 */
		public void mousePressed(MouseEvent e) {
			maybeShowPopup(e);
		}

		/**
		 * 
		 */
		public void mouseReleased(MouseEvent e) {
			maybeShowPopup(e);
		}

		/**
		 * 
		 * @param e
		 */
		private void maybeShowPopup(MouseEvent e) {
			if (e.isPopupTrigger()) {
				
				MainWindow mainWindow = MainWindow.getInstance();
				String prj = null;
				try {
					prj = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					mainWindow.getEditor().getAddFile().setEnabled(false);
					mainWindow.getEditor().getRemoveFile().setEnabled(false);
				} else {
					String file = mainWindow
							.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getPath();
					boolean a = false;
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration().getFile(i).getPath()
								.equals(file)) {
							a = true;
						}
					}
					if (a) {
						mainWindow.getEditor().getRemoveFile().setEnabled(true);
						mainWindow.getEditor().getAddFile().setEnabled(false);
					} else {
						mainWindow.getEditor().getRemoveFile().setEnabled(false);
						mainWindow.getEditor().getAddFile().setEnabled(true);
					}
				}
				_popup.show(e.getComponent(), e.getX(), e.getY());
			}
		}
	}

	/**
	 * 
	 */
	class PopupListener2 extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent e) {
			maybeShowPopup(e);
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
		 */
		public void mouseReleased(MouseEvent e) {
			maybeShowPopup(e);
		}

		/**
		 * 
		 * @param e
		 */
		@SuppressWarnings("static-access")
		private void maybeShowPopup(MouseEvent e) {
			if (e.isPopupTrigger()) {

				MainWindow mainWindow = MainWindow.getInstance();
				Editor edit = mainWindow.getEditorBuilder().getSelectedEditor();
				edit.getCopy2().setEnabled(false);
				edit.getCut2().setEnabled(false);
				edit.getPaste2().setEnabled(false);
				edit.getAddFile2().setEnabled(false);
				edit.getRemoveFile2().setEnabled(false);
				edit.getSetCompilable2().setEnabled(false);
				edit.getUnsetCompilable2().setEnabled(false);
				edit.getSetMain2().setEnabled(false);
				edit.getUnsetMain2().setEnabled(false);

				String prj2 = null;

				try {
					prj2 = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if (Toolkit.getDefaultToolkit().getSystemClipboard()
						.getContents(null) != null) {
					edit.getPaste2().setEnabled(true);
				}
				if (edit.getEditor().getSelectedText() != null) {
					edit.getCopy2().setEnabled(true);
					edit.getCut2().setEnabled(true);
				}
				if ((prj2.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					edit.getAddFile2().setEnabled(false);
					edit.getRemoveFile2().setEnabled(false);
				} else {
					String file = mainWindow
							.getEditorBuilder()
							.getEditorAt(
									mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.getPath();
					boolean a = false;
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration().getFile(i).getPath()
								.equals(file)) {
							a = true;
						}
					}
					if (a) {
						edit.getRemoveFile2().setEnabled(true);
						edit.getAddFile2().setEnabled(false);
					} else {
						edit.getRemoveFile2().setEnabled(false);
						edit.getAddFile2().setEnabled(true);
					}
				}

				if ((prj2.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					if (!edit.isMainFile())
						edit.getSetMain2().setEnabled(true);
					if (edit.isMainFile())
						edit.getUnsetMain2().setEnabled(true);
					if (!edit.isCompilerFile()
							|| (edit.isCompilerFile() && edit.isMainFile()))
						edit.getSetCompilable2().setEnabled(true);
					if (edit.isCompilerFile() && !edit.isMainFile())
						edit.getUnsetCompilable2().setEnabled(true);
				} else {
					boolean belongsToTheProject = false;
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration().getFile(i).getPath()
								.equals(edit.getPath()))
							belongsToTheProject = true;
					}
					if (belongsToTheProject) {
						if (!edit.isMainFile())
							edit.getSetMain2().setEnabled(true);
						if (edit.isMainFile())
							edit.getUnsetMain2().setEnabled(true);
						if (!edit.isCompilerFile()
								|| (edit.isCompilerFile() && edit.isMainFile()))
							edit.getSetCompilable2().setEnabled(true);
						if (edit.isCompilerFile() && !edit.isMainFile())
							edit.getUnsetCompilable2().setEnabled(true);
					}
				}

				_popup2.show(e.getComponent(), e.getX(), e.getY());
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public SyntaxisDoc getDoc() {
		return _syntaxisDoc;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCut() {
		return _cutMenu;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCut2() {
		return _cutMenu2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCopy() {
		return _copyMenu;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCopy2() {
		return _copyMenu2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getPaste() {
		return _pasteMenu;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getPaste2() {
		return _pasteMenu2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getAddFile() {
		return _addFileMenu;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getAddFile2() {
		return _addFileMenu2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getRemoveFile() {
		return _removeFileMenu;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getRemoveFile2() {
		return _removeFileMenu2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilable;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSetCompilable2() {
		return _setCompilable2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilable;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getUnsetCompilable2() {
		return _unsetCompilable2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSetMain() {
		return _setMain;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSetMain2() {
		return _setMain2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getUnsetMain2() {
		return _unsetMain2;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMain;
	}

	/**
	 * 
	 */
	public void resetDoc() {
		String s = "";
		try {
			s = _syntaxisDoc.getText(0, _syntaxisDoc.getLength());
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		SyntaxisDoc sy = new SyntaxisDoc();
		_editor.setStyledDocument(sy);
		_editor.setText(s);
		_editor2.setStyledDocument(sy);
		_editor.setCaretPosition(0);
		_editor2.setCaretPosition(0);
		_editor.getDocument().addDocumentListener(_editor1Listener);
		_editor2.getDocument().addDocumentListener(_editor2Listener);
	}

	/**
	 * Position the caret at the start of a line.
	 *
	 * @param line
	 */
	public void goToLine(int line) {
		JTextComponent component = getEditor();
		Element root = component.getDocument().getDefaultRootElement();
		line = Math.max(line, 1);
		line = Math.min(line, root.getElementCount());
		component.setCaretPosition(root.getElement(line - 1).getStartOffset());
	}

	/**
	 * 
	 */
	class MyDocumentListener implements DocumentListener {
		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.DocumentEvent)
		 */
		public void insertUpdate(DocumentEvent e) {
			MainWindow
					.getInstance()
					.getEditorBuilder()
					.getTestPlaf()
					.getCloseButtoni(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).redButton();

			MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.DocumentEvent)
		 */
		public void removeUpdate(DocumentEvent e) {
			MainWindow
					.getInstance()
					.getEditorBuilder()
					.getTestPlaf()
					.getCloseButtoni(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).redButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.DocumentEvent)
		 */
		public void changedUpdate(DocumentEvent e) {
			MainWindow
					.getInstance()
					.getEditorBuilder()
					.getTestPlaf()
					.getCloseButtoni(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).redButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		}

		/**
		 * 
		 * @param action
		 */
		public void textChanged(String action) {
			MainWindow
					.getInstance()
					.getEditorBuilder()
					.getTestPlaf()
					.getCloseButtoni(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).redButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		}
	}

	/**
	 * 
	 */
	class MyDocumentListener2 implements DocumentListener {

		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.DocumentEvent)
		 */
		public void insertUpdate(DocumentEvent e) {
			MainWindow
					.getInstance()
					.getEditorBuilder()
					.getTestPlaf()
					.getCloseButtoni(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).redButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.DocumentEvent)
		 */
		public void removeUpdate(DocumentEvent e) {
			MainWindow
					.getInstance()
					.getEditorBuilder()
					.getTestPlaf()
					.getCloseButtoni(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).redButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.DocumentEvent)
		 */
		public void changedUpdate(DocumentEvent e) {
			MainWindow
					.getInstance()
					.getEditorBuilder()
					.getTestPlaf()
					.getCloseButtoni(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).redButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		}

		/**
		 * 
		 * @param action
		 */
		public void textChanged(String action) {
			MainWindow
					.getInstance()
					.getEditorBuilder()
					.getTestPlaf()
					.getCloseButtoni(
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditorIndex()).redButton();
			MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		}
	}

	/**
	 * 
	 */
	class MyCaretListener1 implements CaretListener {

		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent)
		 */
		public void caretUpdate(CaretEvent e) {
			if (_editor.isFocusOwner()) {
				_activeEditor = 1;
			}
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
	 * 
	 */
	class MyCaretListener2 implements CaretListener {

		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent)
		 */
		public void caretUpdate(CaretEvent e) {
			if (_editor2.isFocusOwner()) {
				_activeEditor = 2;
			}
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
	 * 
	 */
	class MyAdjustmentListener1 implements AdjustmentListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.AdjustmentListener#adjustmentValueChanged(java.awt.event.AdjustmentEvent)
		 */
		public void adjustmentValueChanged(AdjustmentEvent evt) {
			Adjustable source = evt.getAdjustable();
			if (evt.getValueIsAdjusting()) {
				_verticalValue = _scrollPane.getVerticalScrollBar().getValue();
				_horizontalValue = _scrollPane.getHorizontalScrollBar().getValue();
				_verticalValue2 = _scrollPane2.getVerticalScrollBar().getValue();
				_horizontalValue2 = _scrollPane2.getHorizontalScrollBar().getValue();
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
			if (_editor.isFocusOwner()
					|| _scrollPane.getVerticalScrollBar().isFocusOwner()
					|| _scrollPane.getHorizontalScrollBar().isFocusOwner()) {
				_scrollPane2.getVerticalScrollBar().setValue(_verticalValue2);
				_scrollPane2.getHorizontalScrollBar().setValue(_horizontalValue2);
				_verticalValue = _scrollPane.getVerticalScrollBar().getValue();
				_horizontalValue = _scrollPane.getHorizontalScrollBar().getValue();
			}
			if (_editor2.isFocusOwner()
					|| _scrollPane2.getVerticalScrollBar().isFocusOwner()
					|| _scrollPane2.getHorizontalScrollBar().isFocusOwner()) {
				_scrollPane.getVerticalScrollBar().setValue(_verticalValue);
				_scrollPane.getHorizontalScrollBar().setValue(_horizontalValue);
				_verticalValue2 = _scrollPane2.getVerticalScrollBar().getValue();
				_horizontalValue2 = _scrollPane2.getHorizontalScrollBar().getValue();
			}
		}
	}

	/**
	 * 
	 */
	class MyAdjustmentListener2 implements AdjustmentListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.AdjustmentListener#adjustmentValueChanged(java.awt.event.AdjustmentEvent)
		 */
		public void adjustmentValueChanged(AdjustmentEvent evt) {
			Adjustable source = evt.getAdjustable();
			if (evt.getValueIsAdjusting()) {
				_verticalValue2 = _scrollPane2.getVerticalScrollBar().getValue();
				_horizontalValue2 = _scrollPane2.getHorizontalScrollBar().getValue();
				_verticalValue = _scrollPane.getVerticalScrollBar().getValue();
				_horizontalValue = _scrollPane.getHorizontalScrollBar().getValue();
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
			if (_editor.isFocusOwner()
					|| _scrollPane.getVerticalScrollBar().isFocusOwner()
					|| _scrollPane.getHorizontalScrollBar().isFocusOwner()) {
				_scrollPane2.getVerticalScrollBar().setValue(_verticalValue2);
				_scrollPane2.getHorizontalScrollBar().setValue(_horizontalValue2);
				_verticalValue = _scrollPane.getVerticalScrollBar().getValue();
				_horizontalValue = _scrollPane.getHorizontalScrollBar().getValue();
			}
			if (_editor2.isFocusOwner()
					|| _scrollPane2.getVerticalScrollBar().isFocusOwner()
					|| _scrollPane2.getHorizontalScrollBar().isFocusOwner()) {
				_scrollPane.getVerticalScrollBar().setValue(_verticalValue);
				_scrollPane.getHorizontalScrollBar().setValue(_horizontalValue);
				_verticalValue2 = _scrollPane2.getVerticalScrollBar().getValue();
				_horizontalValue2 = _scrollPane2.getHorizontalScrollBar().getValue();
			}
		}
	}

	/**
	 * 
	 */
	class EditorMouseClick1 extends MouseAdapter{

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {
			_editor.requestFocus();
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			_editor.requestFocus();
		}
	}

	/**
	 * 
	 */
	class EditorMouseClick2 extends MouseAdapter{

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {
			_editor2.requestFocus();
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			_editor2.requestFocus();
		}
	}

	/**
	 * 
	 */
	class EditorButtonClick1 extends MouseAdapter{

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {
			_editor.requestFocus();
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			_editor.requestFocus();
		}
	}

	/**
	 * 
	 */
	class EditorButtonClick2 extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {
			_editor2.requestFocus();
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			_editor2.requestFocus();
		}
	}

	/**
	 * 
	 */
	class EditorDoubleClick extends MouseAdapter {
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
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

			MainWindow mainWindow = MainWindow.getInstance();
			String prj = null;
			try {
				prj = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
					.getProjectConfiguration().getName().equals(""))) {

				ExplorerFile f = new ExplorerFile();
				int y = -1;
				for (int j = 0; j < mainWindow.getProjectConfiguration().getNumFilesFromList(); j++) {

					if (mainWindow.getProjectConfiguration()
							.getFile(j)
							.getPath()
							.equals(mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath())) {
						f = mainWindow.getProjectConfiguration().getFile(j);
						for (int m = 0; m < mainWindow.getProjectConfiguration()
								.getNumFilesFromList() + 1; m++) {
							if (mainWindow.getExplorer().getTree().getPathForRow(m)
									.getLastPathComponent().toString()
									.equals(f.getLastPathComponent())) {

								y = m;
							}
						}
					}
				}

				TreePath currentSelection = mainWindow.getExplorer().getTree()
						.getPathForRow(y);
				mainWindow.getExplorer().getTree().setSelectionPath(currentSelection);
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCompilerFile() {
		return _isCompiledFile;
	}

	/**
	 * 
	 * @param compilerFile
	 */
	public void setCompilerFile(boolean compilerFile) {
		_isCompiledFile = compilerFile;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isMainFile() {
		return _isMainFile;
	}

	/**
	 * 
	 * @param mainFile
	 */
	public void setMainFile(boolean mainFile) {
		_isMainFile = mainFile;
	}

	/**
	 * 
	 * @return
	 */
	public Icon getIcon() {
		return _icon;
	}

	/**
	 * 
	 * @param i
	 */
	public void setIcon(Icon i) {
		_icon = i;
	}
}