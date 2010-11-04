package gui.menu.edit;

import es.configuration.menu.MenuConfiguration;
import gui.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

import language.Language;

import operations.log.Log;
import properties.PropertiesManager;

/**
 * Edit menu of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class EditMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the icon of the undo menu item.
	 */
	private final static String UNDO = "./resources/icons/menu/edit/undo.png";
	/**
	 * Image file for the icon of the redo menu item.
	 */
	private final static String REDO = "./resources/icons/menu/edit/redo.png";
	/**
	 * Image file for the icon of the copy menu item.
	 */
	private final static String COPY = "./resources/icons/menu/edit/copy.png";
	/**
	 * Image file for the icon of the paste menu item.
	 */
	private final static String PASTE = "./resources/icons/menu/edit/paste.png";
	/**
	 * Image file for the icon of the cut menu item.
	 */
	private final static String CUT = "./resources/icons/menu/edit/cut.png";
	/**
	 * Image file for the icon of the select all menu item.
	 */
	private final static String SELECT_ALL = "./resources/icons/menu/edit/selectAll.png";
	/**
	 * Image file for the icon of the go to line menu item.
	 */
	private final static String GO_TO_LINE = "./resources/icons/menu/edit/goToLine.png";
	/**
	 * Image file for the icon of the search menu item.
	 */
	private final static String SEARCH = "./resources/icons/menu/edit/search.png";
	/**
	 * Image file for the icon of the replace menu item.
	 */
	private final static String REPLACE = "./resources/icons/menu/edit/replace.png";
	/**
	 * Undo menu item.
	 */
	private JMenuItem _undo;
	/**
	 * Redo menu item.
	 */
	private JMenuItem _redo;
	/**
	 * Search menu item.
	 */
	private JMenuItem _search;
	/**
	 * Paste menu item.
	 */
	private JMenuItem _paste;
	/**
	 * Copy menu item.
	 */
	private JMenuItem _copy;
	/**
	 * Cut menu item.
	 */
	private JMenuItem _cut;
	/**
	 * Select all menu item.
	 */
	private JMenuItem _selectAll;
	/**
	 * Go to line menu item.
	 */
	private JMenuItem _goToLine;
	/**
	 * Replace menu item.
	 */
	private JMenuItem _replace;
	/**
	 * Undo manager of the application.
	 */
	private UndoManager _undoManager = new UndoManager();
	
	/**
	 * Constructor of the class.
	 */
	public EditMenu(){
		
		// MENU ITEMS
		_undo = new JMenuItem(new ImageIcon(UNDO));
		_redo = new JMenuItem(new ImageIcon(REDO));
		_copy = new JMenuItem(new ImageIcon(COPY));
		_paste = new JMenuItem(new ImageIcon(PASTE));
		_cut = new JMenuItem(new ImageIcon(CUT));
		_search = new JMenuItem(new ImageIcon(SEARCH));
		_replace = new JMenuItem(new ImageIcon(REPLACE));
		_selectAll = new JMenuItem(new ImageIcon(SELECT_ALL));
		_goToLine = new JMenuItem(new ImageIcon(GO_TO_LINE));
		
		setLanguageLabels();
	}
	
	/**
	 * Set the language labels in the selected language.
	 */
	public void setLanguageLabels() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		disableMenu();
		
		// UNDO 
		_undo.setText(labels.getString("s21"));
		_undo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z,
				ActionEvent.CTRL_MASK));
		
		// REDO
		_redo.setText(labels.getString("s22"));
		_redo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Y,
				ActionEvent.CTRL_MASK));
		
		// COPY
		_copy.setText(labels.getString("s23"));
		_copy.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.CTRL_MASK));
		
		// CUT
		_cut.setText(labels.getString("s24"));
		_cut.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.CTRL_MASK));
		
		// PASTE
		_paste.setText(labels.getString("s25"));
		_paste.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V,
				ActionEvent.CTRL_MASK));
		
		// SEARCH
		_search.setText(labels.getString("s26"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_search.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F,
					ActionEvent.CTRL_MASK));
		else
			_search.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B,
					ActionEvent.CTRL_MASK));
		
		// REPLACE
		_replace.setText(labels.getString("s27"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_replace.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R,
					ActionEvent.CTRL_MASK));
		else
			_replace.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
					ActionEvent.CTRL_MASK));
		
		// SELECT ALL
		_selectAll.setText(labels.getString("s190"));
		_selectAll.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.CTRL_MASK));

		// GO TO LINE
		_goToLine.setText(labels.getString("s222"));
	}

	/**
	 * Builds the menu.
	 */
	public void buildMenu(){
	
		removeAll();
		
		if (MenuConfiguration.getUndo())
			add(_undo);
		if (MenuConfiguration.getRedo())
			add(_redo);
		if ((MenuConfiguration.getUndo() || MenuConfiguration.getRedo())
				&& (MenuConfiguration.getCopy()
						|| MenuConfiguration.getPaste()
						|| MenuConfiguration.getCut() || MenuConfiguration
						.getSelectAll()))
			addSeparator();
		if (MenuConfiguration.getCopy())
			add(_copy);
		if (MenuConfiguration.getPaste())
			add(_paste);
		if (MenuConfiguration.getCut())
			add(_cut);
		if (MenuConfiguration.getSelectAll())
			add(_selectAll);
		if ((MenuConfiguration.getUndo() || MenuConfiguration.getRedo()
				|| MenuConfiguration.getCopy()
				|| MenuConfiguration.getPaste()
				|| MenuConfiguration.getCut() || MenuConfiguration
				.getSelectAll()) && MenuConfiguration.getGoToLine())
			addSeparator();
		if (MenuConfiguration.getGoToLine())
			add(_goToLine);
		if ((MenuConfiguration.getUndo() || MenuConfiguration.getRedo()
				|| MenuConfiguration.getCopy()
				|| MenuConfiguration.getPaste()
				|| MenuConfiguration.getCut()
				|| MenuConfiguration.getSelectAll() || MenuConfiguration
				.getGoToLine())
				&& (MenuConfiguration.getSearch() || MenuConfiguration
						.getReplace()))
			addSeparator();
		if (MenuConfiguration.getSearch())
			add(_search);
		if (MenuConfiguration.getReplace())
			add(_replace);
	}
	
	/**
	 * Set the listeners.
	 */
	public void setListeners(){
		
		// UNDO 
		_undo.addActionListener(new UndoListener());
		
		// REDO
		_redo.addActionListener(new RedoListener());
		
		// SEARCH
		_search.addActionListener(new SearchListener());
		
		// REPLACE
		_replace.addActionListener(new ReplaceListener());
		
		// CUT
		_cut.addActionListener(new CutListener());
		
		// PASTE
		_paste.addActionListener(new PasteListener());
		
		// COPY
		_copy.addActionListener(new CopyListener());
		
		// SELECT ALL
		_selectAll.addActionListener(new SelectAllListener());
		
		// GO TO LINE
		_goToLine.addActionListener(new GoToLineListener());	
	}
	
	/**
	 * Disable the paste menu item.
	 */
	public void disablePaste() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		_paste.setEnabled(false);

		// UPDATES THE LOG
		Log.getLog().info(labels.getString("s73"));
	}

	/**
	 * Enable the paste menu item.
	 */
	public void enablePaste() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		_paste.setEnabled(true);
		
		// UPDATES THE LOG
		Log.getLog().info(labels.getString("s74"));
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSearch() {
		return _search;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getGoToLine() {
		return _goToLine;
	}

	/**
	 * 
	 * @param goToLine
	 */
	public void setGoToLine(JMenuItem goToLine) {
		_goToLine = goToLine;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getUndo() {
		return _undo;
	}

	/**
	 * 
	 * @param undo
	 */
	public void setUndo(JMenuItem undo) {
		_undo = undo;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * 
	 * @param copy
	 */
	public void setCopy(JMenuItem copy) {
		_copy = copy;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * 
	 * @param cut
	 */
	public void setCut(JMenuItem cut) {
		_cut = cut;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getPaste() {
		return _paste;
	}

	/**
	 * 
	 * @param paste
	 */
	public void setPaste(JMenuItem paste) {
		_paste = paste;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getReplace() {
		return _replace;
	}

	/**
	 * 
	 * @param replace
	 */
	public void setReplace(JMenuItem replace) {
		_replace = replace;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getRepeat() {
		return _redo;
	}

	/**
	 * 
	 * @param repetir
	 */
	public void setRepeat(JMenuItem repetir) {
		_redo = repetir;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSelectAll() {
		return _selectAll;
	}

	/**
	 * 
	 * @param selectAll
	 */
	public void setSelectAll(JMenuItem selectAll) {
		_selectAll = selectAll;
	}
	
	/**
	 * 
	 * @param search
	 */
	public void setSearch(JMenuItem search) {
		_search = search;
	}

	/**
	 * 
	 * @return
	 */
	public UndoManager getUndoManager() {
		return _undoManager;
	}
	
	/**
	 * 
	 * @param undoManager
	 */
	public void setUndoManager(UndoManager undoManager) {
		_undoManager = undoManager;
	}
	
	/**
	 * Enable the edit menu.
	 */
	public void enableMenu() {
		
		_undo.setEnabled(true);
		_redo.setEnabled(true);
		_copy.setEnabled(true);
		_paste.setEnabled(true);
		_cut.setEnabled(true);
		_selectAll.setEnabled(true);
		_goToLine.setEnabled(true);
		_search.setEnabled(true);
		_replace.setEnabled(true);
	}
	
	/**
	 * Disable the edit menu.
	 */
	public void disableMenu() {
		
		_undo.setEnabled(false);
		_redo.setEnabled(false);
		_copy.setEnabled(false);
		_paste.setEnabled(false);
		_cut.setEnabled(false);
		_selectAll.setEnabled(false);
		_goToLine.setEnabled(false);
		_search.setEnabled(false);
		_replace.setEnabled(false);
	}
}

/**
 * Search menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class SearchListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		if (SearchGUI.getInstance().isShowing())
			SearchGUI.getInstance().setVisible(false);
		else
			SearchGUI.getInstance().setVisible(true);
	}
}

/**
 * Replace menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class ReplaceListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		// UPDATES THE LOG
		Log.getLog().info(labels.getString("s96"));
		
		if (ReplaceGUI.getInstance().isShowing())
			ReplaceGUI.getInstance().setVisible(false);
		else
			ReplaceGUI.getInstance().setVisible(true);
	}
}

/**
 * Undo menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class UndoListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent arg0) {

		try {

			if (MainWindow.getInstance().getMenu().getEdit().getUndoManager().canUndo())
				MainWindow.getInstance().getMenu().getEdit().getUndoManager().undo();
			else
				MainWindow.getInstance().getMenu().getEdit().getUndoManager().die();
		} catch (CannotUndoException e) {
		
		}
	}
}

/**
 * Redo menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class RedoListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent arg0) {
		
		try {
			
			if (MainWindow.getInstance().getMenu().getEdit().getUndoManager().canRedo())
				MainWindow.getInstance().getMenu().getEdit().getUndoManager().redo();
		} catch (CannotRedoException e) {
			
		}
	}
}

/**
 * Cut menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class CutListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		// UPDATES THE LOG
		Log.getLog().info(labels.getString("s97"));
		
		// CUT
		if (MainWindow.getInstance().getEditorBuilder().getNumEditors() > 0)
			MainWindow.getInstance().getEditorBuilder()
					.getEditorAt(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
					.getEditor().cut();
		MainWindow.getInstance().getOutput().getTextComponent().cut();
	}
}

/**
 * Paste menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class PasteListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		Log.getLog().info(labels.getString("s98"));
		
		// PASTE
		int numEditors = MainWindow.getInstance().getEditorBuilder().getNumEditors();
		
		if (numEditors > 0) {
			
			if (MainWindow.getInstance().getMenu().isShellFocus())
				MainWindow.getInstance().getOutput().getTextComponent().paste();
			else
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().paste();
		} else {
			MainWindow.getInstance().getOutput().getTextComponent().paste();
		}
	}
}

/**
 * Copy menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class CopyListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		// UPDATES THE LOG
		Log.getLog().info(labels.getString("s99"));
		
		// COPY
		if (MainWindow.getInstance().getEditorBuilder().getNumEditors() > 0)
			MainWindow.getInstance().getEditorBuilder()
					.getEditorAt(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
					.getEditor().copy();
		MainWindow.getInstance().getOutput().getTextComponent().copy();
	}
}

/**
 * Select all menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class SelectAllListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		// SET THE CARET IN THE FIRST POSITION
		MainWindow.getInstance().getEditorBuilder()
				.getEditorAt(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
				.getEditor().setCaretPosition(0);
		
		// OBTAINS THE TEXT LENGHT
		int length = MainWindow.getInstance().getEditorBuilder()
				.getEditorAt(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
				.getEditor().getText().length();
		
		// SET THE SELECTION FROM THE FIRST TO THE LAST
		MainWindow.getInstance().getEditorBuilder()
				.getEditorAt(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
				.getEditor().setSelectionEnd(length);
	}
}

/**
 * Go to line menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class GoToLineListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent arg0) {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		// ASK FOR THE LINE
		String line = (String) JOptionPane.showInputDialog(null,
				labels.getString("s448"), labels.getString("s447"),
				JOptionPane.YES_NO_CANCEL_OPTION, null, null, null);

// TODO: COMPROBAR QUE EL NòMERO DE LêNEA SEA UN NòMERO
		if ((line != null)) {
			try {
				
				int selectedEditor = MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex();
				
				if (selectedEditor >= 0)
					MainWindow.getInstance().getEditorBuilder().getEditorAt(selectedEditor)
							.goToLine(Integer.parseInt(line));
			} catch (Exception e) {
				
			}
		}
	}
}
