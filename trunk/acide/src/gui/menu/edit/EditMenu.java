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

import org.apache.log4j.Logger;

import language.Language;

import operations.log.Log;
import properties.PropertiesManager;

/**
 * 
 */
public class EditMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private final static String UNDO = "./resources/icons/menu/edit/undo.png";
	/**
	 * 
	 */
	private final static String REDO = "./resources/icons/menu/edit/redo.png";
	/**
	 * 
	 */
	private final static String COPY = "./resources/icons/menu/edit/copy.png";
	/**
	 * 
	 */
	private final static String PASTE = "./resources/icons/menu/edit/paste.png";
	/**
	 * 
	 */
	private final static String CUT = "./resources/icons/menu/edit/cut.png";
	/**
	 * 
	 */
	private final static String SELECT_ALL = "./resources/icons/menu/edit/selectAll.png";
	/**
	 * 
	 */
	private final static String GO_TO_LINE = "./resources/icons/menu/edit/goToLine.png";
	/**
	 * 
	 */
	private final static String SEARCH = "./resources/icons/menu/edit/search.png";
	/**
	 * 
	 */
	private final static String REPLACE = "./resources/icons/menu/edit/replace.png";
	/**
	 * 
	 */
	private JMenuItem _undo;
	/**
	 * 
	 */
	private JMenuItem _redo;
	/**
	 * 
	 */
	private JMenuItem _search;
	/**
	 * 
	 */
	private JMenuItem _paste;
	/**
	 * 
	 */
	private JMenuItem _copy;
	/**
	 * 
	 */
	private JMenuItem _cut;
	/**
	 * 
	 */
	private JMenuItem _selectAll;
	/**
	 * 
	 */
	private JMenuItem _goToLine;
	/**
	 * 
	 */
	private JMenuItem _replace;
	/**
	 * 
	 */
	private UndoManager _undoManager = new UndoManager();
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	
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
	 * 
	 */
	public void setLanguageLabels() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
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
	 * 
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
	 * 
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
	 * 
	 */
	public void disablePaste() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();
		
		_paste.setEnabled(false);
		
		_logger.info(labels.getString("s73"));
	}

	/**
	 * 
	 */
	public void enablePaste() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		
		_paste.setEnabled(true);
		_logger.info(labels.getString("s74"));
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
	 * 
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
	 * 
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
 * 
 */
class SearchListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		SearchGUI searchGUI = SearchGUI.getInstance();
		if (searchGUI.isShowing())
			searchGUI.setVisible(false);
		else
			searchGUI.setVisible(true);
	}
}

/**
 * 
 */
class ReplaceListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		
		Logger logger = Log.getLog();

		logger.info(labels.getString("s96"));
		
		ReplaceGUI replace = ReplaceGUI.getInstance();
		if (replace.isShowing())
			replace.setVisible(false);
		else
			replace.setVisible(true);
	}
}

/**
 * 
 */
class UndoListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent arg0) {

		MainWindow mainWindow = MainWindow.getInstance();
		
		try {

			if (mainWindow.getMenu().getEdit().getUndoManager().canUndo())
				mainWindow.getMenu().getEdit().getUndoManager().undo();
			else
				mainWindow.getMenu().getEdit().getUndoManager().die();
		} catch (CannotUndoException e) {
		
		}
	}
}

/**
 *
 */
class RedoListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent arg0) {
		
		MainWindow mainWindow = MainWindow.getInstance();
		
		try {
			
			if (mainWindow.getMenu().getEdit().getUndoManager().canRedo())
				mainWindow.getMenu().getEdit().getUndoManager().redo();
		} catch (CannotRedoException e) {
			
		}
	}
}

/**
 * 
 */
class CutListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();
		
		MainWindow mainWindow = MainWindow.getInstance();
		Logger logger = Log.getLog();
		logger.info(labels.getString("s97"));
		
		if (mainWindow.getEditorBuilder().getNumEditors() > 0)
			mainWindow.getEditorBuilder()
					.getEditorAt(mainWindow.getEditorBuilder().getSelectedEditorIndex())
					.getEditor().cut();
		mainWindow.getOutput().getTextComponent().cut();
	}
}

/**
 * 
 */
class PasteListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		
		MainWindow mainWindow = MainWindow.getInstance();
		Logger logger = Log.getLog();
		logger.info(labels.getString("s98"));
		
		int editor = mainWindow.getEditorBuilder().getNumEditors();
		if (editor > 0) {
			if (mainWindow.getMenu().isShellFocus()) {
				mainWindow.getOutput().getTextComponent().paste();
			} else
				mainWindow.getEditorBuilder()
						.getEditorAt(mainWindow.getEditorBuilder().getSelectedEditorIndex())
						.getEditor().paste();
		} else {
			mainWindow.getOutput().getTextComponent().paste();
		}
	}
}

/**
 * 
 */
class CopyListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();
		
		MainWindow mainWindow = MainWindow.getInstance();
		Logger logger = Log.getLog();logger.info(labels.getString("s99"));
		if (mainWindow.getEditorBuilder().getNumEditors() > 0)
			mainWindow.getEditorBuilder()
					.getEditorAt(mainWindow.getEditorBuilder().getSelectedEditorIndex())
					.getEditor().copy();
		mainWindow.getOutput().getTextComponent().copy();
	}
}

/**
 * 
 */
class SelectAllListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.getEditorBuilder()
				.getEditorAt(mainWindow.getEditorBuilder().getSelectedEditorIndex())
				.getEditor().setCaretPosition(0);
		int length = mainWindow.getEditorBuilder()
				.getEditorAt(mainWindow.getEditorBuilder().getSelectedEditorIndex())
				.getEditor().getText().length();
		mainWindow.getEditorBuilder()
				.getEditorAt(mainWindow.getEditorBuilder().getSelectedEditorIndex())
				.getEditor().setSelectionEnd(length);
	}
}

/**
 * 
 */
class GoToLineListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent arg0) {
		
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();
		
		String n = (String) JOptionPane.showInputDialog(null,
				labels.getString("s448"), labels.getString("s447"),
				JOptionPane.YES_NO_CANCEL_OPTION, null, null, null);

		if ((n != null)) {
			try {
				MainWindow mainWindow = MainWindow.getInstance();
				int e = mainWindow.getEditorBuilder().getSelectedEditorIndex();
				if (e >= 0)
					mainWindow.getEditorBuilder().getEditorAt(e)
							.goToLine(Integer.parseInt(n));
			} catch (Exception e) {
				// Not a number
			}
		}
	}
}
