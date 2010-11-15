package gui.menu.edit;

import es.configuration.menu.MenuConfiguration;
import gui.menu.edit.listeners.CopyListener;
import gui.menu.edit.listeners.CutListener;
import gui.menu.edit.listeners.GoToLineListener;
import gui.menu.edit.listeners.PasteListener;
import gui.menu.edit.listeners.RedoListener;
import gui.menu.edit.listeners.ReplaceListener;
import gui.menu.edit.listeners.SearchListener;
import gui.menu.edit.listeners.SelectAllListener;
import gui.menu.edit.listeners.UndoListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.undo.UndoManager;

import language.Language;

import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************
 * Edit menu of ACIDE - A Configurable IDE
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
 * @see JMenu
 ***********************************************************************/
public class EditMenu extends JMenu {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the undo menu item icon
	 */
	private final static String UNDO = "./resources/icons/menu/edit/undo.png";
	/**
	 * Image file for the redo menu item icon
	 */
	private final static String REDO = "./resources/icons/menu/edit/redo.png";
	/**
	 * Image file for the copy menu item icon
	 */
	private final static String COPY = "./resources/icons/menu/edit/copy.png";
	/**
	 * Image file for the paste menu item icon
	 */
	private final static String PASTE = "./resources/icons/menu/edit/paste.png";
	/**
	 * Image file for the cut menu item icon
	 */
	private final static String CUT = "./resources/icons/menu/edit/cut.png";
	/**
	 * Image file for the select all menu item icon
	 */
	private final static String SELECT_ALL = "./resources/icons/menu/edit/selectAll.png";
	/**
	 * Image file for the go to line menu item icon
	 */
	private final static String GO_TO_LINE = "./resources/icons/menu/edit/goToLine.png";
	/**
	 * Image file for the search menu item icon
	 */
	private final static String SEARCH = "./resources/icons/menu/edit/search.png";
	/**
	 * Image file for the replace menu item icon
	 */
	private final static String REPLACE = "./resources/icons/menu/edit/replace.png";
	/**
	 * Undo menu item
	 */
	private JMenuItem _undo;
	/**
	 * Redo menu item
	 */
	private JMenuItem _redo;
	/**
	 * Search menu item
	 */
	private JMenuItem _search;
	/**
	 * Paste menu item
	 */
	private JMenuItem _paste;
	/**
	 * Copy menu item
	 */
	private JMenuItem _copy;
	/**
	 * Cut menu item
	 */
	private JMenuItem _cut;
	/**
	 * Select all menu item
	 */
	private JMenuItem _selectAll;
	/**
	 * Go to line menu item
	 */
	private JMenuItem _goToLine;
	/**
	 * Replace menu item
	 */
	private JMenuItem _replace;
	/**
	 * Undo manager of the application
	 */
	private UndoManager _undoManager = new UndoManager();

	/**
	 * Class constructor
	 */
	public EditMenu() {

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
	 * Sets the language labels to display in the selected language
	 */
	public void setLanguageLabels() {

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
	 * Builds the edit menu
	 */
	public void buildMenu() {

		removeAll();

		if (MenuConfiguration.getUndo())
			add(_undo);
		if (MenuConfiguration.getRedo())
			add(_redo);
		if ((MenuConfiguration.getUndo() || MenuConfiguration.getRedo())
				&& (MenuConfiguration.getCopy() || MenuConfiguration.getPaste()
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
				|| MenuConfiguration.getCopy() || MenuConfiguration.getPaste()
				|| MenuConfiguration.getCut() || MenuConfiguration
				.getSelectAll()) && MenuConfiguration.getGoToLine())
			addSeparator();
		if (MenuConfiguration.getGoToLine())
			add(_goToLine);
		if ((MenuConfiguration.getUndo() || MenuConfiguration.getRedo()
				|| MenuConfiguration.getCopy() || MenuConfiguration.getPaste()
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
	 * Sets the menu item Listeners
	 */
	public void setListeners() {

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
	 * Disables the paste menu item
	 */
	public void disablePaste() {

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

		_paste.setEnabled(false);

		// Updates the log
		Log.getLog().info(labels.getString("s73"));
	}

	/**
	 * Enables the paste menu item
	 */
	public void enablePaste() {

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

		_paste.setEnabled(true);

		// Updates the log
		Log.getLog().info(labels.getString("s74"));
	}

	/**
	 * Returns the search menu item
	 * 
	 * @return the search menu item
	 */
	public JMenuItem getSearch() {
		return _search;
	}

	/**
	 * Returns the go to line menu item
	 * 
	 * @return the go to line menu item
	 */
	public JMenuItem getGoToLine() {
		return _goToLine;
	}

	/**
	 * Sets a new value to the go to line menu item
	 * 
	 * @param goToLine new value to set
	 */
	public void setGoToLine(JMenuItem goToLine) {
		_goToLine = goToLine;
	}

	/**
	 * Returns the undo menu item
	 * 
	 * @return the undo menu item
	 */
	public JMenuItem getUndo() {
		return _undo;
	}

	/**
	 * Sets a new value to the undo menu item
	 * 
	 * @param undo new value to set
	 */
	public void setUndo(JMenuItem undo) {
		_undo = undo;
	}

	/**
	 * Returns the copy menu item
	 * 
	 * @return the copy menu item
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Sets a new value to the copy menu item
	 * 
	 * @param copy new value to set
	 */
	public void setCopy(JMenuItem copy) {
		_copy = copy;
	}

	/**
	 * Returns the cut menu item
	 * 
	 * @return the cut menu item
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Sets a new value to the cut menu item
	 * 
	 * @param cut new value to set
	 */ 
	public void setCut(JMenuItem cut) {
		_cut = cut;
	}

	/**
	 * Returns the paste menu item
	 * 
	 * @return the paste menu item
	 */
	public JMenuItem getPaste() {
		return _paste;
	}

	/**
	 * Sets a new value to the paste menu item
	 * 
	 * @param paste new value to set
	 */
	public void setPaste(JMenuItem paste) {
		_paste = paste;
	}

	/**
	 * Returns the replace menu item
	 * 
	 * @return the replace menu item
	 */
	public JMenuItem getReplace() {
		return _replace;
	}

	/**
	 * Sets a new value to the replace menu item
	 * 
	 * @param replace new value to set
	 */
	public void setReplace(JMenuItem replace) {
		_replace = replace;
	}

	/**
	 * Returns the redo menu item
	 * 
	 * @return the redo menu item
	 */
	public JMenuItem getRedo() {
		return _redo;
	}

	/**
	 * Sets a new value to the redo menu item
	 * 
	 * @param redo new value to set
	 */
	public void setRedo(JMenuItem redo) {
		_redo = redo;
	}

	/**
	 * Returns the select all menu item
	 * 
	 * @return the select all menu item
	 */
	public JMenuItem getSelectAll() {
		return _selectAll;
	}

	/**
	 * Sets a new value to the select all menu item
	 * 
	 * @param selectAll new value to set
	 */
	public void setSelectAll(JMenuItem selectAll) {
		_selectAll = selectAll;
	}

	/**
	 * Sets a new value to the search menu item
	 * 
	 * @param search new value to set
	 */
	public void setSearch(JMenuItem search) {
		_search = search;
	}

	/**
	 * Returns the undo-redo manager
	 * 
	 * @return the undo-redo manager
	 * @see UndoManager
	 */
	public UndoManager getUndoManager() {
		return _undoManager;
	}

	/**
	 * Sets a new value to the undo-redo manager
	 * 
	 * @param undoManager new value to set
	 * @see UndoManager
	 */
	public void setUndoManager(UndoManager undoManager) {
		_undoManager = undoManager;
	}

	/**
	 * Enables the edit menu
	 */
	public void enableMenu() {

		_undo.setEnabled(false);
		_redo.setEnabled(false);
		_copy.setEnabled(true);
		_paste.setEnabled(true);
		_cut.setEnabled(true);
		_selectAll.setEnabled(true);
		_goToLine.setEnabled(true);
		_search.setEnabled(true);
		_replace.setEnabled(true);
	}

	/**
	 * Disables the edit menu
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