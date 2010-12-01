package gui.menuBar.editMenu;

import es.configuration.menu.MenuConfiguration;
import gui.menuBar.editMenu.listeners.CopyMenuItemListener;
import gui.menuBar.editMenu.listeners.CutMenuItemListener;
import gui.menuBar.editMenu.listeners.GoToLineMenuItemListener;
import gui.menuBar.editMenu.listeners.PasteMenuItemListener;
import gui.menuBar.editMenu.listeners.RedoMenuItemListener;
import gui.menuBar.editMenu.listeners.ReplaceMenuItemListener;
import gui.menuBar.editMenu.listeners.SearchMenuItemListener;
import gui.menuBar.editMenu.listeners.SelectAllMenuItemListener;
import gui.menuBar.editMenu.listeners.UndoMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.AcideLanguage;

import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Edit menu of ACIDE - A Configurable IDE.
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
	 * Edit menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Undo menu item name.
	 */
	public static final String UNDO_NAME = "Undo";
	/**
	 * Redo menu item name.
	 */
	public static final String REDO_NAME = "Redo";
	/**
	 * Copy menu item name.
	 */
	public static final String COPY_NAME = "Copy";
	/**
	 * Paste menu item name.
	 */
	public static final String PASTE_NAME = "Paste";
	/**
	 * Cut menu item name.
	 */
	public static final String CUT_NAME = "Cut";
	/**
	 * Select all files menu item name.
	 */
	public static final String SELECT_ALL_FILES_NAME = "Select All Files";
	/**
	 * Go to line menu item name.
	 */
	public static final String GO_TO_LINE_NAME = "Go To Line";
	/**
	 * Search menu item name.
	 */
	public static final String SEARCH_NAME = "Search";
	/**
	 * Replace menu item name.
	 */
	public static final String REPLACE_NAME = "Replace";
	/**
	 * Undo menu item image icon.
	 */
	private final static ImageIcon UNDO_IMAGE = new ImageIcon("./resources/icons/menu/edit/undo.png");
	/**
	 * Redo menu item image icon.
	 */
	private final static ImageIcon REDO_IMAGE = new ImageIcon("./resources/icons/menu/edit/redo.png");
	/**
	 * Copy menu item image icon.
	 */
	private final static ImageIcon COPY_IMAGE = new ImageIcon("./resources/icons/menu/edit/copy.png");
	/**
	 * Paste menu item image icon.
	 */
	private final static ImageIcon PASTE_IMAGE = new ImageIcon("./resources/icons/menu/edit/paste.png");
	/**
	 * Cut menu item image icon.
	 */
	private final static ImageIcon CUT_IMAGE = new ImageIcon("./resources/icons/menu/edit/cut.png");
	/**
	 * Select all files menu item image icon.
	 */
	private final static ImageIcon SELECT_ALL_FILES_IMAGE = new ImageIcon("./resources/icons/menu/edit/selectAll.png");
	/**
	 * Go to line menu item image icon.
	 */
	private final static ImageIcon GO_TO_LINE_IMAGE = new ImageIcon("./resources/icons/menu/edit/goToLine.png");
	/**
	 * Search menu item image icon.
	 */
	private final static ImageIcon SEARCH_IMAGE = new ImageIcon("./resources/icons/menu/edit/search.png");
	/**
	 * Replace menu item image icon.
	 */
	private final static ImageIcon REPLACE_IMAGE = new ImageIcon("./resources/icons/menu/edit/replace.png");
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
	private JMenuItem _selectAllFiles;
	/**
	 * Go to line menu item.
	 */
	private JMenuItem _goToLine;
	/**
	 * Replace menu item.
	 */
	private JMenuItem _replace;

	/**
	 * Creates a new edit menu.
	 */
	public EditMenu() {

		// MENU ITEMS
		_undo = new JMenuItem(UNDO_IMAGE);
		_redo = new JMenuItem(REDO_IMAGE);
		_copy = new JMenuItem(COPY_IMAGE);
		_paste = new JMenuItem(PASTE_IMAGE);
		_cut = new JMenuItem(CUT_IMAGE);
		_search = new JMenuItem(SEARCH_IMAGE);
		_replace = new JMenuItem(REPLACE_IMAGE);
		_selectAllFiles = new JMenuItem(SELECT_ALL_FILES_IMAGE);
		_goToLine = new JMenuItem(GO_TO_LINE_IMAGE);

		setLanguageLabels();
	}

	/**
	 * Sets the language labels to display in the selected language.
	 */
	public void setLanguageLabels() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
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
		_selectAllFiles.setText(labels.getString("s190"));
		_selectAllFiles.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.CTRL_MASK));

		// GO TO LINE
		_goToLine.setText(labels.getString("s222"));
	}

	/**
	 * Builds the edit menu.
	 */
	public void buildMenu() {

		removeAll();

		// UNDO MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME))
			add(_undo);
		
		// REDO MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(REDO_NAME))
			add(_redo);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(REDO_NAME))
				&& (MenuConfiguration.getInstance().getIsDisplayed(COPY_NAME) 
						|| MenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(CUT_NAME) 
						|| MenuConfiguration.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME)))
			addSeparator();
		
		// COPY MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(COPY_NAME))
			add(_copy);
		
		// PASTE MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME))
			add(_paste);
		
		// CUT MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(CUT_NAME))
			add(_cut);
		
		// SELECT ALL FILES MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME))
			add(_selectAllFiles);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(REDO_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(COPY_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(CUT_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME)) 
				&& MenuConfiguration.getInstance().getIsDisplayed(GO_TO_LINE_NAME))
			addSeparator();
		
		// GO TO LINE MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(GO_TO_LINE_NAME))
			add(_goToLine);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(REDO_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(COPY_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(CUT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(GO_TO_LINE_NAME))
				&& (MenuConfiguration.getInstance().getIsDisplayed(SEARCH_NAME) 
						|| MenuConfiguration.getInstance().getIsDisplayed(REPLACE_NAME)))
			addSeparator();
		
		// SEARCH MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(SEARCH_NAME))
			add(_search);
		
		// REPLACE MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(REPLACE_NAME))
			add(_replace);
	}

	/**
	 * Sets the edit menu item listeners.
	 */
	public void setListeners() {

		// UNDO
		_undo.addActionListener(new UndoMenuItemListener());

		// REDO
		_redo.addActionListener(new RedoMenuItemListener());

		// SEARCH
		_search.addActionListener(new SearchMenuItemListener());

		// REPLACE
		_replace.addActionListener(new ReplaceMenuItemListener());

		// CUT
		_cut.addActionListener(new CutMenuItemListener());

		// PASTE
		_paste.addActionListener(new PasteMenuItemListener());

		// COPY
		_copy.addActionListener(new CopyMenuItemListener());

		// SELECT ALL
		_selectAllFiles.addActionListener(new SelectAllMenuItemListener());

		// GO TO LINE
		_goToLine.addActionListener(new GoToLineMenuItemListener());
	}

	/**
	 * Disables the paste menu item.
	 */
	public void disablePaste() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		_paste.setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s73"));
	}

	/**
	 * Enables the paste menu item.
	 */
	public void enablePaste() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		_paste.setEnabled(true);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s74"));
	}

	/**
	 * Returns the search menu item.
	 * 
	 * @return the search menu item.
	 */
	public JMenuItem getSearch() {
		return _search;
	}

	/**
	 * Returns the go to line menu item.
	 * 
	 * @return the go to line menu item.
	 */
	public JMenuItem getGoToLine() {
		return _goToLine;
	}

	/**
	 * Sets a new value to the go to line menu item.
	 * 
	 * @param goToLine new value to set.
	 */
	public void setGoToLine(JMenuItem goToLine) {
		_goToLine = goToLine;
	}

	/**
	 * Returns the undo menu item.
	 * 
	 * @return the undo menu item.
	 */
	public JMenuItem getUndo() {
		return _undo;
	}

	/**
	 * Sets a new value to the undo menu item.
	 * 
	 * @param undo new value to set.
	 */
	public void setUndo(JMenuItem undo) {
		_undo = undo;
	}

	/**
	 * Returns the copy menu item.
	 * 
	 * @return the copy menu item.
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Sets a new value to the copy menu item.
	 * 
	 * @param copy new value to set.
	 */
	public void setCopy(JMenuItem copy) {
		_copy = copy;
	}

	/**
	 * Returns the cut menu item.
	 * 
	 * @return the cut menu item.
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Sets a new value to the cut menu item.
	 * 
	 * @param cut new value to set.
	 */ 
	public void setCut(JMenuItem cut) {
		_cut = cut;
	}

	/**
	 * Returns the paste menu item.
	 * 
	 * @return the paste menu item.
	 */
	public JMenuItem getPaste() {
		return _paste;
	}

	/**
	 * Sets a new value to the paste menu item.
	 * 
	 * @param paste new value to set.
	 */
	public void setPaste(JMenuItem paste) {
		_paste = paste;
	}

	/**
	 * Returns the replace menu item.
	 * 
	 * @return the replace menu item.
	 */
	public JMenuItem getReplace() {
		return _replace;
	}

	/**
	 * Sets a new value to the replace menu item.
	 * 
	 * @param replace new value to set.
	 */
	public void setReplace(JMenuItem replace) {
		_replace = replace;
	}

	/**
	 * Returns the redo menu item.
	 * 
	 * @return the redo menu item.
	 */
	public JMenuItem getRedo() {
		return _redo;
	}

	/**
	 * Sets a new value to the redo menu item.
	 * 
	 * @param redo new value to set.
	 */
	public void setRedo(JMenuItem redo) {
		_redo = redo;
	}

	/**
	 * Returns the select all menu item.
	 * 
	 * @return the select all menu item.
	 */
	public JMenuItem getSelectAll() {
		return _selectAllFiles;
	}

	/**
	 * Sets a new value to the select all menu item.
	 * 
	 * @param selectAll new value to set.
	 */
	public void setSelectAll(JMenuItem selectAll) {
		_selectAllFiles = selectAll;
	}

	/**
	 * Sets a new value to the search menu item.
	 * 
	 * @param search new value to set.
	 */
	public void setSearch(JMenuItem search) {
		_search = search;
	}

	/**
	 * Enables the edit menu.
	 */
	public void enableMenu() {

		_undo.setEnabled(false);
		_redo.setEnabled(false);
		_copy.setEnabled(true);
		_paste.setEnabled(true);
		_cut.setEnabled(true);
		_selectAllFiles.setEnabled(true);
		_goToLine.setEnabled(true);
		_search.setEnabled(true);
		_replace.setEnabled(true);
	}

	/**
	 * Disables the edit menu.
	 */
	public void disableMenu() {

		_undo.setEnabled(false);
		_redo.setEnabled(false);
		_copy.setEnabled(false);
		_paste.setEnabled(false);
		_cut.setEnabled(false);
		_selectAllFiles.setEnabled(false);
		_goToLine.setEnabled(false);
		_search.setEnabled(false);
		_replace.setEnabled(false);
	}
}