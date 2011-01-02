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
package gui.menuBar.editMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.editMenu.listeners.CopyMenuItemListener;
import gui.menuBar.editMenu.listeners.CutMenuItemListener;
import gui.menuBar.editMenu.listeners.GoToLineMenuItemListener;
import gui.menuBar.editMenu.listeners.PasteMenuItemListener;
import gui.menuBar.editMenu.listeners.RedoMenuItemListener;
import gui.menuBar.editMenu.listeners.ReplaceMenuItemListener;
import gui.menuBar.editMenu.listeners.SearchMenuItemListener;
import gui.menuBar.editMenu.listeners.SelectAllMenuItemListener;
import gui.menuBar.editMenu.listeners.UndoMenuItemListener;

/**
 * ACIDE - A Configurable IDE edit menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class EditMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE edit menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE edit menu undo menu item name.
	 */
	public static final String UNDO_NAME = "Undo";
	/**
	 * ACIDE - A Configurable IDE edit menu redo menu item name.
	 */
	public static final String REDO_NAME = "Redo";
	/**
	 * ACIDE - A Configurable IDE edit menu copy menu item name.
	 */
	public static final String COPY_NAME = "Copy";
	/**
	 * ACIDE - A Configurable IDE edit menu paste menu item name.
	 */
	public static final String PASTE_NAME = "Paste";
	/**
	 * ACIDE - A Configurable IDE edit menu cut menu item name.
	 */
	public static final String CUT_NAME = "Cut";
	/**
	 * ACIDE - A Configurable IDE edit menu select all files menu item name.
	 */
	public static final String SELECT_ALL_FILES_NAME = "Select All Files";
	/**
	 * ACIDE - A Configurable IDE edit menu go to line menu item name.
	 */
	public static final String GO_TO_LINE_NAME = "Go To Line";
	/**
	 * ACIDE - A Configurable IDE edit menu search menu item name.
	 */
	public static final String SEARCH_NAME = "Search";
	/**
	 * ACIDE - A Configurable IDE edit menu replace menu item name.
	 */
	public static final String REPLACE_NAME = "Replace";
	/**
	 * ACIDE - A Configurable IDE edit menu undo menu item image icon.
	 */
	private final static ImageIcon UNDO_IMAGE = new ImageIcon("./resources/icons/menu/edit/undo.png");
	/**
	 * ACIDE - A Configurable IDE edit menu redo menu item image icon.
	 */
	private final static ImageIcon REDO_IMAGE = new ImageIcon("./resources/icons/menu/edit/redo.png");
	/**
	 * ACIDE - A Configurable IDE edit menu copy menu item image icon.
	 */
	private final static ImageIcon COPY_IMAGE = new ImageIcon("./resources/icons/menu/edit/copy.png");
	/**
	 * ACIDE - A Configurable IDE edit menu paste menu item image icon.
	 */
	private final static ImageIcon PASTE_IMAGE = new ImageIcon("./resources/icons/menu/edit/paste.png");
	/**
	 * ACIDE - A Configurable IDE edit menu cut menu item image icon.
	 */
	private final static ImageIcon CUT_IMAGE = new ImageIcon("./resources/icons/menu/edit/cut.png");
	/**
	 * ACIDE - A Configurable IDE edit menu select all files menu item image icon.
	 */
	private final static ImageIcon SELECT_ALL_FILES_IMAGE = new ImageIcon("./resources/icons/menu/edit/selectAll.png");
	/**
	 * ACIDE - A Configurable IDE edit menu go to line menu item image icon.
	 */
	private final static ImageIcon GO_TO_LINE_IMAGE = new ImageIcon("./resources/icons/menu/edit/goToLine.png");
	/**
	 * ACIDE - A Configurable IDE edit menu search menu item image icon.
	 */
	private final static ImageIcon SEARCH_IMAGE = new ImageIcon("./resources/icons/menu/edit/search.png");
	/**
	 * ACIDE - A Configurable IDE edit menu replace menu item image icon.
	 */
	private final static ImageIcon REPLACE_IMAGE = new ImageIcon("./resources/icons/menu/edit/replace.png");
	/**
	 * ACIDE - A Configurable IDE edit menu undo menu item.
	 */
	private JMenuItem _undo;
	/**
	 * ACIDE - A Configurable IDE edit menu redo menu item.
	 */
	private JMenuItem _redo;
	/**
	 * ACIDE - A Configurable IDE edit menu search menu item.
	 */
	private JMenuItem _search;
	/**
	 * ACIDE - A Configurable IDE edit menu paste menu item.
	 */
	private JMenuItem _paste;
	/**
	 * ACIDE - A Configurable IDE edit menu copy menu item.
	 */
	private JMenuItem _copy;
	/**
	 * ACIDE - A Configurable IDE edit menu cut menu item.
	 */
	private JMenuItem _cut;
	/**
	 * ACIDE - A Configurable IDE edit menu select all menu item.
	 */
	private JMenuItem _selectAllFiles;
	/**
	 * ACIDE - A Configurable IDE edit menu go to line menu item.
	 */
	private JMenuItem _goToLine;
	/**
	 * ACIDE - A Configurable IDE edit menu replace menu item.
	 */
	private JMenuItem _replace;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE edit menu.
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
	 * Sets the ACIDE - A Configurable IDE edit menu language labels.
	 */
	public void setLanguageLabels() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
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
	 * Builds the ACIDE - A Configurable IDE edit menu.
	 */
	public void buildMenu() {

		removeAll();

		// UNDO MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME))
			add(_undo);
		
		// REDO MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REDO_NAME))
			add(_redo);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(REDO_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(COPY_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(CUT_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME)))
			addSeparator();
		
		// COPY MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(COPY_NAME))
			add(_copy);
		
		// PASTE MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME))
			add(_paste);
		
		// CUT MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(CUT_NAME))
			add(_cut);
		
		// SELECT ALL FILES MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME))
			add(_selectAllFiles);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(REDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(COPY_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(CUT_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME)) 
				&& AcideMenuConfiguration.getInstance().getIsDisplayed(GO_TO_LINE_NAME))
			addSeparator();
		
		// GO TO LINE MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(GO_TO_LINE_NAME))
			add(_goToLine);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(REDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(COPY_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(CUT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GO_TO_LINE_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(SEARCH_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(REPLACE_NAME)))
			addSeparator();
		
		// SEARCH MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SEARCH_NAME))
			add(_search);
		
		// REPLACE MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REPLACE_NAME))
			add(_replace);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE edit menu menu item listeners.
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
	 * Disables the ACIDE - A Configurable IDE edit menu paste menu item.
	 */
	public void disablePaste() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
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
	 * Enables the ACIDE - A Configurable IDE edit menu paste menu item.
	 */
	public void enablePaste() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
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
	 * Returns the ACIDE - A Configurable IDE edit menu search menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu search menu item.
	 */
	public JMenuItem getSearch() {
		return _search;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu go to line menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu go to line menu item.
	 */
	public JMenuItem getGoToLine() {
		return _goToLine;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu go to line menu item.
	 * 
	 * @param goToLine new value to set.
	 */
	public void setGoToLine(JMenuItem goToLine) {
		_goToLine = goToLine;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu undo menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu undo menu item.
	 */
	public JMenuItem getUndo() {
		return _undo;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu undo menu item.
	 * 
	 * @param undo new value to set.
	 */
	public void setUndo(JMenuItem undo) {
		_undo = undo;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu copy menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu copy menu item.
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu copy menu item.
	 * 
	 * @param copy new value to set.
	 */
	public void setCopy(JMenuItem copy) {
		_copy = copy;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu cut menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu cut menu item.
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu cut menu item.
	 * 
	 * @param cut new value to set.
	 */ 
	public void setCut(JMenuItem cut) {
		_cut = cut;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu paste menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu paste menu item.
	 */
	public JMenuItem getPaste() {
		return _paste;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu paste menu item.
	 * 
	 * @param paste new value to set.
	 */
	public void setPaste(JMenuItem paste) {
		_paste = paste;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu replace menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu replace menu item.
	 */
	public JMenuItem getReplace() {
		return _replace;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu replace menu item.
	 * 
	 * @param replace new value to set.
	 */
	public void setReplace(JMenuItem replace) {
		_replace = replace;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu redo menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu redo menu item.
	 */
	public JMenuItem getRedo() {
		return _redo;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu redo menu item.
	 * 
	 * @param redo new value to set.
	 */
	public void setRedo(JMenuItem redo) {
		_redo = redo;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu select all menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu select all menu item.
	 */
	public JMenuItem getSelectAll() {
		return _selectAllFiles;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu select all menu item.
	 * 
	 * @param selectAll new value to set.
	 */
	public void setSelectAll(JMenuItem selectAll) {
		_selectAllFiles = selectAll;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE edit menu search menu item.
	 * 
	 * @param search new value to set.
	 */
	public void setSearch(JMenuItem search) {
		_search = search;
	}

	/**
	 * Enables the ACIDE - A Configurable IDE edit menu.
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
	 * Disables the ACIDE - A Configurable IDE edit menu.
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