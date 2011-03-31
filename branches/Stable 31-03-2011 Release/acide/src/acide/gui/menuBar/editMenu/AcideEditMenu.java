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
package acide.gui.menuBar.editMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.editMenu.listeners.AcideCopyMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideCutMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideGoToLineMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcidePasteMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideRedoMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideReplaceMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideSearchMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideSelectAllMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideUndoMenuItemListener;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE edit menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideEditMenu extends JMenu {

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
	private final static ImageIcon UNDO_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/undo.png");
	/**
	 * ACIDE - A Configurable IDE edit menu redo menu item image icon.
	 */
	private final static ImageIcon REDO_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/redo.png");
	/**
	 * ACIDE - A Configurable IDE edit menu copy menu item image icon.
	 */
	private final static ImageIcon COPY_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/copy.png");
	/**
	 * ACIDE - A Configurable IDE edit menu paste menu item image icon.
	 */
	private final static ImageIcon PASTE_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/paste.png");
	/**
	 * ACIDE - A Configurable IDE edit menu cut menu item image icon.
	 */
	private final static ImageIcon CUT_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/cut.png");
	/**
	 * ACIDE - A Configurable IDE edit menu select all files menu item image
	 * icon.
	 */
	private final static ImageIcon SELECT_ALL_FILES_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/selectAll.png");
	/**
	 * ACIDE - A Configurable IDE edit menu go to line menu item image icon.
	 */
	private final static ImageIcon GO_TO_LINE_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/goToLine.png");
	/**
	 * ACIDE - A Configurable IDE edit menu search menu item image icon.
	 */
	private final static ImageIcon SEARCH_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/search.png");
	/**
	 * ACIDE - A Configurable IDE edit menu replace menu item image icon.
	 */
	private final static ImageIcon REPLACE_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/replace.png");
	/**
	 * ACIDE - A Configurable IDE edit menu undo menu item.
	 */
	private JMenuItem _undoMenuItem;
	/**
	 * ACIDE - A Configurable IDE edit menu redo menu item.
	 */
	private JMenuItem _redoMenuItem;
	/**
	 * ACIDE - A Configurable IDE edit menu search menu item.
	 */
	private JMenuItem _searchMenuItem;
	/**
	 * ACIDE - A Configurable IDE edit menu paste menu item.
	 */
	private JMenuItem _pasteMenuItem;
	/**
	 * ACIDE - A Configurable IDE edit menu copy menu item.
	 */
	private JMenuItem _copyMenuItem;
	/**
	 * ACIDE - A Configurable IDE edit menu cut menu item.
	 */
	private JMenuItem _cutMenuItem;
	/**
	 * ACIDE - A Configurable IDE edit menu select all menu item.
	 */
	private JMenuItem _selectAllMenuItem;
	/**
	 * ACIDE - A Configurable IDE edit menu go to line menu item.
	 */
	private JMenuItem _goToLineMenuItem;
	/**
	 * ACIDE - A Configurable IDE edit menu replace menu item.
	 */
	private JMenuItem _replaceMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE edit menu.
	 */
	public AcideEditMenu() {

		// UNDO MENU ITEM
		_undoMenuItem = new JMenuItem(UNDO_IMAGE);

		// REDO MENU ITEM
		_redoMenuItem = new JMenuItem(REDO_IMAGE);

		// COPY MENU ITEM
		_copyMenuItem = new JMenuItem(COPY_IMAGE);

		// PASTE MENU ITEM
		_pasteMenuItem = new JMenuItem(PASTE_IMAGE);

		// CUT MENU ITEM
		_cutMenuItem = new JMenuItem(CUT_IMAGE);

		// SEARCH MENU ITEM
		_searchMenuItem = new JMenuItem(SEARCH_IMAGE);

		// REPLACE MENU ITEM
		_replaceMenuItem = new JMenuItem(REPLACE_IMAGE);

		// SELECT ALL MENU ITEM
		_selectAllMenuItem = new JMenuItem(SELECT_ALL_FILES_IMAGE);

		// GO TO LINE MENU ITEM
		_goToLineMenuItem = new JMenuItem(GO_TO_LINE_IMAGE);

		// Sets the text of the edit menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE edit menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Disables the menu
		disableMenu();

		// UNDO MENU ITEM
		_undoMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s21"));
		_undoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z,
				ActionEvent.CTRL_MASK));

		// REDO MENU ITEM
		_redoMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s22"));
		_redoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Y,
				ActionEvent.CTRL_MASK));

		// COPY MENU ITEM
		_copyMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s23"));
		_copyMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.CTRL_MASK));

		// CUT MENU ITEM
		_cutMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s24"));
		_cutMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.CTRL_MASK));

		// PASTE MENU ITEM
		_pasteMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s25"));
		_pasteMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V,
				ActionEvent.CTRL_MASK));

		// SEARCH MENU ITEM
		_searchMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s26"));
		if (AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN")))
			_searchMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_F, ActionEvent.CTRL_MASK));
		else
			_searchMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_B, ActionEvent.CTRL_MASK));

		// REPLACE MENU ITEM
		_replaceMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s27"));
		if (AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN")))
			_replaceMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_R, ActionEvent.CTRL_MASK));
		else
			_replaceMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_L, ActionEvent.CTRL_MASK));

		// SELECT ALL MENU ITEM
		_selectAllMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s190"));
		_selectAllMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.CTRL_MASK));

		// GO TO LINE MENU ITEM
		_goToLineMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s222"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE edit menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// UNDO MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME))
			add(_undoMenuItem);

		// REDO MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REDO_NAME))
			add(_redoMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(REDO_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(
						COPY_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								PASTE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								CUT_NAME) || AcideMenuConfiguration
						.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME)))
			addSeparator();

		// COPY MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(COPY_NAME))
			add(_copyMenuItem);

		// PASTE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME))
			add(_pasteMenuItem);

		// CUT MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(CUT_NAME))
			add(_cutMenuItem);

		// SELECT ALL MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SELECT_ALL_FILES_NAME))
			add(_selectAllMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						REDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						COPY_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						PASTE_NAME)
				|| AcideMenuConfiguration.getInstance()
						.getIsDisplayed(CUT_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(SELECT_ALL_FILES_NAME))
				&& AcideMenuConfiguration.getInstance().getIsDisplayed(
						GO_TO_LINE_NAME))
			addSeparator();

		// GO TO LINE MENU ITEM
		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(GO_TO_LINE_NAME))
			add(_goToLineMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						REDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						COPY_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						PASTE_NAME)
				|| AcideMenuConfiguration.getInstance()
						.getIsDisplayed(CUT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SELECT_ALL_FILES_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(GO_TO_LINE_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(
						SEARCH_NAME) || AcideMenuConfiguration.getInstance()
						.getIsDisplayed(REPLACE_NAME)))
			addSeparator();

		// SEARCH MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SEARCH_NAME))
			add(_searchMenuItem);

		// REPLACE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REPLACE_NAME))
			add(_replaceMenuItem);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE edit menu menu item listeners.
	 */
	public void setListeners() {

		// UNDO MENU ITEM
		_undoMenuItem.addActionListener(new AcideUndoMenuItemListener());

		// REDO MENU ITEM
		_redoMenuItem.addActionListener(new AcideRedoMenuItemListener());

		// SEARCH MENU ITEM
		_searchMenuItem.addActionListener(new AcideSearchMenuItemListener());

		// REPLACE MENU ITEM
		_replaceMenuItem.addActionListener(new AcideReplaceMenuItemListener());

		// CUT MENU ITEM
		_cutMenuItem.addActionListener(new AcideCutMenuItemListener());

		// PASTE MENU ITEM
		_pasteMenuItem.addActionListener(new AcidePasteMenuItemListener());

		// COPY MENU ITEM
		_copyMenuItem.addActionListener(new AcideCopyMenuItemListener());

		// SELECT ALL MENU ITEM
		_selectAllMenuItem
				.addActionListener(new AcideSelectAllMenuItemListener());

		// GO TO LINE MENU ITEM
		_goToLineMenuItem
				.addActionListener(new AcideGoToLineMenuItemListener());
	}

	/**
	 * Disables the ACIDE - A Configurable IDE edit menu paste menu item.
	 */
	public void disablePaste() {

		// Disables the paste menu item
		_pasteMenuItem.setEnabled(false);

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s73"));
	}

	/**
	 * Enables the ACIDE - A Configurable IDE edit menu paste menu item.
	 */
	public void enablePaste() {

		// Enables the paste menu item
		_pasteMenuItem.setEnabled(true);

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s74"));
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu search menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu search menu item.
	 */
	public JMenuItem getSearch() {
		return _searchMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu go to line menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu go to line menu item.
	 */
	public JMenuItem getGoToLineMenuItem() {
		return _goToLineMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu undo menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu undo menu item.
	 */
	public JMenuItem getUndoMenuItem() {
		return _undoMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu copy menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu copy menu item.
	 */
	public JMenuItem getCopyMenuItem() {
		return _copyMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu cut menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu cut menu item.
	 */
	public JMenuItem getCutMenuItem() {
		return _cutMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu paste menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu paste menu item.
	 */
	public JMenuItem getPasteMenuItem() {
		return _pasteMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu replace menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu replace menu item.
	 */
	public JMenuItem getReplaceMenuItem() {
		return _replaceMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu redo menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu redo menu item.
	 */
	public JMenuItem getRedoMenuItem() {
		return _redoMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu select all menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu select all menu item.
	 */
	public JMenuItem getSelectAllMenuItem() {
		return _selectAllMenuItem;
	}

	/**
	 * Enables the ACIDE - A Configurable IDE edit menu.
	 */
	public void enableMenu() {

		_undoMenuItem.setEnabled(false);
		_redoMenuItem.setEnabled(false);
		_copyMenuItem.setEnabled(true);
		_pasteMenuItem.setEnabled(true);
		_cutMenuItem.setEnabled(true);
		_selectAllMenuItem.setEnabled(true);
		_goToLineMenuItem.setEnabled(true);
		_searchMenuItem.setEnabled(true);
		_replaceMenuItem.setEnabled(true);
	}

	/**
	 * Disables the ACIDE - A Configurable IDE edit menu.
	 */
	public void disableMenu() {

		_undoMenuItem.setEnabled(false);
		_redoMenuItem.setEnabled(false);
		_copyMenuItem.setEnabled(false);
		_pasteMenuItem.setEnabled(false);
		_cutMenuItem.setEnabled(false);
		_selectAllMenuItem.setEnabled(false);
		_goToLineMenuItem.setEnabled(false);
		_searchMenuItem.setEnabled(false);
		_replaceMenuItem.setEnabled(false);
	}
}