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

		// Creates the undo menu item
		_undoMenuItem = new JMenuItem(UNDO_IMAGE);

		// Creates the redo menu item
		_redoMenuItem = new JMenuItem(REDO_IMAGE);

		// Creates the copy menu item
		_copyMenuItem = new JMenuItem(COPY_IMAGE);

		// Creates the paste menu item
		_pasteMenuItem = new JMenuItem(PASTE_IMAGE);

		// Creates the cut menu item
		_cutMenuItem = new JMenuItem(CUT_IMAGE);

		// Creates the search menu item
		_searchMenuItem = new JMenuItem(SEARCH_IMAGE);

		// Creates the replace menu item
		_replaceMenuItem = new JMenuItem(REPLACE_IMAGE);

		// Creates the select all menu item
		_selectAllMenuItem = new JMenuItem(SELECT_ALL_FILES_IMAGE);

		// Creates the go to line menu item
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

		// Sets the undo menu item text
		_undoMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s21"));
		
		// Sets the undo menu item accelerator
		_undoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z,
				ActionEvent.CTRL_MASK));

		// Sets the redo menu item text
		_redoMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s22"));
		
		// Sets the redo menu item accelerator
		_redoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Y,
				ActionEvent.CTRL_MASK));

		// Sets the copy menu item text
		_copyMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s23"));
		
		// Sets the copy menu item accelerator
		_copyMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.CTRL_MASK));

		// Sets the cut menu item text
		_cutMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s24"));
		
		// Sets the cut menu item accelerator
		_cutMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.CTRL_MASK));

		// Sets the paste menu item text
		_pasteMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s25"));
		
		// Sets the paste menu item accelerator
		_pasteMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V,
				ActionEvent.CTRL_MASK));

		// Sets the search menu item text
		_searchMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s26"));
		
		// Sets the search menu item accelerator
		if (AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN")))
			_searchMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_F, ActionEvent.CTRL_MASK));
		else
			_searchMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_B, ActionEvent.CTRL_MASK));

		// Sets the replace menu item text
		_replaceMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s27"));
		
		// Sets the replace menu item accelerator
		if (AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN")))
			_replaceMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_R, ActionEvent.CTRL_MASK));
		else
			_replaceMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_L, ActionEvent.CTRL_MASK));

		// Sets the select all menu item text
		_selectAllMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s190"));
		
		// Sets the select all menu item accelerator
		_selectAllMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.CTRL_MASK));

		// Sets the go to line menu item text
		_goToLineMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s222"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE edit menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// Adds the undo menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(UNDO_NAME))
			add(_undoMenuItem);

		// Adds the redo menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REDO_NAME))
			add(_redoMenuItem);

		// Adds a separator to the menu
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

		// Adds the copy menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(COPY_NAME))
			add(_copyMenuItem);

		// Adds the paste menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(PASTE_NAME))
			add(_pasteMenuItem);

		// Adds the cut menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(CUT_NAME))
			add(_cutMenuItem);

		// Adds the select all menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SELECT_ALL_FILES_NAME))
			add(_selectAllMenuItem);

		// Adds a separator to the menu
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

		// Adds the go to line menu item to the menu
		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(GO_TO_LINE_NAME))
			add(_goToLineMenuItem);

		// Adds a separator to the menu
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

		// Adds the search menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SEARCH_NAME))
			add(_searchMenuItem);

		// Adds the replace menu item to the menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REPLACE_NAME))
			add(_replaceMenuItem);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE edit menu menu item listeners.
	 */
	public void setListeners() {

		// Sets the undo menu item action listener
		_undoMenuItem.addActionListener(new AcideUndoMenuItemListener());

		// Sets the redo menu item action listener
		_redoMenuItem.addActionListener(new AcideRedoMenuItemListener());

		// Sets the search menu item action listener
		_searchMenuItem.addActionListener(new AcideSearchMenuItemListener());

		// Sets the replace menu item action listener
		_replaceMenuItem.addActionListener(new AcideReplaceMenuItemListener());

		// Sets the cut menu item action listener
		_cutMenuItem.addActionListener(new AcideCutMenuItemListener());

		// Sets the paste menu item action listener
		_pasteMenuItem.addActionListener(new AcidePasteMenuItemListener());

		// Sets the copy menu item action listener
		_copyMenuItem.addActionListener(new AcideCopyMenuItemListener());

		// Sets the select all menu item action listener
		_selectAllMenuItem
				.addActionListener(new AcideSelectAllMenuItemListener());

		// Sets the go to line menu item action listener
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

		// Disables the undo menu item
		_undoMenuItem.setEnabled(false);
		
		// Disables the redo menu item
		_redoMenuItem.setEnabled(false);
		
		// Enables the copy menu item
		_copyMenuItem.setEnabled(true);
		
		// Enables the paste menu item
		_pasteMenuItem.setEnabled(true);
		
		// Enables the cut menu item
		_cutMenuItem.setEnabled(true);
		
		// Enables the select all menu item
		_selectAllMenuItem.setEnabled(true);
		
		// Enables the go to line menu item
		_goToLineMenuItem.setEnabled(true);
		
		// Enables the search menu item
		_searchMenuItem.setEnabled(true);
		
		// Enables the replace menu item
		_replaceMenuItem.setEnabled(true);
	}

	/**
	 * Disables the ACIDE - A Configurable IDE edit menu.
	 */
	public void disableMenu() {

		// Disables the undo menu item
		_undoMenuItem.setEnabled(false);
		
		// Disables the redo menu item
		_redoMenuItem.setEnabled(false);
		
		// Disables the copy menu item
		_copyMenuItem.setEnabled(false);
		
		// Disables the paste menu item
		_pasteMenuItem.setEnabled(false);
		
		// Disables the cut menu item
		_cutMenuItem.setEnabled(false);
		
		// Disables the select all menu item
		_selectAllMenuItem.setEnabled(false);
		
		// Disables the go the line menu item
		_goToLineMenuItem.setEnabled(false);
		
		// Disables the search menu item
		_searchMenuItem.setEnabled(false);
		
		// Disables the replace menu item
		_replaceMenuItem.setEnabled(false);
	}
}