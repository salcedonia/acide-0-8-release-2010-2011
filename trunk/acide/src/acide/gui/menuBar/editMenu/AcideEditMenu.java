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

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.listeners.AcideCopyMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideCutMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideGoToLineMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcidePasteMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideRedoAction;
import acide.gui.menuBar.editMenu.listeners.AcideReplaceMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideSearchMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideSelectAllMenuItemListener;
import acide.gui.menuBar.editMenu.listeners.AcideUndoAction;
import acide.gui.menuBar.editMenu.utils.AcideUndoManager;
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
	 * ACIDE - A Configurable IDE edit menu select all menu item name.
	 */
	public static final String SELECT_ALL_NAME = "Select All";
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
	 * ACIDE - A Configurable IDE edit menu select all menu item image icon.
	 */
	private final static ImageIcon SELECT_ALL_IMAGE = new ImageIcon(
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
	 * ACIDE - A Configurable IDE edit menu redo copy separator.
	 */
	private JSeparator _redoCopySeparator;
	/**
	 * ACIDE - A Configurable IDE edit menu select all go to line separator.
	 */
	private JSeparator _selectAllGoToLineSeparator;
	/**
	 * ACIDE - A Configurable IDE edit menu go to line search separator.
	 */
	private JSeparator _goToLineSearchSeparator;
	/**
	 * ACIDE - A Configurable IDE edit menu undo action.
	 */
	private AcideUndoAction _undoAction;
	/**
	 * ACIDE - A Configurable IDE edit menu redo action.
	 */
	private AcideRedoAction _redoAction;

	/**
	 * Creates a new ACIDE - A Configurable IDE edit menu.
	 */
	public AcideEditMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the edit menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE edit menu.
	 */
	private void addComponents() {

		// Adds the undo menu item to the menu
		add(_undoMenuItem);

		// Adds the redo menu item to the menu
		add(_redoMenuItem);

		// Adds the redo copy menu item to the menu
		add(_redoCopySeparator);

		// Adds the copy menu item to the menu
		add(_copyMenuItem);

		// Adds the paste menu item to the menu
		add(_pasteMenuItem);

		// Adds the cut menu item to the menu
		add(_cutMenuItem);

		// Adds the select all menu item to the menu
		add(_selectAllMenuItem);

		// Adds the select all go to line menu item to the menu
		add(_selectAllGoToLineSeparator);

		// Adds the go to line menu item to the menu
		add(_goToLineMenuItem);

		// Adds the go to line search menu item to the menu
		add(_goToLineSearchSeparator);

		// Adds the search menu item to the menu
		add(_searchMenuItem);

		// Adds the replace menu item to the menu
		add(_replaceMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE edit menu components.
	 */
	private void buildComponents() {

		// Creates the undo action
		_undoAction = new AcideUndoAction();

		// Creates the redo action
		_redoAction = new AcideRedoAction();

		// Creates the undo menu item
		_undoMenuItem = new JMenuItem(UNDO_IMAGE);

		// Sets the undo menu item name
		_undoMenuItem.setName(UNDO_NAME);

		// Creates the redo menu item
		_redoMenuItem = new JMenuItem(REDO_IMAGE);

		// Sets the redo menu item name
		_redoMenuItem.setName(REDO_NAME);

		// Creates the redo copy separator
		_redoCopySeparator = new JSeparator();

		// Creates the copy menu item
		_copyMenuItem = new JMenuItem(COPY_IMAGE);

		// Sets the copy menu item name
		_copyMenuItem.setName(COPY_NAME);

		// Creates the paste menu item
		_pasteMenuItem = new JMenuItem(PASTE_IMAGE);

		// Sets the paste menu item name
		_pasteMenuItem.setName(PASTE_NAME);

		// Creates the cut menu item
		_cutMenuItem = new JMenuItem(CUT_IMAGE);

		// Sets the cut menu item name
		_cutMenuItem.setName(CUT_NAME);

		// Creates the select all menu item
		_selectAllMenuItem = new JMenuItem(SELECT_ALL_IMAGE);

		// Sets the select all menu item
		_selectAllMenuItem.setName(SELECT_ALL_NAME);

		// Creates the select all go to line copy separator
		_selectAllGoToLineSeparator = new JSeparator();

		// Creates the go to line menu item
		_goToLineMenuItem = new JMenuItem(GO_TO_LINE_IMAGE);

		// Sets the go to line menu item name
		_goToLineMenuItem.setName(GO_TO_LINE_NAME);

		// Creates the go to line search separator
		_goToLineSearchSeparator = new JSeparator();

		// Creates the search menu item
		_searchMenuItem = new JMenuItem(SEARCH_IMAGE);

		// Sets the search menu item name
		_searchMenuItem.setName(SEARCH_NAME);

		// Creates the replace menu item
		_replaceMenuItem = new JMenuItem(REPLACE_IMAGE);

		// Sets the replace menu item name
		_replaceMenuItem.setName(REPLACE_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE edit menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

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
	 * Updates the ACIDE - A Configurable IDE edit menu components visibility
	 * with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the undo menu item to visible or not visible
		_undoMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(UNDO_NAME));

		// Sets the redo menu item to visible or not visible
		_redoMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(REDO_NAME));

		// Sets the redo copy separator to visible or not visible
		_redoCopySeparator.setVisible((AcideMenuConfiguration.getInstance()
				.getIsDisplayed(UNDO_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(REDO_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(
						COPY_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								PASTE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								CUT_NAME) || AcideMenuConfiguration
						.getInstance().getIsDisplayed(SELECT_ALL_NAME)));

		// Sets the copy menu item to visible or not visible
		_copyMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(COPY_NAME));

		// Sets the paste menu item to visible or not visible
		_pasteMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(PASTE_NAME));

		// Sets the cut menu item to visible or not visible
		_cutMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(CUT_NAME));

		// Sets the select all menu item to visible or not visible
		_selectAllMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SELECT_ALL_NAME));

		// Set the select all go to line separator to visible or not visible
		_selectAllGoToLineSeparator.setVisible((AcideMenuConfiguration
				.getInstance().getIsDisplayed(UNDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						REDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						COPY_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						PASTE_NAME)
				|| AcideMenuConfiguration.getInstance()
						.getIsDisplayed(CUT_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(SELECT_ALL_NAME))
				&& AcideMenuConfiguration.getInstance().getIsDisplayed(
						GO_TO_LINE_NAME));

		// Sets the go to line menu item to visible or not visible
		_goToLineMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(GO_TO_LINE_NAME));

		// Sets go to line search separator to visible or not visible
		_goToLineSearchSeparator.setVisible((AcideMenuConfiguration
				.getInstance().getIsDisplayed(UNDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						REDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						COPY_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						PASTE_NAME)
				|| AcideMenuConfiguration.getInstance()
						.getIsDisplayed(CUT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SELECT_ALL_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(GO_TO_LINE_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(
						SEARCH_NAME) || AcideMenuConfiguration.getInstance()
						.getIsDisplayed(REPLACE_NAME)));

		// Sets the search menu item to visible or not visible
		_searchMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SEARCH_NAME));

		// Sets the replace menu item to visible or not visible
		_replaceMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(REPLACE_NAME));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE edit menu menu item listeners.
	 */
	public void setListeners() {

		// Sets the undo menu item action listener
		_undoMenuItem.addActionListener(_undoAction);

		// Sets the redo menu item action listener
		_redoMenuItem.addActionListener(_redoAction);

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
	 * Configures the ACIDE - A Configurable IDE edit menu menu item options.
	 */
	public void configure() {

		// Disables the copy menu item
		_copyMenuItem.setEnabled(false);

		// Disables the paste menu item
		_pasteMenuItem.setEnabled(false);

		// Disables the cut menu item
		_cutMenuItem.setEnabled(false);

		// Enables or disables the undo menu item
		_undoMenuItem.setEnabled(AcideUndoManager.getInstance().canUndo());

		// Enables or disables the the redo menu item
		_redoAction.setEnabled(AcideUndoManager.getInstance().canRedo());

		// If the system clipboard is not empty
		if (Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null) != null) {

			// If the console panel does not have the focus in the window
			if (!AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.hasFocus())

				// Enables the paste menu item
				_pasteMenuItem.setEnabled(true);
			else
			// If the caret is after the prompt position
			if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.getSelectionStart() >= AcideMainWindow.getInstance()
					.getConsolePanel().getPromptCaretPosition())

				// Enables the paste menu item
				_pasteMenuItem.setEnabled(true);
		}

		// If there are opened editors
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// If the console panel has the focus and there is selected text
			if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.hasFocus()
					&& AcideMainWindow.getInstance().getConsolePanel()
							.getTextPane().getSelectedText() != null) {

				// Enables the copy menu item
				_copyMenuItem.setEnabled(true);

				// If the caret position is after the prompt position
				if (AcideMainWindow.getInstance().getConsolePanel()
						.getTextPane().getSelectionStart() >= AcideMainWindow
						.getInstance().getConsolePanel()
						.getPromptCaretPosition())

					// Enables the cut menu item
					_cutMenuItem.setEnabled(true);
			} else

			// If the file editor text edition area has the focus and
			// there is something selected
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getActiveTextEditionArea()
					.hasFocus()
					&& AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().getSelectedText() != null) {

				// Enables the copy menu item
				_copyMenuItem.setEnabled(true);

				// Enables the cut menu item
				_cutMenuItem.setEnabled(true);
			}

			// Enables the go to line menu item
			_goToLineMenuItem.setEnabled(true);

			// Enables the select all menu item
			_selectAllMenuItem.setEnabled(true);

			// Enables the search menu item
			_searchMenuItem.setEnabled(true);

			// Enables the replace menu item
			_replaceMenuItem.setEnabled(true);

		} else {

			// We can copy from the output
			if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.getSelectedText() != null) {

				// Enables the copy menu item
				_copyMenuItem.setEnabled(true);

				// If the caret position is after the prompt position
				if (AcideMainWindow.getInstance().getConsolePanel()
						.getTextPane().getSelectionStart() >= AcideMainWindow
						.getInstance().getConsolePanel()
						.getPromptCaretPosition())

					// Enables the cut menu item
					_cutMenuItem.setEnabled(true);
			}

			// Disables the go to line menu item
			_goToLineMenuItem.setEnabled(false);

			// Disables the select all menu item
			_selectAllMenuItem.setEnabled(false);

			// Disables the search menu item
			_searchMenuItem.setEnabled(false);

			// Disables the replace menu item
			_replaceMenuItem.setEnabled(false);
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu search menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu search menu item.
	 */
	public JMenuItem getSearchMenuItem() {
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
	 * Returns the ACIDE - A Configurable IDE edit menu undo action.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu undo action.
	 */
	public AcideUndoAction getUndoAction() {
		return _undoAction;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE edit menu redo action.
	 * 
	 * @return the ACIDE - A Configurable IDE edit menu redo action.
	 */
	public AcideRedoAction getRedoAction() {
		return _redoAction;
	}
}