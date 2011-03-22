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
package acide.gui.menuBar.configurationMenu.lexiconMenu;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideLoadLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideModifyLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideNewLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideSaveAsLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideSaveLexiconMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE lexicon menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideLexiconMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE lexicon menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon menu new lexicon menu item name.
	 */
	public static final String NEW_LEXICON_NAME = "New Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu modify lexicon menu item name.
	 */
	public static final String MODIFY_LEXICON_NAME = "Modify Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu save lexicon menu item name.
	 */
	public static final String SAVE_LEXICON_NAME = "Save Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu load lexicon menu item name.
	 */
	public static final String LOAD_LEXICON_NAME = "Load Lexicon";
	/**
	 * ACIDE - A Configurable IDE lexicon menu save lexicon as menu item name.
	 */
	public static final String SAVE_LEXICON_AS_NAME = "Save Lexicon As";
	/**
	 * ACIDE - A Configurable IDE lexicon menu new lexicon menu item.
	 */
	private JMenuItem _newLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE lexicon menu load lexicon menu item.
	 */
	private JMenuItem _loadLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE lexicon menu modify lexicon menu item.
	 */
	private JMenuItem _modifyLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE lexicon menu save lexicon menu item.
	 */
	private JMenuItem _saveLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE lexicon menu save lexicon as menu item.
	 */
	private JMenuItem _saveLexiconAsMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon menu.
	 */
	public AcideLexiconMenu() {

		// NEW LEXICON MENU ITEM
		_newLexiconMenuItem = new JMenuItem();

		// LOAD LEXICON MENU ITEM
		_loadLexiconMenuItem = new JMenuItem();

		// MODIFY LEXICON MENU ITEM
		_modifyLexiconMenuItem = new JMenuItem();

		// SAVE LEXICON MENU ITEM
		_saveLexiconMenuItem = new JMenuItem();

		// SAVE LEXICON AS MENU ITEM
		_saveLexiconAsMenuItem = new JMenuItem();

		// Sets the text of the lexicon menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE lexicon menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// NEW LEXICON MENU ITEM
		_newLexiconMenuItem.setText(labels.getString("s249"));

		// LOAD LEXICON MENU ITEM
		_loadLexiconMenuItem.setText(labels.getString("s35"));
		_loadLexiconMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_L, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// MODIFY LEXICON MENU ITEM
		_modifyLexiconMenuItem.setText(labels.getString("s29"));
		_modifyLexiconMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_X, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// SAVE LEXICON MENU ITEM
		_saveLexiconMenuItem.setText(labels.getString("s250"));

		// SAVE LEXICON AS MENU ITEM
		_saveLexiconAsMenuItem.setText(labels.getString("s286"));
	}

	/**
	 * Sets the lACIDE - A Configurable IDE lexicon menu menu item listeners.
	 */
	public void setListeners() {

		// NEW LEXICON MENU ITEM
		_newLexiconMenuItem
				.addActionListener(new AcideNewLexiconMenuItemListener());

		// MODIFY LEXICON MENU ITEM
		_modifyLexiconMenuItem
				.addActionListener(new AcideModifyLexiconMenuItemListener());

		// LOAD LEXICON MENU ITEM
		_loadLexiconMenuItem
				.addActionListener(new AcideLoadLexiconMenuItemListener());

		// SAVE LEXICON MENU ITEM
		_saveLexiconMenuItem
				.addActionListener(new AcideSaveLexiconMenuItemListener());

		// SAVE LEXICON AS MENU ITEM
		_saveLexiconAsMenuItem
				.addActionListener(new AcideSaveAsLexiconMenuItemListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE lexicon menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// NEW LEXICON MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				NEW_LEXICON_NAME))
			add(_newLexiconMenuItem);

		// LOAD LEXICON MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				LOAD_LEXICON_NAME))
			add(_loadLexiconMenuItem);

		// MODIFY LEXICON MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				MODIFY_LEXICON_NAME))
			add(_modifyLexiconMenuItem);

		// SAVE LEXICON MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_LEXICON_NAME))
			add(_saveLexiconMenuItem);

		// SAVE LEXICON AS MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_LEXICON_AS_NAME))
			add(_saveLexiconAsMenuItem);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu new lexicon menu item
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu new lexicon menu item
	 */
	public JMenuItem getNewLexiconMenuItem() {
		return _newLexiconMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu load lexicon menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu load lexicon menu
	 *         item.
	 */
	public JMenuItem getLoadLexiconMenuItem() {
		return _loadLexiconMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu modify lexicon menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu modify lexicon menu
	 *         item.
	 */
	public JMenuItem getModifyLexiconMenuItem() {
		return _modifyLexiconMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu save lexicon menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu save lexicon menu
	 *         item.
	 */
	public JMenuItem getSaveLexiconMenuItem() {
		return _saveLexiconMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon menu save lexicon as menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon menu save lexicon as menu
	 *         item.
	 */
	public JMenuItem getSaveLexiconAsMenuItem() {
		return _saveLexiconAsMenuItem;
	}
}