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

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideLoadLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideModifyLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideNewLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcidePredetermineLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideSaveAsLexiconMenuItemListener;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideSaveLexiconMenuItemListener;
import acide.language.AcideLanguageManager;

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
	 * ACIDE - A Configurable IDE lexicon menu predetermine lexicon menu item
	 * name.
	 */
	public static final String PREDETERMINE_LEXICON_NAME = "Predetermine Lexicon";
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
	 * ACIDE - A Configurable IDE lexicon menu save lexicon as Predetermine
	 * lexicon separator.
	 */
	private JSeparator _saveLexiconAsPredetermineLexiconSeparator;
	/**
	 * ACIDE - A Configurable IDE lexicon menu predetermine lexicon menu item.
	 */
	private JMenuItem _predetermineLexiconMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon menu.
	 */
	public AcideLexiconMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the lexicon menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE lexicon menu.
	 */
	private void addComponents() {

		// Adds the new lexicon menu item to the menu
		add(_newLexiconMenuItem);

		// Adds the load lexicon menu item to the menu
		add(_loadLexiconMenuItem);

		// Adds the modify lexicon menu item to the menu
		add(_modifyLexiconMenuItem);

		// Adds the save lexicon menu item to the menu
		add(_saveLexiconMenuItem);

		// Adds the save lexicon as menu item to the menu
		add(_saveLexiconAsMenuItem);

		// Adds the save lexicon as predetermine lexicon separator
		add(_saveLexiconAsPredetermineLexiconSeparator);

		// Adds the predetermine lexicon menu item
		add(_predetermineLexiconMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE lexicon menu components.
	 */
	private void buildComponents() {

		// Creates the new lexicon menu item
		_newLexiconMenuItem = new JMenuItem();

		// Sets the new lexicon menu item name
		_newLexiconMenuItem.setName(NEW_LEXICON_NAME);

		// Creates the load lexicon menu item
		_loadLexiconMenuItem = new JMenuItem();

		// Sets the load lexicon menu item name
		_loadLexiconMenuItem.setName(LOAD_LEXICON_NAME);

		// Creates the modify lexicon menu item
		_modifyLexiconMenuItem = new JMenuItem();

		// Sets the modify lexicon menu item name
		_modifyLexiconMenuItem.setName(MODIFY_LEXICON_NAME);

		// Creates the save lexicon menu item
		_saveLexiconMenuItem = new JMenuItem();

		// Sets the save lexicon menu item name
		_saveLexiconMenuItem.setName(SAVE_LEXICON_NAME);

		// Creates the save lexicon as menu item
		_saveLexiconAsMenuItem = new JMenuItem();

		// Sets the save lexicon as menu item name
		_saveLexiconAsMenuItem.setName(SAVE_LEXICON_AS_NAME);

		// Creates the save lexicon as predetermine lexicon separator
		_saveLexiconAsPredetermineLexiconSeparator = new JSeparator();

		// Creates the predetermine lexicon menu item
		_predetermineLexiconMenuItem = new JMenuItem();

		// Sets the predetermine lexicon menu item name
		_predetermineLexiconMenuItem.setName(PREDETERMINE_LEXICON_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE lexicon menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the new lexicon menu item text
		_newLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s249"));

		// Sets the load lexicon menu item text
		_loadLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s35"));

		// Sets the load lexicon menu item accelerator
		_loadLexiconMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_L, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// Sets the modify lexicon menu item text
		_modifyLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s29"));

		// Sets the modify lexicon menu item accelerator
		_modifyLexiconMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_X, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// Sets the save lexicon menu item text
		_saveLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s250"));

		// Sets the save lexicon as menu item text
		_saveLexiconAsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s286"));

		// Sets the predetermine lexicon menu item text
		_predetermineLexiconMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s1080"));
	}

	/**
	 * Sets the lACIDE - A Configurable IDE lexicon menu menu item listeners.
	 */
	public void setListeners() {

		// Sets the new lexicon menu item action listener
		_newLexiconMenuItem
				.addActionListener(new AcideNewLexiconMenuItemListener());

		// Sets the modify lexicon menu item action listener
		_modifyLexiconMenuItem
				.addActionListener(new AcideModifyLexiconMenuItemListener());

		// Sets the load lexicon menu item action listener
		_loadLexiconMenuItem
				.addActionListener(new AcideLoadLexiconMenuItemListener());

		// Sets the save lexicon menu item action listener
		_saveLexiconMenuItem
				.addActionListener(new AcideSaveLexiconMenuItemListener());

		// Sets the save lexicon as menu item action listener
		_saveLexiconAsMenuItem
				.addActionListener(new AcideSaveAsLexiconMenuItemListener());

		// Sets the predetermine lexicon menu item action listener
		_predetermineLexiconMenuItem
				.addActionListener(new AcidePredetermineLexiconMenuItemListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE lexicon menu.
	 */
	public void build() {

		// Sets the new lexicon menu item to visible or not visible
		_newLexiconMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(NEW_LEXICON_NAME));

		// Sets the load lexicon menu item to visible or not visible
		_loadLexiconMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(LOAD_LEXICON_NAME));

		// Sets the modify lexicon menu item to visible or not visible
		_modifyLexiconMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(MODIFY_LEXICON_NAME));

		// Sets the save lexicon menu item to visible or not visible
		_saveLexiconMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SAVE_LEXICON_NAME));

		// Sets the save lexicon as menu item to visible or not visible
		_saveLexiconAsMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SAVE_LEXICON_AS_NAME));

		// Sets the save lexicon as predetermine lexicon separator to visible or
		// not visible
		_saveLexiconAsPredetermineLexiconSeparator
				.setVisible(AcideMenuConfiguration.getInstance()
						.getIsDisplayed(NEW_LEXICON_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								LOAD_LEXICON_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								MODIFY_LEXICON_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								SAVE_LEXICON_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								SAVE_LEXICON_AS_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								PREDETERMINE_LEXICON_NAME));

		// Sets the predetermine lexicon menu item to visible or not visible
		_predetermineLexiconMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(PREDETERMINE_LEXICON_NAME));
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