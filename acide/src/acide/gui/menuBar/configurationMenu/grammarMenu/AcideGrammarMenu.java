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
package acide.gui.menuBar.configurationMenu.grammarMenu;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.grammarMenu.listeners.AcideAutoAnalysisMenuItemListener;
import acide.gui.menuBar.configurationMenu.grammarMenu.listeners.AcideLoadGrammarMenuItemListener;
import acide.gui.menuBar.configurationMenu.grammarMenu.listeners.AcideModifyGrammaMenuItemrListener;
import acide.gui.menuBar.configurationMenu.grammarMenu.listeners.AcideNewGrammarMenuItemListener;
import acide.gui.menuBar.configurationMenu.grammarMenu.listeners.AcideSaveAsGrammarMenuItemListener;
import acide.gui.menuBar.configurationMenu.grammarMenu.listeners.AcideSaveGrammarMenuItemListener;
import acide.gui.menuBar.configurationMenu.grammarMenu.listeners.AcideSetPathsMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE grammar menu.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideGrammarMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE grammar menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE grammar menu new grammar menu item name.
	 */
	public static final String NEW_GRAMMAR_NAME = "New Grammar";
	/**
	 * ACIDE - A Configurable IDE grammar menu load grammar menu item name.
	 */
	public static final String LOAD_GRAMMAR_NAME = "Load Grammar";
	/**
	 * ACIDE - A Configurable IDE grammar menu modify grammar menu item name.
	 */
	public static final String MODIFY_GRAMMAR_NAME = "Modify Grammar";
	/**
	 * ACIDE - A Configurable IDE grammar menu save grammar menu item name.
	 */
	public static final String SAVE_GRAMMAR_NAME = "Save Grammar";
	/**
	 * ACIDE - A Configurable IDE grammar menu save grammar as menu item name.
	 */
	public static final String SAVE_GRAMMAR_AS_NAME = "Save Grammar As";
	/**
	 * ACIDE - A Configurable IDE grammar menu set paths menu item name.
	 */
	public static final String SET_PATHS_NAME = "Set Paths";
	/**
	 * ACIDE - A Configurable IDE grammar menu suto-Analysis menu item name.
	 */
	public static final String AUTO_ANALYSIS_NAME = "Auto-Analysis";
	/**
	 * ACIDE - A Configurable IDE grammar menu new grammar menu item.
	 */
	private JMenuItem _newGrammarMenuItem;
	/**
	 * ACIDE - A Configurable IDE grammar menu load grammar menu item.
	 */
	private JMenuItem _loadGrammarMenuItem;
	/**
	 * ACIDE - A Configurable IDE grammar menu modify menu item.
	 */
	private JMenuItem _modifyGrammarMenuItem;
	/**
	 * ACIDE - A Configurable IDE grammar menu save menu item.
	 */
	private JMenuItem _saveGrammarMenuItem;
	/**
	 * ACIDE - A Configurable IDE grammar menu save grammar as menu item.
	 */
	private JMenuItem _saveGrammarAsMenuItem;
	/**
	 * ACIDE - A Configurable IDE grammar menu set paths menu item.
	 */
	private JMenuItem _setPathsMenuItem;
	/**
	 * ACIDE - A Configurable IDE grammar menu auto analysis check box menu
	 * item.
	 */
	private JCheckBoxMenuItem _autoAnalysisCheckBoxMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE grammar menu.
	 */
	public AcideGrammarMenu() {

		// NEW GRAMMAR MENU ITEM
		_newGrammarMenuItem = new JMenuItem();

		// LOAD GRAMMAR MENU ITEM
		_loadGrammarMenuItem = new JMenuItem();

		// MODIFY GRAMMAR MENU ITEM
		_modifyGrammarMenuItem = new JMenuItem();

		// SAVE GRAMMAR MENU ITEM
		_saveGrammarMenuItem = new JMenuItem();
		_saveGrammarMenuItem.setEnabled(false);

		// SAVE GRAMMAR AS MENU ITEM
		_saveGrammarAsMenuItem = new JMenuItem();

		// SET PATHS MENU ITEM
		_setPathsMenuItem = new JMenuItem();

		// AUTO ANALYSIS CHECK BOX MENU ITEM
		_autoAnalysisCheckBoxMenuItem = new JCheckBoxMenuItem(
				AcideLanguageManager.getInstance().getLabels().getString("s911"));
		_autoAnalysisCheckBoxMenuItem.setSelected(false);

		// Sets the text of the grammar menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE grammar menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// NEW GRAMMAR MENU ITEM
		_newGrammarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s30"));
		_newGrammarMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_T, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// LOAD GRAMMAR MENU ITEM
		_loadGrammarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s226"));

		// MODIFY GRAMMAR MENU ITEM
		_modifyGrammarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s227"));

		// SAVE GRAMMAR MENU ITEM
		_saveGrammarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s251"));

		// SAVE GRAMMAR AS MENU ITEM
		_saveGrammarAsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s285"));

		// SET PATHS MENU ITEM
		_setPathsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s912"));

		// AUTO GRAMMAR ANALYSIS CHECK BOX MENU ITEM
		_autoAnalysisCheckBoxMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s911"));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE grammar menu item listeners.
	 */
	public void setListeners() {

		// NEW GRAMMAR MENU ITEM
		_newGrammarMenuItem
				.addActionListener(new AcideNewGrammarMenuItemListener());

		// LOAD GRAMMAR MENU ITEM
		_loadGrammarMenuItem
				.addActionListener(new AcideLoadGrammarMenuItemListener());

		// MODIFY GRAMMAR MENU ITEM
		_modifyGrammarMenuItem
				.addActionListener(new AcideModifyGrammaMenuItemrListener());

		// SAVE GRAMMAR MENU ITEM
		_saveGrammarMenuItem
				.addActionListener(new AcideSaveGrammarMenuItemListener());

		// SAVE AS GRAMMAR MENU ITEM
		_saveGrammarAsMenuItem
				.addActionListener(new AcideSaveAsGrammarMenuItemListener());

		// SET PATHS MENU ITEM
		_setPathsMenuItem
				.addActionListener(new AcideSetPathsMenuItemListener());

		// AUTO GRAMMAR ANALYSIS CHECK BOX MENU ITEM
		_autoAnalysisCheckBoxMenuItem
				.addActionListener(new AcideAutoAnalysisMenuItemListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE grammar menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// NEW GRAMMAR MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				NEW_GRAMMAR_NAME))
			add(_newGrammarMenuItem);

		// LOAD GRAMMAR MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				LOAD_GRAMMAR_NAME))
			add(_loadGrammarMenuItem);

		// MODIFY GRAMMAR MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				MODIFY_GRAMMAR_NAME))
			add(_modifyGrammarMenuItem);

		// SAVE GRAMMAR MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_GRAMMAR_NAME))
			add(_saveGrammarMenuItem);

		// SAVE GRAMMAR AS MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_GRAMMAR_AS_NAME))
			add(_saveGrammarAsMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(
				NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_GRAMMAR_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(SAVE_GRAMMAR_AS_NAME))
				&& (AcideMenuConfiguration.getInstance()
						.getIsDisplayed(SET_PATHS_NAME)))
			addSeparator();

		// SET PATHS MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SET_PATHS_NAME))
			add(_setPathsMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(
				NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_GRAMMAR_AS_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(SET_PATHS_NAME))
				&& (AcideMenuConfiguration.getInstance()
						.getIsDisplayed(AUTO_ANALYSIS_NAME)))
			addSeparator();

		// AUTO ANALYSIS CHECK BOX MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				AUTO_ANALYSIS_NAME))
			add(_autoAnalysisCheckBoxMenuItem);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu new grammar menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu new grammar menu
	 *         item.
	 */
	public JMenuItem getNewGrammarMenuItem() {
		return _newGrammarMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu load grammar menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu load grammar menu
	 *         item.
	 */
	public JMenuItem getLoadGrammarMenuItem() {
		return _loadGrammarMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu modify grammar menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu modify grammar menu
	 *         item.
	 */
	public JMenuItem getModifyGrammarMenuItem() {
		return _modifyGrammarMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu save grammar menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu save grammar menu
	 *         item.
	 */
	public JMenuItem getSaveGrammarMenuItem() {
		return _saveGrammarMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE grammar menu save grammar as menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE grammar menu save grammar as menu
	 *         item.
	 */
	public JMenuItem getSaveGrammarAsMenuItem() {
		return _saveGrammarAsMenuItem;
	}
}