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
import javax.swing.JSeparator;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import acide.gui.mainWindow.AcideMainWindow;
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
	 * ACIDE - A Configurable IDE grammar menu save grammar as set paths
	 * separator.
	 */
	private JSeparator _saveGrammarAsSetPathsSeparator;
	/**
	 * ACIDE - A Configurable IDE grammar menu set paths auto analysis
	 * separator.
	 */
	private JSeparator _setPathsAutoAnalyisisSeparator;

	/**
	 * Creates a new ACIDE - A Configurable IDE grammar menu.
	 */
	public AcideGrammarMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the grammar menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE grammar menu.
	 */
	private void addComponents() {

		// Adds the new grammar menu item to the menu
		add(_newGrammarMenuItem);

		// Adds the load grammar menu item to the menu
		add(_loadGrammarMenuItem);

		// Adds the modify grammar menu item to the menu
		add(_modifyGrammarMenuItem);

		// Adds the save grammar menu item to the menu
		add(_saveGrammarMenuItem);

		// Adds the save grammar as menu item to the menu
		add(_saveGrammarAsMenuItem);

		// Adds the save grammar as set paths separator to the menu
		add(_saveGrammarAsSetPathsSeparator);

		// Adds the set paths menu item to the menu
		add(_setPathsMenuItem);

		// Adds the set paths auto analysis separator to the menu
		add(_setPathsAutoAnalyisisSeparator);

		// Adds the auto analysis check box menu item to the menu
		add(_autoAnalysisCheckBoxMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE grammar menu components.
	 */
	private void buildComponents() {

		// Creates the new grammar menu item
		_newGrammarMenuItem = new JMenuItem();

		// Sets the new grammar menu item name
		_newGrammarMenuItem.setName(NEW_GRAMMAR_NAME);

		// Creates the load grammar menu item
		_loadGrammarMenuItem = new JMenuItem();

		// Sets the load grammar menu item name
		_loadGrammarMenuItem.setName(LOAD_GRAMMAR_NAME);

		// Creates the modify grammar menu item
		_modifyGrammarMenuItem = new JMenuItem();

		// Sets the modify grammar menu item name
		_modifyGrammarMenuItem.setName(MODIFY_GRAMMAR_NAME);

		// Creates the save grammar menu item
		_saveGrammarMenuItem = new JMenuItem();

		// Sets the save grammar menu item name
		_saveGrammarMenuItem.setName(SAVE_GRAMMAR_NAME);

		// Disables the save grammar menu item
		_saveGrammarMenuItem.setEnabled(false);

		// Creates the save grammar as menu item
		_saveGrammarAsMenuItem = new JMenuItem();

		// Sets the save grammar as menu item name
		_saveGrammarAsMenuItem.setName(SAVE_GRAMMAR_AS_NAME);

		// Creates the save grammar as set paths separator
		_saveGrammarAsSetPathsSeparator = new JSeparator();
		
		// Creates the set paths menu item
		_setPathsMenuItem = new JMenuItem();

		// Sets the set paths menu item name
		_setPathsMenuItem.setName(SET_PATHS_NAME);

		// Creates the auto analysis check box menu item
		_autoAnalysisCheckBoxMenuItem = new JCheckBoxMenuItem(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s911"));

		// Creates the set paths auto analysis separator
		_setPathsAutoAnalyisisSeparator = new JSeparator();
		
		// Sets the auto analysis check box menu item name
		_autoAnalysisCheckBoxMenuItem.setName(AUTO_ANALYSIS_NAME);

		// Sets the auto analysis check box menu item as not selected
		_autoAnalysisCheckBoxMenuItem.setSelected(false);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE grammar menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the new grammar menu item text
		_newGrammarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s30"));

		// Sets the new grammar menu item accelerator
		_newGrammarMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_T, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// Sets the load grammar menu item text
		_loadGrammarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s226"));

		// Sets the modify grammar menu item text
		_modifyGrammarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s227"));

		// Sets the save grammar menu item text
		_saveGrammarMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s251"));

		// Sets the save grammar as menu item text
		_saveGrammarAsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s285"));

		// Sets the set paths menu item text
		_setPathsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s912"));

		// Sets the auto analysis check box menu item text
		_autoAnalysisCheckBoxMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s911"));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE grammar menu item listeners.
	 */
	public void setListeners() {

		// Sets the new grammar menu item action listener
		_newGrammarMenuItem
				.addActionListener(new AcideNewGrammarMenuItemListener());

		// Sets the load grammar menu item action listener
		_loadGrammarMenuItem
				.addActionListener(new AcideLoadGrammarMenuItemListener());

		// Sets the modify grammar menu item action listener
		_modifyGrammarMenuItem
				.addActionListener(new AcideModifyGrammaMenuItemrListener());

		// Sets the save grammar menu item action listener
		_saveGrammarMenuItem
				.addActionListener(new AcideSaveGrammarMenuItemListener());

		// Sets the save grammar as menu item action listener
		_saveGrammarAsMenuItem
				.addActionListener(new AcideSaveAsGrammarMenuItemListener());

		// Sets the set paths menu item action listener
		_setPathsMenuItem
				.addActionListener(new AcideSetPathsMenuItemListener());

		// Sets the auto analysis check box menu item action listener
		_autoAnalysisCheckBoxMenuItem
				.addActionListener(new AcideAutoAnalysisMenuItemListener());
	}

	/**
	 * Updates the ACIDE - A Configurable IDE grammar menu components visibility
	 * with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the new grammar menu item to visible or not visible
		_newGrammarMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(NEW_GRAMMAR_NAME));

		// Sets the load grammar menu item to visible or not visible
		_loadGrammarMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(LOAD_GRAMMAR_NAME));

		// Sets the modify grammar menu item to visible or not visible
		_modifyGrammarMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(MODIFY_GRAMMAR_NAME));

		// Sets the save grammar menu item to visible or not visible
		_saveGrammarMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SAVE_GRAMMAR_NAME));

		// Sets the save grammar as menu item to visible or not visible
		_saveGrammarAsMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SAVE_GRAMMAR_AS_NAME));

		// Sets the save grammar as set paths separator as visible or not
		// visible
		_saveGrammarAsSetPathsSeparator.setVisible((AcideMenuConfiguration
				.getInstance().getIsDisplayed(NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_GRAMMAR_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(SAVE_GRAMMAR_AS_NAME))
				&& (AcideMenuConfiguration.getInstance()
						.getIsDisplayed(SET_PATHS_NAME)));

		// Sets the set paths menu item to visible or not visible
		_setPathsMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(SET_PATHS_NAME));

		// Sets the set paths auto analysis separator to visible or not visible
		_setPathsAutoAnalyisisSeparator.setVisible((AcideMenuConfiguration
				.getInstance().getIsDisplayed(NEW_GRAMMAR_NAME)
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
						.getIsDisplayed(AUTO_ANALYSIS_NAME)));

		// Sets the auto analysis check box menu item to visible or not visible
		_autoAnalysisCheckBoxMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(AUTO_ANALYSIS_NAME));
	}

	/**
	 * Enables the ACIDE - A Configurable IDE grammar menu.
	 */
	public void enableMenu() {

		// Enables the new grammar menu item
		_newGrammarMenuItem.setEnabled(true);
		
		// Enables the load grammar menu item
		_loadGrammarMenuItem.setEnabled(true);
		
		// Enables the modify grammar menu item
		_modifyGrammarMenuItem.setEnabled(true);
		
		// Enables the save grammar menu item
		_saveGrammarMenuItem.setEnabled(true);
		
		// Enables the save grammar as menu item
		_saveGrammarAsMenuItem.setEnabled(true);
		
		// Enables the set paths menu item
		_setPathsMenuItem.setEnabled(true);
		
		// Enables the auto analysis check box menu item
		_autoAnalysisCheckBoxMenuItem.setEnabled(true);
	}
	
	/**
	 * Disables the ACIDE - A Configurable IDE grammar menu.
	 */
	public void disableMenu() {

		// Disables the new grammar menu item
		_newGrammarMenuItem.setEnabled(false);
		
		// Disables the load grammar menu item
		_loadGrammarMenuItem.setEnabled(false);
		
		// Disables the modify grammar menu item
		_modifyGrammarMenuItem.setEnabled(false);
		
		// Disables the save grammar menu item
		_saveGrammarMenuItem.setEnabled(false);
		
		// Disables the save grammar as menu item
		_saveGrammarAsMenuItem.setEnabled(false);
		
		// Enables the set paths menu item
		_setPathsMenuItem.setEnabled(true);
		
		// Enables the auto analysis check box menu item
		_autoAnalysisCheckBoxMenuItem.setEnabled(true);
	}
	
	/**
	 * Configures the ACIDE - A Configurable IDE grammar menu.
	 */
	public void configure(){
		
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Enables the grammar menu
			enableMenu();

			// Gets the selected file editor panel
			AcideFileEditorPanel selectedFileEditorPanel = AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel();

			// If the current grammar configuration is lastModified or
			// newGrammar
			if (selectedFileEditorPanel.getCurrentGrammarConfiguration()
					.getName().matches("newGrammar")
					|| selectedFileEditorPanel.getCurrentGrammarConfiguration()
							.getName().matches("lastModified"))

				// Enables the save grammar menu
				_saveGrammarMenuItem
						.setEnabled(true);
			else
				// Disables the save grammar menu
				_saveGrammarMenuItem
						.setEnabled(false);
		} else {

			// Disables the grammar menu
			disableMenu();
		}
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