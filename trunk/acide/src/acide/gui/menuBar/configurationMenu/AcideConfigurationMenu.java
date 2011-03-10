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
package acide.gui.menuBar.configurationMenu;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.consoleMenu.AcideConsoleMenu;
import acide.gui.menuBar.configurationMenu.fileEditor.AcideFileEditorMenu;
import acide.gui.menuBar.configurationMenu.grammarMenu.AcideGrammarMenu;
import acide.gui.menuBar.configurationMenu.languageMenu.AcideLanguageMenu;
import acide.gui.menuBar.configurationMenu.lexiconMenu.AcideLexiconMenu;
import acide.gui.menuBar.configurationMenu.listeners.AcideCompilerMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.AcideMenuMenu;
import acide.gui.menuBar.configurationMenu.toolBarMenu.AcideToolBarMenu;

import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE configuration menu.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideConfigurationMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE configuration menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE configuration menu lexicon menu item name.
	 */
	public static final String LEXICON_NAME = "Lexicon";
	/**
	 * ACIDE - A Configurable IDE configuration menu grammar menu item name.
	 */
	public static final String GRAMMAR_NAME = "Grammar";
	/**
	 * ACIDE - A Configurable IDE configuration menu language menu item name.
	 */
	public static final String LANGUAGE_NAME = "Language";
	/**
	 * ACIDE - A Configurable IDE configuration menu file editor menu item name.
	 */
	public static final String FILE_EDITOR_NAME = "File Editor";
	/**
	 * ACIDE - A Configurable IDE configuration menu menu menu item name.
	 */
	public static final String MENU_NAME = "Menu";
	/**
	 * ACIDE - A Configurable IDE configuration menu output menu item name.
	 */
	public static final String OUTPUT_NAME = "Output";
	/**
	 * ACIDE - A Configurable IDE configuration menu toolbar menu item name.
	 */
	public static final String TOOLBAR_NAME = "Toolbar";
	/**
	 * ACIDE - A Configurable IDE configuration menu compiler menu item name.
	 */
	public static final String COMPILER_NAME = "Compiler";
	/**
	 * ACIDE - A Configurable IDE configuration menu compiler menu item image
	 * icon.
	 */
	public static final ImageIcon COMPILER_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/compiler.png");
	/**
	 * ACIDE - A Configurable IDE configuration menu menu menu item.
	 */
	private AcideMenuMenu _menuMenu;
	/**
	 * ACIDE - A Configurable IDE configuration menu language menu item.
	 */
	private AcideLanguageMenu _languageMenu;
	/**
	 * ACIDE - A Configurable IDE configuration menu file editor menu item.
	 */
	private AcideFileEditorMenu _fileEditorMenu;
	/**
	 * ACIDE - A Configurable IDE configuration menu console menu item.
	 */
	private AcideConsoleMenu _consoleMenu;
	/**
	 * ACIDE - A Configurable IDE configuration menu tool bar menu item.
	 */
	private AcideToolBarMenu _toolBarMenu;
	/**
	 * ACIDE - A Configurable IDE configuration menu lexicon menu item.
	 */
	private AcideLexiconMenu _lexiconMenu;
	/**
	 * ACIDE - A Configurable IDE configuration menu grammar menu item.
	 */
	private AcideGrammarMenu _grammarMenu;
	/**
	 * ACIDE - A Configurable IDE configuration menu compiler menu item.
	 */
	private JMenuItem _compilerMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE configuration menu.
	 */
	public AcideConfigurationMenu() {

		// CONSOLE MENU
		_consoleMenu = new AcideConsoleMenu();
		
		// FILE EDITOR MENU
		_fileEditorMenu = new AcideFileEditorMenu();
		
		// LANGUAGE MENU
		_languageMenu = new AcideLanguageMenu();
		
		// MENU MENU
		_menuMenu = new AcideMenuMenu();
		
		// TOOL BAR MENU
		_toolBarMenu = new AcideToolBarMenu();
		
		// LEXICON MENU
		_lexiconMenu = new AcideLexiconMenu();
		
		// GRAMMAR MENU
		_grammarMenu = new AcideGrammarMenu();
		
		// COMPILER MENU ITEM
		_compilerMenuItem = new JMenuItem(COMPILER_IMAGE);

		// Sets the text of the configuration menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE configuration menu
	 * components with the labels in the selected language to display.
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

		// LANGUAGE MENU
		_languageMenu.setText(labels.getString("s6"));
		_languageMenu.setTextOfMenuComponents();

		// CONSOLE MENU
		_consoleMenu.setText(labels.getString("s332"));
		_consoleMenu.setTextOfMenuComponents();

		// FILE EDITOR MENU
		_fileEditorMenu.setText(labels.getString("s1045"));
		_fileEditorMenu.setTextOfMenuComponets();

		// MENU MENU
		_menuMenu.setText(labels.getString("s34"));
		_menuMenu.setTextOfMenuComponents();

		// TOOL BAR MENU
		_toolBarMenu.setText(labels.getString("s169"));
		_toolBarMenu.setTextOfMenuComponents();

		// LEXICON MENU
		_lexiconMenu.setText(labels.getString("s224"));
		_lexiconMenu.setTextOfMenuComponents();

		// GRAMMAR MENU
		_grammarMenu.setText(labels.getString("s225"));
		_grammarMenu.setTextOfMenuComponents();

		// COMPILER MENU ITEM
		_compilerMenuItem.setText(labels.getString("s240"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE configuration menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// LEXICON MENU
		_lexiconMenu.build();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideLexiconMenu.LOAD_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.MODIFY_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.NEW_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.SAVE_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.SAVE_LEXICON_AS_NAME))

			add(_lexiconMenu);

		// GRAMMAR MENU
		_grammarMenu.build();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideGrammarMenu.NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.SAVE_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.SAVE_GRAMMAR_AS_NAME))
			add(_grammarMenu);

		// COMPILER MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(COMPILER_NAME))
			add(_compilerMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideLexiconMenu.LOAD_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.MODIFY_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						COMPILER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.NEW_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.SAVE_LEXICON_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideGrammarMenu.SAVE_GRAMMAR_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideConsoleMenu.CONFIGURE_NAME) || AcideMenuConfiguration
						.getInstance().getIsDisplayed(
								AcideConsoleMenu.EXTERNAL_COMMAND_NAME)))
			addSeparator();

		// FILE EDITOR MENU
		_fileEditorMenu.build();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideFileEditorMenu.FILE_EDITOR_DISPLAY_OPTIONS_NAME))
			add(_fileEditorMenu);

		// CONSOLE MENU
		_consoleMenu.build();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideConsoleMenu.CONFIGURE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideConsoleMenu.EXTERNAL_COMMAND_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideConsoleMenu.CONSOLE_DISPLAY_OPTIONS_NAME))
			add(_consoleMenu);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideLexiconMenu.LOAD_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.MODIFY_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						COMPILER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideConsoleMenu.CONFIGURE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideConsoleMenu.EXTERNAL_COMMAND_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideConsoleMenu.CONSOLE_DISPLAY_OPTIONS_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.NEW_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.SAVE_LEXICON_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideGrammarMenu.SAVE_GRAMMAR_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLanguageMenu.SPANISH_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								AcideLanguageMenu.ENGLISH_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								MENU_NAME) || AcideMenuConfiguration
						.getInstance().getIsDisplayed(TOOLBAR_NAME)))
			addSeparator();

		// LANGUAGE MENU
		_languageMenu.build();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideLanguageMenu.SPANISH_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLanguageMenu.ENGLISH_NAME))
			add(_languageMenu);

		// MENU MENU
		_menuMenu.build();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideMenuMenu.NEW_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideMenuMenu.LOAD_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideMenuMenu.MODIFY_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideMenuMenu.SAVE_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideMenuMenu.SAVE_MENU_AS_NAME))
			add(_menuMenu);

		// TOOL BAR MENU
		_toolBarMenu.build();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				AcideToolBarMenu.NEW_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideToolBarMenu.LOAD_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideToolBarMenu.MODIFY_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideToolBarMenu.SAVE_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideToolBarMenu.SAVE_TOOLBAR_AS_NAME))
			add(_toolBarMenu);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE configuration menu menu item
	 * listeners.
	 */
	public void setListeners() {

		// LEXICON MENU
		_lexiconMenu.setListeners();

		// GRAMMAR MENU
		_grammarMenu.setListeners();

		// FILE EDITOR MENU
		_fileEditorMenu.setListeners();

		// CONSOLE MENU
		_consoleMenu.setListeners();

		// MENU MENU
		_menuMenu.setListeners();

		// LANGUAGE MENU
		_languageMenu.setListeners();

		// TOOL BAR MENU
		_toolBarMenu.setListeners();

		// COMPILER MENU ITEM
		_compilerMenuItem
				.addActionListener(new AcideCompilerMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu grammar menu.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu grammar menu.
	 */
	public AcideGrammarMenu getGrammarMenu() {
		return _grammarMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu menu menu.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu menu menu.
	 */
	public AcideMenuMenu getMenuMenu() {
		return _menuMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu lexicon menu.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu lexicon menu.
	 */
	public AcideLexiconMenu getLexiconMenu() {
		return _lexiconMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu console menu.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu console menu.
	 */
	public AcideConsoleMenu getConsoleMenu() {
		return _consoleMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu file editor menu.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu file editor menu.
	 */
	public AcideFileEditorMenu getFileEditorMenu() {
		return _fileEditorMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu tool bar menu.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu tool bar menu.
	 */
	public AcideToolBarMenu getToolBarMenu() {
		return _toolBarMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu language menu.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu language menu.
	 */
	public AcideLanguageMenu getLanguageMenu() {
		return _languageMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu compiler menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu compiler menu
	 *         item.
	 */
	public JMenuItem getCompilerMenuItem() {
		return _compilerMenuItem;
	}
}