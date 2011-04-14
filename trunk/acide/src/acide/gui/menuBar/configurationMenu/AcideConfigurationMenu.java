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

import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.configurationMenu.consoleMenu.AcideConsoleMenu;
import acide.gui.menuBar.configurationMenu.fileEditor.AcideFileEditorMenu;
import acide.gui.menuBar.configurationMenu.grammarMenu.AcideGrammarMenu;
import acide.gui.menuBar.configurationMenu.languageMenu.AcideLanguageMenu;
import acide.gui.menuBar.configurationMenu.lexiconMenu.AcideLexiconMenu;
import acide.gui.menuBar.configurationMenu.listeners.AcideCompilerMenuItemListener;
import acide.gui.menuBar.configurationMenu.menuMenu.AcideMenuMenu;
import acide.gui.menuBar.configurationMenu.toolBarMenu.AcideToolBarMenu;
import acide.language.AcideLanguageManager;

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
	 * ACIDE - A Configurable IDE configuration menu console menu item name.
	 */
	public static final String CONSOLE_NAME = "Console";
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
	 * ACIDE - A Configurable IDE configuration menu compiler file editor
	 * separator.
	 */
	private JSeparator _compilerFileEditorSeparator;
	/**
	 * ACIDE - A Configurable IDE configuration menu console language separator.
	 */
	private JSeparator _consoleLanguageSeparator;

	/**
	 * Creates a new ACIDE - A Configurable IDE configuration menu.
	 */
	public AcideConfigurationMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the configuration menu components
		setTextOfMenuComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE configuration menu.
	 */
	private void addComponents() {

		// Adds the lexicon menu to the menu
		add(_lexiconMenu);

		// Adds the grammar menu to the menu
		add(_grammarMenu);

		// Adds the compiler menu item to the menu
		add(_compilerMenuItem);

		// Adds the compiler file editor separator
		add(_compilerFileEditorSeparator);

		// Adds the file editor menu to the menu
		add(_fileEditorMenu);

		// Adds the console menu to the menu
		add(_consoleMenu);

		// Adds the console language separator to the menu
		add(_consoleLanguageSeparator);

		// Adds the language menu to the menu
		add(_languageMenu);

		// Adds the menu menu to the menu
		add(_menuMenu);

		// Adds the tool bar menu to the menu
		add(_toolBarMenu);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE configuration menu components.
	 */
	private void buildComponents() {

		// Creates the console menu
		_consoleMenu = new AcideConsoleMenu();

		// Sets the console menu name
		_consoleMenu.setName(CONSOLE_NAME);

		// Creates the file editor menu
		_fileEditorMenu = new AcideFileEditorMenu();

		// Sets the file editor menu menu name
		_fileEditorMenu.setName(FILE_EDITOR_NAME);

		// Creates the language menu
		_languageMenu = new AcideLanguageMenu();

		// Sets the language menu menu name
		_languageMenu.setName(LANGUAGE_NAME);

		// Creates the menu menu
		_menuMenu = new AcideMenuMenu();

		// Sets the menu menu menu name
		_menuMenu.setName(MENU_NAME);

		// Creates the tool bar menu
		_toolBarMenu = new AcideToolBarMenu();

		// Sets the tool bar menu menu name
		_toolBarMenu.setName(TOOLBAR_NAME);

		// Creates the lexicon menu
		_lexiconMenu = new AcideLexiconMenu();

		// Sets the lexicon menu menu name
		_lexiconMenu.setName(LEXICON_NAME);

		// Creates the grammar menu
		_grammarMenu = new AcideGrammarMenu();

		// Sets the grammar menu menu name
		_grammarMenu.setName(GRAMMAR_NAME);

		// Creates the compiler menu item
		_compilerMenuItem = new JMenuItem(COMPILER_IMAGE);

		// Sets the compiler menu menu name
		_compilerMenuItem.setName(COMPILER_NAME);

		// Creates the compiler file editor separator
		_compilerFileEditorSeparator = new JSeparator();

		// Creates the console language separator
		_consoleLanguageSeparator = new JSeparator();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE configuration menu
	 * components with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the language menu text
		_languageMenu.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s6"));

		// Sets the language menu items text
		_languageMenu.setTextOfMenuComponents();

		// Sets the console menu text
		_consoleMenu.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s332"));

		// Sets the console menu items text
		_consoleMenu.setTextOfMenuComponents();

		// Sets the file editor menu text
		_fileEditorMenu.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s1045"));

		// Sets the file editor menu items text
		_fileEditorMenu.setTextOfMenuComponets();

		// Sets the menu menu text
		_menuMenu.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s34"));

		// Sets the menu menu items text
		_menuMenu.setTextOfMenuComponents();

		// Sets the tool bar menu text
		_toolBarMenu.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s169"));

		// Sets the tool bar menu items text
		_toolBarMenu.setTextOfMenuComponents();

		// Sets the lexicon menu text
		_lexiconMenu.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s224"));

		// Sets the lexicon menu items text
		_lexiconMenu.setTextOfMenuComponents();

		// Disables the lexicon menu
		_lexiconMenu.setEnabled(false);

		// Sets the grammar menu text
		_grammarMenu.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s225"));

		// Sets the grammar menu items text
		_grammarMenu.setTextOfMenuComponents();

		// Disables the grammar menu
		_grammarMenu.setEnabled(false);

		// Sets the compiler menu item text
		_compilerMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s240"));
	}

	/**
	 * Updates the ACIDE - A Configurable IDE configuration menu components
	 * visibility with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Builds the lexicon menu
		_lexiconMenu.build();

		// Sets the lexicon menu to visible or not visible
		_lexiconMenu.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLexiconMenu.LOAD_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.MODIFY_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.NEW_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.SAVE_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLexiconMenu.SAVE_LEXICON_AS_NAME));

		// Builds the grammar menu
		_grammarMenu.updateComponentsVisibility();

		// Sets the grammar menu to visible or not visible
		_grammarMenu.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideGrammarMenu.NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.SAVE_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideGrammarMenu.SAVE_GRAMMAR_AS_NAME));

		// Sets the compiler menu item to visible or not visible
		_compilerMenuItem.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(COMPILER_NAME));

		// Sets the compiler filer editor separator to visible or not visible
		_compilerFileEditorSeparator
				.setVisible((AcideMenuConfiguration.getInstance()
						.getIsDisplayed(AcideLexiconMenu.LOAD_LEXICON_NAME)
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
						&& (AcideMenuConfiguration
								.getInstance()
								.getIsDisplayed(AcideConsoleMenu.CONFIGURE_NAME) || AcideMenuConfiguration
								.getInstance().getIsDisplayed(
										AcideConsoleMenu.EXTERNAL_COMMAND_NAME)));

		// Builds the file editor menu
		_fileEditorMenu.updateComponentsVisibility();

		// Sets the file editor menu to visible or not visible
		_fileEditorMenu.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(
						AcideFileEditorMenu.FILE_EDITOR_DISPLAY_OPTIONS_NAME));

		// builds the console menu
		_consoleMenu.updateComponentsVisibility();

		// Sets the console menu to visible or not visible
		_consoleMenu.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideConsoleMenu.CONFIGURE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideConsoleMenu.EXTERNAL_COMMAND_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideConsoleMenu.CONSOLE_DISPLAY_OPTIONS_NAME));

		// Sets the console language separator to visible or not visible
		_consoleLanguageSeparator
				.setVisible((AcideMenuConfiguration.getInstance()
						.getIsDisplayed(AcideLexiconMenu.LOAD_LEXICON_NAME)
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
						&& (AcideMenuConfiguration.getInstance()
								.getIsDisplayed(AcideLanguageMenu.SPANISH_NAME)
								|| AcideMenuConfiguration.getInstance()
										.getIsDisplayed(
												AcideLanguageMenu.ENGLISH_NAME)
								|| AcideMenuConfiguration.getInstance()
										.getIsDisplayed(MENU_NAME) || AcideMenuConfiguration
								.getInstance().getIsDisplayed(TOOLBAR_NAME)));

		// Builds the language menu
		_languageMenu.updateComponentsVisibility();

		// Sets the language menu to visible or not visible
		_languageMenu.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLanguageMenu.SPANISH_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideLanguageMenu.ENGLISH_NAME));

		// Builds the menu menu
		_menuMenu.updateComponentsVisibility();

		// Sets the menu menu to visible or not visible
		_menuMenu.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideMenuMenu.NEW_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideMenuMenu.LOAD_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideMenuMenu.MODIFY_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideMenuMenu.SAVE_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideMenuMenu.SAVE_MENU_AS_NAME));

		// Builds the tool bar menu
		_toolBarMenu.updateComponentsVisibility();

		// Sets the tool bar menu to visible or not visible
		_toolBarMenu.setVisible(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideToolBarMenu.NEW_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideToolBarMenu.LOAD_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideToolBarMenu.MODIFY_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideToolBarMenu.SAVE_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						AcideToolBarMenu.SAVE_TOOLBAR_AS_NAME));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE configuration menu menu item
	 * listeners.
	 */
	public void setListeners() {

		// Sets the lexicon menu listeners
		_lexiconMenu.setListeners();

		// Sets the grammar menu listeners
		_grammarMenu.setListeners();

		// Sets the file editor menu listeners
		_fileEditorMenu.setListeners();

		// Sets the console menu listeners
		_consoleMenu.setListeners();

		// Sets the menu menu listeners
		_menuMenu.setListeners();

		// Sets the language menu listeners
		_languageMenu.setListeners();

		// Sets the tool bar menu listeners
		_toolBarMenu.setListeners();

		// Sets the compiler menu item listener
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
	 * Returns the ACIDE - A Configurable IDE configuration menu file editor
	 * menu.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu file editor
	 *         menu.
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