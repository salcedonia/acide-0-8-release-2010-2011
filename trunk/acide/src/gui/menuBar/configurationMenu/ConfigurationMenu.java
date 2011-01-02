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
package gui.menuBar.configurationMenu;

import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.configurationMenu.consoleMenu.ConsoleMenu;
import gui.menuBar.configurationMenu.grammarMenu.GrammarMenu;
import gui.menuBar.configurationMenu.languageMenu.LanguageMenu;
import gui.menuBar.configurationMenu.lexiconMenu.LexiconMenu;
import gui.menuBar.configurationMenu.listeners.CompilerMenuItemListener;
import gui.menuBar.configurationMenu.menuMenu.MenuMenu;
import gui.menuBar.configurationMenu.toolBarMenu.ToolBarMenu;

import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE configuration menu.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class ConfigurationMenu extends JMenu {

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
	 * ACIDE - A Configurable IDE configuration menu menu menu item.
	 */
	private MenuMenu _menu;
	/**
	 * ACIDE - A Configurable IDE configuration menu language menu item.
	 */
	private LanguageMenu _language;
	/**
	 * ACIDE - A Configurable IDE configuration menu console menu item.
	 */
	private ConsoleMenu _console;
	/**
	 * ACIDE - A Configurable IDE configuration menu tool bar menu item.
	 */
	private ToolBarMenu _toolBar;
	/**
	 * ACIDE - A Configurable IDE configuration menu lexicon menu item.
	 */
	private LexiconMenu _lexicon;
	/**
	 * ACIDE - A Configurable IDE configuration menu grammar menu item.
	 */
	private GrammarMenu _grammar;
	/**
	 * ACIDE - A Configurable IDE configuration menu compiler menu item.
	 */
	private JMenuItem _compiler;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE configuration menu.
	 */
	public ConfigurationMenu(){
				
		// MENU
		_console = new ConsoleMenu();
		_language = new LanguageMenu();
		_menu = new MenuMenu();
		_toolBar = new ToolBarMenu();
		_lexicon = new LexiconMenu();
		_grammar = new GrammarMenu();
		_compiler = new JMenuItem();
		
		setLanguageLabels();
	}
	
	/**
	 * Sets the ACIDE - A Configurable IDE configuration menu language labels.
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
		
		// LANGUAGE
		_language.setText(labels.getString("s6"));
		_language.setLanguageLabels();
		
		// CONSOLE
		_console.setText(labels.getString("s332"));
		_console.setLanguageLabels();
		
		// MENU
		_menu.setText(labels.getString("s34"));
		_menu.setLanguageLabels();
		
		// TOOLBAR
		_toolBar.setText(labels.getString("s169"));
		_toolBar.setLanguageLabels();
		
		// LEXICON
		_lexicon.setText(labels.getString("s224"));
		_lexicon.setLanguageLabels();
		
		// GRAMMAR
		_grammar.setText(labels.getString("s225"));
		_grammar.setLanguageLabels();
		
		// COMPILER
		_compiler.setText(labels.getString("s240"));
	}
	
	/**
	 * Builds the ACIDE - A Configurable IDE configuration menu.
	 */
	public void buildMenu(){
	
		removeAll();
				
		// LEXICON MENU
		_lexicon.buildMenu();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.LOAD_LEXICON_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.MODIFY_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.NEW_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_AS_NAME))

			add(_lexicon);
		
		// GRAMMAR MENU
		_grammar.buildMenu();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_AS_NAME))
			add(_grammar);
		
		// COMPILER MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(COMPILER_NAME))
			add(_compiler);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.LOAD_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.MODIFY_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(COMPILER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.NEW_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(ConsoleMenu.CONFIGURE_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ConsoleMenu.EXTERNAL_COMMAND_NAME)))
			addSeparator();
		
		// CONSOLE MENU
		_console.buildMenu();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ConsoleMenu.CONFIGURE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ConsoleMenu.EXTERNAL_COMMAND_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ConsoleMenu.CONSOLE_DISPLAY_OPTIONS_NAME))
			add(_console);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.LOAD_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.MODIFY_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.NEW_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.LOAD_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.MODIFY_GRAMMAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(COMPILER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ConsoleMenu.CONFIGURE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ConsoleMenu.EXTERNAL_COMMAND_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ConsoleMenu.CONSOLE_DISPLAY_OPTIONS_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.NEW_LEXICON_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(LanguageMenu.SPANISH_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(LanguageMenu.ENGLISH_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(MENU_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(TOOLBAR_NAME)))
			addSeparator();
		
		// LANGUAGE MENU
		_language.buildMenu();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(LanguageMenu.SPANISH_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(LanguageMenu.ENGLISH_NAME))
			add(_language);
		
		// MENU MENU
		_menu.buildMenu();
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(MenuMenu.NEW_MENU_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(MenuMenu.LOAD_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(MenuMenu.MODIFY_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(MenuMenu.SAVE_MENU_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(MenuMenu.SAVE_MENU_AS_NAME))
			add(_menu);
		
		// TOOL BAR MENU
		_toolBar.buildMenu();	
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.NEW_TOOLBAR_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.LOAD_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.MODIFY_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.SAVE_TOOLBAR_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.SAVE_TOOLBAR_AS_NAME))
			add(_toolBar);
	}
	
	/**
	 * Sets the ACIDE - A Configurable IDE configuration menu menu item listeners.
	 */
	public void setListeners(){
		
		// LEXICON
		_lexicon.setListeners();
		
		// GRAMMAR
		_grammar.setListeners();
		
		// OUTPUT
		_console.setListeners();
		
		// MENU
		_menu.setListeners();
		
		// MENU
		_language.setListeners();
		
		// TOOL BAR
		_toolBar.setListeners();
		
		// COMPILER
		_compiler.addActionListener(new CompilerMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu grammar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu grammar menu item.
	 */
	public GrammarMenu getGrammar() {
		return _grammar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE configuration menu grammar menu item.
	 * 
	 * @param grammarnew value to set.
	 */
	public void setGrammar(GrammarMenu grammar) {
		_grammar = grammar;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu menu menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu menu menu item.
	 */
	public MenuMenu getMenu() {
		return _menu;
	}
	
	/**
	 * Sets a new value to the ACIDE - A Configurable IDE configuration menu menu menu item.
	 * 
	 * @param menu new value to set.
	 */
	public void setMenu(MenuMenu menu) {
		_menu = menu;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu lexicon menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu lexicon menu item.
	 */
	public LexiconMenu getLexicon() {
		return _lexicon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE configuration menu lexicon menu item.
	 * 
	 * @param lexicon new value to set.
	 */
	public void setLexicon(LexiconMenu lexicon) {
		_lexicon = lexicon;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu console menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu console menu item.
	 */
	public ConsoleMenu getConsole() {
		return _console;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE configuration menu console menu item.
	 * 
	 * @param console new value to set.
	 */
	public void setConsole(ConsoleMenu console) {
		_console = console;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu tool bar menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu tool bar menu item.
	 */
	public ToolBarMenu getToolBar() {
		return _toolBar;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE configuration menu tool bar menu item.
	 * 
	 * @param toolBar new value to set.
	 */
	public void setToolBar(ToolBarMenu toolBar) {
		_toolBar = toolBar;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu language menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu language menu item.
	 */
	public LanguageMenu getLanguage() {
		return _language;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE configuration menu language menu item.
	 * 
	 * @param language new value to set.
	 */
	public void setLanguage(LanguageMenu language) {
		_language = language;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE configuration menu compiler menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE configuration menu compiler menu item.
	 */
	public JMenuItem getCompiler() {
		return _compiler;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE configuration menu compiler menu item.
	 * 
	 * @param compiler new value to set.
	 */
	public void setCompiler(JMenuItem compiler) {
		_compiler = compiler;
	}
}