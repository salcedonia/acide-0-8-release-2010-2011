package gui.menuBar.configurationMenu;

import es.configuration.menu.MenuConfiguration;
import gui.menuBar.configurationMenu.grammarMenu.GrammarMenu;
import gui.menuBar.configurationMenu.languageMenu.LanguageMenu;
import gui.menuBar.configurationMenu.lexiconMenu.LexiconMenu;
import gui.menuBar.configurationMenu.listeners.CompilerMenuItemListener;
import gui.menuBar.configurationMenu.menuMenu.MenuMenu;
import gui.menuBar.configurationMenu.outputMenu.OutputMenu;
import gui.menuBar.configurationMenu.toolBarMenu.ToolBarMenu;

import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Configuration menu of ACIDE - A Configurable IDE.											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see ActionListener																													
 ***********************************************************************/
public class ConfigurationMenu extends JMenu {

	/**
	 * Configuration menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Lexicon menu item name.
	 */
	public static final String LEXICON_NAME = "Lexicon";
	/**
	 * Grammar menu item name.
	 */
	public static final String GRAMMAR_NAME = "Grammar";
	/**
	 * Language menu item name.
	 */
	public static final String LANGUAGE_NAME = "Language";
	/**
	 * Menu menu item name.
	 */
	public static final String MENU_NAME = "Menu";
	/**
	 * Output menu item name.
	 */
	public static final String OUTPUT_NAME = "Output";
	/**
	 * Toolbar menu item name.
	 */
	public static final String TOOLBAR_NAME = "Toolbar";
	/**
	 * Compiler menu item name.
	 */
	public static final String COMPILER_NAME = "Compiler";
	/**
	 * Menu menu item.
	 */
	private MenuMenu _menu;
	/**
	 * Language menu item.
	 */
	private LanguageMenu _language;
	/**
	 * Output menu item.
	 */
	private OutputMenu _output;
	/**
	 * Tool bar menu item.
	 */
	private ToolBarMenu _toolBar;
	/**
	 * Lexicon menu item.
	 */
	private LexiconMenu _lexicon;
	/**
	 * Grammar menu item.
	 */
	private GrammarMenu _grammar;
	/**
	 * Compiler menu item.
	 */
	private JMenuItem _compiler;
	
	/**
	 * Creates a new configuration menu.
	 */
	public ConfigurationMenu(){
				
		// MENU
		_output = new OutputMenu();
		_language = new LanguageMenu();
		_menu = new MenuMenu();
		_toolBar = new ToolBarMenu();
		_lexicon = new LexiconMenu();
		_grammar = new GrammarMenu();
		_compiler = new JMenuItem();
		
		setLanguageLabels();
	}
	
	/**
	 * Sets the labels to display in the selected language.
	 */
	public void setLanguageLabels() {
		
		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
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
		
		// OUTPUT
		_output.setText(labels.getString("s332"));
		_output.setLanguageLabels();
		
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
	 * Builds the configuration menu.
	 */
	public void buildMenu(){
	
		removeAll();
				
		// LEXICON MENU
		_lexicon.buildMenu();
		if (MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.LOAD_LEXICON_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.MODIFY_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.NEW_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_AS_NAME))

			add(_lexicon);
		
		// GRAMMAR MENU
		_grammar.buildMenu();
		if (MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.NEW_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.LOAD_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.MODIFY_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_AS_NAME))
			add(_grammar);
		
		// COMPILER MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(COMPILER_NAME))
			add(_compiler);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.LOAD_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.MODIFY_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.NEW_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.LOAD_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.MODIFY_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(COMPILER_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.NEW_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_NAME))
				&& (MenuConfiguration.getInstance().getIsDisplayed(OutputMenu.CONFIGURE_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(OutputMenu.EXTERNAL_COMMAND_NAME)))
			addSeparator();
		
		// OUTPUT MENU
		_output.buildMenu();
		if (MenuConfiguration.getInstance().getIsDisplayed(OutputMenu.CONFIGURE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(OutputMenu.EXTERNAL_COMMAND_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(OutputMenu.SHELL_DISPLAY_OPTIONS_NAME))
			add(_output);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.LOAD_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.MODIFY_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.NEW_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.LOAD_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.MODIFY_GRAMMAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(COMPILER_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(OutputMenu.CONFIGURE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(OutputMenu.EXTERNAL_COMMAND_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(OutputMenu.SHELL_DISPLAY_OPTIONS_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.NEW_LEXICON_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_NAME))
				&& (MenuConfiguration.getInstance().getIsDisplayed(LanguageMenu.SPANISH_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(LanguageMenu.ENGLISH_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(MENU_NAME) 
						|| MenuConfiguration.getInstance().getIsDisplayed(TOOLBAR_NAME)))
			addSeparator();
		
		// LANGUAGE MENU
		_language.buildMenu();
		if (MenuConfiguration.getInstance().getIsDisplayed(LanguageMenu.SPANISH_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(LanguageMenu.ENGLISH_NAME))
			add(_language);
		
		// MENU MENU
		_menu.buildMenu();
		if (MenuConfiguration.getInstance().getIsDisplayed(MenuMenu.NEW_MENU_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(MenuMenu.LOAD_MENU_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(MenuMenu.MODIFY_MENU_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(MenuMenu.SAVE_MENU_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(MenuMenu.SAVE_MENU_AS_NAME))
			add(_menu);
		
		// TOOL BAR MENU
		_toolBar.buildMenu();	
		if (MenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.NEW_TOOLBAR_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.LOAD_TOOLBAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.MODIFY_TOOLBAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.SAVE_TOOLBAR_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(ToolBarMenu.SAVE_TOOLBAR_AS_NAME))
			add(_toolBar);
	}
	
	/**
	 * Sets the configuration menu item listeners.
	 */
	public void setListeners(){
		
		// LEXICON
		_lexicon.setListeners();
		
		// GRAMMAR
		_grammar.setListeners();
		
		// OUTPUT
		_output.setListeners();
		
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
	 * Returns the grammar menu item.
	 * 
	 * @return the grammar menu item.
	 */
	public GrammarMenu getGrammar() {
		return _grammar;
	}

	/**
	 * Sets a new value to the grammar menu item.
	 * 
	 * @param grammarnew value to set.
	 */
	public void setGrammar(GrammarMenu grammar) {
		_grammar = grammar;
	}
	
	/**
	 * Returns the menu menu item.
	 * 
	 * @return the menu menu item.
	 */
	public MenuMenu getMenu() {
		return _menu;
	}
	
	/**
	 * Sets a new value to the menu menu item.
	 * 
	 * @param menu new value to set.
	 */
	public void setMenu(MenuMenu menu) {
		_menu = menu;
	}
	
	/**
	 * Returns the lexicon menu item.
	 * 
	 * @return the lexicon menu item.
	 */
	public LexiconMenu getLexicon() {
		return _lexicon;
	}

	/**
	 * Sets a new value to the lexicon menu item.
	 * 
	 * @param lexicon new value to set.
	 */
	public void setLexicon(LexiconMenu lexicon) {
		_lexicon = lexicon;
	}
	
	/**
	 * Returns the output menu item.
	 * 
	 * @return the output menu item.
	 */
	public OutputMenu getOutput() {
		return _output;
	}

	/**
	 * Sets a new value to the output menu item.
	 * 
	 * @param output new value to set.
	 */
	public void setOutput(OutputMenu output) {
		_output = output;
	}
	
	/**
	 * Returns the tool bar menu item.
	 * 
	 * @return the tool bar menu item.
	 */
	public ToolBarMenu getToolBar() {
		return _toolBar;
	}

	/**
	 * Sets a new value to the tool bar menu item.
	 * 
	 * @param toolBar new value to set.
	 */
	public void setToolBar(ToolBarMenu toolBar) {
		_toolBar = toolBar;
	}

	/**
	 * Returns the language menu item.
	 * 
	 * @return the language menu item.
	 */
	public LanguageMenu getLanguage() {
		return _language;
	}

	/**
	 * Sets a new value to the language menu item.
	 * 
	 * @param language new value to set.
	 */
	public void setLanguage(LanguageMenu language) {
		_language = language;
	}
	
	/**
	 * Returns the compiler menu item.
	 * 
	 * @return the compiler menu item.
	 */
	public JMenuItem getCompiler() {
		return _compiler;
	}

	/**
	 * Sets a new value to the compiler menu item.
	 * 
	 * @param compiler new value to set.
	 */
	public void setCompiler(JMenuItem compiler) {
		_compiler = compiler;
	}
}