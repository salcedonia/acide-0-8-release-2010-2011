package gui.menu.configuration;

import es.configuration.menu.MenuConfiguration;
import gui.menu.configuration.grammar.GrammarMenu;
import gui.menu.configuration.language.LanguageMenu;
import gui.menu.configuration.lexicon.LexiconMenu;
import gui.menu.configuration.listeners.CompilerListener;
import gui.menu.configuration.menu.MenuMenu;
import gui.menu.configuration.output.OutputMenu;
import gui.menu.configuration.toolBar.ToolBarMenu;

import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Configuration menu of ACIDE - A Configurable IDE											
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
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Menu menu item
	 */
	private MenuMenu _menu;
	/**
	 * Language menu item
	 */
	private LanguageMenu _language;
	/**
	 * Output menu item
	 */
	private OutputMenu _output;
	/**
	 * Tool bar menu item
	 */
	private ToolBarMenu _toolBar;
	/**
	 * Lexicon menu item
	 */
	private LexiconMenu _lexicon;
	/**
	 * Grammar menu item
	 */
	private GrammarMenu _grammar;
	/**
	 * Compiler menu item
	 */
	private JMenuItem _compiler;
	
	/**
	 * Class constructor
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
	 * Sets the labels to display in the selected language
	 */
	public void setLanguageLabels() {
		
		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
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
	 * Builds the configuration menu
	 */
	public void buildMenu(){
	
		removeAll();
				
		_lexicon.buildMenu();
		if (MenuConfiguration.getLoadParameters() || MenuConfiguration.getLexicon()
				|| MenuConfiguration.getNewLexical()
				|| MenuConfiguration.getSaveLexical()
				|| MenuConfiguration.getSaveAslexical())
			add(_lexicon);
		
		_grammar.buildMenu();
		if (MenuConfiguration.getNewGrammar()
				|| MenuConfiguration.getLoadGrammar()
				|| MenuConfiguration.getModifyGrammar()
				|| MenuConfiguration.getSaveGrammar()
				|| MenuConfiguration.getSaveAsGrammar()) {
			add(_grammar);
		}
		
		if (MenuConfiguration.getCompiler())
			add(_compiler);
		
		if ((MenuConfiguration.getLoadParameters()
				|| MenuConfiguration.getLexicon()
				|| MenuConfiguration.getNewGrammar()
				|| MenuConfiguration.getLoadGrammar()
				|| MenuConfiguration.getModifyGrammar()
				|| MenuConfiguration.getCompiler()
				|| MenuConfiguration.getNewLexical()
				|| MenuConfiguration.getSaveLexical() || MenuConfiguration
				.getSaveGrammar())
				&& (MenuConfiguration.getConfigure() || MenuConfiguration
						.getExternalCommand()))
			addSeparator();
		
		_output.buildMenu();
		if (MenuConfiguration.getConfigure()
				|| MenuConfiguration.getExternalCommand())
			add(_output);
		
		if ((MenuConfiguration.getLoadParameters()
				|| MenuConfiguration.getLexicon()
				|| MenuConfiguration.getNewGrammar()
				|| MenuConfiguration.getLoadGrammar()
				|| MenuConfiguration.getModifyGrammar()
				|| MenuConfiguration.getCompiler()
				|| MenuConfiguration.getConfigure()
				|| MenuConfiguration.getExternalCommand()
				|| MenuConfiguration.getNewLexical()
				|| MenuConfiguration.getSaveLexical() || MenuConfiguration
				.getSaveGrammar())
				&& (MenuConfiguration.getSpanish()
						|| MenuConfiguration.getEnglish()
						|| MenuConfiguration.getMenu() || MenuConfiguration
						.getToolBar()))
			addSeparator();
		
		_language.buildMenu();
		if (MenuConfiguration.getSpanish() || MenuConfiguration.getEnglish())
			add(_language);
		
		_menu.buildMenu();
		if (MenuConfiguration.getNewMenu() || MenuConfiguration.getLoadMenu()
				|| MenuConfiguration.getModifyMenu()
				|| MenuConfiguration.getSaveMenu()
				|| MenuConfiguration.getSaveAsMenu())
			add(_menu);
		
		_toolBar.buildMenu();	
		if (MenuConfiguration.getNewToolBar() || MenuConfiguration.getLoadToolBar()
				|| MenuConfiguration.getModifyToolBar()
				|| MenuConfiguration.getSaveToolBar()
				|| MenuConfiguration.getSaveAsToolBar())
			add(_toolBar);
	}
	
	/**
	 * Sets the menu item listeners
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
		_compiler.addActionListener(new CompilerListener());
	}

	/**
	 * Returns the grammar menu item
	 * 
	 * @return the grammar menu item
	 */
	public GrammarMenu getGrammar() {
		return _grammar;
	}

	/**
	 * Sets a new value to the grammar menu item
	 * 
	 * @param grammarnew value to set
	 */
	public void setGrammar(GrammarMenu grammar) {
		_grammar = grammar;
	}
	
	/**
	 * Returns the menu menu item
	 * 
	 * @return the menu menu item
	 */
	public MenuMenu getMenu() {
		return _menu;
	}
	
	/**
	 * Sets a new value to the menu menu item
	 * 
	 * @param menu new value to set
	 */
	public void setMenu(MenuMenu menu) {
		_menu = menu;
	}
	
	/**
	 * Returns the lexicon menu item
	 * 
	 * @return the lexicon menu item
	 */
	public LexiconMenu getLexicon() {
		return _lexicon;
	}

	/**
	 * Sets a new value to the lexicon menu item
	 * 
	 * @param lexicon new value to set
	 */
	public void setLexicon(LexiconMenu lexicon) {
		_lexicon = lexicon;
	}
	
	/**
	 * Returns the output menu item
	 * 
	 * @return the output menu item
	 */
	public OutputMenu getOutput() {
		return _output;
	}

	/**
	 * Sets a new value to the output menu item
	 * 
	 * @param output new value to set
	 */
	public void setOutput(OutputMenu output) {
		_output = output;
	}
	
	/**
	 * Returns the tool bar menu item
	 * 
	 * @return the tool bar menu item
	 */
	public ToolBarMenu getToolBar() {
		return _toolBar;
	}

	/**
	 * Sets a new value to the tool bar menu item
	 * 
	 * @param toolBar new value to set
	 */
	public void setToolBar(ToolBarMenu toolBar) {
		_toolBar = toolBar;
	}

	/**
	 * Returns the language menu item
	 * 
	 * @return the language menu item
	 */
	public LanguageMenu getLanguage() {
		return _language;
	}

	/**
	 * Sets a new value to the language menu item
	 * 
	 * @param language new value to set
	 */
	public void setLanguage(LanguageMenu language) {
		_language = language;
	}
	
	/**
	 * Returns the compiler menu item
	 * 
	 * @return the compiler menu item
	 */
	public JMenuItem getCompiler() {
		return _compiler;
	}

	/**
	 * Sets a new value to the compiler menu item
	 * 
	 * @param compiler new value to set
	 */
	public void setCompiler(JMenuItem compiler) {
		_compiler = compiler;
	}
}