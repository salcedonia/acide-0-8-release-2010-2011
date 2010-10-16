package gui.menu.configuration;

import gui.menu.configuration.grammar.GrammarMenu;
import gui.menu.configuration.language.LanguageMenu;
import gui.menu.configuration.lexical.LexiconMenu;
import gui.menu.configuration.menu.MenuMenu;
import gui.menu.configuration.output.OutputMenu;
import gui.menu.configuration.toolBar.ToolBarMenu;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import operations.configuration.MenuConfiguration;
import operations.factory.GUIFactory;

import language.Language;
import properties.PropertiesManager;

/**
 * 
 */
public class ConfigurationMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private MenuMenu _menu;
	/**
	 * 
	 */
	private LanguageMenu _language;
	/**
	 * 
	 */
	private OutputMenu _output;
	/**
	 * 
	 */
	private ToolBarMenu _toolBar;
	/**
	 * 
	 */
	private LexiconMenu _lexicon;
	/**
	 * 
	 */
	private GrammarMenu _grammar;
	/**
	 * 
	 */
	private JMenuItem _compiler;
	
	/**
	 * Constructor of the class.
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
	 * 
	 * 
	 */
	public void setLanguageLabels() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
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
	 * 
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
	 * 
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
	 * 
	 * @return
	 */
	public GrammarMenu getGrammar() {
		return _grammar;
	}

	/**
	 * 
	 * @param grammar
	 */
	public void setGrammar(GrammarMenu grammar) {
		_grammar = grammar;
	}
	
	/**
	 * 
	 * @return
	 */
	public MenuMenu getMenu() {
		return _menu;
	}
	
	/**
	 * 
	 * @param menu
	 */
	public void setMenu(MenuMenu menu) {
		_menu = menu;
	}
	
	/**
	 * 
	 * @return
	 */
	public LexiconMenu getLexicon() {
		return _lexicon;
	}

	/**
	 * 
	 * @param lexicon
	 */
	public void setLexicon(LexiconMenu lexicon) {
		_lexicon = lexicon;
	}
	
	/**
	 * 
	 * @return
	 */
	public OutputMenu getOutput() {
		return _output;
	}

	/**
	 * 
	 * @param output
	 */
	public void setOutput(OutputMenu output) {
		_output = output;
	}
	
	/**
	 * 
	 * @return
	 */
	public ToolBarMenu getToolBar() {
		return _toolBar;
	}

	/**
	 * 
	 * @param toolBar
	 */
	public void setToolBar(ToolBarMenu toolBar) {
		_toolBar = toolBar;
	}

	/**
	 * 
	 * @return
	 */
	public LanguageMenu getLanguage() {
		return _language;
	}

	/**
	 * 
	 * @param language
	 */
	public void setLanguage(LanguageMenu language) {
		_language = language;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getCompiler() {
		return _compiler;
	}

	/**
	 * 
	 * @param compiler
	 */
	public void setCompiler(JMenuItem compiler) {
		_compiler = compiler;
	}
}

/**
 * 
 */
class CompilerListener implements ActionListener{
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GUIFactory.getInstance().buildCompilerGUI();
	}
}