package operations.factory;

import language.Language;
import gui.MainWindow;
import gui.editor.EditorBuilder;
import gui.explorer.Explorer;
import gui.menu.Menu;
import gui.menu.configuration.grammar.GrammarGUI;
import gui.menu.configuration.lexicon.LexiconGUI;
import gui.menu.configuration.menu.MenuGUI;
import gui.menu.configuration.output.ExternalCommandGUI;
import gui.menu.configuration.output.OutputGUI;
import gui.menu.configuration.toolBar.ToolBarCommandGUI;
import gui.menu.edit.ReplaceGUI;
import gui.menu.edit.SearchGUI;
import gui.menu.help.AboutUs;
import gui.menu.print.PrintGUI;
import gui.menu.project.CompilerGUI;
import gui.menu.project.ExecutionGUI;
import gui.menu.project.NewLexiconGUI;
import gui.menu.project.ProjectGUI;
import gui.menu.view.LogTab;
import gui.output.Output;
import gui.statusBar.StatusBar;
import gui.toolBarButton.ToolBarCommand;

/**
 * Build the most relevant GUI objects of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class GUIFactory {

	/**
	 * Instance of the class.
	 */
	private static GUIFactory _instance;

	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class..
	 */
	public static GUIFactory getInstance() {
		if (_instance == null)
			_instance = new GUIFactory();
		return _instance;
	}

	/**
	 * Build the main window of the application.
	 * 
	 * @return The main window of the application.
	 */
	public MainWindow buildMainWindow() {
		return new MainWindow();
	}

	/**
	 * Build the menu of the application.
	 * 
	 * @return The menu of the application.
	 */
	public Menu buildMenu() {
		return new Menu();
	}

	/**
	 * Build the about us window of the application.
	 * 
	 * @return The about us window of the application.
	 */
	public AboutUs buildAboutUs() {
		return new AboutUs();
	}

	/**
	 * Build the replaceGUI of the application.
	 * 
	 * @return The replaceGUI of the application.
	 */
	public ReplaceGUI buildReplaceGUI() {
		return new ReplaceGUI();
	}

	/**
	 * Build the searchGUI of the application.
	 * 
	 * @return The searchGUI of the application.
	 */
	public SearchGUI buildSearchGUI() {
		return new SearchGUI();
	}

	/**
	 * Build the toolBar of the application.
	 * 
	 * @return The toolBar of the application.
	 */
	public ToolBarCommand buildToolBarCommand() {
		return new ToolBarCommand();
	}

	/**
	 * Build the output of the application.
	 * 
	 * @return The output of the application.
	 */ 
	public Output buildOutput() {
		return new Output(true);
	}

	/**
	 * Build the explorer of the application.
	 * 
	 * @return The explorer of the application.
	 */
	public Explorer buildExplorer() {
		return new Explorer();
	}

	/**
	 * Build the menuGUI of the application.
	 * 
	 * @param modified Flag that indicates if is modified or not.
	 * 
	 * @return The menuGUI of the application.
	 */
	public MenuGUI buildMenuGUI(boolean modified) {
		return new MenuGUI(modified);
	}

	/**
	 * Build the grammarGUI of the application.
	 * 
	 * @param modified Flag that indicates if is modified or not.
	 * 
	 * @return The grammarGUI of the application.
	 */
	public GrammarGUI buildGrammarGUI(boolean modified) {
		return new GrammarGUI(modified);
	}

	/**
	 * Build the lexiconGUI of the application.
	 * 
	 * @return The lexiconGUI of the application.
	 */
	public LexiconGUI buildLexiconGUI() {
		return new LexiconGUI();
	}

	/**
	 * Build the editorBuilder of the application.
	 * 
	 * @return The editorBuilder of the application.
	 */
	public EditorBuilder buildEditorBuilder() {
		return new EditorBuilder();
	}

	/**
	 * Build the logTab of the application.
	 * 
	 * @return The logTab of the application.
	 */
	public LogTab buildLogTab() {
		return new LogTab();
	}

	/**
	 * Build the language of the application.
	 * 
	 * @return The language of the application.
	 */
	public Language buildLanguage() {
		return new Language();
	}

	/**
	 * Build the languageGUI of the application.
	 * 
	 * @return The languageGUI of the application.
	 */
	public NewLexiconGUI buildNewLexiconGUI() {
		return new NewLexiconGUI();
	}

	/**
	 * Build the projectGUI of the application.
	 * 
	 * @return The projectGUI of the application.
	 */
	public ProjectGUI buildProjectGUI() {
		return new ProjectGUI();
	}

	/**
	 * Build the OutputGUI of the application.
	 * 
	 * @return The OutputGUI of the application.
	 */
	public OutputGUI generaOutputGUI() {
		return new OutputGUI();
	}

	/**
	 * Build the ExternalCommandGUI of the application.
	 * 
	 * @return The ExternalCommandGUI of the application.
	 */
	public ExternalCommandGUI buildExternalCommandGUI() {
		return new ExternalCommandGUI();
	}

	/**
	 * 
	 * @param b 
	 * @return
	 */
	public ToolBarCommandGUI buildToolBarCommandGUI(boolean b) {
		return new ToolBarCommandGUI(b);
	}

	/**
	 * Build the StatusBar of the application.
	 * 
	 * @return The StatusBar of the application.
	 */
	public StatusBar buildStatusBar() {
		return new StatusBar();
	}

	/**
	 * Build the PrintGUI of the application.
	 * 
	 * @return The PrintGUI of the application.
	 */
	public PrintGUI buildPrintGUI() {
		return new PrintGUI();
	}

	/**
	 * Build the ExecutionGUI of the application.
	 * 
	 * @return The ExecutionGUI of the application.
	 */
	public ExecutionGUI buildExecutionGUI() {
		return new ExecutionGUI();
	}

	/**
	 * Build the CompilerGUI of the application.
	 * 
	 * @return The CompilerGUI of the application.
	 */
	public CompilerGUI buildCompilerGUI() {
		return new CompilerGUI();
	}

	/**
	 * Build the help of the application.
	 * 
	 * @return The help of the application.
	 */
	public void buildHelp() {
		// TODO Implement the help
	}
}
