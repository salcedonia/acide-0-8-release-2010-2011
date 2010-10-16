package operations.factory;

import language.Language;
import gui.MainWindow;
import gui.editor.EditorBuilder;
import gui.explorer.Explorer;
import gui.menu.Menu;
import gui.menu.configuration.grammar.GrammarGUI;
import gui.menu.configuration.lexical.LexiconGUI;
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
 * 
 */
public class GUIFactory {

	/**
	 * 
	 */
	private static GUIFactory _instance;

	/**
	 * 
	 * @return
	 */
	public static GUIFactory getInstance() {
		if (_instance == null)
			_instance = new GUIFactory();
		return _instance;
	}

	/**
	 * 
	 * @return
	 */
	public MainWindow generaMainWindow() {
		return new MainWindow();
	}

	/**
	 * 
	 * @return
	 */
	public Menu buildMenu() {
		return new Menu();
	}

	/**
	 * 
	 * @return
	 */
	public AboutUs buildAboutUs() {
		return new AboutUs();
	}

	/**
	 * 
	 * @return
	 */
	public ReplaceGUI buildReplaceGUI() {
		return new ReplaceGUI();
	}

	/**
	 * 
	 * @return
	 */
	public SearchGUI buildSearchGUI() {
		return new SearchGUI();
	}

	/**
	 * 
	 * @return
	 */
	public ToolBarCommand buildToolBarCommand() {
		return new ToolBarCommand();
	}

	/**
	 * 
	 * @return
	 */
	public Output buildOutput() {
		return new Output(true);
	}

	/**
	 * 
	 * @return
	 */
	public Explorer generaExplorer() {
		return new Explorer();
	}

	/**
	 * 
	 * @param b 
	 * @return
	 */
	public MenuGUI buildMenuGUI(boolean b) {
		return new MenuGUI(b);
	}

	/**
	 * 
	 * @param b 
	 * @return
	 */
	public GrammarGUI buildGrammarGUI(boolean b) {
		return new GrammarGUI(b);
	}

	/**
	 * 
	 * @return
	 */
	public LexiconGUI buildLexiconGUI() {
		return new LexiconGUI();
	}

	/**
	 * 
	 * @return
	 */
	public EditorBuilder buildEditorBuilder() {
		return new EditorBuilder();
	}

	/**
	 * 
	 * @return
	 */
	public LogTab buildLogTab() {
		return new LogTab();
	}

	/**
	 * 
	 * @return
	 */
	public Language buildLanguage() {
		return new Language();
	}

	/**
	 * 
	 * @return
	 */
	public NewLexiconGUI buildLanguageGUI() {
		return new NewLexiconGUI();
	}

	/**
	 * 
	 * @return
	 */
	public ProjectGUI buildProjectGUI() {
		return new ProjectGUI();
	}

	/**
	 * 
	 * @return
	 */
	public OutputGUI generaOutputGUI() {
		return new OutputGUI();
	}

	/**
	 * 
	 * @return
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
	 * 
	 * @return
	 */
	public StatusBar buildStatusBar() {
		return new StatusBar();
	}

	/**
	 * 
	 * @return
	 */
	public PrintGUI buildPrintGUI() {
		return new PrintGUI();
	}

	/**
	 * 
	 * @return
	 */
	public ExecutionGUI buildExecutionGUI() {
		return new ExecutionGUI();
	}

	/**
	 * 
	 * @return
	 */
	public CompilerGUI buildCompilerGUI() {
		return new CompilerGUI();
	}

	/**
	 * 
	 */
	public void buildHelp() {
		// TODO Implement the help
	}
}
