package operations.factory;

import language.Language;
import gui.editor.editorManager.EditorManager;
import gui.explorer.ExplorerPanel;
import gui.mainWindow.MainWindow;
import gui.menu.Menu;
import gui.menu.configuration.grammar.gui.GrammarConfigurationWindow;
import gui.menu.configuration.lexicon.gui.LexiconConfigurationWindow;
import gui.menu.configuration.menu.gui.MenuConfigurationWindow;
import gui.menu.configuration.output.gui.ExternalCommandConfigurationWindow;
import gui.menu.configuration.output.gui.OutputConfigurationWindow;
import gui.menu.configuration.toolBar.gui.ToolBarConfigurationWindow;
import gui.menu.edit.gui.replace.ReplaceWindow;
import gui.menu.edit.gui.search.SearchWindow;
import gui.menu.file.gui.PrintConfigurationWindow;
import gui.menu.help.gui.AboutUsWindow;
import gui.menu.project.gui.CompilerConfigurationWindow;
import gui.menu.project.gui.ExecutionConfigurationWindow;
import gui.menu.project.gui.NewLexiconConfigurationWindow;
import gui.menu.project.gui.NewProjectConfigurationWindow;
import gui.menu.view.utils.LogTab;
import gui.output.OutputPanel;
import gui.output.utils.OutputVisualizationOptionsWindow;
import gui.statusBar.StatusBar;
import gui.toolBar.ToolBar;

/************************************************************************																
 * Builds the GUI of ACIDE - A Configurable IDE											
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
 ***********************************************************************/
public class GUIFactory {

	/**
	 * Class instance
	 */
	private static GUIFactory _instance;

	/**
	 * Returns the unique instance of the class
	 * 
	 * @return the unique instance of the class
	 */
	public static GUIFactory getInstance() {
		if (_instance == null)
			_instance = new GUIFactory();
		return _instance;
	}

	/**
	 * Builds the main window of ACIDE - A Configurable IDE
	 * 
	 * @return the main window of ACIDE - A Configurable IDE
	 * @see MainWindow
	 */
	public MainWindow buildMainWindow() {
		return new MainWindow();
	}

	/**
	 * Builds the menu of ACIDE - A Configurable IDE
	 * 
	 * @return the menu of ACIDE - A Configurable IDE
	 * @see Menu
	 */
	public Menu buildMenu() {
		return new Menu();
	}

	/**
	 * Builds the about us window of ACIDE - A Configurable IDE
	 * 
	 * @return the about us window of ACIDE - A Configurable IDE
	 * @see AboutUsWindow
	 */
	public AboutUsWindow buildAboutUsWindow() {
		return new AboutUsWindow();
	}

	/**
	 * Builds the replace window of ACIDE - A Configurable IDE
	 * 
	 * @return the replace window of ACIDE - A Configurable IDE
	 * @see ReplaceWindow
	 */
	public ReplaceWindow buildReplaceWindow() {
		return new ReplaceWindow();
	}

	/**
	 * Builds the search window of ACIDE - A Configurable IDE
	 * 
	 * @return the search window of ACIDE - A Configurable IDE
	 * @see SearchWindow
	 */
	public SearchWindow buildSearchWindow() {
		return new SearchWindow();
	}

	/**
	 * Builds the tool bar of ACIDE - A Configurable IDE
	 * 
	 * @return the tool bar of ACIDE - A Configurable IDE
	 * @see ToolBar
	 */
	public ToolBar buildToolBar() {
		return new ToolBar();
	}

	/**
	 * Builds the output of ACIDE - A Configurable IDE initially modifiable by
	 * default.
	 * 
	 * @return the output of ACIDE - A Configurable IDE
	 * @see OutputPanel
	 */
	public OutputPanel buildOutput() {
		return new OutputPanel(true);
	}

	/**
	 * Builds the explorer of ACIDE - A Configurable IDE
	 * 
	 * @return the explorer of ACIDE - A Configurable IDE
	 * @see ExplorerPanel
	 */
	public ExplorerPanel buildExplorer() {
		return new ExplorerPanel();
	}

	/**
	 * Build the menu configuration window of ACIDE - A Configurable IDE
	 * 
	 * @param forModify
	 *            indicates if the menu configuration window has to be used to
	 *            modify the menu configuration or not
	 * @return the menu configuration window of ACIDE - A Configurable IDE
	 * @see MenuConfigurationWindow
	 */
	public MenuConfigurationWindow buildMenuGUI(boolean forModify) {
		return new MenuConfigurationWindow(forModify);
	}

	/**
	 * Builds the grammar configuration window of ACIDE - A Configurable IDE
	 * 
	 * @param forModify
	 *            indicates if the grammar configuration window has to be used
	 *            to modify the grammar configuration or not
	 * @return The grammar configuration window of ACIDE - A Configurable IDE
	 * @see GrammarConfigurationWindow
	 */
	public GrammarConfigurationWindow buildGrammarGUI(boolean forModify) {
		return new GrammarConfigurationWindow(forModify);
	}

	/**
	 * Builds the lexicon configuration window of ACIDE - A Configurable IDE
	 * 
	 * @return the lexicon configuration window of ACIDE - A Configurable IDE
	 * @see LexiconConfigurationWindow
	 */
	public LexiconConfigurationWindow buildLexiconConfigurationWindow() {
		return new LexiconConfigurationWindow();
	}

	/**
	 * Builds the editor manager of ACIDE - A Configurable IDE
	 * 
	 * @return the editor manager of ACIDE - A Configurable IDE
	 */
	public EditorManager buildEditorManager() {
		return new EditorManager();
	}

	/**
	 * Builds the log tab to be displayed on the editor of ACIDE - A
	 * Configurable IDE
	 * 
	 * @return the log tab to be displayed on the editor of ACIDE - A
	 *         Configurable IDE
	 * @see LogTab
	 */
	public LogTab buildLogTab() {
		return new LogTab();
	}

	/**
	 * Builds the language of ACIDE - A Configurable IDE
	 * 
	 * @return the language of ACIDE - A Configurable IDE
	 * @see Language
	 */
	public Language buildLanguage() {
		return new Language();
	}

	/**
	 * Build the new lexicon configuration window of ACIDE - A Configurable IDE
	 * 
	 * @return the new lexicon configuration window of ACIDE - A Configurable
	 *         IDE
	 * @see NewLexiconConfigurationWindow
	 */
	public NewLexiconConfigurationWindow buildNewLexiconConfigurationWindow() {
		return new NewLexiconConfigurationWindow();
	}

	/**
	 * Builds the new project configuration window of ACIDE - A Configurable IDE
	 * 
	 * @return the new project configuration window of ACIDE - A Configurable
	 *         IDE
	 * @see NewProjectConfigurationWindow
	 */
	public NewProjectConfigurationWindow buildNewProjectConfigurationWindow() {
		return new NewProjectConfigurationWindow();
	}

	/**
	 * Builds the output configuration window of ACIDE - A Configurable IDE
	 * 
	 * @return the output configuration window of ACIDE - A Configurable IDE
	 * @see OutputConfigurationWindow
	 */
	public OutputConfigurationWindow buildOutputConfigurationWindow() {
		return new OutputConfigurationWindow();
	}

	/**
	 * Builds the external command configuration window of ACIDE - A
	 * Configurable IDE
	 * 
	 * @return the ExternalCommand configuration window of ACIDE - A
	 *         Configurable IDE
	 * @see ExternalCommandConfigurationWindow
	 */
	public ExternalCommandConfigurationWindow buildExternalCommandConfigurationWindow() {
		return new ExternalCommandConfigurationWindow();
	}

	/**
	 * Builds the tool bar configuration window of ACIDE - A Configurable IDE
	 * 
	 * @param forModify
	 *            indicates if the tool bar configuration window has to be used
	 *            to modify the tool bar configuration or not
	 * @return the tool bar command configuration window of ACIDE - A
	 *         Configurable IDE
	 * @see ToolBarConfigurationWindow
	 */
	public ToolBarConfigurationWindow buildToolBarConfigurationWindow(
			boolean forModify) {
		return new ToolBarConfigurationWindow(forModify);
	}

	/**
	 * Builds the status bar of ACIDE - A Configurable IDE
	 * 
	 * @return The status bar of ACIDE - A Configurable IDE
	 * @see StatusBar
	 */
	public StatusBar buildStatusBar() {
		return new StatusBar();
	}

	/**
	 * Builds the print configuration window of ACIDE - A Configurable IDE
	 * 
	 * @return the print configuration window of ACIDE - A Configurable IDE
	 * @see PrintConfigurationWindow
	 */
	public PrintConfigurationWindow buildPrintConfigurationWindow() {
		return new PrintConfigurationWindow();
	}

	/**
	 * Builds the execution configuration window of ACIDE - A Configurable IDE
	 * 
	 * @return the execution configuration window of ACIDE - A Configurable IDE
	 * @see ExecutionConfigurationWindow
	 */
	public ExecutionConfigurationWindow buildExecutionConfigurationWindow() {
		return new ExecutionConfigurationWindow();
	}

	/**
	 * Builds the compiler configuration window of ACIDE - A Configurable IDE
	 * 
	 * @return the compiler configuration window of ACIDE - A Configurable IDE
	 * @see CompilerConfigurationWindow
	 */
	public CompilerConfigurationWindow buildCompilerConfigurationWindow() {
		return new CompilerConfigurationWindow();
	}

	/**
	 * Builds the output visualization options window of ACIDE - A
	 * Configurable IDE
	 * 
	 * @return the output visualization options window of ACIDE - A
	 *         Configurable IDE
	 * @see OutputVisualizationOptionsWindow
	 */
	public OutputVisualizationOptionsWindow buildOutputVisualizationOptionsWindow() {
		return new OutputVisualizationOptionsWindow();
	}
}
