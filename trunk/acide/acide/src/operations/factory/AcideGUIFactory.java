package operations.factory;

import language.AcideLanguage;
import gui.explorerPanel.AcideExplorerPanel;
import gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import gui.mainWindow.MainWindow;
import gui.menuBar.Menu;
import gui.menuBar.configurationMenu.grammarMenu.gui.GrammarConfigurationWindow;
import gui.menuBar.configurationMenu.lexiconMenu.gui.LexiconConfigurationWindow;
import gui.menuBar.configurationMenu.menuMenu.gui.MenuConfigurationWindow;
import gui.menuBar.configurationMenu.outputMenu.gui.ExternalCommandConfigurationWindow;
import gui.menuBar.configurationMenu.outputMenu.gui.OutputConfigurationWindow;
import gui.menuBar.configurationMenu.toolBarMenu.gui.ToolBarConfigurationWindow;
import gui.menuBar.editMenu.gui.replace.ReplaceWindow;
import gui.menuBar.editMenu.gui.search.SearchWindow;
import gui.menuBar.fileMenu.gui.PrintConfigurationWindow;
import gui.menuBar.helpMenu.gui.AboutUsWindow;
import gui.menuBar.projectMenu.gui.CompilerConfigurationWindow;
import gui.menuBar.projectMenu.gui.ExecutionConfigurationWindow;
import gui.menuBar.projectMenu.gui.NewLexiconConfigurationWindow;
import gui.menuBar.projectMenu.gui.NewProjectConfigurationWindow;
import gui.menuBar.viewMenu.utils.AcideLogTab;
import gui.outputPanel.AcideOutputPanel;
import gui.outputPanel.utils.ShellDisplayOptionsWindow;
import gui.statusBarPanel.AcideStatusBar;
import gui.toolBarPanel.AcideToolBarPanel;

/************************************************************************																
 * ACIDE - A Configurable IDE class which creates the GUI components of the
 * application.											
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
public class AcideGUIFactory {

	/**
	 * ACIDE - A Configurable IDE GUI factory unique class instance.
	 */
	private static AcideGUIFactory _instance;

	/**
	 * Returns the unique ACIDE - A Configurable IDE GUI factory class instance.
	 * 
	 * @return the unique ACIDE - A Configurable IDE GUI factory class instance.
	 */
	public static AcideGUIFactory getInstance() {
		if (_instance == null)
			_instance = new AcideGUIFactory();
		return _instance;
	}

	/**
	 * Builds the main window of ACIDE - A Configurable IDE.
	 * 
	 * @return the main window of ACIDE - A Configurable IDE.
	 * @see MainWindow
	 */
	public MainWindow buildMainWindow() {
		return new MainWindow();
	}

	/**
	 * Builds the menu of ACIDE - A Configurable IDE.
	 * 
	 * @return the menu of ACIDE - A Configurable IDE.
	 * @see Menu
	 */
	public Menu buildMenu() {
		return new Menu();
	}

	/**
	 * Builds the about us window of ACIDE - A Configurable IDE.
	 * 
	 * @return the about us window of ACIDE - A Configurable IDE.
	 * @see AboutUsWindow
	 */
	public AboutUsWindow buildAboutUsWindow() {
		return new AboutUsWindow();
	}

	/**
	 * Builds the replace window of ACIDE - A Configurable IDE.
	 * 
	 * @return the replace window of ACIDE - A Configurable IDE.
	 * @see ReplaceWindow
	 */
	public ReplaceWindow buildReplaceWindow() {
		return new ReplaceWindow();
	}

	/**
	 * Builds the search window of ACIDE - A Configurable IDE.
	 * 
	 * @return the search window of ACIDE - A Configurable IDE.
	 * @see SearchWindow
	 */
	public SearchWindow buildSearchWindow() {
		return new SearchWindow();
	}

	/**
	 * Builds the tool bar of ACIDE - A Configurable IDE.
	 * 
	 * @return the tool bar of ACIDE - A Configurable IDE.
	 * @see AcideToolBarPanel
	 */
	public AcideToolBarPanel buildToolBar() {
		return new AcideToolBarPanel();
	}

	/**
	 * Builds the output of ACIDE - A Configurable IDE initially modifiable by
	 * default.
	 * 
	 * @return the output of ACIDE - A Configurable IDE.
	 * @see AcideOutputPanel
	 */
	public AcideOutputPanel buildAcideOutputPanel() {
		return new AcideOutputPanel(true);
	}

	/**
	 * Builds the explorer of ACIDE - A Configurable IDE.
	 * 
	 * @return the explorer of ACIDE - A Configurable IDE.
	 * @see AcideExplorerPanel
	 */
	public AcideExplorerPanel buildAcideExplorerPanel() {
		return new AcideExplorerPanel();
	}

	/**
	 * Build the menu configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @param forModify
	 *            indicates if the menu configuration window has to be used to
	 *            modify the menu configuration or not.
	 * @return the menu configuration window of ACIDE - A Configurable IDE.
	 * @see MenuConfigurationWindow
	 */
	public MenuConfigurationWindow buildMenuGUI(boolean forModify) {
		return new MenuConfigurationWindow(forModify);
	}

	/**
	 * Builds the grammar configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @param forModify
	 *            indicates if the grammar configuration window has to be used
	 *            to modify the grammar configuration or not.
	 * @return The grammar configuration window of ACIDE - A Configurable IDE.
	 * @see GrammarConfigurationWindow
	 */
	public GrammarConfigurationWindow buildGrammarGUI(boolean forModify) {
		return new GrammarConfigurationWindow(forModify);
	}

	/**
	 * Builds the lexicon configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @return the lexicon configuration window of ACIDE - A Configurable IDE.
	 * @see LexiconConfigurationWindow
	 */
	public LexiconConfigurationWindow buildLexiconConfigurationWindow() {
		return new LexiconConfigurationWindow();
	}

	/**
	 * Builds the editor manager of ACIDE - A Configurable IDE.
	 * 
	 * @return the editor manager of ACIDE - A Configurable IDE.
	 */
	public AcideFileEditorManager buildAcideFileEditorManager() {
		return new AcideFileEditorManager();
	}

	/**
	 * Builds the log tab to be displayed on the editor of ACIDE - A
	 * Configurable IDE.
	 * 
	 * @return the log tab to be displayed on the editor of ACIDE - A
	 *         Configurable IDE.
	 * @see AcideLogTab
	 */
	public AcideLogTab buildAcideLogTab() {
		return new AcideLogTab();
	}

	/**
	 * Builds the language of ACIDE - A Configurable IDE.
	 * 
	 * @return the language of ACIDE - A Configurable IDE.
	 * @see AcideLanguage
	 */
	public AcideLanguage buildAcideLanguage() {
		return new AcideLanguage();
	}

	/**
	 * Build the new lexicon configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @return the new lexicon configuration window of ACIDE - A Configurable
	 *         IDE.
	 * @see NewLexiconConfigurationWindow
	 */
	public NewLexiconConfigurationWindow buildNewLexiconConfigurationWindow() {
		return new NewLexiconConfigurationWindow();
	}

	/**
	 * Builds the new project configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @return the new project configuration window of ACIDE - A Configurable
	 *         IDE.
	 * @see NewProjectConfigurationWindow
	 */
	public NewProjectConfigurationWindow buildNewProjectConfigurationWindow() {
		return new NewProjectConfigurationWindow();
	}

	/**
	 * Builds the output configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @return the output configuration window of ACIDE - A Configurable IDE.
	 * @see OutputConfigurationWindow
	 */
	public OutputConfigurationWindow buildOutputConfigurationWindow() {
		return new OutputConfigurationWindow();
	}

	/**
	 * Builds the external command configuration window of ACIDE - A
	 * Configurable IDE.
	 * 
	 * @return the ExternalCommand configuration window of ACIDE - A
	 *         Configurable IDE.
	 * @see ExternalCommandConfigurationWindow
	 */
	public ExternalCommandConfigurationWindow buildExternalCommandConfigurationWindow() {
		return new ExternalCommandConfigurationWindow();
	}

	/**
	 * Builds the tool bar configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @param forModify
	 *            indicates if the tool bar configuration window has to be used
	 *            to modify the tool bar configuration or not.
	 * @return the tool bar command configuration window of ACIDE - A
	 *         Configurable IDE.
	 * @see ToolBarConfigurationWindow
	 */
	public ToolBarConfigurationWindow buildToolBarConfigurationWindow(
			boolean forModify) {
		return new ToolBarConfigurationWindow(forModify);
	}

	/**
	 * Builds the status bar of ACIDE - A Configurable IDE.
	 * 
	 * @return The status bar of ACIDE - A Configurable IDE.
	 * @see AcideStatusBar
	 */
	public AcideStatusBar buildAcideStatusBar() {
		return new AcideStatusBar();
	}

	/**
	 * Builds the print configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @return the print configuration window of ACIDE - A Configurable IDE.
	 * @see PrintConfigurationWindow
	 */
	public PrintConfigurationWindow buildPrintConfigurationWindow() {
		return new PrintConfigurationWindow();
	}

	/**
	 * Builds the execution configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @return the execution configuration window of ACIDE - A Configurable IDE.
	 * @see ExecutionConfigurationWindow
	 */
	public ExecutionConfigurationWindow buildExecutionConfigurationWindow() {
		return new ExecutionConfigurationWindow();
	}

	/**
	 * Builds the compiler configuration window of ACIDE - A Configurable IDE.
	 * 
	 * @return the compiler configuration window of ACIDE - A Configurable IDE.
	 * @see CompilerConfigurationWindow
	 */
	public CompilerConfigurationWindow buildCompilerConfigurationWindow() {
		return new CompilerConfigurationWindow();
	}

	/**
	 * Builds the output visualization options window of ACIDE - A
	 * Configurable IDE.
	 * 
	 * @return the output visualization options window of ACIDE - A
	 *         Configurable IDE.
	 * @see ShellDisplayOptionsWindow
	 */
	public ShellDisplayOptionsWindow buildOutputVisualizationOptionsWindow() {
		return new ShellDisplayOptionsWindow();
	}
}
