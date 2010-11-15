package gui.mainWindow;

import es.configuration.output.OutputConfiguration;
import es.configuration.project.ProjectConfiguration;
import es.configuration.toolBar.ModifiableCommandList;
import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.editor.editorManager.EditorManager;
import gui.editor.editorPanel.EditorPanel;
import gui.explorer.ExplorerPanel;
import gui.mainWindow.listeners.MainWindowListener;
import gui.menu.Menu;
import gui.menu.project.gui.NewProjectConfigurationWindow;
import gui.output.OutputPanel;
import gui.splashScreen.SplashScreen;
import gui.statusBar.StatusBar;
import gui.toolBar.ToolBar;

import java.awt.BorderLayout;
import java.awt.HeadlessException;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import language.Language;

import operations.factory.IOFactory;
import operations.factory.GUIFactory;
import operations.factory.OperationsFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Main window of ACIDE - A Configurable IDE											
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
 * @see JFrame																														
 ***********************************************************************/
public class MainWindow extends JFrame {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Icon for the main window
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Instance of the class
	 */
	private static MainWindow _instance;
	/**
	 * Editor builder for the main window
	 */
	private EditorManager _editorBuilder;
	/**
	 * Menu of the main window
	 */
	private Menu _menu;
	/**
	 * Output shell of the main window
	 */
	private OutputPanel _output;
	/**
	 * Status bar of the main window
	 */
	private StatusBar _statusBar;
	/**
	 * Explorer of the main window
	 */
	private ExplorerPanel _explorer;
	/**
	 * ToolBar of the main window
	 */
	private JToolBar _toolBar;
	/**
	 * Project configuration of the application
	 */
	private ProjectConfiguration _projectConfiguration;
	/**
	 * ProjectGUI of the application
	 */
	private NewProjectConfigurationWindow _projectGUI;
	/**
	 * Vertical split panel of the main window
	 */
	private JSplitPane _splitPaneVertical;
	/**
	 * Horizontal split panel of the main window
	 */
	private JSplitPane _splitPaneHorizontal;

	/**
	 * Returns the unique instance of the class
	 * 
	 * @return the unique instance of the class
	 */
	public static MainWindow getInstance() {
		if (_instance == null)
			_instance = new MainWindow();
		return _instance;
	}

	/**
	 * Class constructor
	 */
	public MainWindow() {

		// Gets the language
		Language language = Language.getInstance();
		SplashScreen.setProgressBar(20);

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		SplashScreen.setProgressBar(22);

		// Sets the window icon
		setIconImage(new ImageIcon(ICON).getImage());
		SplashScreen.setProgressBar(25);

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		SplashScreen.setProgressBar(27);
		Log.getLog().info(labels.getString("s67"));

		SplashScreen.setProgressBar(32);

		JPanel contentPane;
		SplashScreen.setProgressBar(35);

		contentPane = (JPanel) getContentPane();
		SplashScreen.setProgressBar(40);

		// Sets the title
		setTitle(labels.getString("s425"));
		SplashScreen.setProgressBar(42);

		// Creates the factory to build the rest of the components
		GUIFactory _guiFactory = GUIFactory.getInstance();
		SplashScreen.setProgressBar(43);

		// MENU
		_menu = _guiFactory.buildMenu();
		SplashScreen.setProgressBar(45);

		// EXPLORER
		_explorer = _guiFactory.buildExplorer();
		SplashScreen.setProgressBar(47);

		// EDITOR
		_editorBuilder = _guiFactory.buildEditorManager();
		SplashScreen.setProgressBar(50);

		// OUTPUT
		_output = _guiFactory.buildOutput();
		SplashScreen.setProgressBar(52);

		// STATUS BAR
		_statusBar = _guiFactory.buildStatusBar();
		SplashScreen.setProgressBar(55);

		contentPane.add(_statusBar.getStatusBar(), BorderLayout.SOUTH);
		SplashScreen.setProgressBar(57);

		// TOOLBAR
		buildToolBar();
		SplashScreen.setProgressBar(60);

		OperationsFactory operationsFactory = OperationsFactory.getInstance();
		SplashScreen.setProgressBar(62);

		// PROJECT CONFIGURATION
		_projectConfiguration = operationsFactory.buildProjectConfiguration();
		SplashScreen.setProgressBar(65);

		setJMenuBar(_menu.getMenuBar());
		SplashScreen.setProgressBar(67);

		contentPane.add(_toolBar, BorderLayout.NORTH);
		SplashScreen.setProgressBar(70);

		_splitPaneVertical = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				_explorer, _editorBuilder.getTabbedPane());
		SplashScreen.setProgressBar(72);

		_splitPaneVertical.setResizeWeight(0.05);
		SplashScreen.setProgressBar(75);

		_splitPaneVertical.setContinuousLayout(true);
		SplashScreen.setProgressBar(77);

		_splitPaneHorizontal = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				_splitPaneVertical, _output);
		SplashScreen.setProgressBar(80);

		_splitPaneHorizontal.setResizeWeight(0.9);
		SplashScreen.setProgressBar(82);

		_splitPaneHorizontal.setContinuousLayout(true);
		SplashScreen.setProgressBar(85);

		contentPane.add(_splitPaneHorizontal, BorderLayout.CENTER);
		SplashScreen.setProgressBar(87);

		// Do not close automatically
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		SplashScreen.setProgressBar(90);

		setLocationRelativeTo(null);
		SplashScreen.setProgressBar(95);

		// Adds the Listeners
		addWindowListener(new MainWindowListener());
		
		// Updates the log
		Log.getLog().info(labels.getString("s66"));
	}

	/**
	 * Builds the ToolBar of the main window
	 */
	public void buildToolBar() {

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
		_toolBar = ToolBar.buildStaticToolBar();

		String currentToolBarConfiguration = null;

		try {

			ModifiableCommandList.clear();
			currentToolBarConfiguration = PropertiesManager
					.getProperty("currentToolBarConfiguration");
			ModifiableCommandList.loadList(currentToolBarConfiguration);
			PropertiesManager.setProperty("currentToolBarConfiguration",
					currentToolBarConfiguration);
		} catch (Exception exception) {

			// Updates the log
			Log.getLog().error(exception.getMessage());
			
			// Gets the grammar name
			String name;
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");
			name = "./configuration/toolbar/"
					+ currentToolBarConfiguration.substring(index + 1,
							currentToolBarConfiguration.length());
			try {
				ModifiableCommandList.loadList(name);
				JOptionPane.showMessageDialog(null,
						labels.getString("s958") + currentToolBarConfiguration
								+ labels.getString("s957") + name);
				PropertiesManager.setProperty("currentToolBarConfiguration",
						name);
			} catch (Exception exception1) {

				// Updates the log
				Log.getLog().error(labels.getString("s127"));
				
				try {

					// Loads the default grammar configuration
					ModifiableCommandList
							.loadList("./configuration/toolbar/default.TBcfg");
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s958")
									+ currentToolBarConfiguration
									+ labels.getString("s959"));
					PropertiesManager.setProperty(
							"currentToolBarConfiguration",
							"./configuration/toolbar/default.TBcfg");
				} catch (HeadlessException exception2) {
					
					// Updates the log
					Log.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				} catch (Exception exception2) {
					
					// Updates the log
					Log.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}
			}
			
			// Updates the log
			Log.getLog().error(labels.getString("s127"));
		}
		_toolBar = ToolBar.buildModifiableToolBar();
		ToolBar.buildStaticToolBar();
	}

	/**
	 * Closes the default configuration of the project
	 */
	public void closeDefaultProject() {

		try {

			String file = PropertiesManager.getProperty("defaultAcideProject");

			// Is default project
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Files which belong to the project
				TextFile textFile = new IOFactory().buildFile();
				getProjectConfiguration().removeFiles();

				// SPECIAL CASE: New file with default configuration
				int newFileIndex = getNewFileIndex();

				// If it has new file opened
				if (newFileIndex != -1) {

					// Closes the new file so it will not be saved
					// in the configuration
					MainWindow.getInstance().getEditorManager().getPane()
							.remove(newFileIndex);
				}

				// SPECIAL CASE: Log file with default configuration
				int logFileIndex = getLogFileIndex();

				// If it has log file opened
				if (logFileIndex != -1) {

					// Closes the log file so it will not be saved in the 
					// project configuration
					MainWindow.getInstance().getEditorManager().getPane()
							.remove(logFileIndex);
				}

				// Sets the all opened files in the editor
				for (int pos = 0; pos < getEditorManager().getNumEditors(); pos++) {

					// Creates the file
					ExplorerFile explorerFile = new ExplorerFile();
					explorerFile.setPath(getEditorManager().getEditorAt(pos)
							.getAbsolutePath());

					// Sets if it is compilable file
					explorerFile.setIsCompilableFile(getEditorManager()
							.getEditorAt(pos).isCompilerFile());

					// Sets if it is main file
					explorerFile.setIsMainFile(getEditorManager().getEditorAt(
							pos).isMainFile());

					// Sets the name
					explorerFile.setName(getEditorManager().getEditorAt(pos)
							.getName());

					// Is a not a directory
					explorerFile.setIsDirectory(false);

					// Adds the file to the configuration
					getProjectConfiguration().addFile(explorerFile);
				}

				// Sets the language
				getProjectConfiguration().setLanguage(
						PropertiesManager.getProperty("language"));

				// Sets the name
				getProjectConfiguration().setName("");

				// Saves the configuration in the file
				textFile.save("./configuration/project/default.acidePrj",
						getProjectConfiguration().save());

				// Sets the default project
				PropertiesManager.setProperty("defaultAcideProject",
						"./configuration/project/default.acidePrj");
			} else {
				
				// Saves the configuration of the project
				TextFile textFile = new IOFactory().buildFile();
				textFile.save(file, getProjectConfiguration().save());
			}

		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Returns the new file index
	 * 
	 * @return the new file index
	 */
	private int getNewFileIndex() {

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

		// Check the opened files in the editor
		int selectedEditor = MainWindow.getInstance().getEditorManager()
				.getSelectedEditorIndex();
		int numEditors = MainWindow.getInstance().getEditorManager()
				.getNumEditors();

		int newFileIndex = -1;

		// Starts with the last editor
		MainWindow.getInstance().getEditorManager()
				.setSelectedEditorAt(numEditors - 1);

		for (int posEditor = numEditors - 1; posEditor >= 0; posEditor--) {

			MainWindow.getInstance().getEditorManager()
					.setSelectedEditorAt(posEditor);

			// If it is new file
			if (MainWindow.getInstance().getEditorManager()
					.getEditorAt(posEditor).getAbsolutePath()
					.equals(labels.getString("s79"))) {

				newFileIndex = posEditor;
			}
		}

		// Set the original selected editor
		MainWindow.getInstance().getEditorManager()
				.setSelectedEditorAt(selectedEditor);
		return newFileIndex;
	}

	/**
	 * Returns the log file index
	 * 
	 * @return the log file index
	 */
	private int getLogFileIndex() {

		// Check the opened files in the editor
		int selectedEditor = MainWindow.getInstance().getEditorManager()
				.getSelectedEditorIndex();
		int numEditors = MainWindow.getInstance().getEditorManager()
				.getNumEditors();

		int logFileIndex = -1;

		// Starts with the last one
		MainWindow.getInstance().getEditorManager()
				.setSelectedEditorAt(numEditors - 1);

		for (int posEditor = numEditors - 1; posEditor >= 0; posEditor--) {

			MainWindow.getInstance().getEditorManager()
					.setSelectedEditorAt(posEditor);

			// If it is the log file
			if (MainWindow.getInstance().getEditorManager()
					.getEditorAt(posEditor).getAbsolutePath().equals("Log")) {

				logFileIndex = posEditor;
			}
		}

		// Sets the original selected editor
		MainWindow.getInstance().getEditorManager()
				.setSelectedEditorAt(selectedEditor);
		return logFileIndex;
	}

	/**
	 * Main window closing operation
	 */
	public void closingOperation() {

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
		final ResourceBundle labels = language.getLabels();

		// Is the project configuration modified?
		if (MainWindow.getInstance().getProjectConfiguration().isModified()) {

			// Ask the user to save the configuration
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is not the cancel option
			if (chosenOption != JOptionPane.CANCEL_OPTION) {

				// If it is yes
				if (chosenOption != JOptionPane.YES_OPTION) {
					
					// Enables the menu
					MainWindow.getInstance().getMenu().getProject()
							.getSaveProject().setEnabled(true);

					// Saves the project
					MainWindow.getInstance().getMenu().getProject()
							.getSaveProject().doClick();
				}

				// Checks the opened files in the editor
				int selectedEditor = MainWindow.getInstance()
						.getEditorManager().getSelectedEditorIndex();
				int numEditors = MainWindow.getInstance().getEditorManager()
						.getNumEditors();

				// Starts with the last one
				MainWindow.getInstance().getEditorManager()
						.setSelectedEditorAt(numEditors - 1);

				for (int cont = numEditors - 1; cont >= 0; cont--) {
					MainWindow.getInstance().getEditorManager()
							.setSelectedEditorAt(cont);

					// Is the file in the editor modified?
					if (MainWindow.getInstance().getEditorManager()
							.isRedButton()) {

						// Asks the user to save the file
						chosenOption = JOptionPane.showConfirmDialog(null,
								labels.getString("s643"),
								labels.getString("s953"),
								JOptionPane.YES_NO_OPTION);

						// If yes
						if (chosenOption == JOptionPane.OK_OPTION) {
							MainWindow.getInstance().getMenu().getFile()
									.saveOrSaveAS();
						}
					}
				}

				// Sets the original selected editor
				MainWindow.getInstance().getEditorManager()
						.setSelectedEditorAt(selectedEditor);

				// Saves the output configuration
				OutputConfiguration.getInstance().save();
				// Closes the output
				MainWindow.getInstance().getOutput().executeExitCommand();

				// Updates the configuration
				try {

					// Menu configuration
					String currentMenu = PropertiesManager
							.getProperty("currentMenuConfiguration");

					if ((currentMenu.endsWith("lastModified.menuCfg"))
							|| (currentMenu.endsWith("newMenu.menuCfg"))) {
						String previous = PropertiesManager
								.getProperty("previousMenuConfiguration");
						PropertiesManager.setProperty(
								"currentMenuConfiguration", previous);
					}

					// Tool bar configuration
					String currentToolBar = PropertiesManager
							.getProperty("currentToolBarConfiguration");
					if ((currentToolBar.endsWith("lastModified.TBcfg"))
							|| currentToolBar.endsWith("newToolBar.TBcfg")) {
						String previous = PropertiesManager
								.getProperty("previousToolBarConfiguration");
						PropertiesManager.setProperty(
								"currentToolBarConfiguration", previous);
					}

					// Grammar configuration
					String currentGrammar = PropertiesManager
							.getProperty("currentGrammar");
					if ((currentGrammar.endsWith("lastModified.jar"))
							|| (currentGrammar.endsWith("newGrammar.jar"))) {
						String previous = PropertiesManager
								.getProperty("previousGrammar");
						PropertiesManager.setProperty("currentGrammar",
								previous);
					}
				} catch (Exception exception) {
					
					// Updates the log
					Log.getLog().error(exception.getMessage());
					
					JOptionPane
							.showMessageDialog(null, exception.getMessage(),
									labels.getString("s294"),
									JOptionPane.ERROR_MESSAGE);
				}

				// Stores the configuration of the files
				MainWindow.getInstance().closeDefaultProject();

				// Saves the configuration parameters of the main window
				MainWindow.getInstance().getProjectConfiguration()
						.saveMainWindowParameters();

				// Closes the main window
				System.exit(0);
			}
		}
		else
			// Closes the main window
			System.exit(0);
	}

	/**
	 * Returns the editor of the main window
	 * 
	 * @return the editor of the main window
	 */
	public EditorPanel getEditor() {
		return (EditorPanel) _editorBuilder.getEditorAt(0);
	}

	/**
	 * Returns the output of the main window
	 * 
	 * @return the output of the main window
	 */
	public OutputPanel getOutput() {
		return _output;
	}

	/**
	 * Disable the main window
	 */
	public void disableMainWindow() {
		setEnabled(false);
	}

	/**
	 * Returns the menu of the main window
	 * 
	 * @return the menu of the main window
	 */
	public Menu getMenu() {
		return _menu;
	}

	/**
	 * Returns the editor of the main window
	 * 
	 * @return the editor of the main window
	 */
	public EditorManager getEditorManager() {
		return _editorBuilder;
	}

	/**
	 * Returns the project configuration of the main window
	 * 
	 * @return the project configuration of the main window
	 */
	public ProjectConfiguration getProjectConfiguration() {
		return _projectConfiguration;
	}

	/**
	 * Set the Listeners for the menu of the main window
	 */
	public void setListenersMenu() {
		_menu.setListeners();
	}

	/**
	 * Returns the explorer of the main window
	 * 
	 * @return the explorer of the main window
	 */
	public ExplorerPanel getExplorer() {
		return _explorer;
	}

	/**
	 * Set a new value for the explorer of the main window.
	 * 
	 * @param explorer
	 *            New value to set.
	 */
	public void setExplorer(ExplorerPanel explorer) {
		_explorer = explorer;
	}

	/**
	 * Returns the projectGUI of the main window.
	 * 
	 * @return The projectGUI of the main window.
	 */
	public NewProjectConfigurationWindow getProjectWindowConfiguration() {
		return _projectGUI;
	}

	/**
	 * Sets a new value for the projectGUI of the main window
	 * 
	 * @param projectGUI
	 *            new value to set
	 */
	public void setProjectGUI(NewProjectConfigurationWindow projectGUI) {
		_projectGUI = projectGUI;
	}

	/**
	 * Returns the status bar of the main window
	 * 
	 * @return the status bar of the main window
	 */
	public StatusBar getStatusBar() {
		return _statusBar;
	}

	/**
	 * Returns the horizontal split pane of the main window
	 * 
	 * @return the horizontal split pane of the main window
	 */
	public JSplitPane getSplitPaneHorizontal() {
		return _splitPaneHorizontal;
	}

	/**
	 * Sets a new value for the horizontal split panel of the main window
	 * 
	 * @param splitPaneHorizontal
	 *            new value to set
	 */
	public void setSplitPaneHorizontal(JSplitPane splitPaneHorizontal) {
		_splitPaneHorizontal = splitPaneHorizontal;
	}

	/**
	 * Sets a new value for the explorer size of the main window
	 * 
	 * @param splitPaneHorizontal
	 *            new value to set
	 */
	public void setExplorerSize(int size) {
		_explorer.setExplorerSize(size);
	}

	/**
	 * Returns the vertical split pane of the main window.
	 * 
	 * @return The vertical split pane of the main window.
	 */
	public JSplitPane getSplitPaneVertical() {
		return _splitPaneVertical;
	}

	/**
	 * Set a new value for the vertical split panel of the main window.
	 * 
	 * @param splitPaneVertical
	 *            New value to set.
	 */
	public void setSplitPaneVertical(JSplitPane splitPaneVertical) {
		_splitPaneVertical = splitPaneVertical;
	}
}
