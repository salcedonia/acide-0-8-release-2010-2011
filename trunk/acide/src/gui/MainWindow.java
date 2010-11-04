package gui;

import es.configuration.output.OutputConfiguration;
import es.configuration.project.ProjectConfiguration;
import es.configuration.toolBar.EditableToolBarCommandList;
import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.editor.EditorBuilder;
import gui.editor.EditorPanel;
import gui.explorer.Explorer;
import gui.menu.Menu;
import gui.menu.project.ProjectGUI;
import gui.output.Output;
import gui.splashScreen.SplashScreen;
import gui.statusBar.StatusBar;
import gui.toolBarButton.ToolBarCommand;

import java.awt.BorderLayout;
import java.awt.HeadlessException;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
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

/**
 * Main Window of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class MainWindow extends JFrame {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Icon for the main window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Instance of the class.
	 */
	private static MainWindow _instance;
	/**
	 * Editor builder for the main window.
	 */
	private EditorBuilder _editorBuilder;
	/**
	 * Menu of the main window.
	 */
	private Menu _menu;
	/**
	 * Output shell of the main window.
	 */
	private Output _output;
	/**
	 * Status bar of the main window.
	 */
	private StatusBar _statusBar;
	/**
	 * Explorer of the main window.
	 */
	private Explorer _explorer;
	/**
	 * ToolBar of the main window.
	 */
	private JToolBar _toolBar;
	/**
	 * Project configuration of the application.
	 */
	private ProjectConfiguration _projectConfiguration;
	/**
	 * ProjectGUI of the application.
	 */
	private ProjectGUI _projectGUI;
	/**
	 * Vertical split panel of the main window.
	 */
	private JSplitPane _splitPaneVertical;
	/**
	 * Horizontal split panel of the main window.
	 */
	private JSplitPane _splitPaneHorizontal;

	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static MainWindow getInstance() {
		if (_instance == null)
			_instance = new MainWindow();
		return _instance;
	}

	/**
	 * Constructor of the class.
	 */
	public MainWindow() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		SplashScreen.setProgressBar(20);

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		SplashScreen.setProgressBar(22);

		// SET THE ICON OF THE WINDOW
		setIconImage(new ImageIcon(ICON).getImage());
		SplashScreen.setProgressBar(25);

		// GET THE LABELS TO DISPLAY
		final ResourceBundle labels = language.getLabels();
		SplashScreen.setProgressBar(27);
		Log.getLog().info(labels.getString("s67"));

		SplashScreen.setProgressBar(32);

		JPanel contentPane;
		SplashScreen.setProgressBar(35);

		contentPane = (JPanel) getContentPane();
		SplashScreen.setProgressBar(40);

		// SET THE TITLE
		setTitle(labels.getString("s425"));
		SplashScreen.setProgressBar(42);

		// CREATES THE FACTORY TO BUILD THE REST OF THE COMPONENTS
		GUIFactory _guiFactory = GUIFactory.getInstance();
		SplashScreen.setProgressBar(43);

		// MENU
		_menu = _guiFactory.buildMenu();
		SplashScreen.setProgressBar(45);

		// EXPLORER
		_explorer = _guiFactory.buildExplorer();
		SplashScreen.setProgressBar(47);

		// EDITOR
		_editorBuilder = _guiFactory.buildEditorBuilder();
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

		setDefaultCloseOperation(EXIT_ON_CLOSE);
		SplashScreen.setProgressBar(90);

		setLocationRelativeTo(null);
		SplashScreen.setProgressBar(95);

		// LISTENERS
		addWindowListener(new MainWindowListener());
		Log.getLog().info(labels.getString("s66"));	    
	}

	/**
	 * Builds the ToolBar of the main window.
	 */
	public void buildToolBar() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		_toolBar = ToolBarCommand.buildToolBar();

		String currentToolBarConfiguration = null;
		
		try {

			EditableToolBarCommandList.clear();
			currentToolBarConfiguration = PropertiesManager
					.getProperty("currentToolBarConfiguration");
			EditableToolBarCommandList.loadList(currentToolBarConfiguration);
			PropertiesManager.setProperty("currentToolBarConfiguration",
					currentToolBarConfiguration);
		} catch (Exception e) {

			// GET THE NAME
			String name;
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if(index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");		
			name = "./configuration/toolbar/"
					+ currentToolBarConfiguration.substring(index + 1,
							currentToolBarConfiguration.length());
			try {
				EditableToolBarCommandList
						.loadList(name);
				JOptionPane.showMessageDialog(null,
						labels.getString("s958") + currentToolBarConfiguration
								+ labels.getString("s957")
								+ name);
				PropertiesManager.setProperty("currentToolBarConfiguration",
						name);
			} catch (Exception e1) {
				
				Log.getLog().error(labels.getString("s127"));
				try {
					
					// LOAD THE DEFAULT CONFIGURATION
					EditableToolBarCommandList
							.loadList("./configuration/toolbar/default.BHcfg");
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s958")
									+ currentToolBarConfiguration
									+ labels.getString("s959"));
					PropertiesManager.setProperty(
							"currentToolBarConfiguration",
							"./configuration/toolbar/default.BHcfg");
				} catch (HeadlessException e2) {
					e2.printStackTrace();
				} catch (Exception e2) {
					e2.printStackTrace();
				}
			}
			Log.getLog().error(labels.getString("s127"));
		}
		_toolBar = ToolBarCommand.buildEditableToolBar();
		ToolBarCommand.buildToolBar();
	}

	/**
	 * Close the default configuration of the project.
	 */
	public void closeDefaultProject() {

		try {

			String file = PropertiesManager.getProperty("defaultAcideProject");

			// DEFAULT PROJECT
			if (MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {

				// FILES WHICH BELONG TO THE PROJECT
				TextFile textFile = new IOFactory().buildFile();
				getProjectConfiguration().removeFiles();
				
				// SPECIAL CASE: NEW FILE WITH DEFAULT CONFIGURATION
				int newFileIndex = getNewFileIndex();
				
				// IF IT HAS NEW FILE 
				if(newFileIndex != -1){
	
					// CLOSES THE NEW FILE SO IT WILL NOT BE SAVED
					// IN THE DEFAULT CONFIGURATION
					MainWindow.getInstance().getEditorBuilder().getPane().remove(newFileIndex);
				}
				
				// SPECIAL CASE: LOG FILE WITH DEFAULT CONFIGURATION
				int logFileIndex = getLogFileIndex();
				
				// IF IT HAS LOG FILE 
				if(logFileIndex != -1){
	
					// CLOSES THE LOG FILE SO IT WILL NOT BE SAVED
					// IN THE DEFAULT CONFIGURATION
					MainWindow.getInstance().getEditorBuilder().getPane().remove(logFileIndex);
				}		
				
				// SET ALL THE OPENED FILES IN THE EDITOR
				for (int pos = 0; pos < getEditorBuilder().getNumEditors(); pos++) {

					// CREATES THE FILE
					ExplorerFile explorerFile = new ExplorerFile();
					explorerFile.setPath(getEditorBuilder().getEditorAt(pos)
							.getAbsolutePath());
					
					// SET IF ITS COMPILABLE FILE
					explorerFile.setIsCompilableFile(getEditorBuilder()
							.getEditorAt(pos).isCompilerFile());
					
					// SET IF IT IS MAIN FILE
					explorerFile.setIsMainFile(getEditorBuilder().getEditorAt(pos)
							.isMainFile());
					
					// SET THE NAME
					explorerFile.setName(getEditorBuilder().getEditorAt(pos)
							.getName());
					
					// IS A FILE NOT A DIRECTORY
					explorerFile.setIsDirectory(false);
				
					// ADD FILE TO THE CONFIGURATION
					getProjectConfiguration().addFile(explorerFile);
				}

				// SET THE LANGUAGE
				getProjectConfiguration().setLanguage(
						PropertiesManager.getProperty("language"));
				
				// SET THE NAME
				getProjectConfiguration().setName("");

				// SAVE THE CONFIGURATION INTO THE FILE
				textFile.save("./configuration/project/default.acidePrj",
						getProjectConfiguration().save());
				
				// SET THE DEFAULT ACIDE PROJECT
				PropertiesManager.setProperty("defaultAcideProject",
						"./configuration/project/default.acidePrj");
			}
			else{			
				// SAVE THE CONFIGURATION OF THE DIFFERENT PROJECT
				TextFile textFile = new IOFactory().buildFile();
				textFile.save(file, getProjectConfiguration().save());
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Returns the new file index.
	 * 
	 * @return The new file index.
	 */
	private int getNewFileIndex(){

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		// CHECK THE OPENED FILES IN THE EDITOR
		int selectedEditor = MainWindow.getInstance().getEditorBuilder()
				.getSelectedEditorIndex();
		int numEditors = MainWindow.getInstance().getEditorBuilder().getNumEditors();

		int newFileIndex = -1;
		
		// STARTS WITH THE LAST ONE
		MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(numEditors - 1);
		
		for (int posEditor = numEditors - 1; posEditor >= 0; posEditor--) {
			
			MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(posEditor);

			// IF IT IS NEW FILE
			if (MainWindow.getInstance().getEditorBuilder().getEditorAt(posEditor).getAbsolutePath()
					.equals(labels.getString("s79"))) {
				
				newFileIndex = posEditor;
			}
		}

		// SET THE SELECTED EDITOR TO THE PREVIOUS 
		MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(selectedEditor);		
		return newFileIndex;
	}
	
	/**
	 * Returns the log file index.
	 * 
	 * @return The log file index.
	 */
	private int getLogFileIndex(){
		
		// CHECK THE OPENED FILES IN THE EDITOR
		int selectedEditor = MainWindow.getInstance().getEditorBuilder()
				.getSelectedEditorIndex();
		int numEditors = MainWindow.getInstance().getEditorBuilder().getNumEditors();

		int logFileIndex = -1;
		
		// STARTS WITH THE LAST ONE
		MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(numEditors - 1);
		
		for (int posEditor = numEditors - 1; posEditor >= 0; posEditor--) {
			
			MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(posEditor);

			// IF IT IS LOG FILE
			if (MainWindow.getInstance().getEditorBuilder().getEditorAt(posEditor).getAbsolutePath()
					.equals("Log")) {
				
				logFileIndex = posEditor;
			}
		}

		// SET THE SELECTED EDITOR TO THE PREVIOUS 
		MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(selectedEditor);		
		return logFileIndex;
	}
	
	/**
	 * Main window closing operation.
	 */
	public void closingOperation() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS TO DISPLAY
		final ResourceBundle labels = language.getLabels();

		// IS THE PROJECT CONFIGURATION MODIFIED?
		if (MainWindow.getInstance().getProjectConfiguration().isModified()) {

			// ASK TO THE USER FOR SAVING THE CONFIGURATION
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_OPTION);

			// IF THE USER CHOOSES YES
			if (chosenOption == JOptionPane.OK_OPTION) {

				// ENABLE THE MENU
				MainWindow.getInstance().getMenu().getProject().getSaveProject()
						.setEnabled(true);
				
				// SAVE THE PROJECT
				MainWindow.getInstance().getMenu().getProject().getSaveProject()
						.doClick();
			}
		}

		// CHECK THE OPENED FILES IN THE EDITOR
		int selectedEditor = MainWindow.getInstance().getEditorBuilder()
				.getSelectedEditorIndex();
		int numEditors = MainWindow.getInstance().getEditorBuilder().getNumEditors();

		// STARTS WITH THE LAST ONE
		MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(numEditors - 1);
		
		for (int cont = numEditors - 1; cont >= 0; cont--) {
			MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(cont);

			// IF THE FILE IN THE EDITOR IS MODIFIED
			if (MainWindow.getInstance().getEditorBuilder().isRedButton()) {

				// ASK TO THE USER IF WANTS TO SAVE IT
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s643"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// IF YES
				if (chosenOption == JOptionPane.OK_OPTION) {
					MainWindow.getInstance().getMenu().getFile().saveOrSaveAS();
				}
			}
		}

		// SET THE SELECTED EDITOR TO THE PREVIOUS 
		MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(selectedEditor);

		// SAVE THE OUTPUT CONFIGURATION
		OutputConfiguration.getInstance().save();
		// CLOSE THE OUTPUT
		MainWindow.getInstance().getOutput().executeExitCommand();

		// UPDATES THE CONFIGURATION
		try {

			// MENU CONFIGURATION
			String currentMenu = PropertiesManager
					.getProperty("currentMenuConfiguration");

			if ((currentMenu.endsWith("lastModified.menuCfg"))
					|| (currentMenu.endsWith("newMenu.menuCfg"))) {
				String previous = PropertiesManager
						.getProperty("previousMenuConfiguration");
				PropertiesManager.setProperty("currentMenuConfiguration",
						previous);
			}

			// TOOLBAR
			String currentToolBar = PropertiesManager
					.getProperty("currentToolBarConfiguration");
			if ((currentToolBar.endsWith("lastModified.BHcfg"))
					|| currentToolBar.endsWith("newToolBar.BHcfg")) {
				String previous = PropertiesManager
						.getProperty("previousToolBarConfiguration");
				PropertiesManager.setProperty(
						"currentToolBarConfiguration", previous);
			}

			// GRAMMAR
			String currentGrammar = PropertiesManager
					.getProperty("currentGrammar");
			if ((currentGrammar.endsWith("lastModified.jar"))
					|| (currentGrammar.endsWith("newGrammar.jar"))) {
				String previous = PropertiesManager
						.getProperty("previousGrammar");
				PropertiesManager.setProperty("currentGrammar", previous);
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null, e.getMessage(),
					labels.getString("s294"), JOptionPane.ERROR_MESSAGE);
		}

		// STORES THE CONFIGURATION OF THE FILES
		MainWindow.getInstance().closeDefaultProject();

		// SAVE THE CONFIGURATION PARAMETERS OF THE MAIN WINDOW
		MainWindow.getInstance().getProjectConfiguration().saveMainWindowParameters();
	}
	
	/**
	 * Returns the editor of the main window.
	 * 
	 * @return The editor of the main window.
	 */
	public EditorPanel getEditor() {
		return (EditorPanel) _editorBuilder.getEditorAt(0);
	}

	/**
	 * Returns the output of the main window.
	 * 
	 * @return The output of the main window.
	 */
	public Output getOutput() {
		return _output;
	}

	/**
	 * Disable the main window.
	 */
	public void disableMainWindow() {
		setEnabled(false);
	}

	/**
	 * Returns the menu of the main window.
	 * 
	 * @return The menu of the main window.
	 */
	public Menu getMenu() {
		return _menu;
	}

	/**
	 * Returns the editor of the main window.
	 * 
	 * @return The editor of the main window.
	 */
	public EditorBuilder getEditorBuilder() {
		return _editorBuilder;
	}

	/**
	 * Returns the project configuration of the main window.
	 * 
	 * @return The project configuration of the main window.
	 */
	public ProjectConfiguration getProjectConfiguration() {
		return _projectConfiguration;
	}

	/**
	 * Set the listeners for the menu of the main window.
	 */
	public void setListenersMenu() {
		_menu.setListeners();
	}

	/**
	 * Returns the explorer of the main window.
	 * 
	 * @return The explorer of the main window.
	 */
	public Explorer getExplorer() {
		return _explorer;
	}

	/**
	 * Set a new value for the explorer of the main window.
	 * 
	 * @param explorer
	 *            New value to set.
	 */
	public void setExplorer(Explorer explorer) {
		_explorer = explorer;
	}

	/**
	 * Returns the projectGUI of the main window.
	 * 
	 * @return The projectGUI of the main window.
	 */
	public ProjectGUI getProjectGUI() {
		return _projectGUI;
	}

	/**
	 * Set a new value for the projectGUI of the main window.
	 * 
	 * @param projectGUI
	 *            New value to set.
	 */
	public void setProjectGUI(ProjectGUI projectGUI) {
		_projectGUI = projectGUI;
	}

	/**
	 * Returns the status bar of the main window.
	 * 
	 * @return The status bar of the main window.
	 */
	public StatusBar getStatusBar() {
		return _statusBar;
	}

	/**
	 * Returns the horizontal split pane of the main window.
	 * 
	 * @return The horizontal split pane of the main window.
	 */
	public JSplitPane getSplitPaneHorizontal() {
		return _splitPaneHorizontal;
	}

	/**
	 * Set a new value for the horizontal split panel of the main window.
	 * 
	 * @param splitPaneHorizontal
	 *            New value to set.
	 */
	public void setSplitPaneHorizontal(JSplitPane splitPaneHorizontal) {
		_splitPaneHorizontal = splitPaneHorizontal;
	}

	/**
	 * Set a new value for the explorer size of the main window.
	 * 
	 * @param splitPaneHorizontal
	 *            New value to set.
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
	
	/**
	 * Listener of the main window of the application.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	public class MainWindowListener extends WindowAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent
		 * )
		 */
		public void windowClosing(WindowEvent arg0) {
			MainWindow.getInstance().closingOperation();
		}
	}
}
