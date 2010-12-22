package gui.mainWindow;

import es.configuration.project.AcideProjectConfiguration;
import es.configuration.toolBar.shellComandToolBar.ShellCommandList;
import gui.explorerPanel.AcideExplorerPanel;
import gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import gui.mainWindow.listeners.MainWindowListener;
import gui.menuBar.Menu;
import gui.menuBar.projectMenu.gui.NewProjectConfigurationWindow;
import gui.outputPanel.AcideOutputPanel;
import gui.splashScreen.AcideSplashScreen;
import gui.statusBarPanel.AcideStatusBar;
import gui.toolBarPanel.AcideToolBarPanel;

import java.awt.BorderLayout;
import java.awt.HeadlessException;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JSplitPane;

import language.AcideLanguage;
import operations.factory.AcideGUIFactory;
import operations.factory.AcideOperationsFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Main window of ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
	 * Main window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Main window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * Main window unique class instance.
	 */
	private static MainWindow _instance;
	/**
	 * Main window editor builder.
	 */
	private AcideFileEditorManager _fileEditorManager;
	/**
	 * Main window menu.
	 */
	private Menu _menu;
	/**
	 * Main window output panel.
	 */
	private AcideOutputPanel _outputPanel;
	/**
	 * Main window status bar.
	 */
	private AcideStatusBar _statusBar;
	/**
	 * Main window explorer panel.
	 */
	private AcideExplorerPanel _explorerPanel;
	/**
	 * Main window tool bar panel.
	 */
	private AcideToolBarPanel _toolBarPanel;
	/**
	 * Project configuration of the application.
	 */
	private AcideProjectConfiguration _projectConfiguration;
	/**
	 * ProjectGUI of the application.
	 */
	private NewProjectConfigurationWindow _projectGUI;
	/**
	 * Main window vertical split panel.
	 */
	private JSplitPane _verticalSplitPanel;
	/**
	 * Main window horizontal split panel.
	 */
	private JSplitPane _horizontalSplitPanel;

	/**
	 * Returns the unique main window class instance.
	 * 
	 * @return the unique main window class instance.
	 */
	public static MainWindow getInstance() {
		if (_instance == null)
			_instance = new MainWindow();
		return _instance;
	}

	/**
	 * Creates a new main window.
	 */
	public MainWindow() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		AcideSplashScreen.setProgressBar(20);

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		AcideSplashScreen.setProgressBar(22);

		// Sets the window icon
		setIconImage(ICON.getImage());
		AcideSplashScreen.setProgressBar(25);

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		AcideSplashScreen.setProgressBar(27);
		AcideLog.getLog().info(labels.getString("s67"));

		AcideSplashScreen.setProgressBar(35);

		// Sets the title
		setTitle(labels.getString("s425"));
		AcideSplashScreen.setProgressBar(42);

		// PROJECT CONFIGURATION
		_projectConfiguration = AcideOperationsFactory.getInstance()
				.buildProjectConfiguration();
		AcideSplashScreen.setProgressBar(45);
		
		// MENU
		_menu = AcideGUIFactory.getInstance().buildMenu();
		AcideSplashScreen.setProgressBar(47);

		// MENU BAR
		setJMenuBar(_menu.getMenuBar());
		AcideSplashScreen.setProgressBar(50);
		
		// EXPLORER
		_explorerPanel = AcideGUIFactory.getInstance().buildAcideExplorerPanel();
		AcideSplashScreen.setProgressBar(53);

		// EDITOR
		_fileEditorManager = AcideGUIFactory.getInstance().buildAcideFileEditorManager();
		AcideSplashScreen.setProgressBar(57);

		// OUTPUT
		_outputPanel = AcideGUIFactory.getInstance().buildAcideOutputPanel();
		AcideSplashScreen.setProgressBar(60);

		// STATUS BAR
		_statusBar = AcideGUIFactory.getInstance().buildAcideStatusBar();
		AcideSplashScreen.setProgressBar(63);

		add(_statusBar.getStatusBar(), BorderLayout.SOUTH);
		AcideSplashScreen.setProgressBar(65);

		// TOOLBAR
		buildToolBarPanel();
		AcideSplashScreen.setProgressBar(67);

		// VERTICAL SPLIT PANEL
		_verticalSplitPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				_explorerPanel, _fileEditorManager.getTabbedPane());
		AcideSplashScreen.setProgressBar(72);

		_verticalSplitPanel.setResizeWeight(0.05);
		AcideSplashScreen.setProgressBar(75);

		_verticalSplitPanel.setContinuousLayout(true);
		AcideSplashScreen.setProgressBar(77);

		// HORIZONTAL SPLIT PANEL
		_horizontalSplitPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				_verticalSplitPanel, _outputPanel);
		AcideSplashScreen.setProgressBar(80);

		_horizontalSplitPanel.setResizeWeight(0.9);
		AcideSplashScreen.setProgressBar(82);

		_horizontalSplitPanel.setContinuousLayout(true);
		AcideSplashScreen.setProgressBar(85);

		add(_horizontalSplitPanel, BorderLayout.CENTER);
		AcideSplashScreen.setProgressBar(87);

		// Do not close automatically
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		AcideSplashScreen.setProgressBar(90);

		setLocationRelativeTo(null);
		AcideSplashScreen.setProgressBar(95);

		// Adds the Listeners
		addWindowListener(new MainWindowListener());
		setMenuListeners();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s66"));
	}

	/**
	 * Builds the main window tool bar panel with the different types of tool
	 * bars in the application.
	 */
	public void buildToolBarPanel() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		String currentToolBarConfiguration = null;

		try {

			ShellCommandList.clear();
			currentToolBarConfiguration = ResourceManager.getInstance()
					.getProperty("currentToolBarConfiguration");
			ShellCommandList.loadFinalList(currentToolBarConfiguration);

			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty(
					"currentToolBarConfiguration", currentToolBarConfiguration);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Gets the grammar name
			String name;
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");
			name = "./configuration/toolbar/"
					+ currentToolBarConfiguration.substring(index + 1,
							currentToolBarConfiguration.length());
			try {

				// Load the command list
				ShellCommandList.loadFinalList(name);

				// Information message
				JOptionPane.showMessageDialog(null,
						labels.getString("s958") + currentToolBarConfiguration
								+ labels.getString("s957") + name);

				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", name);
			} catch (Exception exception1) {

				// Updates the log
				AcideLog.getLog().error(labels.getString("s127"));

				try {

					// Loads the default grammar configuration
					ShellCommandList
							.loadFinalList("./configuration/toolbar/default.TBcfg");

					// Information message
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s958")
									+ currentToolBarConfiguration
									+ labels.getString("s959"));

					// Updates the RESOURCE MANAGER
					ResourceManager.getInstance().setProperty(
							"currentToolBarConfiguration",
							"./configuration/toolbar/default.TBcfg");
				} catch (HeadlessException exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				} catch (Exception exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}
			}

			// Updates the log
			AcideLog.getLog().error(labels.getString("s127"));
		}

		// Updates the tool bar panel
		updateToolBarPanel();
	}

	/**
	 * Returns the main window output panel.
	 * 
	 * @return the main window output panel.
	 */
	public AcideOutputPanel getOutputPanel() {
		return _outputPanel;
	}

	/**
	 * Returns the main window menu.
	 * 
	 * @return the main window menu.
	 */
	public Menu getMenu() {
		return _menu;
	}

	/**
	 * Returns the file editor manager of the main window.
	 * 
	 * @return the file editor manager of the main window.
	 */
	public AcideFileEditorManager getFileEditorManager() {
		return _fileEditorManager;
	}

	/**
	 * Returns the main window project configuration.
	 * 
	 * @return the main window project configuration.
	 */
	public AcideProjectConfiguration getProjectConfiguration() {
		return _projectConfiguration;
	}

	/**
	 * Set the Listeners for the menu of the main window.
	 */
	public void setMenuListeners() {
		_menu.setListeners();
	}

	/**
	 * Returns the explorer panel of the main window.
	 * 
	 * @return the explorer panel of the main window.
	 */
	public AcideExplorerPanel getExplorerPanel() {
		return _explorerPanel;
	}

	/**
	 * Set a new value for the main window explorer panel.
	 * 
	 * @param explorerPanel
	 *            new value to set.
	 */
	public void setExplorerPanel(AcideExplorerPanel explorerPanel) {
		_explorerPanel = explorerPanel;
	}

	/**
	 * Returns the main window projectGUI.
	 * 
	 * @return the main window projectGUI.
	 */
	public NewProjectConfigurationWindow getProjectWindowConfiguration() {
		return _projectGUI;
	}

	/**
	 * Sets a new value for the main window projectGUI.
	 * 
	 * @param projectGUI
	 *            new value to set.
	 */
	public void setProjectGUI(NewProjectConfigurationWindow projectGUI) {
		_projectGUI = projectGUI;
	}

	/**
	 * Returns the main window status bar.
	 * 
	 * @return the main window status bar.
	 */
	public AcideStatusBar getStatusBar() {
		return _statusBar;
	}

	/**
	 * Returns the horizontal split pane of the main window.
	 * 
	 * @return the horizontal split pane of the main window.
	 */
	public JSplitPane getHorizontalSplitPane() {
		return _horizontalSplitPanel;
	}

	/**
	 * Sets a new value for the horizontal split panel of the main window.
	 * 
	 * @param splitPaneHorizontal
	 *            new value to set.
	 */
	public void setSplitPaneHorizontal(JSplitPane splitPaneHorizontal) {
		_horizontalSplitPanel = splitPaneHorizontal;
	}

	/**
	 * Sets a new value for the main window explorer panel size.
	 * 
	 * @param size
	 *            new value to set.
	 */
	public void setExplorerPanelSize(int size) {
		_explorerPanel.setExplorerSize(size);
	}

	/**
	 * Returns the main window vertical split panel.
	 * 
	 * @return the main window vertical split panel.
	 */
	public JSplitPane getVerticalSplitPane() {
		return _verticalSplitPanel;
	}

	/**
	 * Set a new value for the main window vertical split panel.
	 * 
	 * @param splitPaneVertical
	 *            new value to set.
	 */
	public void setSplitPaneVertical(JSplitPane splitPaneVertical) {
		_verticalSplitPanel = splitPaneVertical;
	}

	/**
	 * Returns the main window tool bar panel.
	 * 
	 * @return the main window tool bar panel.
	 */
	public AcideToolBarPanel getToolBarPanel() {
		return _toolBarPanel;
	}

	/**
	 * Sets a new value to the main window tool bar panel.
	 * 
	 * @param toolBar
	 *            new value to set.
	 */
	public void setToolBarPanel(AcideToolBarPanel toolBar) {
		_toolBarPanel = toolBar;
	}
	
	/**
	 * Updates the tool bar panel. Removes the tool bar panel if exists, and
	 * builds it, adding it to the main window afterwards.
	 */
	public void updateToolBarPanel(){
		
		if(_toolBarPanel != null)
			remove(_toolBarPanel);
		
		// Creates the tool bar
		_toolBarPanel = new AcideToolBarPanel();
		
		// Builds the tool bar panel
		_toolBarPanel.buildAcideToolBarPanel();
		
		// Adds the tool bar panel to the main window
		add(_toolBarPanel, BorderLayout.NORTH);
	}
}
