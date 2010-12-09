package gui.mainWindow;

import es.configuration.project.AcideProjectConfiguration;
import es.configuration.toolBar.shellComandToolBar.ShellCommandList;
import gui.explorerPanel.AcideExplorerPanel;
import gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
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
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;

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
	 * Main window icon.
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
	private AcideFileEditorManager _editorBuilder;
	/**
	 * Main window menu.
	 */
	private Menu _menu;
	/**
	 * Main window output shell.
	 */
	private AcideOutputPanel _output;
	/**
	 * Main window status bar.
	 */
	private AcideStatusBar _statusBar;
	/**
	 * Main window explorer.
	 */
	private AcideExplorerPanel _explorer;
	/**
	 * Main window tool bar.
	 */
	private JToolBar _toolBar;
	/**
	 * Project configuration of the application.
	 */
	private AcideProjectConfiguration _projectConfiguration;
	/**
	 * ProjectGUI of the application.
	 */
	private NewProjectConfigurationWindow _projectGUI;
	/**
	 * Vertical split panel of the main window.
	 */
	private JSplitPane _splitPaneVertical;
	/**
	 * Horizontal split panel of the main window.
	 */
	private JSplitPane _splitPaneHorizontal;

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

		AcideSplashScreen.setProgressBar(32);

		JPanel contentPane;
		AcideSplashScreen.setProgressBar(35);

		contentPane = (JPanel) getContentPane();
		AcideSplashScreen.setProgressBar(40);

		// Sets the title
		setTitle(labels.getString("s425"));
		AcideSplashScreen.setProgressBar(42);

		// Creates the factory to build the rest of the components
		AcideGUIFactory _guiFactory = AcideGUIFactory.getInstance();
		AcideSplashScreen.setProgressBar(43);

		// MENU
		_menu = _guiFactory.buildMenu();
		AcideSplashScreen.setProgressBar(45);

		// EXPLORER
		_explorer = _guiFactory.buildAcideExplorerPanel();
		AcideSplashScreen.setProgressBar(47);

		// EDITOR
		_editorBuilder = _guiFactory.buildAcideFileEditorManager();
		AcideSplashScreen.setProgressBar(50);

		// OUTPUT
		_output = _guiFactory.buildAcideOutputPanel();
		AcideSplashScreen.setProgressBar(52);

		// STATUS BAR
		_statusBar = _guiFactory.buildAcideStatusBar();
		AcideSplashScreen.setProgressBar(55);

		contentPane.add(_statusBar.getStatusBar(), BorderLayout.SOUTH);
		AcideSplashScreen.setProgressBar(57);

		// TOOLBAR
		buildToolBar();
		AcideSplashScreen.setProgressBar(60);

		AcideOperationsFactory operationsFactory = AcideOperationsFactory
				.getInstance();
		AcideSplashScreen.setProgressBar(62);

		// PROJECT CONFIGURATION
		_projectConfiguration = operationsFactory.buildProjectConfiguration();
		AcideSplashScreen.setProgressBar(65);

		// MENU BAR
		setJMenuBar(_menu.getMenuBar());
		AcideSplashScreen.setProgressBar(67);

		// TOOL BAR
		contentPane.add(_toolBar, BorderLayout.NORTH);
		AcideSplashScreen.setProgressBar(70);

		_splitPaneVertical = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				_explorer, _editorBuilder.getTabbedPane());
		AcideSplashScreen.setProgressBar(72);

		_splitPaneVertical.setResizeWeight(0.05);
		AcideSplashScreen.setProgressBar(75);

		_splitPaneVertical.setContinuousLayout(true);
		AcideSplashScreen.setProgressBar(77);

		_splitPaneHorizontal = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				_splitPaneVertical, _output);
		AcideSplashScreen.setProgressBar(80);

		_splitPaneHorizontal.setResizeWeight(0.9);
		AcideSplashScreen.setProgressBar(82);

		_splitPaneHorizontal.setContinuousLayout(true);
		AcideSplashScreen.setProgressBar(85);

		contentPane.add(_splitPaneHorizontal, BorderLayout.CENTER);
		AcideSplashScreen.setProgressBar(87);

		// Do not close automatically
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		AcideSplashScreen.setProgressBar(90);

		setLocationRelativeTo(null);
		AcideSplashScreen.setProgressBar(95);

		// Adds the Listeners
		addWindowListener(new MainWindowListener());

		// Updates the log
		AcideLog.getLog().info(labels.getString("s66"));
	}

	/**
	 * Builds the ToolBar of the main window with the different types of tool
	 * bars in the application.
	 */
	public void buildToolBar() {

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
			ShellCommandList.loadList(currentToolBarConfiguration);

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
				ShellCommandList.loadList(name);
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
							.loadList("./configuration/toolbar/default.TBcfg");
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

		// Builds the tool bar
		_toolBar = AcideToolBarPanel.buildAcideToolBarPanel();
	}

	/**
	 * Returns the editor of the main window.
	 * 
	 * @return the editor of the main window.
	 */
	public AcideFileEditorPanel getEditor() {
		return (AcideFileEditorPanel) _editorBuilder.getFileEditorPanelAt(0);
	}

	/**
	 * Returns the output of the main window.
	 * 
	 * @return the output of the main window.
	 */
	public AcideOutputPanel getOutput() {
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
	 * @return the menu of the main window.
	 */
	public Menu getMenu() {
		return _menu;
	}

	/**
	 * Returns the editor of the main window.
	 * 
	 * @return the editor of the main window.
	 */
	public AcideFileEditorManager getFileEditorManager() {
		return _editorBuilder;
	}

	/**
	 * Returns the project configuration of the main window.
	 * 
	 * @return the project configuration of the main window.
	 */
	public AcideProjectConfiguration getProjectConfiguration() {
		return _projectConfiguration;
	}

	/**
	 * Set the Listeners for the menu of the main window.
	 */
	public void setListenersMenu() {
		_menu.setListeners();
	}

	/**
	 * Returns the explorer of the main window.
	 * 
	 * @return the explorer of the main window.
	 */
	public AcideExplorerPanel getExplorer() {
		return _explorer;
	}

	/**
	 * Set a new value for the explorer of the main window.
	 * 
	 * @param explorer
	 *            new value to set.
	 */
	public void setExplorer(AcideExplorerPanel explorer) {
		_explorer = explorer;
	}

	/**
	 * Returns the projectGUI of the main window.
	 * 
	 * @return the projectGUI of the main window.
	 */
	public NewProjectConfigurationWindow getProjectWindowConfiguration() {
		return _projectGUI;
	}

	/**
	 * Sets a new value for the projectGUI of the main window.
	 * 
	 * @param projectGUI
	 *            new value to set.
	 */
	public void setProjectGUI(NewProjectConfigurationWindow projectGUI) {
		_projectGUI = projectGUI;
	}

	/**
	 * Returns the status bar of the main window.
	 * 
	 * @return the status bar of the main window.
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
		return _splitPaneHorizontal;
	}

	/**
	 * Sets a new value for the horizontal split panel of the main window.
	 * 
	 * @param splitPaneHorizontal
	 *            new value to set.
	 */
	public void setSplitPaneHorizontal(JSplitPane splitPaneHorizontal) {
		_splitPaneHorizontal = splitPaneHorizontal;
	}

	/**
	 * Sets a new value for the explorer size of the main window.
	 * 
	 * @param splitPaneHorizontal
	 *            new value to set.
	 */
	public void setExplorerSize(int size) {
		_explorer.setExplorerSize(size);
	}

	/**
	 * Returns the vertical split pane of the main window.
	 * 
	 * @return The vertical split pane of the main window.
	 */
	public JSplitPane getVerticalSplitPane() {
		return _splitPaneVertical;
	}

	/**
	 * Set a new value for the vertical split panel of the main window.
	 * 
	 * @param splitPaneVertical
	 *            new value to set.
	 */
	public void setSplitPaneVertical(JSplitPane splitPaneVertical) {
		_splitPaneVertical = splitPaneVertical;
	}
}
