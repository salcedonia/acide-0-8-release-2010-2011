package gui.menu;

import javax.swing.*;

import language.Language;

import es.configuration.menu.MenuConfiguration;

import operations.log.Log;
import properties.PropertiesManager;

import java.awt.HeadlessException;
import java.util.ResourceBundle;

import gui.menu.configuration.ConfigurationMenu;
import gui.menu.edit.EditMenu;
import gui.menu.file.FileMenu;
import gui.menu.help.HelpMenu;
import gui.menu.listeners.MenuMouseClickListener;
import gui.menu.project.ProjectMenu;
import gui.menu.view.ViewMenu;

/************************************************************************																
 * Menu of ACIDE - A Configurable IDE											
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
public class Menu {

	/**
	 * Menu bar
	 */
	private JMenuBar _menuBar;
	/**
	 * File menu
	 */
	private FileMenu _file;
	/**
	 * Edit menu
	 */
	private EditMenu _edit;
	/**
	 * Project menu
	 */
	private ProjectMenu _project;
	/**
	 * View menu
	 */
	private ViewMenu _view;
	/**
	 * Configuration menu
	 */
	private ConfigurationMenu _configuration;
	/**
	 * Help menu
	 */
	private HelpMenu _help;
	/**
	 * Indicates if the shell is focused
	 */
	private boolean _isShellFocused;
	/**
	 * Indicates if 
	 */
	private boolean _isNPF;

	/**
	 * Class constructor
	 */
	public Menu() {
		
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
		
		// Updates the log
		Log.getLog().info(labels.getString("s68"));
		
		// MENU BAR
		_menuBar = new JMenuBar();
		
		// MENUS
		_file = new FileMenu();
		_edit = new EditMenu();
		_project = new ProjectMenu();
		_view = new ViewMenu();
		_configuration = new ConfigurationMenu();
		_help = new HelpMenu();
								
		setLanguageLabels();
		buildMenu();
		
		// Updates the log
		Log.getLog().info(labels.getString("s69"));
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
		final ResourceBundle labels = language.getLabels();
		
		// FILE
		_file.setText(labels.getString("s1"));
		_file.setLanguageLabels();
		
		// EDIT 
		_edit.setText(labels.getString("s2"));
		_edit.setLanguageLabels();
		
		// PROJECT
		_project.setText(labels.getString("s3"));
		_project.setLanguageLabels();
		
		// VIEW
		_view.setText(labels.getString("s4"));
		_view.setLanguageLabels();
		
		// CONFIGURATION
		_configuration.setText(labels.getString("s5"));
		_configuration.setLanguageLabels();
		
		// HELP
		_help.setText(labels.getString("s7"));	
		_help.setLanguageLabels();
		
		/*depurar.setText(labels.getString("s20"));
		depurar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D,
				ActionEvent.ALT_MASK));
		compilador.setText(labels.getString("s31"));
		compilador.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		interprete.setText(labels.getString("s32"));
		interprete.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		parser.setText(labels.getString("s33"));
		parser.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
				
		nuevaConfLenguaje.setText(labels.getString("s37"));
		nuevaConfLenguaje.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));*/	
		
	}

	/**
	 * Builds the menu
	 */
	public void buildMenu() {
			
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
		
		String currentMenu = null;
		
		try {
			currentMenu = PropertiesManager
					.getProperty("currentMenuConfiguration");
			boolean[] values = MenuConfiguration
					.loadMenuConfigurationFile(currentMenu);
			MenuConfiguration.setAll(values);
			PropertiesManager.setProperty("currentMenuConfiguration",
					currentMenu);
			
			// Updates the log
			Log.getLog().info(labels.getString("s70") + " " + currentMenu);
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().info(labels.getString("s71") + exception.getMessage());
			
			// GET THE NAME
			String currentMenu2;
			int index = currentMenu.lastIndexOf("\\");
			if (index == -1)
				index = currentMenu.lastIndexOf("/");
			currentMenu2 = ".\\configuration\\menu\\"
					+ currentMenu.substring(index + 1, currentMenu.length());
			boolean[] values;
			try {
				values = MenuConfiguration.loadMenuConfigurationFile(currentMenu2);
				MenuConfiguration.setAll(values);
				PropertiesManager.setProperty("currentMenuConfiguration",
						currentMenu2);
				
				// Updates the log
				Log.getLog().info(labels.getString("s70") + " " + currentMenu2);
				
				JOptionPane
						.showMessageDialog(null, labels.getString("s956")
								+ currentMenu + labels.getString("s957")
								+ currentMenu2);
			} catch (Exception exception1) {
				try {
					
					// Updates the log
					Log.getLog().error(exception1.getMessage());
					exception1.printStackTrace();
					
					values = MenuConfiguration
							.loadMenuConfigurationFile("./configuration/menu/defaultAllOn.menuCfg");
					MenuConfiguration.setAll(values);
					PropertiesManager.setProperty("currentMenuConfiguration",
							"./configuration/menu/defaultAllOn.menuCfg");
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s956") + currentMenu
									+ labels.getString("s959"));
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
		}

		_menuBar.removeAll();

		_file.buildMenu(labels,language);
		_edit.buildMenu();
		_project.buildMenu();
		_view.buildMenu();
		_configuration.buildMenu();		
		_help.buildMenu();
		
		if (MenuConfiguration.getFile()
				|| MenuConfiguration.getOpenFile()
				|| MenuConfiguration.getSaveFileAs()
				|| MenuConfiguration.getSaveFile()
				|| MenuConfiguration.getSaveAllFiles()
				|| MenuConfiguration.getPrintFile() || MenuConfiguration.getExit())
			_menuBar.add(_file);
		if (MenuConfiguration.getUndo() || MenuConfiguration.getRedo()
				|| MenuConfiguration.getCopy()
				|| MenuConfiguration.getPaste()
				|| MenuConfiguration.getCut()
				|| MenuConfiguration.getSelectAll()
				|| MenuConfiguration.getGoToLine() || MenuConfiguration.getSearch()
				|| MenuConfiguration.getReplace())
			_menuBar.add(_edit);
		if (MenuConfiguration.getProject()
				|| MenuConfiguration.getOpenProject()
				|| MenuConfiguration.getSaveProject()
				|| MenuConfiguration.getCloseProject()
				|| MenuConfiguration.getAddFile()
				|| MenuConfiguration.getRemoveFile()
				|| MenuConfiguration.getDeleteFile()
				|| MenuConfiguration.getNewProjectFile()
				|| MenuConfiguration.getAddFolder()
				|| MenuConfiguration.getRemoveFolder()
				|| MenuConfiguration.getCompile()
				|| MenuConfiguration.getExecute())
			_menuBar.add(_project);
		if (MenuConfiguration.getShowLog()
				|| MenuConfiguration.getShowBrowser()
				|| MenuConfiguration.getShowShellWindow())
			_menuBar.add(_view);
		
		_menuBar.add(_configuration);
		
		if (MenuConfiguration.getShowHelp()
				|| MenuConfiguration.getShowAboutUs())
			_menuBar.add(_help);
		
		_menuBar.setVisible(true);
	}

	/**
	 * Sets the menu Listeners
	 */
	public void setListeners() {
		
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
		
		// FILE
		_file.addMouseListener(new MenuMouseClickListener());
		_file.setListeners();
		
		// EDIT
		_edit.addMouseListener(new MenuMouseClickListener());
		_edit.setListeners();
		
		// PROJECT
		_project.addMouseListener(new MenuMouseClickListener());
		_project.setListeners();
		
		// VIEW
		_view.addMouseListener(new MenuMouseClickListener());
		_view.setListeners();
		
		// CONFIGURATION
		_configuration.addMouseListener(new MenuMouseClickListener());
		_configuration.setListeners();
		
		// HELP
		_help.addMouseListener(new MenuMouseClickListener());
		_help.setListeners();
		
		// Updates the log
		Log.getLog().info(labels.getString("s72"));
	}

	/**
	 * Returns the menu bar
	 * 
	 * @return the menu bar
	 */
	public JMenuBar getMenuBar() {
		return _menuBar;
	}
	
	/**
	 * Enables the file menu
	 */
	public void enableFileMenu() {
		_file.enableMenu();
	}

	/**
	 * 
	 */
	public void disableFileMenu() {
		_file.disableMenu();	
	}
	
	/**
	 * Enables the project menu
	 */
	public void enableProjectMenu() {
		_project.enableMenu();
	}

	/**
	 * Disables the project menu
	 */
	public void disableProjectMenu() {
		_project.disableMenu();		
	}

	/**
	 * Sets a new value to the file menu
	 * 
	 * @param file new value to set
	 */
	public void setFile(FileMenu file) {
		_file = file;
	}
	
	/**
	 * Returns the help menu
	 * 
	 * @return the help menu
	 */
	public HelpMenu getHelp() {
		return _help;
	}

	/**
	 * Sets a new value to the help menu
	 * 
	 * @param help new value to set
	 */
	public void setHelp(HelpMenu help) {
		_help = help;
	}
	
	/**
	 * Returns the edit menu
	 * 
	 * @return the edit menu
	 */
	public EditMenu getEdit() {
		return _edit;
	}

	/**
	 * 
	 * @param edit
	 */
	public void setEdit(EditMenu edit) {
		_edit = edit;
	}

	/**
	 * Returns the configuration menu
	 * 
	 * @return the configuration menu
	 */
	public ConfigurationMenu getConfiguration() {
		return _configuration;
	}

	/**
	 * Sets a new value to the configuration menu
	 * 
	 * @param configuration new value to set
	 */
	public void setConfiguration(ConfigurationMenu configuration) {
		_configuration = configuration;
	}

	/**
	 * Returns the project menu
	 * 
	 * @return the project menu
	 */
	public ProjectMenu getProject() {
		return _project;
	}

	/**
	 * Sets a new value to the project menu
	 * 
	 * @param project new value to set
	 */
	public void setProject(ProjectMenu project) {
		_project = project;
	}

	/**
	 * Returns the view menu
	 * 
	 * @return the view menu
	 */
	public ViewMenu getView() {
		return _view;
	}

	/**
	 * Sets a new value to the view menu
	 * 
	 * @param view new value to set
	 */
	public void setView(ViewMenu view) {
		_view = view;
	}

	/**
	 * Sets a new value to the menu bar
	 * 
	 * @param menuBar new value to set
	 */
	public void setMenuBar(JMenuBar menuBar) {
		_menuBar = menuBar;
	}

	/**
	 * Returns the is shell focused flag
	 * 
	 * @return the is shell focused flag
	 */
	public boolean isShellFocused() {
		return _isShellFocused;
	}

	/**
	 * Sets a new value to the is shell focused flag
	 * 
	 * @param shellIsFocused new value to set
	 */
	public void setIsShellFocus(boolean shellIsFocused) {
		_isShellFocused = shellIsFocused;
	}

	/**
	 * Returns the is NPF flag
	 * 
	 * @return the is NPF flag
	 */
	public boolean isNPF() {
		return _isNPF;
	}

	/**
	 * Sets a new value to the is NPF flag
	 * 
	 * @param isNPF new value to set
	 */
	public void setIsNPF(boolean isNPF) {
		_isNPF = isNPF;
	}

	/**
	 * Returns the file menu
	 * 
	 * @return the file menu
	 */
	public FileMenu getFile() {
		
		return _file;
	}
	
	/**
	 * Enables the edit menu
	 */
	public void enableEditMenu() {
		_edit.enableMenu();
	}
	
	/**
	 * Disables the edit menu
	 */
	public void disableEditMenu() {
		_edit.disableMenu();
	}
}
