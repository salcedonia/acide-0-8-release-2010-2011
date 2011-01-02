/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package gui.menuBar;

import javax.swing.*;

import language.AcideLanguageManager;

import es.configuration.menu.AcideMenuConfiguration;

import operations.log.AcideLog;
import resources.AcideResourceManager;

import java.awt.HeadlessException;
import java.util.ResourceBundle;

import gui.menuBar.configurationMenu.ConfigurationMenu;
import gui.menuBar.editMenu.EditMenu;
import gui.menuBar.fileMenu.FileMenu;
import gui.menuBar.helpMenu.HelpMenu;
import gui.menuBar.listeners.MenuMouseClickListener;
import gui.menuBar.projectMenu.ProjectMenu;
import gui.menuBar.viewMenu.ViewMenu;

/**																
 * ACIDE - A Configurable IDE menu.
 *					
 * @version 0.8																														
 */
public class Menu {

	/**
	 * ACIDE - A Configurable IDE menu bar.
	 */
	private JMenuBar _menuBar;
	/**
	 * ACIDE - A Configurable IDE menu file menu.
	 */
	private FileMenu _file;
	/**
	 * ACIDE - A Configurable IDE menu edit menu.
	 */
	private EditMenu _edit;
	/**
	 * ACIDE - A Configurable IDE menu project menu.
	 */
	private ProjectMenu _project;
	/**
	 * ACIDE - A Configurable IDE menu view menu.
	 */
	private ViewMenu _view;
	/**
	 * ACIDE - A Configurable IDE menu configuration menu.
	 */
	private ConfigurationMenu _configuration;
	/**
	 * ACIDE - A Configurable IDE menu help menu.
	 */
	private HelpMenu _help;
	/**
	 * Indicates if the console is focused.
	 */
	private boolean _isConsoleFocused;
	/**
	 * Indicates if ?
	 */
	private boolean _isNPF;

	/**
	 * Creates a new ACIDE - A Configurable IDE menu.
	 */
	public Menu() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s68"));
		
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
		AcideLog.getLog().info(labels.getString("s69"));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE menu labels to display.
	 */
	public void setLanguageLabels() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
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
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu.
	 */
	public void buildMenu() {
			
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		String currentMenu = null;
		
		try {
			
			// Gets the menu configuration
			currentMenu = AcideResourceManager
					.getInstance().getProperty("currentMenuConfiguration");
			
			// Sets the new menu item list
			AcideMenuConfiguration.getInstance().setMenuElementList(AcideMenuConfiguration
					.getInstance().loadMenuConfigurationFile(currentMenu));
			
			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("currentMenuConfiguration",
					currentMenu);
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s70") + " " + currentMenu);
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s71") + exception.getMessage());
			
			// Gets the name
			String currentMenu2;
			int index = currentMenu.lastIndexOf("\\");
			if (index == -1)
				index = currentMenu.lastIndexOf("/");
			currentMenu2 = ".\\configuration\\menu\\"
					+ currentMenu.substring(index + 1, currentMenu.length());
			
			try {
				
				// Sets the new menu item list
				AcideMenuConfiguration.getInstance().setMenuElementList(AcideMenuConfiguration.getInstance().loadMenuConfigurationFile(currentMenu2));
				
				// Updates the RESOURCES MANAGER
				AcideResourceManager.getInstance().setProperty("currentMenuConfiguration",
						currentMenu2);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s70") + " " + currentMenu2);
				
				// Error message
				JOptionPane
						.showMessageDialog(null, labels.getString("s956")
								+ currentMenu + labels.getString("s957")
								+ currentMenu2);
			} catch (Exception exception1) {
				try {
					
					// Updates the log
					AcideLog.getLog().error(exception1.getMessage());
					exception1.printStackTrace();
					
					// Loads the new menu item list
					AcideMenuConfiguration.getInstance().setMenuElementList(AcideMenuConfiguration
							.getInstance().loadMenuConfigurationFile("./configuration/menu/defaultAllOn.menuCfg"));
					
					// Updates the RESOURCE MANAGER
					AcideResourceManager.getInstance().setProperty("currentMenuConfiguration",
							"./configuration/menu/defaultAllOn.menuCfg");
					
					// Error message
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s956") + currentMenu
									+ labels.getString("s959"));
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
		}

		_menuBar.removeAll();

		_file.buildMenu(labels,language);
		_edit.buildMenu();
		_project.buildMenu();
		_view.buildMenu();
		_configuration.buildMenu();		
		_help.buildMenu();
		
		// FILE MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(FileMenu.NEW_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(FileMenu.OPEN_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(FileMenu.OPEN_ALL_FILES_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(FileMenu.SAVE_FILE_AS_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(FileMenu.SAVE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(FileMenu.SAVE_ALL_FILES_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(FileMenu.PRINT_FILE_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(FileMenu.EXIT_NAME))
			_menuBar.add(_file);
		
		// EDIT MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.UNDO_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.REDO_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.COPY_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.PASTE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.CUT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.SELECT_ALL_FILES_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.GO_TO_LINE_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.SEARCH_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EditMenu.REPLACE_NAME))
			_menuBar.add(_edit);
		
		// PROJECT MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.NEW_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.OPEN_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.SAVE_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.CLOSE_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.ADD_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.REMOVE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.DELETE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.NEW_PROJECT_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.ADD_FOLDER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.REMOVE_FOLDER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.COMPILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ProjectMenu.EXECUTE_NAME))
			_menuBar.add(_project);
		
		// VIEW MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ViewMenu.SHOW_LOG_TAB_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ViewMenu.SHOW_EXPLORER_PANEL_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ViewMenu.SHOW_CONSOLE_PANEL_NAME))
			_menuBar.add(_view);
		
		// CONFIGURATION
		_menuBar.add(_configuration);
		
		// HELP MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(HelpMenu.SHOW_HELP_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(HelpMenu.SHOW_ABOUT_US_NAME))
			_menuBar.add(_help);
		
		// Shows the menu bar
		_menuBar.setVisible(true);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE menu listeners.
	 */
	public void setListeners() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
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
		AcideLog.getLog().info(labels.getString("s72"));
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu bar.
	 * 
	 * @return the ACIDE - A Configurable IDE menu bar.
	 */
	public JMenuBar getMenuBar() {
		return _menuBar;
	}
	
	/**
	 * Enables the ACIDE - A Configurable IDE menu file menu.
	 */
	public void enableFileMenu() {
		_file.enableMenu();
	}

	/**
	 * Disables the ACIDE - A Configurable IDE menu file menu.
	 */
	public void disableFileMenu() {
		_file.disableMenu();	
	}
	
	/**
	 * Enables the ACIDE - A Configurable IDE menu project menu.
	 */
	public void enableProjectMenu() {
		_project.enableMenu();
	}

	/**
	 * Disables the ACIDE - A Configurable IDE menu project menu.
	 */
	public void disableProjectMenu() {
		_project.disableMenu();		
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu file menu.
	 * 
	 * @param file new value to set.
	 */
	public void setFile(FileMenu file) {
		_file = file;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE menu help menu.
	 * 
	 * @return the ACIDE - A Configurable IDE menu help menu.
	 */
	public HelpMenu getHelp() {
		return _help;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu help menu.
	 * 
	 * @param help new value to set.
	 */
	public void setHelp(HelpMenu help) {
		_help = help;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE menu edit menu.
	 * 
	 * @return the ACIDE - A Configurable IDE menu edit menu.
	 */
	public EditMenu getEdit() {
		return _edit;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu edit menu item.
	 * 
	 * @param edit new value to set.
	 */
	public void setEdit(EditMenu edit) {
		_edit = edit;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu configuration menu.
	 * 
	 * @return the ACIDE - A Configurable IDE menu configuration menu.
	 */
	public ConfigurationMenu getConfiguration() {
		return _configuration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu configuration menu.
	 * 
	 * @param configuration new value to set.
	 */
	public void setConfiguration(ConfigurationMenu configuration) {
		_configuration = configuration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu project menu.
	 * 
	 * @return the ACIDE - A Configurable IDE menu project menu.
	 */
	public ProjectMenu getProject() {
		return _project;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu project menu.
	 * 
	 * @param project new value to set.
	 */
	public void setProject(ProjectMenu project) {
		_project = project;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu view menu.
	 * 
	 * @return the ACIDE - A Configurable IDE menu view menu.
	 */
	public ViewMenu getView() {
		return _view;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu view menu.
	 * 
	 * @param view new value to set.
	 */
	public void setView(ViewMenu view) {
		_view = view;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu bar.
	 * 
	 * @param menuBar new value to set.
	 */
	public void setMenuBar(JMenuBar menuBar) {
		_menuBar = menuBar;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu is shell focused flag.
	 * 
	 * @return the ACIDE - A Configurable IDE menu is shell focused flag.
	 */
	public boolean getIsConsoleFocused() {
		return _isConsoleFocused;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu is console focused flag.
	 * 
	 * @param consoleIsFocused new value to set.
	 */
	public void setIsConsoleFocused(boolean consoleIsFocused) {
		_isConsoleFocused = consoleIsFocused;
	}

	/**
	 * Returns the is NPF flag.
	 * 
	 * @return the is NPF flag.
	 */
	public boolean isNPF() {
		return _isNPF;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE menu is NPF flag.
	 * 
	 * @param isNPF new value to set.
	 */
	public void setIsNPF(boolean isNPF) {
		_isNPF = isNPF;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu file menu.
	 * 
	 * @return the ACIDE - A Configurable IDE menu file menu.
	 */
	public FileMenu getFile() {	
		return _file;
	}
	
	/**
	 * Enables the ACIDE - A Configurable IDE menu edit menu.
	 */
	public void enableEditMenu() {
		_edit.enableMenu();
	}
	
	/**
	 * Disables the ACIDE - A Configurable IDE menu edit menu.
	 */
	public void disableEditMenu() {
		_edit.disableMenu();
	}
}
