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
package gui.menuBar.projectMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.menu.AcideMenuConfiguration;
import gui.menuBar.projectMenu.listeners.AddFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.AddFolderMenuItemListener;
import gui.menuBar.projectMenu.listeners.CloseProjecMenuItemtListener;
import gui.menuBar.projectMenu.listeners.CompileMenuItemListener;
import gui.menuBar.projectMenu.listeners.DeleteFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.ExecuteMenuItemListener;
import gui.menuBar.projectMenu.listeners.NewProjectFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.NewProjectMenuItemListener;
import gui.menuBar.projectMenu.listeners.OpenProjectMenuItemListener;
import gui.menuBar.projectMenu.listeners.RemoveFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.RemoveFolderMenuItemListener;
import gui.menuBar.projectMenu.listeners.SaveAsProjectMenuItemListener;
import gui.menuBar.projectMenu.listeners.SaveProjectMenuItemListener;
import gui.menuBar.projectMenu.listeners.SetCompilableFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.SetMainFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.UnsetCompilableFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.UnsetMainFileMenuItemListener;

/**																
 * ACIDE - A Configurable IDE project menu.											
 *					
 * @version 0.8	
 * @see JMenu																													
 */
public class ProjectMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE project menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE project menu new project menu item name.
	 */
	public static final String NEW_PROJECT_NAME = "New Project";
	/**
	 * ACIDE - A Configurable IDE project menu open project menu item name.
	 */
	public static final String OPEN_PROJECT_NAME = "Open Project";
	/**
	 * ACIDE - A Configurable IDE project menu save project menu item name.
	 */
	public static final String SAVE_PROJECT_NAME = "Save Project";
	/**
	 * ACIDE - A Configurable IDE project menu save project as menu item image icon.
	 */
	public static final String SAVE_PROJECT_AS_NAME = "Save Project As";
	/**
	 * ACIDE - A Configurable IDE project menu new project file menu item name.
	 */
	public static final String NEW_PROJECT_FILE_NAME = "New Project File";
	/**
	 * ACIDE - A Configurable IDE project menu add file menu item name.
	 */
	public static final String ADD_FILE_NAME = "Add File";
	/**
	 * ACIDE - A Configurable IDE project menu add folder menu item name.
	 */
	public static final String ADD_FOLDER_NAME = "Add Folder";
	/**
	 * ACIDE - A Configurable IDE project menu remove folder menu item name.
	 */
	public static final String REMOVE_FOLDER_NAME = "Remove Folder";
	/**
	 * ACIDE - A Configurable IDE project menu remove file menu item name.
	 */
	public static final String REMOVE_FILE_NAME = "Remove File";
	/**
	 * ACIDE - A Configurable IDE project menu delete file menu item name.
	 */
	public static final String DELETE_FILE_NAME = "Delete File";
	/**
	 * ACIDE - A Configurable IDE project menu close project menu item name.
	 */
	public static final String CLOSE_PROJECT_NAME = "Close Project";
	/**
	 * ACIDE - A Configurable IDE project menu compile menu item name.
	 */
	public static final String COMPILE_NAME = "Compile";
	/**
	 * ACIDE - A Configurable IDE project menu execute menu item name.
	 */
	public static final String EXECUTE_NAME = "Execute";
	/**
	 * ACIDE - A Configurable IDE project menu set main file menu item name.
	 */
	public static final String SET_MAIN_FILE_NAME = "Set Main File";
	/**
	 * ACIDE - A Configurable IDE project menu unset main file menu item name.
	 */
	public static final String UNSET_MAIN_FILE_NAME = "Unset Main File";
	/**
	 * ACIDE - A Configurable IDE project menu set compilable file menu item name.
	 */
	public static final String SET_COMPILABLE_FILE_NAME = "Set Compilable File";
	/**
	 * ACIDE - A Configurable IDE project menu unset compilable file menu item name.
	 */
	public static final String UNSET_COMPILABLE_FILE_NAME = "Unset Compilable File";
	/**
	 * ACIDE - A Configurable IDE project menu new project menu item image icon.
	 */
	private static final ImageIcon NEW_PROJECT_IMAGE = new ImageIcon("./resources/icons/menu/project/newProject.png");
	/**
	 * ACIDE - A Configurable IDE project menu open project menu item image icon.
	 */
	private static final ImageIcon OPEN_PROJECT_IMAGE = new ImageIcon("./resources/icons/menu/project/openProject.png");
	/**
	 * ACIDE - A Configurable IDE project menu save project menu item image icon.
	 */
	private static final ImageIcon SAVE_PROJECT_IMAGE = new ImageIcon("./resources/icons/menu/project/saveProject.png");
	/**
	 * ACIDE - A Configurable IDE project menu new project file menu item image icon.
	 */
	private static final ImageIcon NEW_PROJECT_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/newFile.png");
	/**
	 * ACIDE - A Configurable IDE project menu add file menu item image icon.
	 */
	private static final ImageIcon ADD_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/addFile.png");
	/**
	 * ACIDE - A Configurable IDE project menu add folder menu item image icon.
	 */
	private static final ImageIcon ADD_FOLDER_IMAGE = new ImageIcon("./resources/icons/menu/project/addFolder.png");
	/**
	 * ACIDE - A Configurable IDE project menu delete file menu item main icon.
	 */
	private static final ImageIcon DELETE_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/deleteFile.png");
	/**
	 * ACIDE - A Configurable IDE project menu compile menu item image icon.
	 */
	private static final ImageIcon COMPILE_IMAGE = new ImageIcon("./resources/icons/menu/project/compile.png");
	/**
	 * ACIDE - A Configurable IDE project menu execute menu item image icon.
	 */
	private static final ImageIcon EXECUTE_IMAGE = new ImageIcon("./resources/icons/menu/project/execute.png");
	/**
	 * ACIDE - A Configurable IDE project menu set main menu item image icon.
	 */
	private static final ImageIcon SET_MAIN_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/setMain.png");
	/**
	 * ACIDE - A Configurable IDE project menu unset main menu item image icon.
	 */
	private static final ImageIcon UNSET_MAIN_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/unsetMain.png");
	/**
	 * ACIDE - A Configurable IDE project menu set compilable menu item image icon.
	 */
	private static final ImageIcon SET_COMPILABLE_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/setCompilable.png");
	/**
	 * ACIDE - A Configurable IDE project menu unset compilable project menu item image icon.
	 */
	private static final ImageIcon UNSET_COMPILABLE_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/unsetCompilable.png");
	/**
	 * ACIDE - A Configurable IDE project menu new project menu item.
	 */
	private JMenuItem _newProject;
	/**
	 * ACIDE - A Configurable IDE project menu open project menu item.
	 */
	private JMenuItem _openProject;
	/**
	 * ACIDE - A Configurable IDE project menu save project menu item.
	 */
	private JMenuItem _saveProject;
	/**
	 * ACIDE - A Configurable IDE project menu new project file menu item.
	 */
	private JMenuItem _newProjectFile;
	/**
	 * ACIDE - A Configurable IDE project menu save as project menu item.
	 */
	private JMenuItem _saveProjectAs;
	/**
	 * ACIDE - A Configurable IDE project menu add file menu item.
	 */
	private JMenuItem _addFile;
	/**
	 * ACIDE - A Configurable IDE project menu close project menu item.
	 */
	private JMenuItem _closeProject;
	/**
	 * ACIDE - A Configurable IDE project menu remove file menu item.
	 */
	private JMenuItem _removeFile;
	/**
	 * ACIDE - A Configurable IDE project menu delete file menu item.
	 */
	private JMenuItem _deleteFile;
	/**
	 * ACIDE - A Configurable IDE project menu add folder menu item.
	 */
	private JMenuItem _addFolder;
	/**
	 * ACIDE - A Configurable IDE project menu remove folder menu item.
	 */
	private JMenuItem _removeFolder;
	/**
	 * ACIDE - A Configurable IDE project menu set compilable menu item.
	 */
	private JMenuItem _setCompilableFile;
	/**
	 * ACIDE - A Configurable IDE project menu unset compilable menu item.
	 */
	private JMenuItem _unsetCompilableFile;
	/**
	 * ACIDE - A Configurable IDE project menu set main menu item.
	 */
	private JMenuItem _setMainFile;
	/**
	 * ACIDE - A Configurable IDE project menu unset main menu item.
	 */
	private JMenuItem _unsetMainFile;
	/**
	 * ACIDE - A Configurable IDE project menu compile menu item.
	 */
	private JMenuItem _compile;
	/**
	 * ACIDE - A Configurable IDE project menu execute menu item.
	 */
	private JMenuItem _execute;

	/**
	 * Creates a new ACIDE - A Configurable IDE project menu.
	 */
	public ProjectMenu() {

		// MENU ITEM
		_newProject = new JMenuItem(NEW_PROJECT_IMAGE);
		_openProject = new JMenuItem(OPEN_PROJECT_IMAGE);
		_saveProject = new JMenuItem(SAVE_PROJECT_IMAGE);
		_saveProjectAs = new JMenuItem();
		_newProjectFile = new JMenuItem(NEW_PROJECT_FILE_IMAGE);
		_addFile = new JMenuItem(ADD_FILE_IMAGE);
		_removeFile = new JMenuItem();
		_deleteFile = new JMenuItem(DELETE_FILE_IMAGE);
		_closeProject = new JMenuItem();
		_compile = new JMenuItem(COMPILE_IMAGE);
		_execute = new JMenuItem(EXECUTE_IMAGE);
		_addFolder = new JMenuItem(ADD_FOLDER_IMAGE);
		_removeFolder = new JMenuItem();
		_setMainFile = new JMenuItem(SET_MAIN_FILE_IMAGE);
		_unsetMainFile = new JMenuItem(UNSET_MAIN_FILE_IMAGE);
		_setCompilableFile = new JMenuItem(SET_COMPILABLE_FILE_IMAGE);
		_unsetCompilableFile = new JMenuItem(UNSET_COMPILABLE_FILE_IMAGE);

		setLanguageLabels();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE project menu language labels.
	 * 
	 * @param labels labels to display in the selected language.
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

		disableMenu();

		// NEW PROJECT
		_newProject.setText(labels.getString("s14"));
		_newProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// OPEN PROJECT
		_openProject.setText(labels.getString("s15"));
		_openProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// SAVE PROJECT
		_saveProject.setText(labels.getString("s16"));
		_saveProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// NEW PROJECT FILE
		_newProjectFile.setText(labels.getString("s947"));

		// ADD FILE
		_addFile.setText(labels.getString("s17"));
		_addFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// REMOVE FILE
		_removeFile.setText(labels.getString("s218"));

		// DELETE FILE
		_deleteFile.setText(labels.getString("s950"));

		// ADD FOLDER
		_addFolder.setText(labels.getString("s219"));

		// REMOVE FOLDER
		_removeFolder.setText(labels.getString("s220"));

		// SAVE AS PROJECT
		_saveProjectAs.setText(labels.getString("s926"));

		// CLOSE PROJECT
		_closeProject.setText(labels.getString("s228"));

		// SET COMPILABLE
		_setCompilableFile.setText(labels.getString("s254"));

		// UNSET COMPILABLE
		_unsetCompilableFile.setText(labels.getString("s255"));

		// SET MAIN
		_setMainFile.setText(labels.getString("s256"));

		// UNSET MAIN
		_unsetMainFile.setText(labels.getString("s952"));

		// COMPILE
		_compile.setText(labels.getString("s18"));
		_compile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.ALT_MASK));

		// EXECUTE
		_execute.setText(labels.getString("s19"));
		_execute.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.ALT_MASK));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE project menu.
	 */
	public void buildMenu() {

		removeAll();

		// NEW PROJECT
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME))
			add(_newProject);
		
		// OPEN PROJECT
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME))
			add(_openProject);
		
		// CLOSE PROJECT
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(CLOSE_PROJECT_NAME))
			add(_closeProject);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(CLOSE_PROJECT_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME)))
			addSeparator();
		
		// SAVE PROJECT
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME))
			add(_saveProject);
		
		// SAVE PROJECT AS
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME))
			add(_saveProjectAs);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(CLOSE_PROJECT_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME)))
			addSeparator();
		
		// NEW PROJECT FILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME))
			add(_newProjectFile);
		
		// ADD FILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME))
			add(_addFile);
		
		// REMOVE FILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME))
			add(_removeFile);
		
		// DELETE FILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME))
			add(_deleteFile);
		
		// ADD FOLDER
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME))
			add(_addFolder);
		
		// REMOVE FOLDER
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME))
			add(_removeFolder);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(COMPILE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(EXECUTE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(SET_COMPILABLE_FILE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(UNSET_COMPILABLE_FILE_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(SET_MAIN_FILE_NAME)))
			addSeparator();
		
		// COMPILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(COMPILE_NAME))
			add(_compile);
		
		// EXECUTE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(EXECUTE_NAME))
			add(_execute);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(COMPILE_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(EXECUTE_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(SET_COMPILABLE_FILE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(UNSET_COMPILABLE_FILE_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(SET_MAIN_FILE_NAME)))
			addSeparator();
		
		// SET COMPILABLE FILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SET_COMPILABLE_FILE_NAME))
			add(_setCompilableFile);
		
		// UNSET COMPILABLE FILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(UNSET_COMPILABLE_FILE_NAME))
			add(_unsetCompilableFile);
		
		// SET MAIN FILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SET_MAIN_FILE_NAME))
			add(_setMainFile);
		
		// UNSET MAIN FILE
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(UNSET_MAIN_FILE_NAME))
			add(_unsetMainFile);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE project menu listeners.
	 */
	public void setListeners() {

		// NEW PROJECT
		_newProject.addActionListener(new NewProjectMenuItemListener());

		// SAVE PROJECT
		_saveProject.addActionListener(new SaveProjectMenuItemListener());

		// SAVE AS PROJECT
		_saveProjectAs.addActionListener(new SaveAsProjectMenuItemListener());

		// OPEN PROJECT
		_openProject.addActionListener(new OpenProjectMenuItemListener());

		// ADD FILE
		_addFile.addActionListener(new AddFileMenuItemListener());

		// EXECUTE
		_execute.addActionListener(new ExecuteMenuItemListener());

		// COMPILE
		_compile.addActionListener(new CompileMenuItemListener());

		// REMOVE FILE
		_removeFile.addActionListener(new RemoveFileMenuItemListener());

		// CLOSE PROJECT
		_closeProject.addActionListener(new CloseProjecMenuItemtListener());

		// DELETE FILE
		_deleteFile.addActionListener(new DeleteFileMenuItemListener());

		// NEW PROJECT FILE
		_newProjectFile.addActionListener(new NewProjectFileMenuItemListener());

		// ADD FOLDER
		_addFolder.addActionListener(new AddFolderMenuItemListener());

		// REMOVE FOLDER
		_removeFolder.addActionListener(new RemoveFolderMenuItemListener());

		// SET MAIN
		_setMainFile.addActionListener(new SetMainFileMenuItemListener());

		// UNSET MAIN
		_unsetMainFile.addActionListener(new UnsetMainFileMenuItemListener());

		// SET COMPILABLE
		_setCompilableFile.addActionListener(new SetCompilableFileMenuItemListener());

		// UNSET COMPILABLE
		_unsetCompilableFile.addActionListener(new UnsetCompilableFileMenuItemListener());
	}

	/**
	 * Enables the ACIDE - A Configurable IDE project menu.
	 */
	public void enableMenu() {

		_closeProject.setEnabled(true);
		_saveProject.setEnabled(false);
		_saveProjectAs.setEnabled(true);
		_newProjectFile.setEnabled(true);
		_addFile.setEnabled(true);
		_removeFile.setEnabled(false);
		_deleteFile.setEnabled(false);
		_addFolder.setEnabled(true);
		_removeFolder.setEnabled(false);
		_compile.setEnabled(true);
		_execute.setEnabled(true);
		_setMainFile.setEnabled(false);
		_unsetMainFile.setEnabled(false);
		_setCompilableFile.setEnabled(false);
		_unsetCompilableFile.setEnabled(false);
	}

	/**
	 * Disables the ACIDE - A Configurable IDE project menu.
	 */
	public void disableMenu() {

		_closeProject.setEnabled(false);
		_saveProject.setEnabled(false);
		_saveProjectAs.setEnabled(false);
		_newProjectFile.setEnabled(false);
		_addFile.setEnabled(false);
		_removeFile.setEnabled(false);
		_deleteFile.setEnabled(false);
		_addFolder.setEnabled(false);
		_removeFolder.setEnabled(false);
		_compile.setEnabled(false);
		_execute.setEnabled(false);
		_setMainFile.setEnabled(false);
		_unsetMainFile.setEnabled(false);
		_setCompilableFile.setEnabled(false);
		_unsetCompilableFile.setEnabled(false);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu new project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu new project menu item.
	 */
	public JMenuItem getNewProject() {
		return _newProject;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu save project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu save project menu item.
	 */
	public JMenuItem getSaveProject() {
		return _saveProject;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu open project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu open project menu item.
	 */
	public JMenuItem getOpenProject() {
		return _openProject;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu new project file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu new project file menu item.
	 */
	public JMenuItem getNewProjectFile() {
		return _newProjectFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu add file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu add file menu item.
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu add folder menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu add folder menu item.
	 */
	public JMenuItem getAddFolder() {
		return _addFolder;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu add folder menu item.
	 * 
	 * @param addFolder new value to set.
	 */
	public void setAddFolder(JMenuItem addFolder) {
		_addFolder = addFolder;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu compile menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu compile menu item.
	 */
	public JMenuItem getCompile() {
		return _compile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu compile menu item.
	 * 
	 * @param compile new value to set.
	 */
	public void setCompile(JMenuItem compile) {
		_compile = compile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu execute menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu execute menu item.
	 */
	public JMenuItem getExecute() {
		return _execute;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu execute menu item.
	 * 
	 * @param execute new value to set.
	 */
	public void setExecute(JMenuItem execute) {
		_execute = execute;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu remove file menu item.
	 * 
	 * @param removeFile new value to set.
	 */
	public void setRemoveFile(JMenuItem removeFile) {
		_removeFile = removeFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu remove folder menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu remove folder menu item.
	 */
	public JMenuItem getRemoveFolder() {
		return _removeFolder;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu remove folder menu item.
	 * 
	 * @param removeFolder new value to set.
	 */
	public void setRemoveFolder(JMenuItem removeFolder) {
		_removeFolder = removeFolder;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu open project menu item.
	 * 
	 * @param openProject new value to set.
	 */
	public void setOpenProject(JMenuItem openProject) {
		_openProject = openProject;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu remove file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu remove file menu item.
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu close project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu close project menu item.
	 */
	public JMenuItem getCloseProject() {
		return _closeProject;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu close project menu item.
	 * 
	 * @param closeProject new value to set.
	 */
	public void setCloseProject(JMenuItem closeProject) {
		_closeProject = closeProject;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu new project menu item.
	 * 
	 * @param newProjectFile new value to set.
	 */
	public void setNewProjectFile(JMenuItem newProjectFile) {
		_newProjectFile = newProjectFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu add file menu item.
	 * 
	 * @param addFile new value to set.
	 */
	public void setAddFile(JMenuItem addFile) {
		_addFile = addFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu save project menu item.
	 * 
	 * @param saveProject new value to set.
	 */
	public void setSaveProject(JMenuItem saveProject) {
		_saveProject = saveProject;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu new project menu item.
	 *  
	 * @param newProject new value to set.
	 */
	public void setNewProject(JMenuItem newProject) {
		_newProject = newProject;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu set compilable menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu set compilable menu item.
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilableFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu set compilable menu item.
	 * 
	 * @param setCompilable new value to set.
	 */
	public void setSetCompilable(JMenuItem setCompilable) {
		_setCompilableFile = setCompilable;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu set main menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu set main menu item.
	 */
	public JMenuItem getSetMain() {
		return _setMainFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu set main menu item.
	 * 
	 * @param setMain new value to set.
	 */
	public void setSetMain(JMenuItem setMain) {
		_setMainFile = setMain;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu unset compilable menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu unset compilable menu item.
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilableFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu unset compilable menu item.
	 * 
	 * @param unsetFile new value to set.
	 */
	public void setUnsetCompilable(JMenuItem unsetFile) {
		_unsetCompilableFile = unsetFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu save as project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu save as project menu item.
	 */
	public JMenuItem getSaveAsProject() {
		return _saveProjectAs;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu save as project menu item.
	 * 
	 * @param saveAsProject new value to set.
	 */
	public void setSaveAsProject(JMenuItem saveAsProject) {
		_saveProjectAs = saveAsProject;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu delete file menu item.
	 * 
	 * @param deleteFile new value to set.
	 */
	public void setDeleteFile(JMenuItem deleteFile) {
		_deleteFile = deleteFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu delete file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu delete file menu item.
	 */
	public JMenuItem getDeleteFile() {
		return _deleteFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu unset main menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu unset main menu item.
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMainFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project menu unset main menu item.
	 * 
	 * @param unsetMain new value to set.
	 */
	public void setUnsetMain(JMenuItem unsetMain) {
		_unsetMainFile = unsetMain;
	}
}