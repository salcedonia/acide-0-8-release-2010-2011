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
package acide.gui.menuBar.projectMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.gui.menuBar.projectMenu.listeners.AcideAddFileMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideAddFolderMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideCloseProjecMenuItemtListener;
import acide.gui.menuBar.projectMenu.listeners.AcideCompileMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideDeleteFileMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideExecuteMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideNewProjectFileMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideNewProjectMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideOpenProjectMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideRemoveFileMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideRemoveFolderMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideSaveProjectAsMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideSaveProjectMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideSetCompilableFileMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideSetMainFileMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideUnsetCompilableFileMenuItemListener;
import acide.gui.menuBar.projectMenu.listeners.AcideUnsetMainFileMenuItemListener;
import acide.language.AcideLanguageManager;

/**																
 * ACIDE - A Configurable IDE project menu.											
 *					
 * @version 0.8	
 * @see JMenu																													
 */
public class AcideProjectMenu extends JMenu {

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
	 * ACIDE - A Configurable IDE project menu unset compilable menu item image icon.
	 */
	private static final ImageIcon UNSET_COMPILABLE_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/unsetCompilable.png");
	/**
	 * ACIDE - A Configurable IDE project menu close project menu item image icon.
	 */
	private static final ImageIcon CLOSE_PROJECT_IMAGE = new ImageIcon("./resources/icons/menu/project/closeProject.png");
	/**
	 * ACIDE - A Configurable IDE project menu remove file menu item image icon.
	 */
	private static final ImageIcon REMOVE_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/removeFile.png");
	/**
	 * ACIDE - A Configurable IDE project menu remove folder menu item image icon.
	 */
	private static final ImageIcon REMOVE_FOLDER_IMAGE = new ImageIcon("./resources/icons/menu/project/removeFolder.png");
	/**
	 * ACIDE - A Configurable IDE project menu save project as menu item image icon.
	 */
	private static final ImageIcon SAVE_PROJECT_AS_IMAGE = new ImageIcon("./resources/icons/menu/project/saveProjectAs.png");
	/**
	 * ACIDE - A Configurable IDE project menu new project menu item.
	 */
	private JMenuItem _newProjectMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu open project menu item.
	 */
	private JMenuItem _openProjectMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu save project menu item.
	 */
	private JMenuItem _saveProjectMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu new project file menu item.
	 */
	private JMenuItem _newProjectFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu save as project menu item.
	 */
	private JMenuItem _saveProjectAsMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu add file menu item.
	 */
	private JMenuItem _addFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu close project menu item.
	 */
	private JMenuItem _closeProjectMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu remove file menu item.
	 */
	private JMenuItem _removeFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu delete file menu item.
	 */
	private JMenuItem _deleteFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu add folder menu item.
	 */
	private JMenuItem _addFolderMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu remove folder menu item.
	 */
	private JMenuItem _removeFolderMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu set compilable menu item.
	 */
	private JMenuItem _setCompilableFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu unset compilable menu item.
	 */
	private JMenuItem _unsetCompilableFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu set main menu item.
	 */
	private JMenuItem _setMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu unset main menu item.
	 */
	private JMenuItem _unsetMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu compile menu item.
	 */
	private JMenuItem _compileMenuItem;
	/**
	 * ACIDE - A Configurable IDE project menu execute menu item.
	 */
	private JMenuItem _executeMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE project menu.
	 */
	public AcideProjectMenu() {

		// NEW PROJECT MENU ITEM
		_newProjectMenuItem = new JMenuItem(NEW_PROJECT_IMAGE);
		
		// OPEN PROJECT MENU ITEM
		_openProjectMenuItem = new JMenuItem(OPEN_PROJECT_IMAGE);
		
		// SAVE PROJECT MENU ITEM
		_saveProjectMenuItem = new JMenuItem(SAVE_PROJECT_IMAGE);
		
		// SAVE PROJECT AS MENU ITEM
		_saveProjectAsMenuItem = new JMenuItem(SAVE_PROJECT_AS_IMAGE);
		
		// NEW PROJECT FILE MENU ITEM
		_newProjectFileMenuItem = new JMenuItem(NEW_PROJECT_FILE_IMAGE);
		
		// ADD FILE MENU ITEM
		_addFileMenuItem = new JMenuItem(ADD_FILE_IMAGE);
		
		// REMOVE FILE MENU ITEM
		_removeFileMenuItem = new JMenuItem(REMOVE_FILE_IMAGE);
		
		// DELETE FILE MENU ITEM
		_deleteFileMenuItem = new JMenuItem(DELETE_FILE_IMAGE);
		
		// CLOSE PROJECT MENU ITEM
		_closeProjectMenuItem = new JMenuItem(CLOSE_PROJECT_IMAGE);
		
		// COMPILE MENU ITEM
		_compileMenuItem = new JMenuItem(COMPILE_IMAGE);
		
		// EXECUTE MENU ITEM
		_executeMenuItem = new JMenuItem(EXECUTE_IMAGE);
		
		// ADD FOLDER MENU ITEM
		_addFolderMenuItem = new JMenuItem(ADD_FOLDER_IMAGE);
		
		// REMOVE FOLDER MENU ITEM
		_removeFolderMenuItem = new JMenuItem(REMOVE_FOLDER_IMAGE);
		
		// SET MAIN FILE MENU ITEM
		_setMainFileMenuItem = new JMenuItem(SET_MAIN_FILE_IMAGE);
		
		// UNSET MAIN FILE MENU ITEM
		_unsetMainFileMenuItem = new JMenuItem(UNSET_MAIN_FILE_IMAGE);
		
		// SET COMPILABLE FILE MENU ITEM
		_setCompilableFileMenuItem = new JMenuItem(SET_COMPILABLE_FILE_IMAGE);
		
		// UNSET COMPILABLE FILE MENU ITEM
		_unsetCompilableFileMenuItem = new JMenuItem(UNSET_COMPILABLE_FILE_IMAGE);

		// Sets the text of the project menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE project menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Disables the menu
		disableMenu();

		// NEW PROJECT MENU ITEM
		_newProjectMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s14"));
		_newProjectMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// OPEN PROJECT MENU ITEM
		_openProjectMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s15"));
		_openProjectMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// SAVE PROJECT MENU ITEM
		_saveProjectMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s16"));
		_saveProjectMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// NEW PROJECT FILE MENU ITEM
		_newProjectFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s947"));

		// ADD FILE MENU ITEM
		_addFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s17"));
		_addFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// REMOVE FILE MENU ITEM
		_removeFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s218"));

		// DELETE FILE MENU ITEM
		_deleteFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s950"));

		// ADD FOLDER MENU ITEM
		_addFolderMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s219"));

		// REMOVE FOLDER MENU ITEM
		_removeFolderMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s220"));

		// SAVE AS PROJECT MENU ITEM
		_saveProjectAsMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s926"));

		// CLOSE PROJECT MENU ITEM
		_closeProjectMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s228"));

		// SET COMPILABLE MENU ITEM
		_setCompilableFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s254"));

		// UNSET COMPILABLE MENU ITEM
		_unsetCompilableFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s255"));

		// SET MAIN MENU ITEM
		_setMainFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s256"));

		// UNSET MAIN MENU ITEM
		_unsetMainFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s952"));

		// COMPILE MENU ITEM
		_compileMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s18"));
		_compileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.ALT_MASK));

		// EXECUTE MENU ITEM
		_executeMenuItem.setText(AcideLanguageManager.getInstance().getLabels().getString("s19"));
		_executeMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.ALT_MASK));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE project menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// NEW PROJECT MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME))
			add(_newProjectMenuItem);
		
		// OPEN PROJECT MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME))
			add(_openProjectMenuItem);
		
		// CLOSE PROJECT MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(CLOSE_PROJECT_NAME))
			add(_closeProjectMenuItem);
		
		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME) 
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(CLOSE_PROJECT_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME) 
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME)))
			addSeparator();
		
		// SAVE PROJECT MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME))
			add(_saveProjectMenuItem);
		
		// SAVE PROJECT AS MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME))
			add(_saveProjectAsMenuItem);
		
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
		
		// NEW PROJECT FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME))
			add(_newProjectFileMenuItem);
		
		// ADD FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME))
			add(_addFileMenuItem);
		
		// REMOVE FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME))
			add(_removeFileMenuItem);
		
		// DELETE FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME))
			add(_deleteFileMenuItem);
		
		// ADD FOLDER MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME))
			add(_addFolderMenuItem);
		
		// REMOVE FOLDER MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME))
			add(_removeFolderMenuItem);
		
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
		
		// COMPILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(COMPILE_NAME))
			add(_compileMenuItem);
		
		// EXECUTE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(EXECUTE_NAME))
			add(_executeMenuItem);
		
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
		
		// SET COMPILABLE FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SET_COMPILABLE_FILE_NAME))
			add(_setCompilableFileMenuItem);
		
		// UNSET COMPILABLE FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(UNSET_COMPILABLE_FILE_NAME))
			add(_unsetCompilableFileMenuItem);
		
		// SET MAIN FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SET_MAIN_FILE_NAME))
			add(_setMainFileMenuItem);
		
		// UNSET MAIN FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(UNSET_MAIN_FILE_NAME))
			add(_unsetMainFileMenuItem);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE project menu listeners.
	 */
	public void setListeners() {

		// NEW PROJECT MENU ITEM
		_newProjectMenuItem.addActionListener(new AcideNewProjectMenuItemListener());

		// SAVE PROJECT MENU ITEM
		_saveProjectMenuItem.addActionListener(new AcideSaveProjectMenuItemListener());

		// SAVE AS PROJECT MENU ITEM
		_saveProjectAsMenuItem.addActionListener(new AcideSaveProjectAsMenuItemListener());

		// OPEN PROJECT MENU ITEM
		_openProjectMenuItem.addActionListener(new AcideOpenProjectMenuItemListener());

		// ADD FILE MENU ITEM
		_addFileMenuItem.addActionListener(new AcideAddFileMenuItemListener());

		// EXECUTE MENU ITEM
		_executeMenuItem.addActionListener(new AcideExecuteMenuItemListener());

		// COMPILE MENU ITEM
		_compileMenuItem.addActionListener(new AcideCompileMenuItemListener());

		// REMOVE FILE MENU ITEM
		_removeFileMenuItem.addActionListener(new AcideRemoveFileMenuItemListener());

		// CLOSE PROJECT MENU ITEM
		_closeProjectMenuItem.addActionListener(new AcideCloseProjecMenuItemtListener());

		// DELETE FILE MENU ITEM
		_deleteFileMenuItem.addActionListener(new AcideDeleteFileMenuItemListener());

		// NEW PROJECT FILE MENU ITEM
		_newProjectFileMenuItem.addActionListener(new AcideNewProjectFileMenuItemListener());

		// ADD FOLDER MENU ITEM
		_addFolderMenuItem.addActionListener(new AcideAddFolderMenuItemListener());

		// REMOVE FOLDER MENU ITEM
		_removeFolderMenuItem.addActionListener(new AcideRemoveFolderMenuItemListener());

		// SET MAIN MENU ITEM
		_setMainFileMenuItem.addActionListener(new AcideSetMainFileMenuItemListener());

		// UNSET MAIN MENU ITEM
		_unsetMainFileMenuItem.addActionListener(new AcideUnsetMainFileMenuItemListener());

		// SET COMPILABLE MENU ITEM
		_setCompilableFileMenuItem.addActionListener(new AcideSetCompilableFileMenuItemListener());

		// UNSET COMPILABLE MENU ITEM
		_unsetCompilableFileMenuItem.addActionListener(new AcideUnsetCompilableFileMenuItemListener());
	}

	/**
	 * Enables the ACIDE - A Configurable IDE project menu.
	 */
	public void enableMenu() {

		_closeProjectMenuItem.setEnabled(true);
		_saveProjectMenuItem.setEnabled(false);
		_saveProjectAsMenuItem.setEnabled(true);
		_newProjectFileMenuItem.setEnabled(true);
		_addFileMenuItem.setEnabled(true);
		_removeFileMenuItem.setEnabled(false);
		_deleteFileMenuItem.setEnabled(false);
		_addFolderMenuItem.setEnabled(true);
		_removeFolderMenuItem.setEnabled(false);
		_compileMenuItem.setEnabled(true);
		_executeMenuItem.setEnabled(true);
		_setMainFileMenuItem.setEnabled(false);
		_unsetMainFileMenuItem.setEnabled(false);
		_setCompilableFileMenuItem.setEnabled(false);
		_unsetCompilableFileMenuItem.setEnabled(false);
	}

	/**
	 * Disables the ACIDE - A Configurable IDE project menu.
	 */
	public void disableMenu() {

		_closeProjectMenuItem.setEnabled(false);
		_saveProjectMenuItem.setEnabled(false);
		_saveProjectAsMenuItem.setEnabled(false);
		_newProjectFileMenuItem.setEnabled(false);
		_addFileMenuItem.setEnabled(false);
		_removeFileMenuItem.setEnabled(false);
		_deleteFileMenuItem.setEnabled(false);
		_addFolderMenuItem.setEnabled(false);
		_removeFolderMenuItem.setEnabled(false);
		_compileMenuItem.setEnabled(false);
		_executeMenuItem.setEnabled(false);
		_setMainFileMenuItem.setEnabled(false);
		_unsetMainFileMenuItem.setEnabled(false);
		_setCompilableFileMenuItem.setEnabled(false);
		_unsetCompilableFileMenuItem.setEnabled(false);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu new project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu new project menu item.
	 */
	public JMenuItem getNewProjectMenuItem() {
		return _newProjectMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu save project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu save project menu item.
	 */
	public JMenuItem getSaveProjectMenuItem() {
		return _saveProjectMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu open project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu open project menu item.
	 */
	public JMenuItem getOpenProjectMenuItem() {
		return _openProjectMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu new project file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu new project file menu item.
	 */
	public JMenuItem getNewProjectFileMenuItem() {
		return _newProjectFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu add file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu add file menu item.
	 */
	public JMenuItem getAddFileMenuItem() {
		return _addFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu add folder menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu add folder menu item.
	 */
	public JMenuItem getAddFolderMenuItem() {
		return _addFolderMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu compile menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu compile menu item.
	 */
	public JMenuItem getCompileMenuItem() {
		return _compileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu execute menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu execute menu item.
	 */
	public JMenuItem getExecuteMenuItem() {
		return _executeMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu remove folder menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu remove folder menu item.
	 */
	public JMenuItem getRemoveFolderMenuItem() {
		return _removeFolderMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu remove file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu remove file menu item.
	 */
	public JMenuItem getRemoveFileMenuItem() {
		return _removeFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu close project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu close project menu item.
	 */
	public JMenuItem getCloseProjectMenuItem() {
		return _closeProjectMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu set compilable file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu set compilable file menu item.
	 */
	public JMenuItem getSetCompilableFileMenuItem() {
		return _setCompilableFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu set main file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu set main file menu item.
	 */
	public JMenuItem getSetMainFileMenuItem() {
		return _setMainFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu unset compilable file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu unset compilable file menu item.
	 */
	public JMenuItem getUnsetCompilableFileMenuItem() {
		return _unsetCompilableFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu save as project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu save as project menu item.
	 */
	public JMenuItem getSaveAsProjectMenuItem() {
		return _saveProjectAsMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu delete file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu delete file menu item.
	 */
	public JMenuItem getDeleteFileMenuItem() {
		return _deleteFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project menu unset main file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE project menu unset main file menu item.
	 */
	public JMenuItem getUnsetMainFileMenuItem() {
		return _unsetMainFileMenuItem;
	}
}