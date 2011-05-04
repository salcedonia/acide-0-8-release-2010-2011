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
package acide.gui.explorerPanel.popup;

import acide.gui.explorerPanel.popup.listeners.AcideAddFileMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideAddFolderMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideDeleteFileMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideNewProjectFileMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideNewProjectMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideOpenProjectMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideRemoveFileMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideRemoveFolderMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideSaveProjectMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideSetCompilableFileMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideSetMainFileMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideUnsetCompilableFileMenuItemAction;
import acide.gui.explorerPanel.popup.listeners.AcideUnsetMainFileMenuItemAction;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE explorer panel popup menu.
 * 
 * @version 0.8
 * @see JPopupMenu
 */
public class AcideExplorerPanelPopupMenu extends JPopupMenu {

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu new project menu
	 * item image icon.
	 */
	private static final ImageIcon NEW_PROJECT_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/newProject.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu open project menu
	 * item image icon.
	 */
	private static final ImageIcon OPEN_PROJECT_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/openProject.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu save project menu
	 * item image icon.
	 */
	private static final ImageIcon SAVE_PROJECT_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/saveProject.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu new file menu item
	 * image icon.
	 */
	private static final ImageIcon NEW_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/newFile.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu add file menu item
	 * image icon.
	 */
	private static final ImageIcon ADD_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/addFile.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu remove file menu
	 * item image icon.
	 */
	private static final ImageIcon REMOVE_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/removeFile.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu add folder menu item
	 * image icon.
	 */
	private static final ImageIcon ADD_FOLDER_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/addFolder.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu remove folder menu
	 * item image icon.
	 */
	private static final ImageIcon REMOVE_FOLDER_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/removeFolder.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu delete file menu
	 * item image icon.
	 */
	private static final ImageIcon DELETE_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/deleteFile.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set main menu item
	 * image icon.
	 */
	private static final ImageIcon SET_MAIN_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/setMain.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu unset main menu item
	 * image icon.
	 */
	private static final ImageIcon UNSET_MAIN_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/unsetMain.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set compilable menu
	 * item image icon.
	 */
	private static final ImageIcon SET_COMPILABLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/setCompilable.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu unset compilable
	 * menu item image icon.
	 */
	private static final ImageIcon UNSET_COMPILABLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/unsetCompilable.png");
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu new project menu
	 * item.
	 */
	private JMenuItem _newProjectMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu open project menu
	 * item.
	 */
	private JMenuItem _openProjectMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu save project menu
	 * item.
	 */
	private JMenuItem _saveProjectMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu add file menu item.
	 */
	private JMenuItem _addFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu remove file menu
	 * item.
	 */
	private JMenuItem _removeFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu new project file
	 * menu item.
	 */
	private JMenuItem _newProjectFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu add folder menu
	 * item.
	 */
	private JMenuItem _addFolderMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu remove folder menu
	 * item.
	 */
	private JMenuItem _removeFolderMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu delete file menu
	 * item.
	 */
	private JMenuItem _deleteFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set main file menu
	 * item.
	 */
	private JMenuItem _setMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set compilable file
	 * menu item.
	 */
	private JMenuItem _setCompilableFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu unset main file menu
	 * item.
	 */
	private JMenuItem _unsetMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu unset compilable
	 * file menu item.
	 */
	private JMenuItem _unsetCompilableFileMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE explorer panel popup menu.
	 */
	public AcideExplorerPanelPopupMenu() {

		// Builds the components
		buildComponents();

		// Adds the components to the popup menu
		addComponents();

		// Sets the menu item listeners
		setListeners();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE explorer panel
	 * popup menu.
	 */
	private void addComponents() {
		
		// Adds the new project menu item to the popup menu
		add(_newProjectMenuItem);
		
		// Adds the open project menu item to the popup menu
		add(_openProjectMenuItem);
		
		// Adds the save project menu item to the popup menu
		add(_saveProjectMenuItem);

		// Adds a separator to the popup menu
		addSeparator();
		
		// Adds the new project file menu item to the popup menu
		add(_newProjectFileMenuItem);
		
		// Adds the add file to the popup menu
		add(_addFileMenuItem);
		
		// Adds the remove file menu item to the popup menu
		add(_removeFileMenuItem);
		
		// Adds the delete file menu item to the popup menu
		add(_deleteFileMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Adds the set compilable file menu item to the popup menu
		add(_setCompilableFileMenuItem);

		// Adds the unset compilable file menu item to the popup menu
		add(_unsetCompilableFileMenuItem);

		// Adds the set main file menu item to the popup menu
		add(_setMainFileMenuItem);

		// Adds the unset main file menu item to the popup menu
		add(_unsetMainFileMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Adds the add folder menu item to the popup menu
		add(_addFolderMenuItem);

		// Adds the remove folder menu item to the popup menu
		add(_removeFolderMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE explorer panel popup menu components.
	 */
	private void buildComponents() {

		// Creates the new project menu item
		_newProjectMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s14"), NEW_PROJECT_IMAGE);

		// Creates the open project menu item
		_openProjectMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s15"), OPEN_PROJECT_IMAGE);

		// Creates the save project menu item
		_saveProjectMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s16"), SAVE_PROJECT_IMAGE);

		// Creates the new project file menu item
		_newProjectFileMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s947"), NEW_FILE_IMAGE);

		// Creates the add file menu item
		_addFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s17"), ADD_FILE_IMAGE);

		// Creates the remove file menu item
		_removeFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s618"), REMOVE_FILE_IMAGE);

		// Creates the delete file menu item
		_deleteFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s950"), DELETE_FILE_IMAGE);

		// Creates the set compilable file menu item
		_setCompilableFileMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s254"),
				SET_COMPILABLE_IMAGE);

		// Creates the unset compilable file menu item
		_unsetCompilableFileMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s255"),
				UNSET_COMPILABLE_IMAGE);

		// Creates the set main file menu item
		_setMainFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s256"), SET_MAIN_IMAGE);

		// Creates the unset main file menu item
		_unsetMainFileMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s952"), UNSET_MAIN_IMAGE);

		// Creates the add folder menu item
		_addFolderMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s219"), ADD_FOLDER_IMAGE);

		// Creates the remove folder menu item
		_removeFolderMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s220"),
				REMOVE_FOLDER_IMAGE);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE explorer panel popup menu menu item
	 * listeners.
	 */
	private void setListeners() {

		// Sets the new project menu item action listener
		_newProjectMenuItem
				.addActionListener(new AcideNewProjectMenuItemAction());

		// Sets the open project menu item action listener
		_openProjectMenuItem
				.addActionListener(new AcideOpenProjectMenuItemAction());

		// Sets the save project menu item action listener
		_saveProjectMenuItem
				.addActionListener(new AcideSaveProjectMenuItemAction());

		// Sets the new project file menu item action listener
		_newProjectFileMenuItem
				.addActionListener(new AcideNewProjectFileMenuItemAction());

		// Sets the add file menu item action listener
		_addFileMenuItem.addActionListener(new AcideAddFileMenuItemAction());

		// Sets the remove file menu item action listener
		_removeFileMenuItem
				.addActionListener(new AcideRemoveFileMenuItemAction());

		// Sets the delete file menu item action listener
		_deleteFileMenuItem
				.addActionListener(new AcideDeleteFileMenuItemAction());

		// Sets the set compilable file menu item action listener
		_setCompilableFileMenuItem
				.addActionListener(new AcideSetCompilableFileMenuItemAction());

		// Sets the unset compilable file menu item action listener
		_unsetCompilableFileMenuItem
				.addActionListener(new AcideUnsetCompilableFileMenuItemAction());

		// Sets the set main file menu item action listener
		_setMainFileMenuItem
				.addActionListener(new AcideSetMainFileMenuItemAction());

		// Sets the unset main file menu item action listener
		_unsetMainFileMenuItem
				.addActionListener(new AcideUnsetMainFileMenuItemAction());

		// Sets the add folder menu item action listener
		_addFolderMenuItem
				.addActionListener(new AcideAddFolderMenuItemAction());

		// Sets the remove folder menu item action listener
		_removeFolderMenuItem
				.addActionListener(new AcideRemoveFolderMenuItemAction());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu add file
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu add file
	 *         menu item.
	 */
	public JMenuItem getAddFileMenuItem() {
		return _addFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu add
	 * folder menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu add
	 *         folder menu item.
	 */
	public JMenuItem getAddFolderMenuItem() {
		return _addFolderMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu save
	 * project menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu save
	 *         project menu item.
	 */
	public JMenuItem getSaveProjectMenuItem() {
		return _saveProjectMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu remove
	 * file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu remove
	 *         file menu item.
	 */
	public JMenuItem getRemoveFileMenuItem() {
		return _removeFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu remove
	 * folder menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu remove
	 *         folder menu item.
	 */
	public JMenuItem getRemoveFolderMenuItem() {
		return _removeFolderMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu delete
	 * file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu delete
	 *         file menu item.
	 */
	public JMenuItem getDeleteFileMenuItem() {
		return _deleteFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu new file
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu new file
	 *         menu item.
	 */
	public JMenuItem getNewProjectFileMenuItem() {
		return _newProjectFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu set main
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu set main
	 *         menu item.
	 */
	public JMenuItem getSetMainFileMenuItem() {
		return _setMainFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu unset
	 * main menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu unset
	 *         main menu item.
	 */
	public JMenuItem getUnsetMainFileMenuItem() {
		return _unsetMainFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu set
	 * compilable menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu set
	 *         compilable menu item.
	 */
	public JMenuItem getSetCompilableFileMenuItem() {
		return _setCompilableFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu unset
	 * compilable menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu unset
	 *         compilable menu item.
	 */
	public JMenuItem getUnsetCompilableFileMenuItem() {
		return _unsetCompilableFileMenuItem;
	}
}
