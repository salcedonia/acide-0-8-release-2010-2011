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

import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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
	 * ACIDE - A Configurable IDE explorer panel popup menu new project file menu item.
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
	 * ACIDE - A Configurable IDE explorer panel popup menu set main file menu item.
	 */
	private JMenuItem _setMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set compilable file menu
	 * item.
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

		// Creates the new project menu item
		_newProjectMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s14"), NEW_PROJECT_IMAGE);

		// Adds the new project menu item to the popup menu
		add(_newProjectMenuItem);

		// Creates the open project menu item
		_openProjectMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s15"), OPEN_PROJECT_IMAGE);

		// Adds the open project menu item to the popup menu
		add(_openProjectMenuItem);

		// Creates the save project menu item
		_saveProjectMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s16"), SAVE_PROJECT_IMAGE);

		// Adds the save project menu item to the popup menu
		add(_saveProjectMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Creates the new project file menu item
		_newProjectFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s947"), NEW_FILE_IMAGE);

		// Adds the new project file menu item to the popup menu
		add(_newProjectFileMenuItem);

		// Creates the add file menu item
		_addFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s17"), ADD_FILE_IMAGE);

		// Adds the add file to the popup menu
		add(_addFileMenuItem);

		// Creates the remove file menu item
		_removeFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s618"), REMOVE_FILE_IMAGE);

		// Adds the remove file menu item to the popup menu
		add(_removeFileMenuItem);

		// Creates the delete file menu item
		_deleteFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s950"), DELETE_FILE_IMAGE);

		// Adds the delete file menu item to the popup menu
		add(_deleteFileMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Creates the set compilable file menu item
		_setCompilableFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s254"), SET_COMPILABLE_IMAGE);

		// Adds the set compilable file menu item to the popup menu
		add(_setCompilableFileMenuItem);

		// Creates the unset compilable file menu item
		_unsetCompilableFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s255"), UNSET_COMPILABLE_IMAGE);

		// Adds the unset compilable file menu item to the popup menu
		add(_unsetCompilableFileMenuItem);

		// Creates the set main file menu item
		_setMainFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s256"), SET_MAIN_IMAGE);

		// Adds the set main file menu item to the popup menu
		add(_setMainFileMenuItem);

		// Creates the unset main file menu item
		_unsetMainFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s952"), UNSET_MAIN_IMAGE);

		// Adds the unset main file menu item to the popup menu
		add(_unsetMainFileMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Creates the add folder menu item
		_addFolderMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s219"), ADD_FOLDER_IMAGE);

		// Adds the add folder menu item to the popup menu
		add(_addFolderMenuItem);

		// Creates the remove folder menu item
		_removeFolderMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s220"), REMOVE_FOLDER_IMAGE);

		// Adds the remove folder menu item to the popup menu
		add(_removeFolderMenuItem);

		// Sets the menu item listeners
		setListeners();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE explorer panel popup menu menu item
	 * listeners.
	 */
	private void setListeners() {

		// Sets the new project menu item action listener
		_newProjectMenuItem.addActionListener(new NewProjectMenuItemAction());

		// Sets the open project menu item action listener
		_openProjectMenuItem.addActionListener(new OpenProjectMenuItemAction());

		// Sets the save project menu item action listener
		_saveProjectMenuItem.addActionListener(new SaveProjectMenuItemAction());

		// Sets the new project file menu item action listener
		_newProjectFileMenuItem.addActionListener(new NewProjectFileMenuItemAction());

		// Sets the add file menu item action listener
		_addFileMenuItem.addActionListener(new AddFileMenuItemAction());

		// Sets the remove file menu item action listener
		_removeFileMenuItem.addActionListener(new RemoveFileMenuItemAction());

		// Sets the delete file menu item action listener
		_deleteFileMenuItem.addActionListener(new DeleteFileMenuItemAction());

		// Sets the set compilable file menu item action listener
		_setCompilableFileMenuItem.addActionListener(new SetCompilableFileMenuItemAction());

		// Sets the unset compilable file menu item action listener
		_unsetCompilableFileMenuItem.addActionListener(new UnsetCompilableFileMenuItemAction());

		// Sets the set main file menu item action listener
		_setMainFileMenuItem.addActionListener(new SetMainFileMenuItemAction());

		// Sets the unset main file menu item action listener
		_unsetMainFileMenuItem.addActionListener(new UnsetMainFileMenuItemAction());

		// Sets the add folder menu item action listener
		_addFolderMenuItem.addActionListener(new AddFolderMenuItemAction());

		// Sets the remove folder menu item action listener
		_removeFolderMenuItem.addActionListener(new RemoveFolderMenuItemAction());
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

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu new project menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class NewProjectMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Calls to new project action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getNewProjectMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu open project menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class OpenProjectMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Calls to open project action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getOpenProjectMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu save project menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SaveProjectMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu save project menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSaveProjectMenuItem().setEnabled(true);

			// Calls to save project action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSaveProjectMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu new project file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class NewProjectFileMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu new project file menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getNewProjectFileMenuItem().setEnabled(true);

			// Calls to new project file action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getNewProjectFileMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu add file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AddFileMenuItemAction implements ActionListener {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu add file menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getAddFileMenuItem().setEnabled(true);

			// Calls to add file action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getAddFileMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu remove file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class RemoveFileMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu remove file menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getRemoveFileMenuItem().setEnabled(true);

			// Calls to remove file action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getRemoveFileMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu delete file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class DeleteFileMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu delete file menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getDeleteFileMenuItem().setEnabled(true);

			// Calls to delete file action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getDeleteFileMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set compilable file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SetCompilableFileMenuItemAction implements ActionListener {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu set compilable menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSetCompilableFileMenuItem().setEnabled(true);

			// Calls to set compilable action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSetCompilableFileMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu unset compilable file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class UnsetCompilableFileMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu unset compilable menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getUnsetCompilableFileMenuItem().setEnabled(true);

			// Calls to unset compilable action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getUnsetCompilableFileMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set main file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SetMainFileMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu set main menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSetMainFileMenuItem().setEnabled(true);

			// Calls to set main action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSetMainFileMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu unset main file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class UnsetMainFileMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu unset main menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getUnsetMainFileMenuItem().setEnabled(true);

			// Calls to unset main action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getUnsetMainFileMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu remove folder menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class RemoveFolderMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu remove folder menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getRemoveFolderMenuItem().setEnabled(true);

			// Calls to remove folder action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getRemoveFolderMenuItem().doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu add folder menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AddFolderMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the project menu add folder menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getAddFolderMenuItem().setEnabled(true);

			// Calls to add folder action performed
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getAddFolderMenuItem().doClick();
		}
	}
}
