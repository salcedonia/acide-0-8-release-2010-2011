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
package gui.explorerPanel.popup;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE explorer panel popup menu.
 * 
 * @version 0.8
 * @see JPopupMenu
 */
public class AcideExplorerPanelPopupMenu extends JPopupMenu {

	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu class serial
	 * version UID.
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
	 * ACIDE - A Configurable IDE explorer panel popup menu remove file menu item
	 * image icon.
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
	 * ACIDE - A Configurable IDE explorer panel popup menu remove folder menu item
	 * image icon.
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
	private JMenuItem _newProject;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu open project menu
	 * item.
	 */
	private JMenuItem _openProject;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu save project menu
	 * item.
	 */
	private JMenuItem _saveProject;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu add file menu item.
	 */
	private JMenuItem _addFile;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu remove file menu
	 * item.
	 */
	private JMenuItem _removeFile;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu new file menu item.
	 */
	private JMenuItem _newFile;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu add folder menu
	 * item.
	 */
	private JMenuItem _addFolder;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu remove folder menu
	 * item.
	 */
	private JMenuItem _removeFolder;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu delete file menu
	 * item.
	 */
	private JMenuItem _deleteFile;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set main menu item.
	 */
	private JMenuItem _setMain;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu set compilable menu
	 * item.
	 */
	private JMenuItem _setCompilable;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu unset main menu
	 * item.
	 */
	private JMenuItem _unsetMain;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu unset compilable
	 * menu item.
	 */
	private JMenuItem _unsetCompilable;

	/**
	 * Creates a new ACIDE - A Configurable IDE explorer panel popup menu.
	 */
	public AcideExplorerPanelPopupMenu() {

		// NEW PROJECT
		_newProject = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s14"), NEW_PROJECT_IMAGE);
		_newProject.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Calls to new project action performed
				MainWindow.getInstance().getMenu().getProject().getNewProject()
						.doClick();
			}
		});
		add(_newProject);

		// OPEN PROJECT
		_openProject = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s15"), OPEN_PROJECT_IMAGE);
		_openProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Calls to open project action performed
				MainWindow.getInstance().getMenu().getProject()
						.getOpenProject().doClick();
			}
		});
		add(_openProject);

		// SAVE PROJECT
		_saveProject = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s16"), SAVE_PROJECT_IMAGE);
		_saveProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu save project menu item
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);

				// Calls to save project action performed
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().doClick();
			}
		});
		add(_saveProject);
		addSeparator();

		// NEW FILE
		_newFile = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s947"), NEW_FILE_IMAGE);
		_newFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu new project file menu item
				MainWindow.getInstance().getMenu().getProject()
						.getNewProjectFile().setEnabled(true);

				// Calls to new project file action performed
				MainWindow.getInstance().getMenu().getProject()
						.getNewProjectFile().doClick();
			}
		});
		add(_newFile);

		// ADD FILE
		_addFile = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s17"), ADD_FILE_IMAGE);
		_addFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu add file menu item
				MainWindow.getInstance().getMenu().getProject().getAddFile()
						.setEnabled(true);

				// Calls to add file action performed
				MainWindow.getInstance().getMenu().getProject().getAddFile()
						.doClick();
			}
		});
		add(_addFile);

		// REMOVE FILE
		_removeFile = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s618"), REMOVE_FILE_IMAGE);
		_removeFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu remove file menu item
				MainWindow.getInstance().getMenu().getProject().getRemoveFile()
						.setEnabled(true);

				// Calls to remove file action performed
				MainWindow.getInstance().getMenu().getProject().getRemoveFile()
						.doClick();
			}
		});
		add(_removeFile);

		// DELETE FILE
		_deleteFile = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s950"), DELETE_FILE_IMAGE);
		_deleteFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu delete file menu item
				MainWindow.getInstance().getMenu().getProject().getDeleteFile()
						.setEnabled(true);

				// Calls to delete file action performed
				MainWindow.getInstance().getMenu().getProject().getDeleteFile()
						.doClick();
			}
		});
		add(_deleteFile);
		addSeparator();

		// SET COMPILABLE
		_setCompilable = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s254"), SET_COMPILABLE_IMAGE);
		_setCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu set compilable menu item
				MainWindow.getInstance().getMenu().getProject()
						.getSetCompilable().setEnabled(true);

				// Calls to set compilable action performed
				MainWindow.getInstance().getMenu().getProject()
						.getSetCompilable().doClick();
			}
		});
		add(_setCompilable);

		// UNSET COMPILABLE
		_unsetCompilable = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s255"), UNSET_COMPILABLE_IMAGE);
		_unsetCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu unset compilable menu item
				MainWindow.getInstance().getMenu().getProject()
						.getUnsetCompilable().setEnabled(true);

				// Calls to unset compilable action performed
				MainWindow.getInstance().getMenu().getProject()
						.getUnsetCompilable().doClick();
			}
		});
		add(_unsetCompilable);

		// SET MAIN
		_setMain = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s256"), SET_MAIN_IMAGE);
		_setMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu set main menu item
				MainWindow.getInstance().getMenu().getProject().getSetMain()
						.setEnabled(true);

				// Calls to set main action performed
				MainWindow.getInstance().getMenu().getProject().getSetMain()
						.doClick();
			}
		});
		add(_setMain);

		// UNSET MAIN
		_unsetMain = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s952"), UNSET_MAIN_IMAGE);
		_unsetMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu unset main menu item
				MainWindow.getInstance().getMenu().getProject().getUnsetMain()
						.setEnabled(true);

				// Calls to unset main action performed
				MainWindow.getInstance().getMenu().getProject().getUnsetMain()
						.doClick();
			}
		});
		add(_unsetMain);
		addSeparator();

		// REMOVE FOLDER
		_removeFolder = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s220"), REMOVE_FOLDER_IMAGE);
		_removeFolder.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu remove folder menu item
				MainWindow.getInstance().getMenu().getProject()
						.getRemoveFolder().setEnabled(true);

				// Calls to remove folder action performed
				MainWindow.getInstance().getMenu().getProject()
						.getRemoveFolder().doClick();
			}
		});

		// ADD FOLDER
		_addFolder = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s219"), ADD_FOLDER_IMAGE);
		_addFolder.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the project menu add folder menu item
				MainWindow.getInstance().getMenu().getProject().getAddFolder()
						.setEnabled(true);

				// Calls to add folder action performed
				MainWindow.getInstance().getMenu().getProject().getAddFolder()
						.doClick();
			}
		});
		add(_addFolder);
		add(_removeFolder);
	}

	/**
	 * Returns the add file menu item.
	 * 
	 * @return the add file menu item.
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * Returns the add folder menu item.
	 * 
	 * @return the add folder menu item.
	 */
	public JMenuItem getAddFolder() {
		return _addFolder;
	}

	/**
	 * Returns the save project menu item.
	 * 
	 * @return the save project menu item.
	 */
	public JMenuItem getSaveProject() {
		return _saveProject;
	}

	/**
	 * Returns the remove file menu item.
	 * 
	 * @return the remove file menu item.
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * Returns the remove folder menu item.
	 * 
	 * @return the remove folder menu item.
	 */
	public JMenuItem getRemoveFolder() {
		return _removeFolder;
	}

	/**
	 * Returns the delete file menu item.
	 * 
	 * @return the delete file menu item.
	 */
	public JMenuItem getDeleteFile() {
		return _deleteFile;
	}

	/**
	 * Returns the new file menu item.
	 * 
	 * @return the new file menu item.
	 */
	public JMenuItem getNewFile() {
		return _newFile;
	}

	/**
	 * Returns the set main menu item.
	 * 
	 * @return The set main menu item.
	 */
	public JMenuItem getSetMainFile() {
		return _setMain;
	}

	/**
	 * Returns the unset main menu item.
	 * 
	 * @return The unset main menu item.
	 */
	public JMenuItem getUnsetMainFile() {
		return _unsetMain;
	}

	/**
	 * Returns the set compilable menu item.
	 * 
	 * @return the set compilable menu item.
	 */
	public JMenuItem getSetCompilableFile() {
		return _setCompilable;
	}

	/**
	 * Returns the unset compilable menu item.
	 * 
	 * @return the unset compilable menu item.
	 */
	public JMenuItem getUnsetCompilableFile() {
		return _unsetCompilable;
	}
}
