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
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import language.AcideLanguageManager;

/**																
 * Explorer panel popup menu of ACIDE - A Configurable IDE.
 *					
 * @version 0.8	
 * @see JPopupMenu																													
 */
public class AcideExplorerPanelPopupMenu extends JPopupMenu {

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the icon of the new project menu item.
	 */
	private static final String NEW_PROJECT = "./resources/icons/menu/project/newProject.png";
	/**
	 * Image file for the icon of the open project menu item.
	 */
	private static final String OPEN_PROJECT = "./resources/icons/menu/project/openProject.png";
	/**
	 * Image file for the icon of the save project menu item.
	 */
	private static final String SAVE_PROJECT = "./resources/icons/menu/project/saveProject.png";
	/**
	 * Image file for the icon of the new file menu item.
	 */
	private static final String NEW_FILE = "./resources/icons/menu/project/newFile.png";
	/**
	 * Image file for the icon of the add file menu item.
	 */
	private static final String ADD_FILE = "./resources/icons/menu/project/addFile.png";
	/**
	 * Image file for the icon of the add folder menu item.
	 */
	private static final String ADD_FOLDER = "./resources/icons/menu/project/addFolder.png";
	/**
	 * Image file for the icon of the delete file menu item.
	 */
	private static final String DELETE_FILE = "./resources/icons/menu/project/deleteFile.png";
	/**
	 * Image file for the icon of the set main menu item.
	 */
	private static final String SET_MAIN = "./resources/icons/menu/project/setMain.png";
	/**
	 * Image file for the icon of the unset main menu item.
	 */
	private static final String UNSET_MAIN = "./resources/icons/menu/project/unsetMain.png";
	/**
	 * Image file for the icon of the set compilable menu item.
	 */
	private static final String SET_COMPILABLE = "./resources/icons/menu/project/setCompilable.png";
	/**
	 * Image file for the icon of the unset compilable menu item.
	 */
	private static final String UNSET_COMPILABLE = "./resources/icons/menu/project/unsetCompilable.png";
	/**
	 * New project menu item.
	 */
	private JMenuItem _newProject;
	/**
	 * Open project menu item.
	 */
	private JMenuItem _openProject;
	/**
	 * Save project menu item.
	 */
	private JMenuItem _saveProject;
	/**
	 * Add file menu item.
	 */
	private JMenuItem _addFile;
	/**
	 * Remove file menu item.
	 */
	private JMenuItem _removeFile;
	/**
	 * New file menu item.
	 */
	private JMenuItem _newFile;
	/**
	 * Add folder menu item.
	 */
	private JMenuItem _addFolder;
	/**
	 * Remove folder menu item.
	 */
	private JMenuItem _removeFolder;
	/**
	 * Delete file menu item.
	 */
	private JMenuItem _deleteFile;
	/**
	 * Set main menu item.
	 */
	private JMenuItem _setMain;
	/**
	 * Set compilable menu item.
	 */
	private JMenuItem _setCompilable;
	/**
	 * Unset main menu item.
	 */
	private JMenuItem _unsetMain;
	/**
	 * Unset compilable menu item.
	 */
	private JMenuItem _unsetCompilable;

	/**
	 * Class constructor.
	 */
	public AcideExplorerPanelPopupMenu() {

		final ResourceBundle labels = AcideLanguageManager.getInstance().getLabels();

		// NEW PROJECT
		_newProject = new JMenuItem(labels.getString("s14"), new ImageIcon(
				NEW_PROJECT));
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
				MainWindow.getInstance().getMenu().getProject().getNewProject()
						.doClick();
			}
		});
		add(_newProject);

		// OPEN PROJECT
		_openProject = new JMenuItem(labels.getString("s15"), new ImageIcon(
				OPEN_PROJECT));
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
				MainWindow.getInstance().getMenu().getProject()
						.getOpenProject().doClick();
			}
		});
		add(_openProject);

		// SAVE PROJECT
		_saveProject = new JMenuItem(labels.getString("s16"), new ImageIcon(
				SAVE_PROJECT));
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
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().doClick();
			}
		});
		add(_saveProject);
		addSeparator();

		// NEW FILE
		_newFile = new JMenuItem(labels.getString("s947"), new ImageIcon(
				NEW_FILE));
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
				MainWindow.getInstance().getMenu().getProject()
						.getNewProjectFile().setEnabled(true);
				MainWindow.getInstance().getMenu().getProject()
						.getNewProjectFile().doClick();
			}
		});
		add(_newFile);

		// ADD FILE
		_addFile = new JMenuItem(labels.getString("s17"), new ImageIcon(
				ADD_FILE));
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
				MainWindow.getInstance().getMenu().getProject().getAddFile()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getProject().getAddFile()
						.doClick();
			}
		});
		add(_addFile);

		// REMOVE FILE
		_removeFile = new JMenuItem(labels.getString("s618"));
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
				MainWindow.getInstance().getMenu().getProject().getRemoveFile()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getProject().getRemoveFile()
						.doClick();
			}
		});
		add(_removeFile);

		// DELETE FILE
		_deleteFile = new JMenuItem(labels.getString("s950"), new ImageIcon(
				DELETE_FILE));
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
				MainWindow.getInstance().getMenu().getProject().getDeleteFile()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getProject().getDeleteFile()
						.doClick();
			}
		});
		add(_deleteFile);
		addSeparator();

		// SET COMPILABLE
		_setCompilable = new JMenuItem(labels.getString("s254"), new ImageIcon(
				SET_COMPILABLE));
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
				MainWindow.getInstance().getMenu().getProject()
						.getSetCompilable().setEnabled(true);
				MainWindow.getInstance().getMenu().getProject()
						.getSetCompilable().doClick();
			}
		});
		add(_setCompilable);

		// UNSET COMPILABLE
		_unsetCompilable = new JMenuItem(labels.getString("s255"),
				new ImageIcon(UNSET_COMPILABLE));
		_unsetCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getMenu().getProject()
						.getUnsetCompilable().setEnabled(true);
				MainWindow.getInstance().getMenu().getProject()
						.getUnsetCompilable().doClick();
			}
		});
		add(_unsetCompilable);

		// SET MAIN
		_setMain = new JMenuItem(labels.getString("s256"), new ImageIcon(
				SET_MAIN));
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
				MainWindow.getInstance().getMenu().getProject().getSetMain()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getProject().getSetMain()
						.doClick();
			}
		});
		add(_setMain);

		// UNSET MAIN
		_unsetMain = new JMenuItem(labels.getString("s952"), new ImageIcon(
				UNSET_MAIN));
		_unsetMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getMenu().getProject().getUnsetMain()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getProject().getUnsetMain()
						.doClick();
			}
		});
		add(_unsetMain);
		addSeparator();

		// REMOVE FOLDER
		_removeFolder = new JMenuItem(labels.getString("s220"));
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
				MainWindow.getInstance().getMenu().getProject()
						.getRemoveFolder().setEnabled(true);
				MainWindow.getInstance().getMenu().getProject()
						.getRemoveFolder().doClick();
			}
		});

		// ADD FOLDER
		_addFolder = new JMenuItem(labels.getString("s219"), new ImageIcon(
				ADD_FOLDER));
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
				MainWindow.getInstance().getMenu().getProject().getAddFolder()
						.setEnabled(true);
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
