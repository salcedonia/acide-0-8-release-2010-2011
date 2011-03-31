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
package acide.gui.fileEditor.fileEditorPanel.popup;

import acide.configuration.project.AcideProjectConfiguration;
import acide.factory.gui.AcideGUIFactory;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu.
 * 
 * @version 0.8
 * @see JPopupMenu
 */
public class AcideFileEditorPopupMenu extends JPopupMenu {

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu display options
	 * window menu item image icon.
	 */
	private final static ImageIcon SHOW_DISPLAY_OPTIONS_WINDOW_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/fileEditorDisplayOptions.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu copy menu item
	 * image icon.
	 */
	private final static ImageIcon COPY_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/copy.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu paste menu item
	 * image icon.
	 */
	private final static ImageIcon PASTE_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/paste.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu cut menu item
	 * image icon.
	 */
	private final static ImageIcon CUT_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/cut.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu select all menu
	 * item image icon.
	 */
	private final static ImageIcon SELECT_ALL_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/selectAll.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu add file menu
	 * item image icon.
	 */
	private static final ImageIcon ADD_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/addFile.png");

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu remove file menu
	 * item image icon.
	 */
	private static final ImageIcon REMOVE_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/removeFile.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu delete file menu
	 * item image icon.
	 */
	private static final ImageIcon DELETE_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/deleteFile.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set main menu
	 * item image icon.
	 */
	private static final ImageIcon SET_MAIN_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/setMain.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset main menu
	 * item image icon.
	 */
	private static final ImageIcon UNSET_MAIN_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/unsetMain.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set compilable
	 * menu item image icon.
	 */
	private static final ImageIcon SET_COMPILABLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/setCompilable.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset compilable
	 * menu item image icon.
	 */
	private static final ImageIcon UNSET_COMPILABLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/unsetCompilable.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu print file menu
	 * item image icon.
	 */
	private final static ImageIcon PRINT_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/printFile.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu show display
	 * options window menu item.
	 */
	private JMenuItem _showDisplayOptionsWindowMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu add file menu
	 * item.
	 */
	private JMenuItem _addFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu delete file menu
	 * item.
	 */
	private JMenuItem _deleteFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu remove file menu
	 * item.
	 */
	private JMenuItem _removeFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu select all menu
	 * item.
	 */
	private JMenuItem _selectAllMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu copy menu item.
	 */
	private JMenuItem _copyMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu paste menu item.
	 */
	private JMenuItem _pasteMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu cut menu item.
	 */
	private JMenuItem _cutMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set compilable
	 * file menu item.
	 */
	private JMenuItem _setCompilableFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset compilable
	 * file menu item.
	 */
	private JMenuItem _unsetCompilableFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set main file
	 * menu item.
	 */
	private JMenuItem _setMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset main file
	 * menu item.
	 */
	private JMenuItem _unsetMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu print file menu
	 * item.
	 */
	private JMenuItem _printFileMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor panel popup menu.
	 */
	public AcideFileEditorPopupMenu() {

		// Creates the show display options window menu item
		_showDisplayOptionsWindowMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s1041"),
				SHOW_DISPLAY_OPTIONS_WINDOW_IMAGE);
		
		// Adds the show display options window menu item to the popup menu
		add(_showDisplayOptionsWindowMenuItem);
		
		// Adds a separator to the popup menu
		add(new JSeparator());

		// Creates the copy menu item
		_copyMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s187"), COPY_IMAGE);
		
		// Adds the copy menu item to the popup menu
		add(_copyMenuItem);

		// Creates the cut menu item
		_cutMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s188"), CUT_IMAGE);
		
		// Adds the cut menu item to the popup menu
		add(_cutMenuItem);

		// Creates the paste menu item
		_pasteMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s189"), PASTE_IMAGE);
		
		// Adds the paste menu item to the popup menu
		add(_pasteMenuItem);

		// Creates the select all menu item
		_selectAllMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s191"), SELECT_ALL_IMAGE);
		
		// Adds the select all menu item to the popup menu
		add(_selectAllMenuItem);
		
		// Adds a separator to the popup menu
		addSeparator();

		// Creates the add file menu item
		_addFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s1037"), ADD_FILE_IMAGE);
		
		// Adds the add file menu item to the popup menu
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
		_setMainFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s256"), SET_MAIN_IMAGE);
		
		// Adds the set main file menu item to the popup menu
		add(_setMainFileMenuItem);

		// Creates the unset main file menu item
		_unsetMainFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s952"), UNSET_MAIN_IMAGE);
		
		// Adds the unset main file menu item to the popup menu
		add(_unsetMainFileMenuItem);
		
		// Adds a separator to the popup menu
		addSeparator();

		// Creates the print file menu item
		_printFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s624"), PRINT_FILE_IMAGE);
		
		// Adds the print file menu item to the popup menu
		add(_printFileMenuItem);

		// Sets the menu item listeners
		setListeners();
	}

	/**
	 * Sets the menu item listeners of the ACIDE - A Configurable IDE file
	 * editor popup menu.
	 */
	private void setListeners() {

		// Sets the show display options window menu item action listener
		_showDisplayOptionsWindowMenuItem
				.addActionListener(new ShowDisplayOptionsWindowAction());

		// Sets the copy menu item action listener
		_copyMenuItem.addActionListener(new CopyMenuItemAction());

		// Sets the cut menu item action listener
		_cutMenuItem.addActionListener(new CutMenuItemAction());

		// Sets the paste menu item action listener
		_pasteMenuItem.addActionListener(new PasteMenuItemAction());

		// Sets the select all menu item action listener
		_selectAllMenuItem.addActionListener(new SelectAllMenuItemAction());

		// Sets the add file item action listener
		_addFileMenuItem.addActionListener(new AddFileMenuItemAction());

		// Sets the remove file menu item action listener
		_removeFileMenuItem.addActionListener(new RemoveFileMenuItemAction());

		// Sets the delete file menu item action listener
		_deleteFileMenuItem.addActionListener(new DeleteFileMenuItemAction());

		// Sets the set compilable file menu item action listener
		_setCompilableFileMenuItem
				.addActionListener(new SetCompilableFileMenuItemAction());

		// Sets the unset compilable file menu item action listener
		_unsetCompilableFileMenuItem
				.addActionListener(new UnsetCompilableFileMenuItemAction());

		// Sets the set main file menu item action listener
		_setMainFileMenuItem.addActionListener(new SetMainFileMenuItemAction());

		// Sets the unset main file menu item action listener
		_unsetMainFileMenuItem.addActionListener(new UnsetMainFileMenuItemAction());

		// Sets the print file menu item action listener
		_printFileMenuItem.addActionListener(new PrintFileMenuItemAction());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu cut
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu cut
	 *         menu item.
	 */
	public JMenuItem getCutMenuItem() {
		return _cutMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu copy
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu copy
	 *         menu item.
	 */
	public JMenuItem getCopyMenuItem() {
		return _copyMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu paste
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu paste
	 *         menu item.
	 */
	public JMenuItem getPasteMenuItem() {
		return _pasteMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu add
	 * file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu add
	 *         file menu item.
	 */
	public JMenuItem getAddFileMenuItem() {
		return _addFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu
	 * remove file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu
	 *         remove file menu item.
	 */
	public JMenuItem getRemoveFileMenuItem() {
		return _removeFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu set
	 * compilable file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu set
	 *         compilable file menu item.
	 */
	public JMenuItem getSetCompilableFileMenuItem() {
		return _setCompilableFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu unset
	 * compilable file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu unset
	 *         compilable file menu item.
	 */
	public JMenuItem getUnsetCompilableFileMenuItem() {
		return _unsetCompilableFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu set
	 * main file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu set
	 *         main file menu item.
	 */
	public JMenuItem getSetMainFileMenuItem() {
		return _setMainFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu unset
	 * main file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu unset
	 *         main file menu item.
	 */
	public JMenuItem getUnsetMainFileMenuItem() {
		return _unsetMainFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu
	 * delete file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu
	 *         delete file menu item.
	 */
	public JMenuItem getDeleteFileMenuItem() {
		return _deleteFileMenuItem;
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu copy menu item
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CopyMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s99"));

			// COPY
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(
							AcideMainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex())
					.getActiveTextEditionArea().copy();
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu cut menu item
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CutMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s97"));

			// CUT
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(
							AcideMainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex())
					.getActiveTextEditionArea().cut();
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu paste menu item
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class PasteMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s98"));

			// PASTE
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(
							AcideMainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex())
					.getActiveTextEditionArea().paste();
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu select all menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SelectAllMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Selects all the text in the text edition area
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(
							AcideMainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex())
					.getActiveTextEditionArea().selectAll();
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu add file menu
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

			try {

				// Gets the current file absolute path
				String filePath = "";
				filePath = AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								AcideMainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getAbsolutePath();

				if (filePath != null) {

					// Gets the current selection
					TreePath currentSelection = AcideMainWindow.getInstance()
							.getExplorerPanel().getTree().getSelectionPath();

					// Gets the current node
					DefaultMutableTreeNode currentNode;

					// Gets the current project file
					AcideProjectFile currentProjectFile;

					// If there is something selected
					if (currentSelection != null) {

						// Gets the current node
						currentNode = (DefaultMutableTreeNode) currentSelection
								.getLastPathComponent();

						// Gets the current project file from the current node
						// info
						currentProjectFile = (AcideProjectFile) currentNode
								.getUserObject();

						// If a file is selected
						if (!currentProjectFile.isDirectory()) {

							// The current node is the node root
							currentNode = AcideMainWindow.getInstance()
									.getExplorerPanel().getRoot().getNextNode();

							// Gets the current project file from the current
							// node info
							currentProjectFile = (AcideProjectFile) currentNode
									.getUserObject();
						}

					} else {

						// The current node is the node root
						currentNode = AcideMainWindow.getInstance()
								.getExplorerPanel().getRoot().getNextNode();

						// Gets the current project file from the current node
						// info
						currentProjectFile = (AcideProjectFile) currentNode
								.getUserObject();
					}

					// Gets the file name
					String fileName = "";
					int lastIndexOfSlash = filePath.lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = filePath.lastIndexOf("/");
					fileName = filePath.substring(lastIndexOfSlash + 1,
							filePath.length());

					// Creates and configures the new project file
					AcideProjectFile newProjectFile = new AcideProjectFile();

					// Sets its absolute path
					newProjectFile.setAbsolutePath(filePath);

					// Sets its name
					newProjectFile.setName(fileName);

					// Checks if it is already added to the project
					boolean isAdded = false;
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index++) {
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(index).getAbsolutePath()
								.equals(newProjectFile.getAbsolutePath())) {
							isAdded = true;
						}
					}

					// If is not
					if (!isAdded) {

						// Puts the file as the root folder child
						newProjectFile.setParent(AcideProjectConfiguration
								.getInstance().getName());

						// Adds the file to the project configuration
						AcideProjectConfiguration.getInstance().addFile(
								newProjectFile);

						// Sets the file as opened
						AcideProjectConfiguration
								.getInstance()
								.getFileAt(
										AcideProjectConfiguration.getInstance()
												.getNumberOfFilesFromList() - 1)
								.setIsOpened(true);

						// Creates the node to be added to the explorer tree
						DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
								newProjectFile);

						// Children are not allowed
						newNode.setAllowsChildren(false);

						// Adds the node to the explorer tree
						currentNode.add(newNode);

						// Updates the explorer tree
						AcideMainWindow.getInstance().getExplorerPanel()
								.getTreeModel().reload();

						// Repaint the explorer tree
						AcideMainWindow.getInstance().getExplorerPanel()
								.expandTree();

						// Enables the remove file menu item in the explorer
						// panel popup menu
						AcideMainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getRemoveFileMenuItem()
								.setEnabled(true);

						// Enables the delete file menu item in the explorer
						// panel popup menu
						AcideMainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getDeleteFileMenuItem()
								.setEnabled(true);

						// The project configuration has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);
					}
				}
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu remove file menu
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

			// Are you sure?
			int returnValueAreYouSure = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s623"));

			// If OK
			if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

				AcideProjectFile projectFile = new AcideProjectFile();
				int currentProjectFileIndex = -1;

				// Gets the selected file editor panel index
				int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				for (int index1 = 0; index1 < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index1++) {

					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index1)
							.getAbsolutePath()
							.equals(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath())) {

						// Gets the project file from the project configuration
						projectFile = AcideProjectConfiguration.getInstance()
								.getFileAt(index1);

						for (int index2 = 0; index2 < AcideProjectConfiguration
								.getInstance().getNumberOfFilesFromList() + 1; index2++) {

							if (AcideMainWindow.getInstance().getExplorerPanel()
									.getTree().getPathForRow(index2)
									.getLastPathComponent().toString()
									.equals(projectFile.getLastPathComponent())) {

								currentProjectFileIndex = index2;
							}
						}
					}
				}

				// Gets the selected tree node
				TreePath currentSelection = AcideMainWindow.getInstance()
						.getExplorerPanel().getTree()
						.getPathForRow(currentProjectFileIndex);

				// Something selected
				if (currentSelection != null) {

					// Gets the current node
					DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
							.getLastPathComponent());

					// Gets the current project file from the current node info
					AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
							.getUserObject();

					// Is not a directory
					if (!currentProjectFile.isDirectory()) {

						// Gets the parent
						MutableTreeNode parent = (MutableTreeNode) (currentNode
								.getParent());

						// Has parent
						if (parent != null) {

							// Removes the node from the parent
							AcideMainWindow.getInstance().getExplorerPanel()
									.getTreeModel()
									.removeNodeFromParent(currentNode);

							// Searches for the file into the project
							// configuration file list
							int fileIndex = -1;

							for (int index = 0; index < AcideProjectConfiguration
									.getInstance().getNumberOfFilesFromList(); index++) {

								if (AcideProjectConfiguration
										.getInstance()
										.getFileAt(index)
										.getAbsolutePath()
										.equals(currentProjectFile
												.getAbsolutePath())) {
									fileIndex = index;
								}
							}

							// Removes the file
							AcideProjectConfiguration.getInstance()
									.removeFileAt(fileIndex);

							// Updates the status message in the status bar
							AcideMainWindow.getInstance().getStatusBar()
									.setStatusMessage(" ");

							// The project configuration has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);

							// If the file has been modified
							if (AcideMainWindow.getInstance().getFileEditorManager()
									.isRedButton(selectedFileEditorPanelIndex)) {

								// Do you want to save it?
								returnValueAreYouSure = JOptionPane
										.showConfirmDialog(
												null,
												AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s643"),
												AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s953"),
												JOptionPane.YES_NO_OPTION);

								// If OK
								if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

									// Enables the save file menu item in the
									// file menu
									AcideMainWindow.getInstance().getMenu()
											.getFileMenu().getSaveFileMenuItem()
											.setEnabled(true);

									// Calls to the file menu save file menu
									// item action performed
									AcideMainWindow.getInstance().getMenu()
											.getFileMenu().getSaveFileMenuItem().doClick();
								}
							}

							// Closes the tab
							AcideMainWindow.getInstance().getFileEditorManager()
									.getTabbedPane()
									.remove(selectedFileEditorPanelIndex);

							// If no more tabs
							if (AcideMainWindow.getInstance().getFileEditorManager()
									.getTabbedPane().getTabCount() == 0) {

								// Disables the file menu
								AcideMainWindow.getInstance().getMenu()
										.disableFileMenu();

								// Disables the edit menu
								AcideMainWindow.getInstance().getMenu()
										.disableEditMenu();
							}

							return;
						}
					}
				}

				// If there are more files in the project configuration
				if (AcideProjectConfiguration.getInstance()
						.getNumberOfFilesFromList() > 0) {

					// Enables the remove file menu item in the explorer panel
					// popup menu
					AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getRemoveFileMenuItem().setEnabled(true);

					// Enables the delete file menu item in the explorer panel
					// popup menu
					AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getDeleteFileMenuItem().setEnabled(true);
				} else {

					// Disables the remove file menu item in the explorer panel
					// popup menu
					AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getRemoveFileMenuItem().setEnabled(false);

					// Disables the delete file menu item in the explorer panel
					// popup menu
					AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getDeleteFileMenuItem().setEnabled(false);
				}
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu delete file menu
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

			// Asks to the user
			int returnValueAreYouSure = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s951"));

			// If OK
			if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

				// If it is the default project
				if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// Deletes the file
					String fileRemove = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath();
					File file = new File(fileRemove);
					file.delete();

					// Removes the tab in the editor
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.remove(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanelIndex());

					// Updates the status message in the status bar
					AcideMainWindow.getInstance().getStatusBar()
							.setStatusMessage(" ");

				} else {

					// Not default project
					AcideProjectFile explorerFile = new AcideProjectFile();

					int fileIndex = -1;

					// Gets the selected file editor panel index
					int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					for (int index1 = 0; index1 < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index1++) {

						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index1)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath())) {

							explorerFile = AcideProjectConfiguration
									.getInstance().getFileAt(index1);

							for (int index2 = 0; index2 < AcideProjectConfiguration
									.getInstance().getNumberOfFilesFromList() + 1; index2++) {

								if (AcideMainWindow
										.getInstance()
										.getExplorerPanel()
										.getTree()
										.getPathForRow(index2)
										.getLastPathComponent()
										.toString()
										.equals(explorerFile
												.getLastPathComponent())) {

									fileIndex = index2;
								}
							}
						}
					}

					// Gets the selected noede
					TreePath currentSelection = AcideMainWindow.getInstance()
							.getExplorerPanel().getTree()
							.getPathForRow(fileIndex);

					// Belongs the the project
					if (currentSelection != null) {

						// Gets the current node
						DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
								.getLastPathComponent());

						// Gets the current project file from the current node
						// info
						AcideProjectFile currentProjectFile = (AcideProjectFile) currentNode
								.getUserObject();

						// Is not a directory
						if (!currentProjectFile.isDirectory()) {

							// Gets the parent
							MutableTreeNode parent = (MutableTreeNode) (currentNode
									.getParent());

							// Has parent
							if (parent != null) {

								// Removes the parent
								AcideMainWindow.getInstance().getExplorerPanel()
										.getTreeModel()
										.removeNodeFromParent(currentNode);

								// Looks for the file in the project
								// configuration
								fileIndex = -1;
								for (int index = 0; index < AcideProjectConfiguration
										.getInstance()
										.getNumberOfFilesFromList(); index++)
									if (AcideProjectConfiguration
											.getInstance()
											.getFileAt(index)
											.getAbsolutePath()
											.equals(currentProjectFile
													.getAbsolutePath()))
										fileIndex = index;

								// Gets the file to be removed
								AcideProjectFile f = AcideProjectConfiguration
										.getInstance().getFileAt(fileIndex);
								// Gets the file to be removed path
								String fileToBeRemovedPath = f
										.getAbsolutePath();

								// Removes the file from the project
								// configuration
								AcideProjectConfiguration.getInstance()
										.removeFileAt(fileIndex);

								File file = new File(fileToBeRemovedPath);
								file.delete();

								// Updates the status message in the status bar
								AcideMainWindow.getInstance().getStatusBar()
										.setStatusMessage(" ");

								// The project has been modified
								AcideProjectConfiguration.getInstance()
										.setIsModified(true);

								// Removes the tab in the file editor
								AcideMainWindow.getInstance().getFileEditorManager()
										.getTabbedPane()
										.remove(selectedFileEditorPanelIndex);

								return;
							}
						}
					} else {

						// Not belongs to the project
						String filePath = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath();

						// Deletes the file
						File file = new File(filePath);
						file.delete();

						// Closes the tab
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.remove(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());

						// Updates the status message in the status bar
						AcideMainWindow.getInstance().getStatusBar()
								.setStatusMessage(" ");
					}

					// If there are more files in the project configuration
					if (AcideProjectConfiguration.getInstance()
							.getNumberOfFilesFromList() > 0) {

						// Enables the remove file menu item in the explorer
						// panel popup menu
						AcideMainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getRemoveFileMenuItem()
								.setEnabled(true);
						// Enables the delete file menu item in the explorer
						// panel popup menu
						AcideMainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getDeleteFileMenuItem()
								.setEnabled(true);
					} else {

						// Disables the remove file menu item in the explorer
						// panel popup menu
						AcideMainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getRemoveFileMenuItem()
								.setEnabled(false);
						// Disables the delete file menu item in the explorer
						// panel popup menu
						AcideMainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getDeleteFileMenuItem()
								.setEnabled(false);
					}
				}
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set compilable
	 * file menu item action listener.
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

			if (!AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().isCompilableFile()
					|| (AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isCompilableFile() && AcideMainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isMainFile())) {

				// Sets the file as compiled
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().setCompilableFile(true);

				// If it is already a MAIN FILE
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())
					// Removes the main file property
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// Search for the file into the project configuration file
					// list
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index++) {

						// If exists
						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath())) {

							// The project has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);

							// Marks it as COMPILABLE FILE
							AcideProjectConfiguration.getInstance()
									.getFileAt(index).setIsCompilableFile(true);

							// It is MAIN FILE
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index).isMainFile())

								// Removes the main file property
								AcideProjectConfiguration.getInstance()
										.getFileAt(index).setIsMainFile(false);

							// Puts the COMPILABLE icon in the selected file
							// editor panel
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setIconAt(
											AcideMainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex(),
											new ImageIcon(
													"./resources/icons/editor/compilable.png"));

							// Updates the status message in the status bar
							AcideMainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideMainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanel()
													.getAbsolutePath()
													+ " <COMPILABLE>");
						}
					}
				} else {

					// DEFAULT PROJECT

					// Puts the COMPILABLE icon in the file editor manager tab
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.setIconAt(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanelIndex(),
									new ImageIcon(
											"./resources/icons/editor/compilable.png"));

					// Updates the status message in the status bar
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath()
											+ " <COMPILABLE>");
				}
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset compilable
	 * file menu item action listener.
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
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If it is COMPILABLE FILE and not MAIN FILE
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().isCompilableFile()
					&& !AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isMainFile()) {

				// Sets COMPILER FILE to false
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().setCompilableFile(false);

				// Updates the status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideMainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath());

				// Removes the COMPILABLE icon from the selected file editor
				// panel
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.setIconAt(
								AcideMainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex(),
								null);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

					// Searches for the file into the project configuration file
					// list
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index++) {

						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath()))
							// Sets the COMPILABLE FILE as false
							AcideProjectConfiguration.getInstance()
									.getFileAt(index)
									.setIsCompilableFile(false);
					}
				}
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set main file
	 * menu item action listener.
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

			// IF it is not MAIN FILE
			if (!AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().isMainFile()) {

				// Removes the previous MAIN FILE
				for (int index = 0; index < AcideMainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

					// Finds the previous MAIN FILE
					if (AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).isMainFile()) {

						// Sets MAIN FILE as false
						AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index).setMainFile(false);

						// Sets COMPILER FILE as false
						AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(index)
								.setCompilableFile(false);

						// Updates the status message in the status bar
						AcideMainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
										AcideMainWindow.getInstance()
												.getFileEditorManager()
												.getFileEditorPanelAt(index)
												.getAbsolutePath());

						// Removes the MAIN icon from the file editor manager
						// tab
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().setIconAt(index, null);
					}
				}

				// Sets MAIN FILE as true
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().setMainFile(true);

				// Sets COMPILER FILE as true
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().setCompilableFile(true);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// Search for the file into the project configuration file
					// list
					for (int index1 = 0; index1 < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index1++) {

						// If exists
						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index1)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath())) {

							// The project has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);

							// Removes the previous MAIN FILE from the project
							// configuration
							for (int index2 = 0; index2 < AcideProjectConfiguration
									.getInstance().getFileListSize(); index2++) {

								// MAIN FILE?
								if (AcideProjectConfiguration.getInstance()
										.getFileAt(index2).isMainFile()) {

									// Sets MAIN FILE to false
									AcideProjectConfiguration.getInstance()
											.getFileAt(index2)
											.setIsMainFile(false);

									// Sets COMPILABLE FILE to false
									AcideProjectConfiguration.getInstance()
											.getFileAt(index2)
											.setIsCompilableFile(false);
								}
							}

							// Sets it as MAIN FILE
							AcideProjectConfiguration.getInstance()
									.getFileAt(index1).setIsMainFile(true);

							// Sets it as COMPILABLE FILE
							AcideProjectConfiguration.getInstance()
									.getFileAt(index1)
									.setIsCompilableFile(true);

							// Puts the MAIN icon in the selected file editor
							// panel
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setIconAt(
											AcideMainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex(),
											new ImageIcon(
													"./resources/icons/editor/main.png"));

							// Updates the status message in the status bar
							AcideMainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideMainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanel()
													.getAbsolutePath()
													+ " <MAIN>");

						}
					}
				} else {

					// DEFAULT CONFIGURATION

					// Puts the MAIN icon in the selected file editor panel
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.setIconAt(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanelIndex(),
									new ImageIcon(
											"./resources/icons/editor/main.png"));

					// Updates the status message in the status bar
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath()
											+ " <MAIN>");
				}
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset main file
	 * menu item action listener.
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
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If it is MAIN FILE
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().isMainFile()) {

				// Sets the MAIN FILE as false
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().setMainFile(false);

				// Updates the status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideMainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath());

				// Removes the MAIN icon from the selected file editor panel
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.setIconAt(
								AcideMainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex(),
								null);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

					// Searches for the file into the project configuration file
					// list
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index++) {

						// If exists
						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath()))
							// Sets MAIN FILE as false
							AcideProjectConfiguration.getInstance()
									.getFileAt(index).setIsMainFile(false);
					}
				}
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu print file menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class PrintFileMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the file menu print file menu item
			AcideMainWindow.getInstance().getMenu().getFileMenu().getPrintFileMenuItem()
					.setEnabled(true);

			// Calls to the file menu print file menu item action performed
			AcideMainWindow.getInstance().getMenu().getFileMenu().getPrintFileMenuItem()
					.doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu show display
	 * options window menu item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ShowDisplayOptionsWindowAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Shows the file editor display options window
			AcideGUIFactory.getInstance().buildAcideFileEditorDisplayOptionsWindow();
		}
	}
}
