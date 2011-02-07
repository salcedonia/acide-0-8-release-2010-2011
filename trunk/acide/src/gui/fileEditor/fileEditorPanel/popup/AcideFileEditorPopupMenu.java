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
package gui.fileEditor.fileEditorPanel.popup;

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;
import operations.log.AcideLog;

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
	 * ACIDE - A Configurable IDE file editor panel popup menu add file menu
	 * item.
	 */
	private JMenuItem _addFile;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu delete file menu
	 * item.
	 */
	private JMenuItem _deleteFile;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu remove file menu
	 * item.
	 */
	private JMenuItem _removeFile;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu select all menu
	 * item.
	 */
	private JMenuItem _selectAll;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu copy menu item.
	 */
	private JMenuItem _copy;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu paste menu item.
	 */
	private JMenuItem _paste;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu cut menu item.
	 */
	private JMenuItem _cut;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set compilable
	 * menu item.
	 */
	private JMenuItem _setCompilable;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset compilable
	 * menu item.
	 */
	private JMenuItem _unsetCompilable;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set main menu
	 * item.
	 */
	private JMenuItem _setMain;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset main menu
	 * item.
	 */
	private JMenuItem _unsetMain;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu print menu item.
	 */
	private JMenuItem _print;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor panel popup menu.
	 */
	public AcideFileEditorPopupMenu() {

		// COPY
		_copy = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s187"), COPY_IMAGE);
		_copy.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s99"));

				// COPY
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getActiveTextEditionArea().copy();
			}
		});
		add(_copy);

		// CUT
		_cut = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s188"), CUT_IMAGE);
		_cut.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s97"));

				// CUT
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getActiveTextEditionArea().cut();
			}
		});
		add(_cut);

		// PASTE
		_paste = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s189"), PASTE_IMAGE);
		_paste.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s98"));

				// PASTE
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getActiveTextEditionArea().paste();
			}
		});
		add(_paste);

		// SELECT ALL
		_selectAll = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s191"), SELECT_ALL_IMAGE);
		_selectAll.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Puts the caret in the first position
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getActiveTextEditionArea().setCaretPosition(0);

				// Gets the file content length
				int length = MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getActiveTextEditionArea().getText().length();

				// Sets the selection from the first position to the last one
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex())
						.getActiveTextEditionArea().setSelectionEnd(length);
			}
		});
		add(_selectAll);
		addSeparator();

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

				try {

					// Gets the current file absolute path
					String filePath = "";
					filePath = MainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(
									MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanelIndex())
							.getAbsolutePath();

					if (filePath != null) {

						// Gets the current selection
						TreePath currentSelection = MainWindow.getInstance()
								.getExplorerPanel().getTree()
								.getSelectionPath();
						
						// Gets the current node
						DefaultMutableTreeNode currentNode;
						
						// Gets the current project file
						AcideProjectFile currentProjectFile;

						// If there is something selected
						if (currentSelection != null) {
							
							// Gets the current node
							currentNode = (DefaultMutableTreeNode) currentSelection
									.getLastPathComponent();
							
							// Gets the current project file from the current node info
							currentProjectFile = (AcideProjectFile) currentNode.getUserObject();

							// If a file is selected
							if (!currentProjectFile.isDirectory()) {
								
								// The current node is the node root
								currentNode = MainWindow.getInstance()
										.getExplorerPanel().getRoot()
										.getNextNode();
								
								// Gets the current project file from the current node info
								currentProjectFile = (AcideProjectFile) currentNode
										.getUserObject();
							}

						} else {
							
							// The current node is the node root
							currentNode = MainWindow.getInstance()
									.getExplorerPanel().getRoot().getNextNode();
							
							// Gets the current project file from the current node info
							currentProjectFile = (AcideProjectFile) currentNode.getUserObject();
						}

						// Gets the file name
						String fileName = "";
						int lastIndexOfSlash = filePath.lastIndexOf("\\");
						if (lastIndexOfSlash == -1)
							lastIndexOfSlash = filePath.lastIndexOf("/");
						fileName = filePath.substring(lastIndexOfSlash + 1, filePath.length());

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
											AcideProjectConfiguration
													.getInstance()
													.getNumberOfFilesFromList() - 1)
									.setIsOpened(true);
							
							// Creates the node to be added to the explorer tree
							DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
									newProjectFile);
							
							// Children are not allowed
							newNode.setAllowsChildren(false);
							
							// Adds the node to the explorer tree
							currentNode.add(newNode);
							
							// Validates the changes in the main window
							MainWindow.getInstance().validate();
							
							// Repaints the main window
							MainWindow.getInstance().repaint();
							
							// Updates the explorer tree
							MainWindow.getInstance().getExplorerPanel()
									.getTreeModel().reload();
							
							// Repaint the explorer tree
							MainWindow.getInstance().getExplorerPanel()
									.expandTree();
							
							// Enables the remove file menu item in the explorer panel popup menu
							MainWindow.getInstance().getExplorerPanel()
									.getPopupMenu().getRemoveFile()
									.setEnabled(true);
							
							// Enables the delete file menu item in the explorer panel popup menu
							MainWindow.getInstance().getExplorerPanel()
									.getPopupMenu().getDeleteFile()
									.setEnabled(true);
							
							// The project configuration has been modified
							AcideProjectConfiguration.getInstance()
									.setIsModified(true);
						}
					}
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
			}
		});

		// REMOVE FILE
		_removeFile = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s618"));
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

				// Are you sure?
				int returnValueAreYouSure = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s623"));

				// If OK
				if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

					AcideProjectFile projectFile = new AcideProjectFile();
					int currentProjectFileIndex = -1;
					
					// Gets the selected file editor panel index
					int selectedFileEditorPanelIndex = MainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					for (int index1 = 0; index1 < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index1++) {

						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index1)
								.getAbsolutePath()
								.equals(MainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath())) {

							// Gets the project file from the project configuration
							projectFile = AcideProjectConfiguration
									.getInstance().getFileAt(index1);

							for (int index2 = 0; index2 < AcideProjectConfiguration
									.getInstance().getNumberOfFilesFromList() + 1; index2++) {

								if (MainWindow
										.getInstance()
										.getExplorerPanel()
										.getTree()
										.getPathForRow(index2)
										.getLastPathComponent()
										.toString()
										.equals(projectFile
												.getLastPathComponent())) {

									currentProjectFileIndex = index2;
								}
							}
						}
					}

					// Gets the selected tree node
					TreePath currentSelection = MainWindow.getInstance()
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
								MainWindow.getInstance().getExplorerPanel()
										.getTreeModel()
										.removeNodeFromParent(currentNode);

								// Searches for the file into the project
								// configuration file list
								int fileIndex = -1;

								for (int index = 0; index < AcideProjectConfiguration
										.getInstance()
										.getNumberOfFilesFromList(); index++) {

									if (AcideProjectConfiguration.getInstance()
											.getFileAt(index)
											.getAbsolutePath()
											.equals(currentProjectFile.getAbsolutePath())) {
										fileIndex = index;
									}
								}

								// Removes the file
								AcideProjectConfiguration.getInstance()
										.removeFileAt(fileIndex);

								// Updates the status message in the status bar
								MainWindow.getInstance().getStatusBar()
										.setStatusMessage(" ");

								// The project configuration has been modified
								AcideProjectConfiguration.getInstance()
										.setIsModified(true);

								// If the file has been modified
								if (MainWindow.getInstance()
										.getFileEditorManager()
										.isRedButton(selectedFileEditorPanelIndex)) {

									// Do you want to save it?
									returnValueAreYouSure = JOptionPane
											.showConfirmDialog(null,
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
										
										// Enables the save file menu item in the file menu
										MainWindow.getInstance().getMenu()
												.getFile().getSaveFile()
												.setEnabled(true);
										
										// Calls to the file menu save file menu item action performed
										MainWindow.getInstance().getMenu()
												.getFile().getSaveFile()
												.doClick();
									}
								}

								// Closes the tab
								MainWindow.getInstance().getFileEditorManager()
										.getTabbedPane()
										.remove(selectedFileEditorPanelIndex);

								// If no more tabs
								if (MainWindow.getInstance()
										.getFileEditorManager().getTabbedPane()
										.getTabCount() == 0) {

									// Disables the file menu
									MainWindow.getInstance().getMenu()
											.disableFileMenu();
									
									// Disables the edit menu
									MainWindow.getInstance().getMenu()
											.disableEditMenu();
								}

								return;
							}
						}
					}

					// If there are more files in the project configuration
					if (AcideProjectConfiguration.getInstance()
							.getNumberOfFilesFromList() > 0) {

						// Enables the remove file menu item in the explorer panel popup menu
						MainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getRemoveFile()
								.setEnabled(true);
						
						// Enables the delete file menu item in the explorer panel popup menu
						MainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getDeleteFile()
								.setEnabled(true);
					} else {

						// Disables the remove file menu item in the explorer panel popup menu
						MainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getRemoveFile()
								.setEnabled(false);
						
						// Disables the delete file menu item in the explorer panel popup menu
						MainWindow.getInstance().getExplorerPanel()
								.getPopupMenu().getDeleteFile()
								.setEnabled(false);
					}
				}
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

				int returnValueAreYouSure = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s951"));
				
				// If OK
				if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

					// If it is default project
					if (AcideProjectConfiguration.getInstance()
							.isDefaultProject()) {

						// Deletes the file
						String fileRemove = MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath();
						File file = new File(fileRemove);
						file.delete();

						// Removes the tab in the editor
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.remove(MainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());

						// Updates the status message in the status bar
						MainWindow.getInstance().getStatusBar()
								.setStatusMessage(" ");

					} else {

						// Not default project
						AcideProjectFile explorerFile = new AcideProjectFile();

						int fileIndex = -1;
						
						// Gets the selected file editor panel index
						int selectedFileEditorPanelIndex = MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanelIndex();

						for (int index1 = 0; index1 < AcideProjectConfiguration
								.getInstance().getNumberOfFilesFromList(); index1++) {

							if (AcideProjectConfiguration
									.getInstance()
									.getFileAt(index1)
									.getAbsolutePath()
									.equals(MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath())) {

								explorerFile = AcideProjectConfiguration
										.getInstance().getFileAt(index1);
								
								for (int index2 = 0; index2 < AcideProjectConfiguration
										.getInstance()
										.getNumberOfFilesFromList() + 1; index2++) {

									if (MainWindow
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
						TreePath currentSelection = MainWindow.getInstance()
								.getExplorerPanel().getTree()
								.getPathForRow(fileIndex);

						// Belongs the the project
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

									// Removes the parent
									MainWindow.getInstance().getExplorerPanel()
											.getTreeModel()
											.removeNodeFromParent(currentNode);

									// Looks for the file in the project configuration
									fileIndex = -1;
									for (int index = 0; index < AcideProjectConfiguration
											.getInstance()
											.getNumberOfFilesFromList(); index++)
										if (AcideProjectConfiguration
												.getInstance()
												.getFileAt(index)
												.getAbsolutePath()
												.equals(currentProjectFile.getAbsolutePath()))
											fileIndex = index;

									// Gets the file to be removed
									AcideProjectFile f = AcideProjectConfiguration
											.getInstance().getFileAt(
													fileIndex);
									// Gets the file to be removed path
									String fileToBeRemovedPath = f.getAbsolutePath();

									// Removes the file from the project
									// configuration
									AcideProjectConfiguration.getInstance()
											.removeFileAt(fileIndex);

									File file = new File(fileToBeRemovedPath);
									file.delete();

									// Updates the status message in the status bar
									MainWindow.getInstance().getStatusBar()
											.setStatusMessage(" ");

									// The project has been modified
									AcideProjectConfiguration.getInstance()
											.setIsModified(true);
									
									// Removes the tab in the file editor
									MainWindow.getInstance()
											.getFileEditorManager()
											.getTabbedPane()
											.remove(selectedFileEditorPanelIndex);

									return;
								}
							}
						} else {

							// Not belongs to the project
							String filePath = MainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath();

							// Deletes the file
							File file = new File(filePath);
							file.delete();

							// Closes the tab
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.remove(MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanelIndex());

							// Updates the status message in the status bar
							MainWindow.getInstance().getStatusBar()
									.setStatusMessage(" ");
						}

						// If there are more files in the project configuration
						if (AcideProjectConfiguration.getInstance()
								.getNumberOfFilesFromList() > 0) {

							// Enables the remove file menu item in the explorer panel popup menu
							MainWindow.getInstance().getExplorerPanel()
									.getPopupMenu().getRemoveFile()
									.setEnabled(true);
							// Enables the delete file menu item in the explorer panel popup menu
							MainWindow.getInstance().getExplorerPanel()
									.getPopupMenu().getDeleteFile()
									.setEnabled(true);
						} else {

							// Disables the remove file menu item in the explorer panel popup menu
							MainWindow.getInstance().getExplorerPanel()
									.getPopupMenu().getRemoveFile()
									.setEnabled(false);
							// Disables the delete file menu item in the explorer panel popup menu
							MainWindow.getInstance().getExplorerPanel()
									.getPopupMenu().getDeleteFile()
									.setEnabled(false);
						}
					}
				}
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

				// Sets compilable file
				MainWindow.getInstance().getFileEditorManager()
						.setCompilableFile();
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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Unsets compilable file
				MainWindow.getInstance().getFileEditorManager()
						.unsetCompilableFile();
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

				// Sets main file
				MainWindow.getInstance().getFileEditorManager().setMainFile();
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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Unsets main file
				MainWindow.getInstance().getFileEditorManager().unsetMainFile();
			}
		});
		add(_unsetMain);
		addSeparator();

		// PRINT MENU
		_print = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s624"), PRINT_FILE_IMAGE);
		_print.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the file menu print file menu item
				MainWindow.getInstance().getMenu().getFile().getPrintFile()
						.setEnabled(true);

				// Calls to the file menu print file menu item action performed
				MainWindow.getInstance().getMenu().getFile().getPrintFile()
						.doClick();
			}
		});
		add(_print);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu cut
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu cut
	 *         menu item.
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu copy
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu copy
	 *         menu item.
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu paste
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu paste
	 *         menu item.
	 */
	public JMenuItem getPaste() {
		return _paste;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu add
	 * file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu add
	 *         file menu item.
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu
	 * remove file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu
	 *         remove file menu item.
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu set
	 * compilable menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu set
	 *         compilable menu item.
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilable;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu unset
	 * compilable menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu unset
	 *         compilable menu item.
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilable;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu set
	 * main menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu set
	 *         main menu item.
	 */
	public JMenuItem getSetMain() {
		return _setMain;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu unset
	 * main menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu unset
	 *         main menu item.
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMain;
	}
}
