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
package gui.menuBar.projectMenu.listeners;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.console.AcideConsoleConfiguration;
import es.configuration.fileEditor.AcideFileEditorConfiguration;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.menu.AcideMenuConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import es.configuration.toolBar.consoleComandToolBar.ConsoleCommandList;
import es.configuration.window.AcideWindowConfiguration;
import es.project.AcideProjectFile;
import es.project.AcideProjectFileType;
import es.text.ExtensionFilter;
import es.text.AcideTextFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.menuMenu.gui.AcideMenuConfigurationWindow;
import gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;
import gui.toolBarPanel.staticToolBar.AcideStaticToolBar;

/**
 * ACIDE -A Configurable IDE project menu open project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class OpenProjectMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		AcideTextFile textFile = AcideIOFactory.getInstance().buildFile();

		boolean cancelOptionSelected = false;

		// Selects the extension for the project
		String[] ExtPide = new String[] { "acidePrj" };
		textFile.getFileChooser().addChoosableFileFilter(
				new ExtensionFilter(ExtPide, AcideLanguageManager.getInstance()
						.getLabels().getString("s328")));
		final String file;
		file = textFile.askAbsolutePath();

		// If the file content is not empty
		if (file != null) {

			// If the project has been modified
			if (AcideProjectConfiguration.getInstance().isModified()) {

				// Do you want to save it?
				int chosenOption = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s657"), AcideLanguageManager
								.getInstance().getLabels().getString("s953"),
						JOptionPane.YES_NO_CANCEL_OPTION);

				// If OK
				if (chosenOption == JOptionPane.OK_OPTION) {

					// Enables the save project menu item
					MainWindow.getInstance().getMenu().getProject()
							.getSaveProject().setEnabled(true);

					// Does the save project menu item action
					MainWindow.getInstance().getMenu().getProject()
							.getSaveProject().doClick();
				} else if (chosenOption != JOptionPane.NO_OPTION)
					cancelOptionSelected = true;
			}

			// If the user does not select the cancel option
			if (!cancelOptionSelected) {

				// Puts the wait cursor
				MainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				// Updates the status message in the status bar
				MainWindow.getInstance().getStatusBar().setStatusMessage(" ");

				// Closes the previous project configuration
				closePreviousProjectConfiguration();

				// Loads the file editor configuration
				loadNewProjectConfiguration(file);

				// Loads the language
				loadLanguage();

				// Loads the main window configuration
				loadMainWindowConfiguration();

				// Loads the menu configuration
				loadMenuConfiguration();

				// Loads the grammar configuration
				loadGrammarConfiguration();

				// Loads the console configuration
				loadConsoleConfiguration();

				// Loads the tool bar configuration
				loadToolBarConfiguration();

				// Loads the explorer configuration
				loadExplorerConfiguration();

				// Enables the show explorer panel menu item
				MainWindow.getInstance().getMenu().getView()
						.getShowExplorerPanel().setSelected(true);

				// Loads the file editor configuration
				loadFileEditorConfiguration();

				// Loads the lexicon configuration
				loadLexiconConfiguration();

				// The project has not been modified
				AcideProjectConfiguration.getInstance().setIsModified(false);

				// This is the first time that it is saved
				AcideProjectConfiguration.getInstance().setFirstSave(true);

				// Enables the project menu
				MainWindow.getInstance().getMenu().enableProjectMenu();

				// Enables the open all files menu item
				MainWindow.getInstance().getMenu().getFile().getOpenAllFiles()
						.setEnabled(true);

				// Enables the save project button in the static tool bar
				AcideStaticToolBar.getInstance().getSaveProjectButton()
						.setEnabled(true);

				// Validates the changes in the main window
				MainWindow.getInstance().validate();

				// Repaints the main window
				MainWindow.getInstance().repaint();

				// Sets the default cursor
				MainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
		}
	}

	/**
	 * Loads the project explorer configuration.
	 */
	public void loadExplorerConfiguration() {

		// Removes all the nodes in the tree
		MainWindow.getInstance().getExplorerPanel().getRoot()
				.removeAllChildren();

		// Creates the folder with the name of the project
		AcideProjectFile projectFile = new AcideProjectFile();

		// Sets the absolute path
		projectFile.setAbsolutePath(AcideProjectConfiguration.getInstance()
				.getName());

		// Sets the name
		projectFile.setName(AcideProjectConfiguration.getInstance().getName());

		// It is directory
		projectFile.setIsDirectory(true);

		// It has no parent
		projectFile.setParent(null);

		// Sets the main window title
		MainWindow.getInstance().setTitle(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s425")
						+ " - "
						+ AcideProjectConfiguration.getInstance().getName());

		// Creates the explorer tree with the project as the root of it
		DefaultMutableTreeNode explorerTree = new DefaultMutableTreeNode(
				projectFile);

		// Allows children
		explorerTree.setAllowsChildren(true);

		// Adds the root node to the tree
		MainWindow.getInstance().getExplorerPanel().getRoot().add(explorerTree);

		// Creates the directory list
		ArrayList<DefaultMutableTreeNode> directoryList = new ArrayList<DefaultMutableTreeNode>();

		// Adds the associated project files to the explorer
		for (int index = 0; index < AcideProjectConfiguration.getInstance()
				.getNumberOfFilesFromList(); index++) {

			// Gets the file from the project configuration
			DefaultMutableTreeNode file = new DefaultMutableTreeNode(
					AcideProjectConfiguration.getInstance().getFileAt(index));

			// If it is directory
			if (AcideProjectConfiguration.getInstance().getFileAt(index)
					.isDirectory()) {

				// It can have files inside
				file.setAllowsChildren(true);

				// Adds the file
				directoryList.add(file);
			} else
				// Can't have files inside
				file.setAllowsChildren(false);

			// If the file is in the same folder than the project folder
			if (AcideProjectConfiguration.getInstance().getFileAt(index)
					.getParent()
					.equals(AcideProjectConfiguration.getInstance().getName())) {
				// Adds the file
				explorerTree.add(file);
			} else {

				// Searches for it in the tree structure
				DefaultMutableTreeNode fileAux = MainWindow
						.getInstance()
						.getExplorerPanel()
						.searchDirectoryList(
								directoryList,
								AcideProjectConfiguration.getInstance()
										.getFileAt(index).getParent());

				// Adds the file
				fileAux.add(file);
			}
		}

		// Updates the explorer tree
		MainWindow.getInstance().getExplorerPanel().getTreeModel().reload();

		// Repaints the explorer tree
		MainWindow.getInstance().getExplorerPanel().expandTree();

		// If there are files associated to the project
		if (AcideProjectConfiguration.getInstance().getNumberOfFilesFromList() > 0) {

			// Enables the remove file menu item
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFile().setEnabled(true);

			// Enables the delete file menu item
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFile().setEnabled(true);
		} else {

			// Disables the remove file menu item
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFile().setEnabled(false);

			// Disables the delete file menu item
			MainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFile().setEnabled(false);
		}

		// Updates the main window
		MainWindow.getInstance().validate();

		// Repaints the main window
		MainWindow.getInstance().repaint();

		// If the show explorer panel is selected
		if (!MainWindow.getInstance().getMenu().getView()
				.getShowExplorerPanel().isSelected())
			// Shows the explorer
			MainWindow.getInstance().getExplorerPanel().showExplorerPanel();
	}

	/**
	 * Loads the project language.
	 */
	public void loadLanguage() {

		// Gets the language configuration
		String configurationLanguage = AcideProjectConfiguration.getInstance()
				.getLanguage();

		// SPANISH
		if (configurationLanguage.equals("spanish"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getSpanish().doClick();

		// ENGLISH
		if (configurationLanguage.equals("english"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getEnglish().doClick();

		// Updates the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s449")
								+ " ");
	}

	/**
	 * Loads the project menu configuration.
	 */
	public void loadMenuConfiguration() {

		String currentMenu = null;

		try {

			// Gets the menu configuration file path
			currentMenu = AcideProjectConfiguration.getInstance().getMenu();

			// Loads the new menu item list
			AcideMenuConfiguration.getInstance().setMenuElementList(
					AcideMenuConfiguration.getInstance()
							.loadMenuConfigurationFile(currentMenu));

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty(
					"currentMenuConfiguration", currentMenu);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Gets the name
			String currentMenu2;
			int index = currentMenu.lastIndexOf("\\");
			if (index == -1)
				index = currentMenu.lastIndexOf("/");
			currentMenu2 = ".\\configuration\\menu\\"
					+ currentMenu.substring(index + 1, currentMenu.length());

			try {

				// Load the new menu item list
				AcideMenuConfiguration.getInstance().setMenuElementList(
						AcideMenuConfiguration.getInstance()
								.loadMenuConfigurationFile(currentMenu));

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", currentMenu2);

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s956")
						+ currentMenu
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s957") + currentMenu2);

			} catch (Exception exception1) {

				try {

					// Loads the menu configuration
					AcideMenuConfiguration.getInstance().setMenuElementList(
							AcideMenuConfiguration.getInstance()
									.loadMenuConfigurationFile(currentMenu));
				} catch (Exception exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration",
						"./configuration/menu/defaultAllOn.menuCfg");

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s956")
						+ currentMenu
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s959"));

				// Updates the log
				AcideLog.getLog().error(exception1.getMessage());
				exception1.printStackTrace();
			}
		}

		// Builds the menu
		MainWindow.getInstance().getMenu().buildMenu();

		// Updates the MAIN WINDOW
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();

		// Enables the save menu menu item
		MainWindow.getInstance().getMenu().getConfiguration().getMenu()
				.getSaveMenu().setEnabled(true);

		// The changes are saved
		AcideMenuConfigurationWindow.setChangesAreSaved(true);
	}

	/**
	 * Loads the project grammar configuration.
	 */
	public void loadGrammarConfiguration() {

		try {

			// Gets the grammar configuration
			String currentGrammar = AcideProjectConfiguration.getInstance()
					.getGrammarConfiguration();

			// Gets the grammar name
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			String grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s248")
									+ " " + grammarName);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("currentGrammar",
					currentGrammar);
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s944"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Loads the project shell configuration.
	 */
	public void loadConsoleConfiguration() {

		// Gets the output configuration
		AcideConsoleConfiguration.getInstance().load(
				AcideProjectConfiguration.getInstance()
						.getConsoleConfiguration());

		// Resets the output
		// MainWindow.getInstance().getOutput().executeExitCommand();
		MainWindow.getInstance().getConsolePanel().resetConsole();

		// Updates the MAIN WINDOW
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
	}

	/**
	 * Loads the project tool bar configuration.
	 */
	public void loadToolBarConfiguration() {

		String currentToolBarConfiguration = null;

		try {

			// Clears the list of shell commands
			ConsoleCommandList.clear();

			// Gets the tool bar configuration
			currentToolBarConfiguration = AcideProjectConfiguration
					.getInstance().getToolBarConfiguration();

			// Loads the command list from the configuration file
			ConsoleCommandList.loadFinalList(currentToolBarConfiguration);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty(
					"currentToolBarConfiguration", currentToolBarConfiguration);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Gets the name
			String currentToolBarConfiguration2;
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");

			// Updates the current tool bar configuration
			currentToolBarConfiguration2 = ".\\configuration\\toolbar\\"
					+ currentToolBarConfiguration.substring(index + 1,
							currentToolBarConfiguration.length());
			try {

				// Loads the command list from the configuration file
				ConsoleCommandList.loadFinalList(currentToolBarConfiguration2);

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s958")
						+ currentToolBarConfiguration
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s957")
						+ currentToolBarConfiguration2);

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration",
						currentToolBarConfiguration2);
			} catch (Exception exception1) {

				// Updates the log
				AcideLog.getLog().error(exception1.getMessage());
				exception1.printStackTrace();

				try {

					// Loads the command list from the configuration file
					ConsoleCommandList
							.loadFinalList("./configuration/toolbar/default.TBcfg");
				} catch (Exception exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s958")
						+ currentToolBarConfiguration
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s959"));

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration",
						"./configuration/toolbar/default.TBcfg");
			}
		}

		// Builds the tool bar
		MainWindow.getInstance().buildToolBarPanel();

		// Enables the save tool bar menu item
		MainWindow.getInstance().getMenu().getConfiguration().getToolBar()
				.getSaveToolBar().setEnabled(true);

		// The changes are saved
		AcideToolBarConfigurationWindow.setAreChangesSaved(true);
	}

	/**
	 * Loads the file editor configuration from the configuration file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void loadNewProjectConfiguration(String configurationFilePath) {

		// Builds the text configuration file
		AcideTextFile textConfigurationFile = AcideIOFactory.getInstance()
				.buildFile();

		// Loads the configuration file content
		String configurationFileContent = textConfigurationFile
				.load(configurationFilePath);

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty("defaultAcideProject",
				configurationFilePath);

		// Sets the project path
		AcideProjectConfiguration.getInstance().setPath(configurationFilePath);

		// Removes the previous associated files to the project
		AcideProjectConfiguration.getInstance().removeFiles();

		// Loads the project configuration
		AcideProjectConfiguration.getInstance().load(configurationFileContent);
	}

	/**
	 * Loads the project main window configuration.
	 */
	public void loadMainWindowConfiguration() {

		// Is explorer panel showed?
		if (!AcideWindowConfiguration.getInstance().isExplorerPanelShowed())

			// Shows the explorer panel
			MainWindow.getInstance().getMenu().getView().getShowExplorerPanel()
					.doClick();

		// Is console panel showed?
		if (!AcideWindowConfiguration.getInstance().isConsolePanelShowed())

			// Shows the console panel
			MainWindow.getInstance().getMenu().getView().getShowConsolePanel()
					.doClick();

		// MAIN WINDOW SIZE
		MainWindow.getInstance().setSize(
				AcideWindowConfiguration.getInstance().getWindowWidth(),
				AcideWindowConfiguration.getInstance().getWindowHeight());

		// MAIN WINDOW LOCATION
		MainWindow.getInstance().setLocation(
				AcideWindowConfiguration.getInstance().getXCoordinate(),
				AcideWindowConfiguration.getInstance().getYCoordinate());

		// VERTICAL SPLIT PANE DIVIDER LOCATION
		MainWindow
				.getInstance()
				.getVerticalSplitPane()
				.setDividerLocation(
						AcideWindowConfiguration.getInstance()
								.getVerticalSplitPaneDividerLocation());

		// HORIZONTAL SPLIT PANE DIVIDER LOCATION
		MainWindow
				.getInstance()
				.getHorizontalSplitPane()
				.setDividerLocation(
						AcideWindowConfiguration.getInstance()
								.getHorizontalSplitPanelDividerLocation());

		// Updates the MAIN WINDOW
		MainWindow.getInstance().validate();

		// Repaints the MAIN WINDOW
		MainWindow.getInstance().repaint();
	}

	/**
	 * Loads the project file editor configuration. SwingUtilities is used to
	 * wait until the end of the execution of all the previous events so it can
	 * add all the editors properly and safety.
	 */
	public void loadFileEditorConfiguration() {

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty(
				"fileEditorConfiguration",
				AcideProjectConfiguration.getInstance()
						.getFileEditorConfiguration());

		// Gets the file editor configuration
		AcideFileEditorConfiguration fileEditorConfiguration = AcideFileEditorConfiguration
				.getInstance();
		try {
			fileEditorConfiguration.load(AcideResourceManager.getInstance()
					.getProperty("fileEditorConfiguration"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					// Checks if the file really exists
					File file = new File(AcideProjectConfiguration
							.getInstance().getFileAt(index).getAbsolutePath());

					// If the file is not a directory and exists
					if (!AcideProjectConfiguration.getInstance()
							.getFileAt(index).isDirectory()
							&& file.exists()) {

						AcideTextFile textFile = AcideIOFactory.getInstance()
								.buildFile();
						String text = null;
						text = textFile.load(AcideProjectConfiguration
								.getInstance().getFileAt(index)
								.getAbsolutePath());

						String fileName = null;
						String filePath = AcideProjectConfiguration
								.getInstance().getFileAt(index)
								.getAbsolutePath();

						// Gets the file name
						if (filePath != null) {

							int lastIndexOfSlash = filePath.lastIndexOf("\\");
							if (lastIndexOfSlash == -1)
								lastIndexOfSlash = filePath.lastIndexOf("/");
							fileName = filePath.substring(lastIndexOfSlash + 1,
									filePath.length());
						}

						// If it is the default project and it has to be opened
						if (AcideProjectConfiguration.getInstance()
								.isDefaultProject()
								|| AcideProjectConfiguration.getInstance()
										.getFileAt(index).isOpened()) {

							// Enables the file menu
							MainWindow.getInstance().getMenu().enableFileMenu();

							// Enables the edit menu
							MainWindow.getInstance().getMenu().enableEditMenu();

							// Updates the status bar
							MainWindow
									.getInstance()
									.getStatusBar()
									.setStatusMessage(
											AcideProjectConfiguration
													.getInstance()
													.getFileAt(index)
													.getAbsolutePath());

							// Check if it is a MAIN or COMPILABLE FILE
							AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

							// COMPILABLE
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index).isCompilableFile()) {

								fileType = AcideProjectFileType.COMPILABLE;

								// Adds the <COMPILABLE> tag in the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setStatusMessage(
												AcideProjectConfiguration
														.getInstance()
														.getFileAt(index)
														.getAbsolutePath()
														+ " <COMPILABLE>");
							}

							// MAIN
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index).isMainFile()) {

								fileType = AcideProjectFileType.MAIN;

								// Adds the <MAIN> tag in the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setStatusMessage(
												AcideProjectConfiguration
														.getInstance()
														.getFileAt(index)
														.getAbsolutePath()
														+ " <MAIN>");
							}

							// Opens a new tab in the editor
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.newTab(fileName, filePath, text, true,
											fileType, 0);

							// Checks if it is marked as a MAIN or COMPILABLE
							// FILE
							for (int i = 0; i < MainWindow.getInstance()
									.getFileEditorManager()
									.getNumberOfFileEditorPanels(); i++) {

								if (MainWindow
										.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(i)
										.getAbsolutePath()
										.equals(AcideProjectConfiguration
												.getInstance().getFileAt(index)
												.getAbsolutePath())) {

									// IS COMPILABLE FILE?
									if (AcideProjectConfiguration.getInstance()
											.getFileAt(index)
											.isCompilableFile())
										MainWindow.getInstance()
												.getFileEditorManager()
												.getFileEditorPanelAt(i)
												.setCompilableFile(true);

									// IS MAIN FILE?
									if (AcideProjectConfiguration.getInstance()
											.getFileAt(index).isMainFile())
										MainWindow.getInstance()
												.getFileEditorManager()
												.getFileEditorPanelAt(i)
												.setMainFile(true);
								}
							}

							// Enables the file menu
							MainWindow.getInstance().getMenu().enableFileMenu();

							// Enables the edit menu
							MainWindow.getInstance().getMenu().enableEditMenu();

							// Updates the undo manager
							AcideUndoRedoManager.getInstance().update();
						}

						// The project configuration has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								false);
					} else {

						// If the file does not exist
						if (!file.exists()) {

							if (!AcideProjectConfiguration.getInstance()
									.getFileAt(index).isDirectory()) {
								// Error message
								JOptionPane.showMessageDialog(
										null,
										AcideLanguageManager.getInstance()
												.getLabels().getString("s1020")
												+ file.getAbsolutePath()
												+ " " +AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s1021"),
										"Warning", JOptionPane.WARNING_MESSAGE);

								// The project configuration has been modified
								AcideProjectConfiguration.getInstance()
										.setIsModified(true);
							}
						}
					}
				}

				// Sets the selected editor
				if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanelIndex() != -1) {

					// Sets the selected file editor
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.setSelectedFileEditorPanelAt(
									MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanelIndex());

					// Sets the focus in the edition area
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().requestFocusInWindow();

					// Sets the caret visible
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().getCaret()
							.setVisible(true);

					// Selects the tree node
					MainWindow.getInstance().getExplorerPanel()
							.selectTreeNodeFromFileEditor();

					// Updates the status bar with the selected editor
					MainWindow.getInstance().getStatusBar()
							.updatesStatusBarFromFileEditor();
				}
			}
		});
	}

	/**
	 * Closes the opened file editors and store the window parameters.
	 */
	public void closePreviousProjectConfiguration() {

		// Saves the window configuration
		AcideWindowConfiguration.getInstance().save();

		// Gets the selected file editor index
		int selectedFileEditorIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the opened file editors number
		int numFileEditors = MainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels();

		// Search for modified opened file editors
		for (int index = numFileEditors - 1; index >= 0; index--) {

			// Starts from the last opened file editor
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index);

			// If it is modified
			if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {

				// Do you want to save it?
				int choosenOption = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s643"), AcideLanguageManager
								.getInstance().getLabels().getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// If OK
				if (choosenOption == JOptionPane.OK_OPTION) {

					// Saves the editor
					MainWindow.getInstance().getMenu().getFile().saveOrSaveAS();
				}
			}
		}

		// Sets the original selected opened file editor
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorIndex);

		// Close the editors
		for (int position = 0; position < numFileEditors; position++) {
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.remove(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.validate();
		}
	}

	/**
	 * Loads the lexicon configuration and apply it to the opened file editors.
	 */
	public void loadLexiconConfiguration() {

		// Gets the lexicon configuration from the project configuration
		AcideResourceManager.getInstance().setProperty(
				"languagePath",
				AcideProjectConfiguration.getInstance()
						.getLexiconConfiguration());

		// Loads the lexicon configuration
		AcideLexiconConfiguration lexiconConfiguration = AcideLexiconConfiguration
				.getInstance();
		lexiconConfiguration.load(AcideProjectConfiguration.getInstance()
				.getLexiconConfiguration());

		// Gets the opened file editor panel number
		int numFileEditorPanels = MainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels();

		// Resets all the opened files with the new lexicon configuration
		for (int index = 0; index < numFileEditorPanels; index++)
			MainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).resetDocument();

		// Loads the language
		loadLanguage();

		// Updates the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s449")
								+ " " + lexiconConfiguration.getName());
	}
}
