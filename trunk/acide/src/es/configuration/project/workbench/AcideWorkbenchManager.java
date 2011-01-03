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
package es.configuration.project.workbench;

import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;
import java.util.Vector;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.console.AcideConsoleConfiguration;
import es.configuration.fileEditor.AcideFileEditorConfiguration;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import es.configuration.window.AcideWindowConfiguration;
import es.project.AcideProjectFile;
import es.project.AcideProjectFileType;
import es.text.TextFile;
import gui.mainWindow.MainWindow;
import gui.mainWindow.utils.AcideSavingResourcesWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;
import gui.splashScreen.AcideSplashScreenWindow;

/**
 * Handles the workbench configuration of ACIDE - A Configurable IDE. Loads the
 * workbench configuration from the workbench configuration file at the
 * beginning of the application, and stores the workbench configuration when the
 * user close the project or the application.
 * 
 * @version 0.8
 */
public class AcideWorkbenchManager {

	/**
	 * Acide - A Configurable IDE workbench manager unique class instance.
	 */
	private static AcideWorkbenchManager _instance;
	/**
	 * Flag which is used to determine if the workbench configuration has been
	 * finally loaded. The method updatesEditorAndProjectState in the class
	 * AcideFileEditorPanelDocumentListener doesn't check the TestPlaf for the
	 * closing buttons avoiding the exceptions.
	 */
	private boolean _workbenchLoaded = false;

	/**
	 * Returns the Acide - A Configurable IDE workbench manager unique class
	 * instance.
	 * 
	 * @return the Acide - A Configurable IDE workbench manager unique class
	 *         instance.
	 */
	public static AcideWorkbenchManager getInstance() {

		if (_instance == null)
			_instance = new AcideWorkbenchManager();
		return _instance;
	}

	/**
	 * Searches for a file into the list of files.
	 * 
	 * @param list
	 *            list of files.
	 * @param fileName
	 *            file name to search for.
	 * @return the file itself if it exists, and null in the opposite case.
	 */
	private static DefaultMutableTreeNode searchDirectoryList(
			ArrayList<DefaultMutableTreeNode> list, String fileName) {

		int pos = 0;

		boolean found = false;

		while (pos < list.size() && !found) {

			DefaultMutableTreeNode temp = list.get(pos);

			AcideProjectFile file = (AcideProjectFile) temp.getUserObject();

			if (file.getName().equals(fileName)) {
				found = true;
				return list.get(pos);
			} else
				pos++;
		}
		return null;
	}

	/**
	 * Loads the configuration associated to the project based in the content of
	 * the file previously loaded.
	 * 
	 * @param configurationFileContent
	 *            configuration file content.
	 */
	public void loadMainWindowWorkbenchConfiguration(
			String configurationFileContent) {

		// Loads the PROJECT CONFIGURATION
		AcideProjectConfiguration.getInstance().load(configurationFileContent);
		AcideSplashScreenWindow.setProgressBar(25,
				"Loading ACIDE - A Configurable IDE Project Configuration");

		// Loads the LEXICON CONFIGURATION
		AcideLexiconConfiguration lexiconConfiguration = loadLexiconWorkbenchConfiguration();
		AcideSplashScreenWindow.setProgressBar(36,
				"Loading ACIDE - A Configurable IDE Lexicon Configuration");

		// Loads the LANGUAGE FOR THE LABELS OF THE APPLICATION
		ResourceBundle labels = loadLanguageWorkbenchConfiguration();
		AcideSplashScreenWindow.setProgressBar(48,
				"Loading ACIDE - A Configurable IDE Language Configuration");

		// Loads the CONSOLE CONFIGURATION
		loadConsoleWorkbenchConfiguration();
		AcideSplashScreenWindow.setProgressBar(56,
				"Loading ACIDE - A Configurable IDE Console Configuration");

		// Loads the EXPLORER CONFIGURATION
		loadExplorerWorkbenchConfiguration(labels);
		AcideSplashScreenWindow.setProgressBar(90,
				"Loading ACIDE - A Configurable IDE Explorer Configuration");

		// Updates the status bar with the lexicon
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						labels.getString("s449") + " "
								+ lexiconConfiguration.getName());

		// Loads the GRAMMAR CONFIGURATION
		loadGrammarWorkbenchConfiguration(labels);
		AcideSplashScreenWindow.setProgressBar(95,
				"Loading ACIDE - A Configurable IDE File Editor Configuration");

		// Loads the EDITOR CONFIGURATION
		loadFileEditorWorkbenchConfiguration(labels);
		AcideSplashScreenWindow.setProgressBar(98,
				"Loading ACIDE - A Configurable IDE Grammar Configuration");

		// Loads the MAIN WINDOW CONFIGURATION
		loadMainWindowConfiguration();
		AcideSplashScreenWindow.setProgressBar(99,
				"Loading ACIDE - A Configurable IDE Main Window Configuration");
	}

	/**
	 * Loads the file editor workbench configuration, opening the related files
	 * to the project.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 * @return
	 */
	public void loadFileEditorWorkbenchConfiguration(ResourceBundle labels) {

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

		for (int index = 0; index < AcideFileEditorConfiguration.getInstance()
				.getNumFilesFromList(); index++) {

			// Checks if the file really exists
			File file = new File(AcideFileEditorConfiguration.getInstance()
					.getFileAt(index).getPath());

			// If the file is not a directory and exists
			if (!file.isDirectory() && file.exists()) {

				// Loads the text file content
				TextFile textFile = AcideIOFactory.getInstance().buildFile();
				String text = null;
				text = textFile.load(AcideFileEditorConfiguration.getInstance()
						.getFileAt(index).getPath());

				String fileName = null;
				String filePath = AcideFileEditorConfiguration.getInstance()
						.getFileAt(index).getPath();

				// Gets the file name
				if (filePath != null) {

					int lastIndexOfSlash = filePath.lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = filePath.lastIndexOf("/");
					fileName = filePath.substring(lastIndexOfSlash + 1,
							filePath.length());
				}

				// Updates the status bar
				MainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index).getPath());

				// Check if it is a MAIN or COMPILABLE FILE
				AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

				// COMPILABLE
				if (AcideFileEditorConfiguration.getInstance().getFileAt(index)
						.getType() == AcideProjectFileType.COMPILABLE) {

					fileType = AcideProjectFileType.COMPILABLE;

					// Adds the <COMPILABLE> tag in the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideFileEditorConfiguration.getInstance()
											.getFileAt(index).getPath()
											+ " <COMPILABLE>");
				}

				// MAIN
				if (AcideFileEditorConfiguration.getInstance().getFileAt(index)
						.getType() == AcideProjectFileType.MAIN) {

					fileType = AcideProjectFileType.MAIN;

					// Adds the <MAIN> tag in the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideFileEditorConfiguration.getInstance()
											.getFileAt(index).getPath()
											+ " <MAIN>");
				}

				// Opens a new tab in the editor
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.newTab(fileName,
								filePath,
								text,
								true,
								fileType,
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index).getCaretPosition());

				// Checks if it is marked as a MAIN or COMPILABLE FILE
				for (int i = 0; i < MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels(); i++) {

					if (MainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(i)
							.getAbsolutePath()
							.equals(AcideFileEditorConfiguration.getInstance()
									.getFileAt(index).getPath())) {

						// IS COMPILABLE FILE?
						if (AcideFileEditorConfiguration.getInstance()
								.getFileAt(index).getType() == AcideProjectFileType.COMPILABLE)
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(i)
									.setCompilableFile(true);

						// IS MAIN FILE?
						if (AcideFileEditorConfiguration.getInstance()
								.getFileAt(index).getType() == AcideProjectFileType.MAIN)
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(i).setMainFile(true);
					}
				}

				// Enables the file menu
				MainWindow.getInstance().getMenu().enableFileMenu();

				// Enables the edit menu
				MainWindow.getInstance().getMenu().enableEditMenu();

				// Updates the undo manager
				AcideUndoRedoManager.getInstance().update();

				// The project configuration has not been modified
				AcideProjectConfiguration.getInstance().setIsModified(false);
			} else {

				// If the file does not exist
				if (!file.exists()) {

					// Error message
					JOptionPane.showMessageDialog(null,
							labels.getString("s970")
									+ AcideProjectConfiguration.getInstance()
											.getFileAt(index).getAbsolutePath()
									+ labels.getString("s971"), "Error",
							JOptionPane.ERROR_MESSAGE);

					// Removes the file from the project
					AcideProjectConfiguration.getInstance().removeFileAt(index);

					// The project configuration has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}
			}
		}
	}

	/**
	 * Loads the grammar workbench configuration of the application.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public static void loadGrammarWorkbenchConfiguration(ResourceBundle labels) {

		String currentGrammar = null;
		String grammarName = null;

		try {

			// Gets the grammar configuration
			currentGrammar = AcideResourceManager.getInstance().getProperty(
					"currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							labels.getString("s248") + " " + grammarName);
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s945"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}

	/**
	 * Loads the language workbench configuration.
	 * 
	 * @return the language configuration.
	 */
	public static ResourceBundle loadLanguageWorkbenchConfiguration() {

		// Gets the language
		String configurationLanguage = AcideProjectConfiguration.getInstance()
				.getLanguage();
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		// SPANISH
		if (configurationLanguage.equals("spanish"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getSpanish().doClick();

		// ENGLISH
		if (configurationLanguage.equals("english"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getEnglish().doClick();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		return labels;
	}

	/**
	 * Loads the main window configuration.
	 */
	public static void loadMainWindowConfiguration() {

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty(
				"windowConfiguration",
				AcideProjectConfiguration.getInstance()
						.getWindowConfiguration());

		// Gets the window configuration
		AcideWindowConfiguration windowConfiguration = AcideWindowConfiguration
				.getInstance();
		try {
			windowConfiguration.load(AcideResourceManager.getInstance()
					.getProperty("windowConfiguration"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// If the explorer panel has not to be showed
		if (!AcideWindowConfiguration.getInstance().isExplorerPanelShowed())

			// Does the show explorer panel check box menu item action
			MainWindow.getInstance().getMenu().getView().getShowExplorerPanel()
					.doClick();

		// If the console panel has not to be showed
		if (!AcideWindowConfiguration.getInstance().isConsolePanelShowed())

			// Does the show explorer panel check box menu item action
			MainWindow.getInstance().getMenu().getView().getShowConsolePanel()
					.doClick();

		// Sets the main window size
		MainWindow.getInstance().setSize(
				AcideWindowConfiguration.getInstance().getWindowWidth(),
				AcideWindowConfiguration.getInstance().getWindowHeight());

		// Sets the main window location
		MainWindow.getInstance().setLocation(
				AcideWindowConfiguration.getInstance().getXCoordinate(),
				AcideWindowConfiguration.getInstance().getYCoordinate());

		// Sets the main window split panel vertical divider location
		MainWindow
				.getInstance()
				.getVerticalSplitPane()
				.setDividerLocation(
						AcideWindowConfiguration.getInstance()
								.getVerticalSplitPaneDividerLocation());

		// Sets the main window split panel horizontal divider location
		MainWindow
				.getInstance()
				.getHorizontalSplitPane()
				.setDividerLocation(
						AcideWindowConfiguration.getInstance()
								.getHorizontalSplitPanelDividerLocation());

		// Not default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// Enables the project menu
			MainWindow.getInstance().getMenu().enableProjectMenu();

			// Enables the open all files menu item
			MainWindow.getInstance().getMenu().getFile().getOpenAllFiles()
					.setEnabled(true);
		}

		// Updates the main window
		MainWindow.getInstance().validate();

		// Repaint the main window
		MainWindow.getInstance().repaint();
	}

	/**
	 * Loads the lexicon workbench configuration of ACIDE - A Configurable IDE.
	 * 
	 * @return the lexicon workbench configuration of ACIDE - Configurable IDE.
	 */
	public static AcideLexiconConfiguration loadLexiconWorkbenchConfiguration() {

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty(
				"languagePath",
				AcideProjectConfiguration.getInstance()
						.getLexiconConfiguration());

		// Gets the lexicon configuration
		AcideLexiconConfiguration lexiconConfiguration = AcideLexiconConfiguration
				.getInstance();
		try {
			lexiconConfiguration.load(AcideResourceManager.getInstance()
					.getProperty("languagePath"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		return lexiconConfiguration;
	}

	/**
	 * Loads the explorer files and builds the explorer in the main window.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public static void loadExplorerWorkbenchConfiguration(ResourceBundle labels) {

		try {

			// Default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Sets the title with the <empty> tag
				MainWindow.getInstance().setTitle(
						MainWindow.getInstance().getTitle() + " - <empty>");
			} else {

				// Sets all the features for the main window to allow the
				// options for an open project configuration
				String name = AcideProjectConfiguration.getInstance().getName();

				// Creates the new project file
				AcideProjectFile projectFile = new AcideProjectFile();
				projectFile.setAbsolutePath(name);
				projectFile.setName(name);
				projectFile.setParent(null);
				projectFile.setIsDirectory(true);

				// Enables add file menu item in the explorer popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getAddFile().setEnabled(true);

				// Enables the remove file menu item in the explorer popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getRemoveFile().setEnabled(true);

				// Sets the project title
				MainWindow.getInstance().setTitle(
						labels.getString("s425")
								+ " - "
								+ AcideProjectConfiguration.getInstance()
										.getName());

				// Builds the EXPLORER TREE with all the associated files
				MainWindow.getInstance().getExplorerPanel().getRoot()
						.removeAllChildren();

				// Creates the new explorer node
				DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
						projectFile);

				// Adds the new node to the explorer tree root
				MainWindow.getInstance().getExplorerPanel().getRoot()
						.add(defaultMutableTreeNode);

				// Creates the directory list
				ArrayList<DefaultMutableTreeNode> directoryList = new ArrayList<DefaultMutableTreeNode>();

				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumFilesFromList(); index++) {

					// Gets the node
					DefaultMutableTreeNode node = new DefaultMutableTreeNode(
							AcideProjectConfiguration.getInstance().getFileAt(
									index));

					// Checks if the file really exists
					File file = new File(AcideProjectConfiguration
							.getInstance().getFileAt(index).getAbsolutePath());

					// If exists
					if (file.exists()) {

						// Directory?
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(index).isDirectory()) {

							// Allows children in the tree
							node.setAllowsChildren(true);

							// Adds the node
							directoryList.add(node);
						} else
							// No children are allowed
							node.setAllowsChildren(false);

						// If the file already exists in the level above
						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index)
								.getParent()
								.equals(AcideProjectConfiguration.getInstance()
										.getName())) {

							// Adds the new node
							defaultMutableTreeNode.add(node);
						} else {

							// Searches for the node
							DefaultMutableTreeNode defaultMutableTreeNode1 = searchDirectoryList(
									directoryList, AcideProjectConfiguration
											.getInstance().getFileAt(index)
											.getParent());

							// Adds the new node
							defaultMutableTreeNode1.add(node);
						}
					}
				}

				// Updates the explorer tree
				MainWindow.getInstance().getExplorerPanel().getTreeModel()
						.reload();

				// Repaint the explorer tree
				MainWindow.getInstance().getExplorerPanel().expandTree();

				// Enables the add file menu item in the popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getAddFile().setEnabled(true);

				// Enables the save project menu item in the popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getSaveProject().setEnabled(true);

				// If it has more than 0 files associated
				if (AcideProjectConfiguration.getInstance()
						.getNumFilesFromList() > 0)

					// Allows to remove files in the EXPLORER menu
					MainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getRemoveFile().setEnabled(true);
				else
					// Removing files in the EXPLORER menu is not allowed
					MainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getRemoveFile().setEnabled(false);

				// Saves the default configuration
				AcideProjectConfiguration.getInstance().setFirstSave(true);

				// Updates the project configuration
				AcideProjectConfiguration.getInstance().setPath(
						AcideResourceManager.getInstance().getProperty(
								"defaultAcideProject"));
			}
		} catch (NumberFormatException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Loads the console configuration and applies it to the Output in the main
	 * window.
	 */
	public static void loadConsoleWorkbenchConfiguration() {

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty(
				"consoleConfiguration",
				AcideProjectConfiguration.getInstance()
						.getConsoleConfiguration());

		// Gets the console configuration
		AcideConsoleConfiguration consoleConfiguration = AcideConsoleConfiguration
				.getInstance();
		try {
			consoleConfiguration.load(AcideResourceManager.getInstance()
					.getProperty("consoleConfiguration"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Sets the console display options
		MainWindow.getInstance().getConsolePanel()
				.updateConsoleDisplayOptions();

		// Resets the console
		MainWindow.getInstance().getConsolePanel().resetConsole();
	}

	/**
	 * Gets the configuration file content which contains the configuration of
	 * the last opened project whether it is the "empty" configuration or
	 * another.
	 * 
	 * @return the content of the default configuration to load.
	 */
	public String getConfigurationFileContent() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// Deletes all the files associated to the project
		AcideProjectConfiguration.getInstance().removeFiles();

		// Gets the default configuration file content
		TextFile textFile = new TextFile();
		String fileContent = null;
		String path = null;

		try {

			path = AcideResourceManager.getInstance().getProperty(
					"defaultAcideProject");
			fileContent = textFile.load(path);

			// If it can't find the file
			if (fileContent == null) {

				// Loads the default file
				fileContent = textFile
						.load("./configuration/project/default.acidePrj");

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"defaultAcideProject",
						"./configuration/project/default.acidePrj");

				// Error message
				JOptionPane.showMessageDialog(null, labels.getString("s960")
						+ path + labels.getString("s959"));
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Loads the default file
			fileContent = textFile
					.load("./configuration/project/default.acidePrj");

			// Error message
			JOptionPane.showMessageDialog(null, labels.getString("s960") + path
					+ labels.getString("s959"));
		}
		return fileContent;
	}

	/**
	 * Saves the current file editor panel configuration. Closes the new and log
	 * tab in the editor and saves the state of each one of the opened files in
	 * the file editor into the project configuration, whether it is the default
	 * project or not.
	 */
	public void saveFileEditorPanelConfiguration() {

		try {

			// SPECIAL CASE: New file
			int newFileIndex = getNewFileIndex();

			// If it has new file opened
			if (newFileIndex != -1) {

				// Closes the new file so it will not be saved
				// in the configuration
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(newFileIndex);
			}

			// SPECIAL CASE: Log file
			int logFileIndex = getLogFileIndex();

			// If it has log file opened
			if (logFileIndex != -1) {

				// Closes the log file so it will not be saved in the
				// project configuration
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(logFileIndex);
			}

			String file = AcideResourceManager.getInstance().getProperty(
					"defaultAcideProject");

			// Saves the editor manager configuration
			AcideFileEditorConfiguration.getInstance().save();

			// Is default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Files which belong to the project
				TextFile textFile = new AcideIOFactory().buildFile();
				AcideProjectConfiguration.getInstance().removeFiles();

				// Sets the all opened files in the editor
				for (int pos = 0; pos < MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels(); pos++) {

					// Creates the file
					AcideProjectFile explorerFile = new AcideProjectFile();
					explorerFile.setAbsolutePath(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(pos)
							.getAbsolutePath());

					// Sets if it is compilable file
					explorerFile.setIsCompilableFile(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(pos)
							.isCompilableFile());

					// Sets if it is main file
					explorerFile.setIsMainFile(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(pos)
							.isMainFile());

					// Sets the name
					explorerFile.setName(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(pos)
							.getName());

					// Is a not a directory
					explorerFile.setIsDirectory(false);

					// Adds the file to the configuration
					AcideProjectConfiguration.getInstance().addFile(
							explorerFile);
				}

				// Sets the language
				AcideProjectConfiguration.getInstance().setLanguage(
						AcideResourceManager.getInstance().getProperty(
								"language"));

				// Sets the name
				AcideProjectConfiguration.getInstance().setName("");

				// Saves the configuration in the file
				textFile.save("./configuration/project/default.acidePrj",
						AcideProjectConfiguration.getInstance().save());

				// Sets the default project
				AcideResourceManager.getInstance().setProperty(
						"defaultAcideProject",
						"./configuration/project/default.acidePrj");
			} else {

				// Saves the configuration of the project
				TextFile textFile = new AcideIOFactory().buildFile();
				textFile.save(file, AcideProjectConfiguration.getInstance()
						.save());
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Returns the new file index
	 * 
	 * @return the new file index
	 */
	private int getNewFileIndex() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// Check the opened files in the editor
		int selectedEditor = MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanelIndex();
		int numEditors = MainWindow.getInstance().getFileEditorManager()
				.getNumFileEditorPanels();

		int newFileIndex = -1;

		// Starts with the last editor
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(numEditors - 1);

		for (int posEditor = numEditors - 1; posEditor >= 0; posEditor--) {

			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(posEditor);

			// If it is new file
			if (MainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(posEditor).getAbsolutePath()
					.equals(labels.getString("s79"))) {

				newFileIndex = posEditor;
			}
		}

		// Set the original selected editor
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedEditor);

		return newFileIndex;
	}

	/**
	 * Returns the log file index.
	 * 
	 * @return the log file index.
	 */
	private int getLogFileIndex() {

		// Check the opened files in the editor
		int selectedEditor = MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanelIndex();
		int numEditors = MainWindow.getInstance().getFileEditorManager()
				.getNumFileEditorPanels();

		int logFileIndex = -1;

		// Starts with the last one
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(numEditors - 1);

		for (int posEditor = numEditors - 1; posEditor >= 0; posEditor--) {

			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(posEditor);

			// If it is the log file
			if (MainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(posEditor).getAbsolutePath()
					.equals("Log")) {

				logFileIndex = posEditor;
			}
		}

		// Sets the original selected editor
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedEditor);
		return logFileIndex;
	}

	/**
	 * Saves the current workbench configuration.
	 */
	public void saveWorkbenchConfiguration() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Is the project configuration modified?
		if (AcideProjectConfiguration.getInstance().isModified()) {

			// Ask the user to save the configuration
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is not the cancel option
			if (chosenOption != JOptionPane.CANCEL_OPTION) {

				// If it is yes
				if (chosenOption == JOptionPane.YES_OPTION) {

					// Enables the menu
					MainWindow.getInstance().getMenu().getProject()
							.getSaveProject().setEnabled(true);

					// Saves the project
					MainWindow.getInstance().getMenu().getProject()
							.getSaveProject().doClick();

					// Gets the selected file editor panel index
					int selectedFileEditorPanelIndex = MainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					// Gets the number of opened file editor panels
					int numberOfFileEditorPanels = MainWindow.getInstance()
							.getFileEditorManager().getNumFileEditorPanels();

					// If there are opened file editor panels
					if (numberOfFileEditorPanels > 0) {
						
						// Stores the modified resources
						Vector<String> resourceList = new Vector<String>();
						
						// Counts the number of modified opened files
						int countModified = 0;
						
						for (int index = numberOfFileEditorPanels - 1; index >= 0; index--) {

							// Updates the selected file editor panel
							MainWindow.getInstance().getFileEditorManager()
									.setSelectedFileEditorPanelAt(index);

							// Is the file in the editor modified?
							if (MainWindow.getInstance().getFileEditorManager()
									.isRedButton()) {

								// Adds the file to the resource list
								resourceList.add(MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel().getFileName());
							
								// Updates the count modified
								countModified++;
							}
						}

						// Sets the original selected editor
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.setSelectedFileEditorPanelAt(
										selectedFileEditorPanelIndex);
						
						// If there are modified files
						if(countModified > 0)
							// Shows the saving resources window
							new AcideSavingResourcesWindow(resourceList);
						else{
							
							// Save the rest of the workbench configuration
							saveRestOfWorkbenchConfiguration();
							
							// Closes the main window
							System.exit(0);
						}
					}
					else{
						// Save the rest of the workbench configuration
						saveRestOfWorkbenchConfiguration();
						
						// Closes the main window
						System.exit(0);
					}
				}
				else
					if(chosenOption == JOptionPane.NO_OPTION)
						// Closes the main window
						System.exit(0);
			}
		} else {

			// Stores the configuration of the files
			saveFileEditorPanelConfiguration();

			// Saves the window configuration
			AcideWindowConfiguration.getInstance().save();

			// Closes the main window
			System.exit(0);
		}
	}

	/**
	 * Saves the rest of the workbench configuration once the opened modified
	 * file editor panels has been saved or not.
	 */
	public void saveRestOfWorkbenchConfiguration() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		// Saves the console configuration
		AcideConsoleConfiguration.getInstance().save();

		// Closes the console panel
		MainWindow.getInstance().getConsolePanel()
				.executeExitCommand();

		try {

			// Menu configuration
			String currentMenu = AcideResourceManager.getInstance()
					.getProperty("currentMenuConfiguration");

			if ((currentMenu.endsWith("lastModified.menuCfg"))
					|| (currentMenu.endsWith("newMenu.menuCfg"))) {
				String previous = AcideResourceManager
						.getInstance().getProperty(
								"previousMenuConfiguration");

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", previous);
			}

			// Tool bar configuration
			String currentToolBar = AcideResourceManager
					.getInstance().getProperty(
							"currentToolBarConfiguration");
			if ((currentToolBar.endsWith("lastModified.TBcfg"))
					|| currentToolBar.endsWith("newToolBar.TBcfg")) {
				String previous = AcideResourceManager
						.getInstance().getProperty(
								"previousToolBarConfiguration");

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", previous);
			}

			// Grammar configuration
			String currentGrammar = AcideResourceManager
					.getInstance().getProperty("currentGrammar");
			if ((currentGrammar.endsWith("lastModified.jar"))
					|| (currentGrammar.endsWith("newGrammar.jar"))) {
				String previous = AcideResourceManager
						.getInstance().getProperty(
								"previousGrammar");

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentGrammar", previous);
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			JOptionPane.showMessageDialog(null,
					exception.getMessage(),
					labels.getString("s294"),
					JOptionPane.ERROR_MESSAGE);
		}

		// Stores the configuration of the files
		saveFileEditorPanelConfiguration();

		// Saves the window configuration
		AcideWindowConfiguration.getInstance().save();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE workbench
	 * configuration loaded flag.
	 * 
	 * @param loaded
	 *            new value to set.
	 */
	public void setWorkbenchLoaded(boolean loaded) {
		_workbenchLoaded = loaded;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE workbench configuration loaded
	 * flag.
	 * 
	 * @return the ACIDE - A Configurable IDE workbench configuration loaded
	 *         flag.
	 */
	public boolean isWorkbenchLoaded() {
		return _workbenchLoaded;
	}
}
