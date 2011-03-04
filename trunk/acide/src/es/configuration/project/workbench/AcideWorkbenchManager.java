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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.ResourceBundle;
import java.util.Vector;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.console.AcideConsoleConfiguration;
import es.configuration.fileEditor.AcideFileEditorConfiguration;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import es.configuration.window.AcideWindowConfiguration;
import es.project.AcideProjectFile;
import es.project.AcideProjectFileType;
import es.text.AcideFileManager;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;
import gui.savingResourcesWindow.AcideSavingResourcesWindow;
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
	 * ACIDE - A Configurable IDE workbench manager unique class instance.
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
	 * ACIDE - A Configurable IDE workbench manager recent files list.
	 */
	private ArrayList<String> _recentFiles;

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
	 * Loads the configuration associated to the recent files opened previously
	 * in the application from its configuration file.
	 */
	public void loadRecentFilesWorkbenchConfiguration() {

		// Creates the recent files list
		_recentFiles = new ArrayList<String>();

		// Loads the configuration file into the list
		try {

			// Creates and configures the buffered reader to read the
			// configuration file
			BufferedReader bufferedReader = new BufferedReader(new FileReader(
					"./configuration/recentFiles.config"));

			String filePath = "";

			// Builds the recent files list
			while ((filePath = bufferedReader.readLine()) != null) {
				_recentFiles.add(filePath);
			}

			// Closes the buffered reader
			bufferedReader.close();

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Saves the configuration associated to the recent files opened previously
	 * in the application into its configuration file.
	 */
	public void saveRecentFilesWorkbenchConfiguration() {

		// Saves the list into the configuration file
		try {

			// Creates and configures the buffered writer to write into the
			// configuration file
			BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(
					"./configuration/recentFiles.config"));

			// Writes the recent file list into the configuration file
			for (String filePath : _recentFiles)
				bufferedWriter.write(filePath + "\n");

			// Closes the buffered writer
			bufferedWriter.close();

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
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

		// Loads the RECENT FILES CONFIGURATION
		loadRecentFilesWorkbenchConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				12,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1027"));

		// Loads the PROJECT CONFIGURATION
		AcideProjectConfiguration.getInstance().load(configurationFileContent);
		AcideSplashScreenWindow.getInstance().setProgressBar(
				25,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1028"));

		// Loads the LEXICON CONFIGURATION
		AcideLexiconConfiguration lexiconConfiguration = loadLexiconWorkbenchConfiguration();
		AcideSplashScreenWindow.getInstance().setProgressBar(
				36,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1029"));

		// Loads the LANGUAGE FOR THE LABELS OF THE APPLICATION
		ResourceBundle labels = loadLanguageWorkbenchConfiguration();
		AcideSplashScreenWindow.getInstance().setProgressBar(
				48,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1030"));

		// Updates the lexicon message in the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						labels.getString("s449") + " "
								+ lexiconConfiguration.getName());

		// Loads the CONSOLE CONFIGURATION
		loadConsoleWorkbenchConfiguration();
		AcideSplashScreenWindow.getInstance().setProgressBar(
				56,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1031"));

		// Loads the EXPLORER CONFIGURATION
		loadExplorerWorkbenchConfiguration();
		AcideSplashScreenWindow.getInstance().setProgressBar(
				90,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1032"));

		// Loads the GRAMMAR CONFIGURATION
		loadGrammarWorkbenchConfiguration(labels);
		AcideSplashScreenWindow.getInstance().setProgressBar(
				95,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1033"));

		// Loads the EDITOR CONFIGURATION
		loadFileEditorWorkbenchConfiguration(labels);
		AcideSplashScreenWindow.getInstance().setProgressBar(
				98,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1034"));

		// Loads the MAIN WINDOW CONFIGURATION
		loadMainWindowConfiguration();
		AcideSplashScreenWindow.getInstance().setProgressBar(
				99,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1035"));
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

		for (int index1 = 0; index1 < AcideFileEditorConfiguration
				.getInstance().getNumberOfFilesFromList(); index1++) {

			// Checks if the file really exists
			File file = new File(AcideFileEditorConfiguration.getInstance()
					.getFileAt(index1).getPath());

			// If the file is not a directory and exists
			if (!file.isDirectory() && file.exists()) {

				// Gets its content
				String fileContent = null;
				fileContent = AcideFileManager.getInstance().load(
						AcideFileEditorConfiguration.getInstance()
								.getFileAt(index1).getPath());

				// Updates the status message in the status bar
				MainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index1).getPath());

				// Check if it is a MAIN or COMPILABLE FILE
				AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

				// COMPILABLE
				if (AcideFileEditorConfiguration.getInstance()
						.getFileAt(index1).getType() == AcideProjectFileType.COMPILABLE) {

					fileType = AcideProjectFileType.COMPILABLE;

					// Adds the <COMPILABLE> tag in the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideFileEditorConfiguration.getInstance()
											.getFileAt(index1).getPath()
											+ " <COMPILABLE>");
				}

				// MAIN
				if (AcideFileEditorConfiguration.getInstance()
						.getFileAt(index1).getType() == AcideProjectFileType.MAIN) {

					fileType = AcideProjectFileType.MAIN;

					// Adds the <MAIN> tag in the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideFileEditorConfiguration.getInstance()
											.getFileAt(index1).getPath()
											+ " <MAIN>");
				}

				// Opens a new tab in the editor
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.updatesTabbedPane(AcideFileEditorConfiguration.getInstance()
								.getFileAt(index1).getPath(),
								fileContent,
								true,
								fileType,
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index1).getCaretPosition(), AcideFileEditorConfiguration.getInstance()
										.getFileAt(index1).getSplitPaneDividerLocation());

				// Checks if it is marked as a MAIN or COMPILABLE FILE
				for (int index2 = 0; index2 < MainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels(); index2++) {

					if (MainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(index2)
							.getAbsolutePath()
							.equals(AcideFileEditorConfiguration.getInstance()
									.getFileAt(index1).getPath())) {

						// IS COMPILABLE FILE?
						if (AcideFileEditorConfiguration.getInstance()
								.getFileAt(index1).getType() == AcideProjectFileType.COMPILABLE)
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(index2)
									.setCompilableFile(true);

						// IS MAIN FILE?
						if (AcideFileEditorConfiguration.getInstance()
								.getFileAt(index1).getType() == AcideProjectFileType.MAIN)
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(index2)
									.setMainFile(true);
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

					// Look for the file in the project configuration
					boolean belongsToTheProject = false;
					for (int index2 = 0; index2 < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index2++) {

						if (AcideFileEditorConfiguration
								.getInstance()
								.getFileAt(index1)
								.getPath()
								.equals(AcideProjectConfiguration.getInstance()
										.getFileAt(index2).getAbsolutePath()))
							belongsToTheProject = true;
					}

					// If the file does not belongs to the project
					if (!belongsToTheProject)
						// Error message
						JOptionPane.showMessageDialog(
								null,
								labels.getString("s1020")
										+ file.getAbsolutePath() + " "
										+ labels.getString("s1021"), "Warning",
								JOptionPane.WARNING_MESSAGE);

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

		// If it is not the default project
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
	 */
	public static void loadExplorerWorkbenchConfiguration() {

		try {

			// If it is the default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Sets the title with the <empty> tag
				MainWindow.getInstance().setTitle(
						MainWindow.getInstance().getTitle() + " - <empty>");
			} else {

				// Sets all the features for the main window to allow the
				// options for an open project configuration
				String name = AcideProjectConfiguration.getInstance().getName();

				// Creates the new project file
				AcideProjectFile newProjectFile = new AcideProjectFile();
				newProjectFile.setAbsolutePath(name);
				newProjectFile.setName(name);
				newProjectFile.setParent(null);
				newProjectFile.setIsDirectory(true);

				// Enables add file menu item in the explorer popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getAddFile().setEnabled(true);

				// Enables the remove file menu item in the explorer popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getRemoveFile().setEnabled(true);

				// Sets the project title
				MainWindow.getInstance().setTitle(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s425")
								+ " - "
								+ AcideProjectConfiguration.getInstance()
										.getName());

				// Builds the EXPLORER TREE with all the associated files
				MainWindow.getInstance().getExplorerPanel().getRoot()
						.removeAllChildren();

				// Creates the new explorer node
				DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode(
						newProjectFile);

				// Adds the new node to the explorer tree root
				MainWindow.getInstance().getExplorerPanel().getRoot()
						.add(rootNode);

				// Creates the directory list
				ArrayList<DefaultMutableTreeNode> directoryList = new ArrayList<DefaultMutableTreeNode>();

				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					// Gets the node
					DefaultMutableTreeNode fileProjectNode = new DefaultMutableTreeNode(
							AcideProjectConfiguration.getInstance().getFileAt(
									index));

					// Directory?
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index).isDirectory()) {

						// Allows children in the tree
						fileProjectNode.setAllowsChildren(true);

						// Adds the node to the directory list
						directoryList.add(fileProjectNode);

					} else {
						// No children are allowed
						fileProjectNode.setAllowsChildren(false);
					}
					// If the file already exists in the level above
					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getParent()
							.equals(AcideProjectConfiguration.getInstance()
									.getName())) {

						// Adds the new node
						rootNode.add(fileProjectNode);
					} else {

						// Searches for the node
						DefaultMutableTreeNode parentNode = MainWindow
								.getInstance()
								.getExplorerPanel()
								.searchDirectoryList(
										directoryList,
										AcideProjectConfiguration.getInstance()
												.getFileAt(index).getParent());

						// Adds the new node
						parentNode.add(fileProjectNode);
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
						.getNumberOfFilesFromList() > 0)

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

		// Deletes all the files associated to the project
		AcideProjectConfiguration.getInstance().removeFiles();

		String fileContent = null;
		String path = null;

		try {

			// Gets the default ACIDE - A Configurable IDE project path
			path = AcideResourceManager.getInstance().getProperty(
					"defaultAcideProject");

			// Loads its content
			fileContent = AcideFileManager.getInstance().load(path);

			// If it can't find the file
			if (fileContent == null) {

				// Loads the default file
				fileContent = AcideFileManager.getInstance().load(
						"./configuration/project/default.acidePrj");

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"defaultAcideProject",
						"./configuration/project/default.acidePrj");

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s960")
						+ path
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s959"));
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Loads the default file
			fileContent = AcideFileManager.getInstance().load(
					"./configuration/project/default.acidePrj");

			// Error message
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s960")
							+ path
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s959"));
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

			// Gets the default ACIDE - A Configurable IDE project file path
			String filePath = AcideResourceManager.getInstance().getProperty(
					"defaultAcideProject");

			// Saves the editor manager configuration
			AcideFileEditorConfiguration.getInstance().save();

			// If it is the default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Removes the files related to the project
				AcideProjectConfiguration.getInstance().removeFiles();

				// Sets the all opened files in the editor
				for (int index = 0; index < MainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

					// Creates the file
					AcideProjectFile explorerFile = new AcideProjectFile();
					explorerFile.setAbsolutePath(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index)
							.getAbsolutePath());

					// Sets if it is compilable file
					explorerFile.setIsCompilableFile(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index)
							.isCompilableFile());

					// Sets if it is main file
					explorerFile.setIsMainFile(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index)
							.isMainFile());

					// Sets the name
					explorerFile.setName(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index)
							.getName());

					// Is a not a directory
					explorerFile.setIsDirectory(false);

					// Adds the file to the configuration
					AcideProjectConfiguration.getInstance().addFile(
							explorerFile);
				}

				// Sets the language
				AcideProjectConfiguration.getInstance()
						.setLanguageConfiguration(
								AcideResourceManager.getInstance().getProperty(
										"language"));

				// Sets the name
				AcideProjectConfiguration.getInstance().setName("");

				// Saves the configuration in the file
				AcideFileManager.getInstance().write(
						"./configuration/project/default.acidePrj",
						AcideProjectConfiguration.getInstance().save());

				// Sets the default project
				AcideResourceManager.getInstance().setProperty(
						"defaultAcideProject",
						"./configuration/project/default.acidePrj");
			} else {

				// Saves the configuration of the project
				AcideFileManager.getInstance().write(filePath,
						AcideProjectConfiguration.getInstance().save());
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Returns the new file index.
	 * 
	 * @return the new file index.
	 */
	private int getNewFileIndex() {

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the number of file editor panels
		int numberOfEditorPanels = MainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels();

		// Index for the searching
		int newFileIndex = -1;

		// Starts with the last editor
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(numberOfEditorPanels - 1);

		for (int index = numberOfEditorPanels - 1; index >= 0; index--) {

			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index);

			// If it is new file
			if (MainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(index)
					.getAbsolutePath()
					.equals(AcideLanguageManager.getInstance().getLabels()
							.getString("s79"))) {

				// Found it
				newFileIndex = index;
			}
		}

		// Set the original selected editor
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorPanelIndex);

		return newFileIndex;
	}

	/**
	 * Returns the log file index.
	 * 
	 * @return the log file index.
	 */
	private int getLogFileIndex() {

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the number of file editor panels
		int numberOfEditorPanels = MainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels();

		// For the searching
		int logFileIndex = -1;

		// Starts with the last one
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(numberOfEditorPanels - 1);

		for (int index = numberOfEditorPanels - 1; index >= 0; index--) {

			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index);

			// If it is the log file
			if (MainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).getAbsolutePath()
					.equals("Log")) {

				// Found it
				logFileIndex = index;
			}
		}

		// Sets the original selected editor
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorPanelIndex);

		return logFileIndex;
	}

	/**
	 * Saves the current workbench configuration.
	 */
	public void saveWorkbenchConfiguration() {

		// Is the project configuration modified?
		if (AcideProjectConfiguration.getInstance().isModified()) {

			// Ask the user to save the configuration
			int resultValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s657"), AcideLanguageManager
							.getInstance().getLabels().getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is not the cancel option
			if (resultValue != JOptionPane.CANCEL_OPTION) {

				// If it is yes
				if (resultValue == JOptionPane.YES_OPTION) {

					// If it is not the default project
					if (!AcideProjectConfiguration.getInstance()
							.isDefaultProject()) {

						// Enables the menu
						MainWindow.getInstance().getMenu().getProject()
								.getSaveProject().setEnabled(true);

						// Saves the project
						MainWindow.getInstance().getMenu().getProject()
								.getSaveProject().doClick();

						// Gets the selected file editor panel index
						int selectedFileEditorPanelIndex = MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex();

						// Gets the number of opened file editor panels
						int numberOfFileEditorPanels = MainWindow.getInstance()
								.getFileEditorManager()
								.getNumberOfFileEditorPanels();

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
								if (MainWindow.getInstance()
										.getFileEditorManager().isRedButton()) {

									// Adds the file to the resource list
									resourceList.add(MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getFileName());

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
							if (countModified > 0)
								// Shows the saving resources window
								new AcideSavingResourcesWindow(resourceList);
							else {

								// Save the rest of the workbench configuration
								saveRestOfWorkbenchConfiguration();

								// Closes the main window
								System.exit(0);
							}
						}else{
							
							// Closes the main window
							System.exit(0);
							
							// Saves the recent files configuration
							saveRecentFilesWorkbenchConfiguration();
						}
					} else {
						
						// Saves the recent files configuration
						saveRecentFilesWorkbenchConfiguration();
						
						// Save the rest of the workbench configuration
						saveRestOfWorkbenchConfiguration();

						// Closes the main window
						System.exit(0);
					}
				} else if (resultValue == JOptionPane.NO_OPTION)
					// Closes the main window
					System.exit(0);
			}
		
		} else {

			// Saves the recent files configuration
			saveRecentFilesWorkbenchConfiguration();
			
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

		// Saves the console configuration
		AcideConsoleConfiguration.getInstance().save();

		// Closes the console panel
		MainWindow.getInstance().getConsolePanel().executeExitCommand();

		try {

			// Menu configuration
			String currentMenu = AcideResourceManager.getInstance()
					.getProperty("currentMenuConfiguration");

			if ((currentMenu.endsWith("lastModified.menuCfg"))
					|| (currentMenu.endsWith("newMenu.menuCfg"))) {
				String previous = AcideResourceManager.getInstance()
						.getProperty("previousMenuConfiguration");

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", previous);
			}

			// Tool bar configuration
			String currentToolBar = AcideResourceManager.getInstance()
					.getProperty("currentToolBarConfiguration");
			if ((currentToolBar.endsWith("lastModified.TBcfg"))
					|| currentToolBar.endsWith("newToolBar.TBcfg")) {
				String previous = AcideResourceManager.getInstance()
						.getProperty("previousToolBarConfiguration");

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", previous);
			}

			// Grammar configuration
			String currentGrammar = AcideResourceManager.getInstance()
					.getProperty("currentGrammar");
			if ((currentGrammar.endsWith("lastModified.jar"))
					|| (currentGrammar.endsWith("newGrammar.jar"))) {
				String previous = AcideResourceManager.getInstance()
						.getProperty("previousGrammar");

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentGrammar", previous);
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s294"), JOptionPane.ERROR_MESSAGE);
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

	/**
	 * Returns the ACIDE - A Configurable IDE workbench configuration recent
	 * files opened.
	 * 
	 * @return the ACIDE - A Configurable IDE workbench configuration recent
	 *         files opened.
	 */
	public ArrayList<String> getRecentFiles() {
		return _recentFiles;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE workbench
	 * configuration recent files opened.
	 * 
	 * @param recentFiles
	 *            new value to set.
	 */
	public void setRecentFiles(ArrayList<String> recentFiles) {
		_recentFiles = recentFiles;
	}

	/**
	 * Adds a new file path to the recent file list, avoiding duplicates.
	 * 
	 * @param filePath
	 *            new file path to add.
	 */
	public void addRecentFileToList(String filePath) {

		if (!_recentFiles.contains(filePath)) {

			// Adds the file to the recent file list
			_recentFiles.add(filePath);

			// Updates the menu
			MainWindow.getInstance().getMenu().getFile().getOpenRecentFiles()
					.buildRecentFilesMenu();
		}
	}
}
