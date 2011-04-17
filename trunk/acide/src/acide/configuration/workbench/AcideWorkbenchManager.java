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
package acide.configuration.workbench;

import java.awt.HeadlessException;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.resources.exception.MissedPropertyException;
import acide.configuration.console.AcideConsoleConfiguration;
import acide.configuration.fileEditor.AcideFileEditorConfiguration;
import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.lexiconAssigner.AcideLexiconAssignerConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.toolBar.AcideToolBarConfiguration;
import acide.configuration.window.AcideWindowConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.splashScreen.AcideSplashScreenWindow;

/**
 * <p>
 * ACIDE - A Configurable IDE workbench configuration manager.
 * </p>
 * <p>
 * Loads the workbench configuration from the workbench configuration file at
 * the beginning of the application, and stores the workbench configuration when
 * the user close the project or the application.
 * </p>
 * 
 * @version 0.8
 */
public class AcideWorkbenchManager {

	/**
	 * ACIDE - A Configurable IDE workbench manager unique class instance.
	 */
	private static AcideWorkbenchManager _instance;
	/**
	 * ACIDE - A Configurable IDE workbench manager recent files path constant.
	 */
	private static final String RECENT_FILES_PATH = "./configuration/recentFiles.config";
	/**
	 * ACIDE - A Configurable IDE workbench manager recent projects path
	 * constant.
	 */
	private static final String RECENT_PROJECTS_PATH = "./configuration/recentProjects.config";
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
	 * ACIDE - A Configurable IDE workbench manager recent projects list.
	 */
	private ArrayList<String> _recentProjects;

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
					RECENT_FILES_PATH));

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
					RECENT_FILES_PATH));

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
	 * Loads the configuration associated to the recent projects opened
	 * previously in the application from its configuration file.
	 */
	public void loadRecentProjectsWorkbenchConfiguration() {

		// Creates the recent projects list
		_recentProjects = new ArrayList<String>();

		// Loads the configuration file into the list
		try {

			// Creates and configures the buffered reader to read the
			// configuration file
			BufferedReader bufferedReader = new BufferedReader(new FileReader(
					RECENT_PROJECTS_PATH));

			String filePath = "";

			// Builds the recent projects list
			while ((filePath = bufferedReader.readLine()) != null) {
				_recentProjects.add(filePath);
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
	 * Saves the configuration associated to the recent projects opened
	 * previously in the application into its configuration file.
	 */
	public void saveRecentProjectsWorkbenchConfiguration() {

		// Saves the list into the configuration file
		try {

			// Creates and configures the buffered writer to write into the
			// configuration file
			BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(
					RECENT_PROJECTS_PATH));

			// Writes the recent projects list into the configuration file
			for (String filePath : _recentProjects)
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
	public void load(String configurationFileContent) {

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				5,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1071"));

		// Loads the recent files configuration
		loadRecentFilesWorkbenchConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				15,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1079"));

		// Loads the recent projects configuration
		loadRecentProjectsWorkbenchConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				30,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1028"));

		// Loads the lexicon assigner configuration
		loadLexiconAssignerWorkbenchConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				40,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1092"));

		// Loads the project configuration
		AcideProjectConfiguration.getInstance().load(configurationFileContent);

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				50,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1072"));

		// Loads the tool bar configuration
		loadToolBarConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				60,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1030"));

		// Loads the language configuration
		loadLanguageWorkbenchConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				70,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1031"));

		// Loads the console panel configuration
		loadConsoleWorkbenchConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				90,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1032"));

		// Loads the explorer panel configuration
		loadExplorerWorkbenchConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				95,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1033"));

		// Loads the file editor configuration
		loadFileEditorWorkbenchConfiguration();

		// Updates the splash screen window
		AcideSplashScreenWindow.getInstance().setProgressBar(
				99,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1035"));

		// Loads the main window configuration
		loadMainWindowConfiguration();
	}

	/**
	 * Loads the ACIDE - A Configurable IDE lexicon assigner workbench
	 * configuration.
	 */
	private void loadLexiconAssignerWorkbenchConfiguration() {

		try {

			// Loads the ACIDE - A Configurable IDE lexicon assigner
			// configuration
			AcideLexiconAssignerConfiguration.getInstance().load(
					AcideResourceManager.getInstance().getProperty(
							"lexiconAssignerConfiguration"));
		} catch (MissedPropertyException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Loads the ACIDE - A Configurable tool bar configuration.
	 */
	private void loadToolBarConfiguration() {

		String currentToolBarConfiguration = null;

		try {

			// Gets the ACIDE - A Configurable IDE current tool bar
			// configuration
			currentToolBarConfiguration = AcideResourceManager.getInstance()
					.getProperty("currentToolBarConfiguration");

			// Loads the ACIDE - A Configurable IDE tool bar configuration the
			// current tool bar configuration
			AcideToolBarConfiguration.getInstance().load(
					currentToolBarConfiguration);

			// Updates the ACIDE - A Configurable IDE current tool bar
			// configuration
			AcideResourceManager.getInstance().setProperty(
					"currentToolBarConfiguration", currentToolBarConfiguration);

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Gets the tool bar configuration name
			String name;
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");
			name = "./configuration/toolbar/"
					+ currentToolBarConfiguration.substring(index + 1,
							currentToolBarConfiguration.length());
			try {

				// Loads the ACIDE - A Configurable IDE tool bar configuration
				// the
				// current tool bar configuration
				AcideToolBarConfiguration.getInstance().load(name);

				// Information message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s958")
						+ currentToolBarConfiguration
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s957") + name);

				// Updates the ACIDE - A Configurable IDE current tool bar
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", name);
			} catch (Exception exception1) {

				// Updates the log
				AcideLog.getLog().error(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s127"));

				try {

					// Loads the default ACIDE - A Configurable IDE tool bar
					// configuration
					AcideToolBarConfiguration.getInstance().load(
							"./configuration/toolbar/default.TBcfg");

					// Information message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s958")
							+ currentToolBarConfiguration
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s959"));

					// Updates the the ACIDE - A Configurable IDE current tool
					// bar configuration
					AcideResourceManager.getInstance().setProperty(
							"currentToolBarConfiguration",
							"./configuration/toolbar/default.TBcfg");
				} catch (HeadlessException exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				} catch (Exception exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}
			}

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s127"));
		}
	}

	/**
	 * Loads the file editor workbench configuration, opening the related files
	 * to the project.
	 */
	public void loadFileEditorWorkbenchConfiguration() {

		// Updates the ACIDE - A Configurable IDE file editor configuration from
		// the project configuration
		AcideResourceManager.getInstance().setProperty(
				"fileEditorConfiguration",
				AcideProjectConfiguration.getInstance()
						.getFileEditorConfiguration());

		// Gets the file editor configuration
		AcideFileEditorConfiguration fileEditorConfiguration = AcideFileEditorConfiguration
				.getInstance();
		try {

			// Loads the file editor configuration
			fileEditorConfiguration.load(AcideResourceManager.getInstance()
					.getProperty("fileEditorConfiguration"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		for (int index = 0; index < AcideFileEditorConfiguration.getInstance()
				.getNumberOfFilesFromList(); index++) {

			// Checks if the file really exists
			File file = new File(AcideFileEditorConfiguration.getInstance()
					.getFileAt(index).getPath());

			// If the file is not a directory and exists
			if (!file.isDirectory() && file.exists()) {

				// Gets its content
				String fileContent = null;
				fileContent = AcideFileManager.getInstance().load(
						AcideFileEditorConfiguration.getInstance()
								.getFileAt(index).getPath());

				// Creates the lexicon configuration
				AcideLexiconConfiguration lexiconConfiguration = new AcideLexiconConfiguration();

				// Loads the lexicon configuration
				lexiconConfiguration.load(AcideFileEditorConfiguration
						.getInstance().getFileAt(index)
						.getLexiconConfiguration());

				// Creates the current grammar configuration
				AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

				// Sets the current grammar configuration path
				currentGrammarConfiguration
						.setPath(AcideFileEditorConfiguration.getInstance()
								.getFileAt(index)
								.getCurrentGrammarConfiguration());

				// Creates the previous grammar configuration
				AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

				// Sets the previous grammar configuration path
				previousGrammarConfiguration
						.setPath(AcideFileEditorConfiguration.getInstance()
								.getFileAt(index)
								.getPreviousGrammarConfiguration());

				// Updates the tabbed pane in the file editor manager
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.updateTabbedPane(
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index).getPath(),
								fileContent,
								true,
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index).getType(),
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index).getCaretPosition(),
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index)
										.getSplitPaneDividerLocation(),
								AcideFileEditorConfiguration.getInstance()
										.getFileAt(index)
										.getActiveTextEditionArea(),
								lexiconConfiguration,
								currentGrammarConfiguration,
								previousGrammarConfiguration);

				// The project configuration has not been modified
				AcideProjectConfiguration.getInstance().setIsModified(false);
			} else {

				// If the file does not exist
				if (!file.exists()) {

					// Gets the file project index
					int fileProjectIndex = AcideProjectConfiguration
							.getInstance().getIndexOfFile(
									AcideFileEditorConfiguration.getInstance()
											.getFileAt(index).getPath());

					// If the file does not belongs to the project
					if (fileProjectIndex == -1)

						// Displays an error message
						JOptionPane
								.showMessageDialog(
										null,
										AcideLanguageManager.getInstance()
												.getLabels().getString("s1020")
												+ file.getAbsolutePath()
												+ " "
												+ AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s1021"),
										"Warning", JOptionPane.WARNING_MESSAGE);

					// The project configuration has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}
			}
		}

		// Updates the edition mode status bar
		if (AcideFileEditorConfiguration.getInstance().getEditionMode())
			AcideMainWindow.getInstance().getStatusBar()
					.setEditionModeMessage("OVR");
		else
			AcideMainWindow.getInstance().getStatusBar()
					.setEditionModeMessage("INS");
	}

	/**
	 * Loads the language workbench configuration.
	 * 
	 * @return the language configuration.
	 */
	public void loadLanguageWorkbenchConfiguration() {

		// If the ACIDE - A Configurable IDE language is Spanish
		if (AcideProjectConfiguration.getInstance().getLanguageConfiguration()
				.equals("spanish"))

			// Performs the Spanish menu item action
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLanguageMenu().getSpanishMenuItem().doClick();

		// If the ACIDE - A Configurable IDE language is English
		if (AcideProjectConfiguration.getInstance().getLanguageConfiguration()
				.equals("english"))

			// Performs the English menu item action
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLanguageMenu().getEnglishMenuItem().doClick();
	}

	/**
	 * Loads the main window configuration.
	 */
	public void loadMainWindowConfiguration() {

		// Updates the ACIDE - A Configurable IDE window configuration
		AcideResourceManager.getInstance().setProperty(
				"windowConfiguration",
				AcideProjectConfiguration.getInstance()
						.getWindowConfiguration());

		// Gets the window configuration
		AcideWindowConfiguration windowConfiguration = AcideWindowConfiguration
				.getInstance();
		try {

			// Loads the ACIDE - A Configurable IDE window configuration
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
			AcideMainWindow.getInstance().getMenu().getViewMenu()
					.getShowExplorerPanelCheckBoxMenuItem().doClick();

		// If the console panel has not to be showed
		if (!AcideWindowConfiguration.getInstance().isConsolePanelShowed())

			// Does the show explorer panel check box menu item action
			AcideMainWindow.getInstance().getMenu().getViewMenu()
					.getShowConsolePanelCheckBoxMenuItem().doClick();

		// Sets the main window size
		AcideMainWindow.getInstance().setSize(
				AcideWindowConfiguration.getInstance().getWindowWidth(),
				AcideWindowConfiguration.getInstance().getWindowHeight());

		// Sets the main window location
		AcideMainWindow.getInstance().setLocation(
				AcideWindowConfiguration.getInstance().getXCoordinate(),
				AcideWindowConfiguration.getInstance().getYCoordinate());

		// Sets the main window split panel vertical divider location
		AcideMainWindow
				.getInstance()
				.getVerticalSplitPane()
				.setDividerLocation(
						AcideWindowConfiguration.getInstance()
								.getVerticalSplitPaneDividerLocation());

		// Sets the main window split panel horizontal divider location
		AcideMainWindow
				.getInstance()
				.getHorizontalSplitPane()
				.setDividerLocation(
						AcideWindowConfiguration.getInstance()
								.getHorizontalSplitPanelDividerLocation());

		// If it is not the default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// Enables the project menu
			AcideMainWindow.getInstance().getMenu().enableProjectMenu();

			// Enables the open all files menu item
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getOpenAllFilesMenuItem().setEnabled(true);
		}

		// Updates the main window
		AcideMainWindow.getInstance().validate();

		// Repaint the main window
		AcideMainWindow.getInstance().repaint();
	}

	/**
	 * Loads the explorer files and builds the explorer in the main window.
	 */
	public void loadExplorerWorkbenchConfiguration() {

		try {

			// If it is the default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Sets the title with the <empty> tag
				AcideMainWindow.getInstance()
						.setTitle(
								AcideMainWindow.getInstance().getTitle()
										+ " - <empty>");
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
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getAddFileMenuItem().setEnabled(true);

				// Enables the remove file menu item in the explorer popup menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getRemoveFileMenuItem().setEnabled(true);

				// Sets the project title
				AcideMainWindow.getInstance().setTitle(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s425")
								+ " - "
								+ AcideProjectConfiguration.getInstance()
										.getName());

				// Builds the explorer tree with all the associated files
				AcideMainWindow.getInstance().getExplorerPanel().getRoot()
						.removeAllChildren();

				// Creates the new explorer node
				DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode(
						newProjectFile);

				// Adds the new node to the explorer tree root
				AcideMainWindow.getInstance().getExplorerPanel().getRoot()
						.add(rootNode);

				// Creates the directory list
				ArrayList<DefaultMutableTreeNode> directoryList = new ArrayList<DefaultMutableTreeNode>();

				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					// Gets the node
					DefaultMutableTreeNode fileProjectNode = new DefaultMutableTreeNode(
							AcideProjectConfiguration.getInstance().getFileAt(
									index));

					// If is a directory
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
						DefaultMutableTreeNode parentNode = AcideMainWindow
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
				AcideMainWindow.getInstance().getExplorerPanel().getTreeModel()
						.reload();

				// Repaint the explorer tree
				AcideMainWindow.getInstance().getExplorerPanel().expandTree();

				// Enables the add file menu item in the popup menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getAddFileMenuItem().setEnabled(true);

				// Enables the save project menu item in the popup menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getSaveProjectMenuItem().setEnabled(true);

				// If it has more than 0 files associated
				if (AcideProjectConfiguration.getInstance()
						.getNumberOfFilesFromList() > 0)

					// Allows to remove files in the EXPLORER menu
					AcideMainWindow.getInstance().getExplorerPanel()
							.getPopupMenu().getRemoveFileMenuItem()
							.setEnabled(true);
				else
					// Removing files in the EXPLORER menu is not allowed
					AcideMainWindow.getInstance().getExplorerPanel()
							.getPopupMenu().getRemoveFileMenuItem()
							.setEnabled(false);

				// Saves the ACIDE - A Configurable IDE project configuration
				AcideProjectConfiguration.getInstance().setFirstSave(true);

				// Updates the ACIDE - A Configurable IDE project configuration
				AcideProjectConfiguration.getInstance().setPath(
						AcideResourceManager.getInstance().getProperty(
								"projectConfiguration"));
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
	public void loadConsoleWorkbenchConfiguration() {

		// Updates the ACIDE - A Configurable IDE console configuration from the
		// project configuration
		AcideResourceManager.getInstance().setProperty(
				"consoleConfiguration",
				AcideProjectConfiguration.getInstance()
						.getConsoleConfiguration());

		// Gets the console configuration
		AcideConsoleConfiguration consoleConfiguration = AcideConsoleConfiguration
				.getInstance();
		try {

			// Loads the console configuration
			consoleConfiguration.load(AcideResourceManager.getInstance()
					.getProperty("consoleConfiguration"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Sets the console display options
		AcideMainWindow.getInstance().getConsolePanel()
				.updateConsoleDisplayOptions();

		// Resets the console
		AcideMainWindow.getInstance().getConsolePanel().resetConsole();
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
		String projectConfiguration = null;

		try {

			// Gets the ACIDE - A Configurable IDE project configuration
			projectConfiguration = AcideResourceManager.getInstance()
					.getProperty("projectConfiguration");

			// Loads its content
			fileContent = AcideFileManager.getInstance().load(
					projectConfiguration);

			// If it can't find the file
			if (fileContent == null) {

				// Loads the default file
				fileContent = AcideFileManager.getInstance().load(
						"./configuration/project/default.acidePrj");

				// Updates the ACIDE - A Configurable IDE project configuration
				AcideResourceManager.getInstance().setProperty(
						"projectConfiguration",
						"./configuration/project/default.acidePrj");

				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s960")
						+ projectConfiguration
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s959"));
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Loads the default file
			fileContent = AcideFileManager.getInstance().load(
					"./configuration/project/default.acidePrj");

			// Displays an error message
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s960")
							+ projectConfiguration
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s959"));
		}
		return fileContent;
	}

	/**
	 * <p>
	 * Saves the current opened files configuration in the file editor panel.
	 * </p>
	 * <p>
	 * Closes the new and log tab in the editor and saves the state of each one
	 * of the opened files in the file editor into the project configuration,
	 * whether it is the default project or not.
	 * </p>
	 */
	public void saveFileEditorOpenedFilesConfiguration() {

		try {

			// SPECIAL CASE: New file
			int newFileIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getNewFileIndex();

			// If it has new file opened
			if (newFileIndex != -1) {

				// Closes the new file so it will not be saved
				// in the configuration
				AcideMainWindow.getInstance().getFileEditorManager()
						.getTabbedPane().remove(newFileIndex);
			}

			// SPECIAL CASE: Log file
			int logFileIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getLogFileIndex();

			// If it has log file opened
			if (logFileIndex != -1) {

				// Closes the log file so it will not be saved in the
				// project configuration
				AcideMainWindow.getInstance().getFileEditorManager()
						.getTabbedPane().remove(logFileIndex);
			}

			// Gets the ACIDE - A Configurable IDE project configuration
			String projectConfiguration = AcideResourceManager.getInstance()
					.getProperty("projectConfiguration");

			// Saves the editor manager configuration
			AcideFileEditorConfiguration.getInstance().save();

			// If it is the default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Removes the files related to the project
				AcideProjectConfiguration.getInstance().removeFiles();

				// Sets the all opened files in the editor
				for (int index = 0; index < AcideMainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

					// Creates the file
					AcideProjectFile explorerFile = new AcideProjectFile();
					explorerFile.setAbsolutePath(AcideMainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index)
							.getAbsolutePath());

					// Sets if it is compilable file
					explorerFile.setIsCompilableFile(AcideMainWindow
							.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).isCompilableFile());

					// Sets if it is main file
					explorerFile.setIsMainFile(AcideMainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(index)
							.isMainFile());

					// Sets the name
					explorerFile.setName(AcideMainWindow.getInstance()
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

				// Sets the ACIDE - A Configurable IDE project configuration
				AcideResourceManager.getInstance().setProperty(
						"projectConfiguration",
						"./configuration/project/default.acidePrj");
			} else {

				// Saves the configuration of the project
				AcideFileManager.getInstance().write(projectConfiguration,
						AcideProjectConfiguration.getInstance().save());
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Saves the current workbench configuration.
	 */
	public void saveWorkbenchConfiguration() {

		// Are the project configuration modified
		if (AcideProjectConfiguration.getInstance().isModified()) {

			// Ask the user to save the configuration
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s657"), AcideLanguageManager
							.getInstance().getLabels().getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is not the cancel option
			if (returnValue != JOptionPane.CANCEL_OPTION) {

				// If it is yes
				if (returnValue == JOptionPane.YES_OPTION) {

					// If it is not the default project
					if (!AcideProjectConfiguration.getInstance()
							.isDefaultProject()) {

						// Enables the menu
						AcideMainWindow.getInstance().getMenu()
								.getProjectMenu().getSaveProjectMenuItem()
								.setEnabled(true);

						// Saves the project
						AcideMainWindow.getInstance().getMenu()
								.getProjectMenu().getSaveProjectMenuItem()
								.doClick();

						// Save the rest of the workbench configuration
						saveRestOfWorkbenchConfiguration();

					} else {

						// Save the rest of the workbench configuration
						saveRestOfWorkbenchConfiguration();
					}

					// Saves the file editor configuration
					saveFileEditorConfiguration();

					// Closes the main window
					System.exit(0);

				} else if (returnValue == JOptionPane.NO_OPTION) {

					// Saves the file editor configuration
					saveFileEditorConfiguration();

					// Closes the main window
					System.exit(0);
				}
			}
		}

		// Saves the file editor configuration
		saveFileEditorConfiguration();

		// Save the rest of the workbench configuration
		saveRestOfWorkbenchConfiguration();

		// Closes the main window
		System.exit(0);
	}

	/**
	 * Saves the file editor configuration.
	 */
	public void saveFileEditorConfiguration() {

		// If the file editor manager is modified
		if (AcideMainWindow.getInstance().getFileEditorManager().isModified()) {

			// Gets the number of file editor panels
			int numberOfFileEditorPanels = AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels();

			// If there are opened file editor panels
			if (numberOfFileEditorPanels > 0) {

				int selectedFileEditorPanelIndex = AcideMainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// Search for modified opened file editors
				for (int index = numberOfFileEditorPanels - 1; index >= 0; index--) {

					// If it is modified
					if (AcideMainWindow.getInstance().getFileEditorManager()
							.isRedButton(index)) {

						// Puts the focus on the current checked file
						// editor panel
						AcideMainWindow.getInstance().getFileEditorManager()
								.setSelectedFileEditorPanelAt(index);

						// Do you want to save it?
						int returnValue2 = JOptionPane.showConfirmDialog(null,
								AcideLanguageManager.getInstance().getLabels()
										.getString("s643"),
								AcideLanguageManager.getInstance().getLabels()
										.getString("s953"),
								JOptionPane.YES_NO_OPTION);

						// If it is OK
						if (returnValue2 == JOptionPane.OK_OPTION) {

							// Saves the file editor panel
							AcideMainWindow.getInstance().getMenu()
									.getFileMenu().saveFile(index);
						}
					}
				}

				// Restores the selected file editor panel
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.setSelectedFileEditorPanelAt(
								selectedFileEditorPanelIndex);
			}
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
		AcideMainWindow.getInstance().getConsolePanel().executeExitCommand();

		try {

			// Gets the ACIDE - A Configurable IDE current menu configuration
			String currentMenuConfiguration = AcideResourceManager
					.getInstance().getProperty("currentMenuConfiguration");

			if ((currentMenuConfiguration.endsWith("lastModified.menuCfg"))
					|| (currentMenuConfiguration.endsWith("newMenu.menuCfg"))) {

				// Gets the ACIDE - A Configurable IDE previous menu
				// configuration
				String previousMenuConfiguration = AcideResourceManager
						.getInstance().getProperty("previousMenuConfiguration");

				// Updates the ACIDE - A Configurable IDE current menu
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", previousMenuConfiguration);
			}

			// Gets the ACIDE - A Configurable IDE current tool bar
			// configuration
			String currentToolBarConfiguration = AcideResourceManager
					.getInstance().getProperty("currentToolBarConfiguration");

			if ((currentToolBarConfiguration.endsWith("lastModified.TBcfg"))
					|| currentToolBarConfiguration.endsWith("newToolBar.TBcfg")) {

				// Gets the ACIDE - A Configurable IDE previous tool bar
				// configuration
				String previousToolBarConfiguration = AcideResourceManager
						.getInstance().getProperty(
								"previousToolBarConfiguration");

				// Updates the ACIDE - A Configurable IDE current tool bar
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration",
						previousToolBarConfiguration);
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// Displays an error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s294"), JOptionPane.ERROR_MESSAGE);
		}

		// Stores the configuration of the files
		saveFileEditorOpenedFilesConfiguration();

		// Saves the window configuration
		AcideWindowConfiguration.getInstance().save();

		// Saves the lexicon assigner configuration
		AcideLexiconAssignerConfiguration.getInstance().save();

		// Saves the recent files configuration
		saveRecentFilesWorkbenchConfiguration();

		// Saves the recent projects configuration
		saveRecentProjectsWorkbenchConfiguration();
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
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getOpenRecentFilesMenu().build();
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE workbench configuration recent
	 * projects opened.
	 * 
	 * @return the ACIDE - A Configurable IDE workbench configuration recent
	 *         projects opened.
	 */
	public ArrayList<String> getRecentProjects() {
		return _recentProjects;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE workbench
	 * configuration recent projects opened.
	 * 
	 * @param recentProjects
	 *            new value to set.
	 */
	public void setRecentProjects(ArrayList<String> recentProjects) {
		_recentProjects = recentProjects;
	}

	/**
	 * Adds a new file path to the recent project list, avoiding duplicates.
	 * 
	 * @param filePath
	 *            new file path to add.
	 */
	public void addRecentProjectToList(String filePath) {

		if (!_recentProjects.contains(filePath)) {

			// Adds the project to the recent projects list
			_recentProjects.add(filePath);

			// Updates the menu
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getOpenRecentProjectMenu().build();
		}
	}
}
