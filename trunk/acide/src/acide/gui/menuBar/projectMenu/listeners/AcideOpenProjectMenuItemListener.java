/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
package acide.gui.menuBar.projectMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.configuration.console.AcideConsoleConfiguration;
import acide.configuration.fileEditor.AcideFileEditorConfiguration;
import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.toolBar.consoleComandToolBar.AcideConsoleCommandConfiguration;
import acide.configuration.window.AcideWindowConfiguration;
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.AcideMenuConfigurationWindow;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;
import acide.gui.toolBarPanel.staticToolBar.AcideStaticToolBar;

/**
 * ACIDE -A Configurable IDE project menu open project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideOpenProjectMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Selects the extension for the project
		String[] extensions = new String[] { "acidePrj" };
		AcideFileManager
				.getInstance()
				.getFileChooser()
				.addChoosableFileFilter(
						new AcideFileExtensionFilterManager(extensions,
								AcideLanguageManager.getInstance().getLabels()
										.getString("s328")));

		// Asks for the file path to the user
		final String filePath;
		filePath = AcideFileManager.getInstance().askAbsolutePath();

		// If the file content is not empty
		if (filePath != null) {

			// Open the project
			openProject(filePath);
		}
	}

	/**
	 * Opens an ACIDE - A Configurable IDE project from a file given as a
	 * parameter.
	 * 
	 * @param filePath
	 *            file path which contains the project configuration file.
	 */
	public void openProject(String filePath) {

		boolean cancelOptionSelected = false;

		// Are the project configuration modified or the file editor manager
		// modified?
		if (AcideProjectConfiguration.getInstance().isModified()
				|| AcideMainWindow.getInstance().getFileEditorManager()
						.isModified()) {

			// Do you want to save it?
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s657"), AcideLanguageManager
							.getInstance().getLabels().getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is OK
			if (returnValue == JOptionPane.OK_OPTION) {

				// Enables the save project menu item
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveProjectMenuItem().setEnabled(true);

				// Does the save project menu item action
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveProjectMenuItem().doClick();
			} else {

				// If it is not NO
				if (returnValue != JOptionPane.NO_OPTION)
					cancelOptionSelected = true;
			}
		}

		// If the user does not select the cancel option
		if (!cancelOptionSelected) {

			// Updates the status message in the status bar
			AcideMainWindow.getInstance().getStatusBar().setStatusMessage(" ");

			// Closes the previous project configuration
			closePreviousProjectConfiguration();

			// Loads the file editor configuration
			loadProjectConfiguration(filePath);

			// Loads the language
			loadLanguage();

			// Loads the main window configuration
			loadMainWindowConfiguration();

			// Loads the menu configuration
			loadMenuConfiguration();

			// Loads the grammar configuration
			// loadGrammarConfiguration();

			// Loads the console configuration
			loadConsoleConfiguration();

			// Loads the tool bar configuration
			loadToolBarConfiguration();

			// Loads the explorer configuration
			loadExplorerConfiguration();

			// Enables the show explorer panel menu item
			AcideMainWindow.getInstance().getMenu().getViewMenu()
					.getShowExplorerPanelCheckBoxMenuItem().setSelected(true);

			// Loads the file editor configuration
			loadFileEditorConfiguration();

			// The project has not been modified
			AcideProjectConfiguration.getInstance().setIsModified(false);

			// This is the first time that it is saved
			AcideProjectConfiguration.getInstance().setFirstSave(true);

			// Enables the project menu
			AcideMainWindow.getInstance().getMenu().enableProjectMenu();

			// Enables the open all files menu item
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getOpenAllFilesMenuItem().setEnabled(true);

			// Enables the save project button in the static tool bar
			AcideStaticToolBar.getInstance().getSaveProjectButton()
					.setEnabled(true);

			// Validates the changes in the main window
			AcideMainWindow.getInstance().validate();

			// Repaints the main window
			AcideMainWindow.getInstance().repaint();
		}
	}

	/**
	 * Loads the project explorer configuration.
	 */
	public void loadExplorerConfiguration() {

		// Removes all the nodes in the tree
		AcideMainWindow.getInstance().getExplorerPanel().getRoot()
				.removeAllChildren();

		// Creates the folder with the name of the project
		AcideProjectFile rootProjectFile = new AcideProjectFile();

		// Sets the absolute path
		rootProjectFile.setAbsolutePath(AcideProjectConfiguration.getInstance()
				.getName());

		// Sets the name
		rootProjectFile.setName(AcideProjectConfiguration.getInstance()
				.getName());

		// It is directory
		rootProjectFile.setIsDirectory(true);

		// It has no parent
		rootProjectFile.setParent(null);

		// Sets the main window title
		AcideMainWindow.getInstance().setTitle(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s425")
						+ " - "
						+ AcideProjectConfiguration.getInstance().getName());

		// Creates the root node of the explorer tree
		DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode(
				rootProjectFile);

		// Allows children
		rootNode.setAllowsChildren(true);

		// Adds the root node to the tree
		AcideMainWindow.getInstance().getExplorerPanel().getRoot()
				.add(rootNode);

		// Creates the directory list
		ArrayList<DefaultMutableTreeNode> directoryList = new ArrayList<DefaultMutableTreeNode>();

		// Adds the associated project files to the explorer
		for (int index = 0; index < AcideProjectConfiguration.getInstance()
				.getNumberOfFilesFromList(); index++) {

			// Gets the file from the project configuration
			DefaultMutableTreeNode projectFileNode = new DefaultMutableTreeNode(
					AcideProjectConfiguration.getInstance().getFileAt(index));

			// If it is directory
			if (AcideProjectConfiguration.getInstance().getFileAt(index)
					.isDirectory()) {

				// It can have files inside
				projectFileNode.setAllowsChildren(true);

				// Adds the file
				directoryList.add(projectFileNode);
			} else
				// Can't have files inside
				projectFileNode.setAllowsChildren(false);

			// If the file is in the same folder than the project folder
			if (AcideProjectConfiguration.getInstance().getFileAt(index)
					.getParent()
					.equals(AcideProjectConfiguration.getInstance().getName())) {
				// Adds the file
				rootNode.add(projectFileNode);
			} else {

				// Searches for it in the tree structure
				DefaultMutableTreeNode parentNode = AcideMainWindow
						.getInstance()
						.getExplorerPanel()
						.searchDirectoryList(
								directoryList,
								AcideProjectConfiguration.getInstance()
										.getFileAt(index).getParent());

				// Adds the file
				parentNode.add(projectFileNode);
			}
		}

		// Updates the explorer tree
		AcideMainWindow.getInstance().getExplorerPanel().getTreeModel()
				.reload();

		// Repaints the explorer tree
		AcideMainWindow.getInstance().getExplorerPanel().expandTree();

		// If there are files associated to the project
		if (AcideProjectConfiguration.getInstance().getNumberOfFilesFromList() > 0) {

			// Enables the remove file menu item
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFileMenuItem().setEnabled(true);

			// Enables the delete file menu item
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFileMenuItem().setEnabled(true);
		} else {

			// Disables the remove file menu item
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getRemoveFileMenuItem().setEnabled(false);

			// Disables the delete file menu item
			AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
					.getDeleteFileMenuItem().setEnabled(false);
		}

		// If the show explorer panel is selected
		if (!AcideMainWindow.getInstance().getMenu().getViewMenu()
				.getShowExplorerPanelCheckBoxMenuItem().isSelected())
			// Shows the explorer
			AcideMainWindow.getInstance().getExplorerPanel()
					.showExplorerPanel();
	}

	/**
	 * Loads the project language.
	 */
	public void loadLanguage() {

		// SPANISH
		if (AcideProjectConfiguration.getInstance().getLanguageConfiguration()
				.equals("spanish"))
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLanguageMenu().getSpanishMenuItem().doClick();

		// ENGLISH
		if (AcideProjectConfiguration.getInstance().getLanguageConfiguration()
				.equals("english"))
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLanguageMenu().getEnglishMenuItem().doClick();
	}

	/**
	 * Loads the project menu configuration.
	 */
	public void loadMenuConfiguration() {

		// Gets the menu configuration from the project configuration
		String menuConfiguration = AcideProjectConfiguration.getInstance()
				.getMenuConfiguration();

		try {

			// Loads the new menu item list
			AcideMenuConfiguration.getInstance().setMenuElementList(
					AcideMenuConfiguration.getInstance()
							.loadMenuConfigurationFile(menuConfiguration));

			// Updates the ACIDE - A Configurable IDE current menu configuration
			AcideResourceManager.getInstance().setProperty(
					"currentMenuConfiguration", menuConfiguration);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Gets the name
			String name;
			int index = menuConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = menuConfiguration.lastIndexOf("/");
			name = ".\\configuration\\menu\\"
					+ menuConfiguration.substring(index + 1,
							menuConfiguration.length());

			try {

				// Load the new menu item list
				AcideMenuConfiguration.getInstance().setMenuElementList(
						AcideMenuConfiguration.getInstance()
								.loadMenuConfigurationFile(menuConfiguration));

				// Updates the ACIDE - A Configurable IDE current menu
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", name);

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s956")
						+ menuConfiguration
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s957") + name);

			} catch (Exception exception1) {

				try {

					// Loads the menu configuration
					AcideMenuConfiguration.getInstance().setMenuElementList(
							AcideMenuConfiguration.getInstance()
									.loadMenuConfigurationFile(
											menuConfiguration));
				} catch (Exception exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}

				// Updates the ACIDE - A Configurable IDE current menu
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration",
						"./configuration/menu/defaultAllOn.menuCfg");

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s956")
						+ menuConfiguration
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s959"));

				// Updates the log
				AcideLog.getLog().error(exception1.getMessage());
				exception1.printStackTrace();
			}
		}

		// Builds the menu
		AcideMainWindow.getInstance().getMenu().build();

		// Validates the changes in the main window
		AcideMainWindow.getInstance().validate();

		// Repaints the main window
		AcideMainWindow.getInstance().repaint();

		// Enables the save menu menu item
		AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
				.getMenuMenu().getSaveMenuMenuItem().setEnabled(true);

		// The changes are saved
		AcideMenuConfigurationWindow.setChangesAreSaved(true);
	}

	/**
	 * Loads the project console configuration.
	 */
	public void loadConsoleConfiguration() {

		// Gets the console configuration
		AcideConsoleConfiguration.getInstance().load(
				AcideProjectConfiguration.getInstance()
						.getConsoleConfiguration());

		// Resets the console panel
		AcideMainWindow.getInstance().getConsolePanel().resetConsole();
	}

	/**
	 * Loads the project tool bar configuration.
	 */
	public void loadToolBarConfiguration() {

		try {

			// Clears the list of shell commands
			AcideConsoleCommandConfiguration.getInstance().clear();

			// Gets the ACIDE - A Configurable IDE tool bar configuration
			String currentToolBarConfiguration = AcideProjectConfiguration
					.getInstance().getToolBarConfiguration();

			// Loads the command list from the configuration file
			AcideConsoleCommandConfiguration.getInstance().loadFinalList(
					currentToolBarConfiguration);

			// Updates the ACIDE - A Configurable IDE current tool bar
			// configuration
			AcideResourceManager.getInstance().setProperty(
					"currentToolBarConfiguration", currentToolBarConfiguration);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Gets the ACIDE - A Configurable IDE tool bar configuration
			String currentToolBarConfiguration = AcideProjectConfiguration
					.getInstance().getToolBarConfiguration();

			// Gets the name
			String name = "";
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");
			name = ".\\configuration\\toolbar\\"
					+ currentToolBarConfiguration.substring(index + 1,
							currentToolBarConfiguration.length());
			try {

				// Loads the command list from the configuration file
				AcideConsoleCommandConfiguration.getInstance().loadFinalList(
						name);

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s958")
						+ currentToolBarConfiguration
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s957") + name);

				// Updates the ACIDE - A Configurable IDE tool bar configuration
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", name);
			} catch (Exception exception1) {

				// Updates the log
				AcideLog.getLog().error(exception1.getMessage());
				exception1.printStackTrace();

				try {

					// Loads the command list from the configuration file
					AcideConsoleCommandConfiguration.getInstance()
							.loadFinalList(
									"./configuration/toolbar/default.TBcfg");
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

				// Updates the ACIDE - A Configurable IDE tool bar configuration
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration",
						"./configuration/toolbar/default.TBcfg");
			}
		}

		// Builds the tool bar
		AcideMainWindow.getInstance().buildToolBarPanel();

		// Enables the save tool bar menu item
		AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
				.getToolBarMenu().getSaveToolBarMenuItem().setEnabled(true);

		// The changes are saved
		AcideToolBarConfigurationWindow.setAreChangesSaved(true);
	}

	/**
	 * Loads the project configuration from the configuration file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void loadProjectConfiguration(String configurationFilePath) {

		// Loads the configuration file content
		String configurationFileContent = AcideFileManager.getInstance().load(
				configurationFilePath);

		// Updates the ACIDE - A Configurable IDE project configuration
		AcideResourceManager.getInstance().setProperty("projectConfiguration",
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
			AcideMainWindow.getInstance().getMenu().getViewMenu()
					.getShowExplorerPanelCheckBoxMenuItem().doClick();

		// Is console panel showed?
		if (!AcideWindowConfiguration.getInstance().isConsolePanelShowed())

			// Shows the console panel
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

		// Sets the vertical split pane divider location
		AcideMainWindow
				.getInstance()
				.getVerticalSplitPane()
				.setDividerLocation(
						AcideWindowConfiguration.getInstance()
								.getVerticalSplitPaneDividerLocation());

		// Sets the horizontal split pane divider location
		AcideMainWindow
				.getInstance()
				.getHorizontalSplitPane()
				.setDividerLocation(
						AcideWindowConfiguration.getInstance()
								.getHorizontalSplitPanelDividerLocation());
	}

	/**
	 * Loads the project file editor configuration. SwingUtilities is used to
	 * wait until the end of the execution of all the previous events so it can
	 * add all the editors properly and safety.
	 */
	public void loadFileEditorConfiguration() {

		// Updates the ACIDE - A Configurable IDE file editor configuration
		AcideResourceManager.getInstance().setProperty(
				"fileEditorConfiguration",
				AcideProjectConfiguration.getInstance()
						.getFileEditorConfiguration());

		// Gets the ACIDE - A Configurable IDE file editor configuration
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

		for (int index = 0; index < AcideProjectConfiguration.getInstance()
				.getNumberOfFilesFromList(); index++) {

			// Checks if the file really exists
			File file = new File(AcideProjectConfiguration.getInstance()
					.getFileAt(index).getAbsolutePath());

			// If the file is not a directory and exists
			if (!AcideProjectConfiguration.getInstance().getFileAt(index)
					.isDirectory()
					&& file.exists()) {

				// Loads the file content
				String fileContent = null;
				fileContent = AcideFileManager.getInstance().load(
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).getAbsolutePath());

				// If the file has to be opened
				if (AcideProjectConfiguration.getInstance().getFileAt(index)
						.isOpened()) {

					// TODO: Load the predefined extension

					// Creates the lexicon configuration
					AcideLexiconConfiguration lexiconConfiguration = new AcideLexiconConfiguration();

					// Loads the lexicon configuration
					lexiconConfiguration
							.load(AcideLexiconConfiguration.DEFAULT_PATH
									+ AcideLexiconConfiguration.DEFAULT_NAME);

					// TODO: Load the predefined extension

					// Creates the grammar configuration
					AcideGrammarConfiguration grammarConfiguration = new AcideGrammarConfiguration();

					// Sets the grammar configuration path
					grammarConfiguration
							.setPath(AcideGrammarConfiguration.DEFAULT_PATH);

					// Updates the tabbed pane in the file editor
					// manager
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.updatesTabbedPane(
									AcideProjectConfiguration.getInstance()
											.getFileAt(index).getAbsolutePath(),
									fileContent,
									true,
									AcideProjectConfiguration.getInstance()
											.getFileAt(index).getType(), 0, 0,
									1, lexiconConfiguration,
									grammarConfiguration);
				}

				// The project configuration has been modified
				AcideProjectConfiguration.getInstance().setIsModified(false);
			} else {

				// If the file does not exist
				if (!file.exists()) {

					if (!AcideProjectConfiguration.getInstance()
							.getFileAt(index).isDirectory()) {
						// Error message
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
						AcideProjectConfiguration.getInstance().setIsModified(
								true);
					}
				}
			}
		}
	}

	/**
	 * Closes the opened file editors and store the window parameters.
	 * 
	 * @return true if the user has selected cancel in any of the modified tabs
	 *         and false in other case.
	 */
	public void closePreviousProjectConfiguration() {

		// Gets the number of file editor panels
		int numberOfFileEditorPanels = AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels();

		// If there are opened file editor panels
		if (numberOfFileEditorPanels != 0) {

			// Search for modified opened file editors
			for (int index = numberOfFileEditorPanels - 1; index >= 0; index--) {

				// If it is modified
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.isRedButton(index)) {

					// Do you want to save it?
					int returnValue = JOptionPane.showConfirmDialog(null,
							AcideLanguageManager.getInstance().getLabels()
									.getString("s643"), AcideLanguageManager
									.getInstance().getLabels()
									.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					// If it is OK
					if (returnValue == JOptionPane.OK_OPTION) {

						// Saves the file editor panel
						AcideMainWindow.getInstance().getMenu().getFileMenu()
								.saveFile(index);
					}
				}
			}

			// Close the file editor panels in the tabbed pane
			for (int index = 0; index < numberOfFileEditorPanels; index++) {

				// Sets the selected tab at index 0
				AcideMainWindow.getInstance().getFileEditorManager()
						.setSelectedFileEditorPanelAt(0);

				// Removes it from the tabbed pane
				AcideMainWindow.getInstance().getFileEditorManager()
						.getTabbedPane().remove(0);

				// Validates the changes in the tabbed pane
				AcideMainWindow.getInstance().getFileEditorManager()
						.getTabbedPane().validate();
			}
		}
	}
}