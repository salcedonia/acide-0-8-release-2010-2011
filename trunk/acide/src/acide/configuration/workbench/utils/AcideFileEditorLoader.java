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
package acide.configuration.workbench.utils;

import java.io.File;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.configuration.workbench.fileEditor.AcideFileEditorConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;

/**
 * <p>
 * ACIDE - A Configurable IDE file editor loader.
 * </p>
 * <p>
 * Loads the ACIDE - A Configurable IDE file editor configuration from the ACIDE
 * - A Configurable IDE workbench and ACIDE - A Configurable IDE project
 * configuration files.
 * </p>
 * <p>
 * As the process is complex, the loading process has been split in 3 different
 * steps:
 * <ul>
 * <li>
 * The files from the ACIDE - A Configurable IDE project configuration are
 * loaded at first place. This guarantee that the configuration of those
 * associated files is preserved.</li>
 * <li>
 * After that, the files from the ACIDE - A Configurable IDE file editor
 * configuration are loaded. For each one of the files, it checks if it is
 * already opened and if so, it updates them with the features stored in the
 * ACIDE - A Configurable IDE workbench configuration file.</li>
 * <li>
 * Finally, it sets the selected file editor from the ACIDE - A Configurable IDE
 * workbench configuration file.</li>
 * </ul>
 * </p>
 * 
 * @version 0.8
 */
public class AcideFileEditorLoader {

	/**
	 * ACIDE - A Configurable IDE file editor loader unique class instance.
	 */
	private static AcideFileEditorLoader _instance;

	/**
	 * Returns the ACIDE - A Configurable IDE file editor loader unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor loader unique class
	 *         instance.
	 */
	public static AcideFileEditorLoader getInstance() {
		if (_instance == null)
			_instance = new AcideFileEditorLoader();
		return _instance;
	}

	/**
	 * Loads the file editor workbench configuration, opening the related files
	 * to the project.
	 * 
	 * @param fileEditorConfiguration
	 *            file editor configuration from the ACIDE - A Configurable IDE
	 *            workbench.
	 */
	public void run(AcideFileEditorConfiguration fileEditorConfiguration) {

		// Loads the project files
		loadProjectFiles();

		// Loads the workbench configuration
		loadWorkbenchConfiguration(fileEditorConfiguration);

		// Sets the selected file editor
		setSelectedFileEditor();

		/*
		 * IMPORTANT: Without this, when a configuration with any file editors
		 * opened, the menu options will not be initialized properly, and it is
		 * mandatory not to remove this line.
		 */

		// If there are not file editors
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() == 0)
			// Updates the menu bar
			AcideMainWindow.getInstance().getMenu().configure();

		// Updates the edition mode status bar
		if (fileEditorConfiguration.getEditionMode())
			AcideMainWindow.getInstance().getStatusBar()
					.setEditionModeMessage("OVR");
		else
			AcideMainWindow.getInstance().getStatusBar()
					.setEditionModeMessage("INS");

		// Updates the automatic indent at the menu bar
		AcideMainWindow
				.getInstance()
				.getMenu()
				.getConfigurationMenu()
				.getFileEditorMenu()
				.getAutomaticIndentCheckBoxMenuItem()
				.setSelected(
						AcideWorkbenchConfiguration.getInstance()
								.getFileEditorConfiguration()
								.getAutomaticIndent());
	}

	/**
	 * Loads the ACIDE - A Configurable IDE project configuration associated
	 * files into the ACIDE - A Configurable IDE file editor manager.
	 */
	public void loadProjectFiles() {

		// Loads the project associated files first
		for (int index = 0; index < AcideProjectConfiguration.getInstance()
				.getFileListSize(); index++) {

			// Gets the project file
			AcideProjectFile projectFile = AcideProjectConfiguration
					.getInstance().getFileAt(index);

			// If it has to be opened
			if (projectFile.isOpened()) {

				// Checks if the file really exists
				File file = new File(projectFile.getAbsolutePath());

				// If the file is not a directory and exists
				if (!file.isDirectory() && file.exists()) {

					// Gets its content
					String fileContent = null;
					fileContent = AcideFileManager.getInstance().load(
							projectFile.getAbsolutePath());

					// Gets the predefined lexicon configuration
					AcideLexiconConfiguration lexiconConfiguration = AcideWorkbenchConfiguration
							.getInstance()
							.getLexiconAssignerConfiguration()
							.getPredifinedLexiconConfiguration(
									projectFile.getAbsolutePath());

					// Creates the current grammar configuration
					AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

					// Sets the current grammar configuration path
					currentGrammarConfiguration
							.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

					// Creates the previous grammar configuration
					AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

					// Sets the previous grammar configuration path
					previousGrammarConfiguration
							.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

					// Updates the tabbed pane in the file editor configuration
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.updateTabbedPane(projectFile.getAbsolutePath(),
									fileContent, true, projectFile.getType(),
									0, 0, 1, lexiconConfiguration,
									currentGrammarConfiguration,
									previousGrammarConfiguration);

					// The project configuration has not been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(false);
				}
			}
		}
	}

	/**
	 * Loads the ACIDE - A Configurable IDE workbench configuration associated
	 * files into the ACIDE - A Configurable IDE file editor manager.
	 */
	public void loadWorkbenchConfiguration(
			AcideFileEditorConfiguration fileEditorConfiguration) {

		for (int index = 0; index < fileEditorConfiguration
				.getNumberOfFilesFromList(); index++) {

			// Checks if the file really exists
			File file = new File(fileEditorConfiguration.getFileAt(index)
					.getPath());

			// If the file is not a directory and exists
			if (!file.isDirectory() && file.exists()) {

				// if it is not opened in the ACIDE - A Configurable IDE
				// file editor
				if (AcideProjectConfiguration.getInstance().getFileAt(
						fileEditorConfiguration.getFileAt(index).getPath()) == null) {

					// Gets its content
					String fileContent = null;
					fileContent = AcideFileManager.getInstance().load(
							fileEditorConfiguration.getFileAt(index).getPath());

					// Creates the lexicon configuration
					AcideLexiconConfiguration lexiconConfiguration = new AcideLexiconConfiguration();

					// Loads the lexicon configuration
					lexiconConfiguration.load(fileEditorConfiguration
							.getFileAt(index).getLexiconConfiguration());

					// Creates the current grammar configuration
					AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

					// Sets the current grammar configuration path
					currentGrammarConfiguration.setPath(fileEditorConfiguration
							.getFileAt(index).getCurrentGrammarConfiguration());

					// Creates the previous grammar configuration
					AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

					// Sets the previous grammar configuration path
					previousGrammarConfiguration
							.setPath(fileEditorConfiguration.getFileAt(index)
									.getPreviousGrammarConfiguration());

					// Updates the tabbed pane in the file editor configuration
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.updateTabbedPane(
									fileEditorConfiguration.getFileAt(index)
											.getPath(),
									fileContent,
									true,
									fileEditorConfiguration.getFileAt(index)
											.getType(),
									fileEditorConfiguration.getFileAt(index)
											.getCaretPosition(),
									fileEditorConfiguration.getFileAt(index)
											.getSplitPaneDividerLocation(),
									fileEditorConfiguration.getFileAt(index)
											.getActiveTextEditionArea(),
									lexiconConfiguration,
									currentGrammarConfiguration,
									previousGrammarConfiguration);

					// The project configuration has not been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(false);
				} else {

					// If it is already opened just updates it

					String fileName = null;

					// Gets the file name from the file absolute path

					// Gets the file path
					String filePath = fileEditorConfiguration.getFileAt(index)
							.getPath();

					// Gets the name
					int lastIndexOfSlash = filePath.lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = filePath.lastIndexOf("/");
					fileName = filePath.substring(lastIndexOfSlash + 1,
							filePath.length());

					// Gets the file editor panel
					AcideFileEditorPanel fileEditorPanel = AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(fileName));

					// Loads the lexicon configuration
					fileEditorPanel.getLexiconConfiguration().load(
							fileEditorConfiguration.getFileAt(index)
									.getLexiconConfiguration());

					// Resets the lexicon configuration
					fileEditorPanel.resetStyledDocument();

					// Updates the lexicon message status bar
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setLexiconMessage(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s449")
											+ " "
											+ fileEditorPanel
													.getLexiconConfiguration()
													.getName());

					// Creates the current grammar configuration
					AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

					// Sets the current grammar configuration path
					currentGrammarConfiguration.setPath(fileEditorConfiguration
							.getFileAt(index).getCurrentGrammarConfiguration());

					// Creates the previous grammar configuration
					AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

					// Sets the previous grammar configuration path
					previousGrammarConfiguration
							.setPath(fileEditorConfiguration.getFileAt(index)
									.getPreviousGrammarConfiguration());

					// Updates its current grammar configuration
					fileEditorPanel
							.setCurrentGrammarConfiguration(currentGrammarConfiguration);

					// Updates its previous grammar configuration
					fileEditorPanel
							.setPreviousGrammarConfiguration(previousGrammarConfiguration);

					// Sets the split pane divider location
					fileEditorPanel.getHorizontalSplitPane()
							.setDividerLocation(
									fileEditorConfiguration.getFileAt(index)
											.getSplitPaneDividerLocation());

					// Sets the active text edition area
					fileEditorPanel
							.setActiveTextEditionAreaIndex(fileEditorConfiguration
									.getFileAt(index)
									.getActiveTextEditionArea());

					// Sets the caret position
					fileEditorPanel.getActiveTextEditionArea()
							.setCaretPosition(
									fileEditorConfiguration.getFileAt(index)
											.getCaretPosition());
				}
			} else {

				// If the file does not exist
				if (!file.exists()) {

					// If the file does not belong to the project
					if (AcideProjectConfiguration.getInstance().getFileAt(
							fileEditorConfiguration.getFileAt(index).getPath()) == null)

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
										"Error", JOptionPane.ERROR_MESSAGE);
				}
			}
		}
	}

	/**
	 * Sets the selected file editor in the ACIDE - A Configurable IDE file
	 * editor manager from the ACIDE - A Configurable IDE workbench
	 * configuration.
	 */
	public void setSelectedFileEditor() {

		// If there are files opened
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Gets the selected file editor index from the workbench
			// configuration
			final int selectedFileEditorIndex = AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(
							AcideWorkbenchConfiguration.getInstance()
									.getFileEditorConfiguration()
									.getSelectedFileEditorPanelName());

			// If the selected file editor panel is inside the bounds
			if (selectedFileEditorIndex >= 0
					&& selectedFileEditorIndex < AcideMainWindow.getInstance()
							.getFileEditorManager().getTabbedPane()
							.getTabCount())

				SwingUtilities.invokeLater(new Runnable() {

					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {

						// Sets the selected file editor from the file
						// editor configuration
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.updateRelatedComponentsAt(
										selectedFileEditorIndex);
					}
				});

			else

				/*
				 * NOTE: By default the selected file editor panel is the last
				 * one opened in the tabbed pane.
				 */

				SwingUtilities.invokeLater(new Runnable() {

					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {

						// Sets the selected file editor panel
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.updateRelatedComponentsAt(
										AcideMainWindow.getInstance()
												.getFileEditorManager()
												.getTabbedPane().getTabCount() - 1);
					}
				});
		} else {

			// Selects the explorer tree root node
			AcideMainWindow.getInstance().getExplorerPanel().getTree()
					.setSelectionInterval(0, 0);

			// Sets the focus in the explorer panel
			AcideMainWindow.getInstance().getExplorerPanel()
					.requestFocusInWindow();
		}
	}
}
