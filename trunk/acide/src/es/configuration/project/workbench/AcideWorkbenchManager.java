package es.configuration.project.workbench;

import java.awt.Cursor;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;

import language.AcideLanguage;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;
import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.output.OutputConfiguration;
import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

/************************************************************************
 * Handles the workbench configuration of ACIDE - A Configurable IDE. Loads
 * the workbench configuration from the workbench configuration file at the 
 * beginning of the application, and stores the workbench configuration when
 * the user close the project or the application.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 ***********************************************************************/
public class AcideWorkbenchManager {

	/**
	 * Acide - A Configurable IDE workbench manager unique class
	 * instance.
	 */
	private static AcideWorkbenchManager _instance;

	/**
	 * Returns the Acide - A Configurable IDE workbench manager
	 * unique class instance.
	 * 
	 * @return the Acide - A Configurable IDE workbench manager
	 *         unique class instance.
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

			ExplorerFile file = (ExplorerFile) temp.getUserObject();

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

		// Sets the WAIT CURSOR
		Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
		MainWindow.getInstance().setCursor(cursor);

		// Loads the PROJECT CONFIGURATION
		MainWindow.getInstance().getProjectConfiguration()
				.load(configurationFileContent);

		// Loads the MAIN WINDOW CONFIGURATION
		loadMainWindowConfiguration();

		// Loads the LEXICON CONFIGURATION
		LexiconConfiguration lexiconConfiguration = loadLexiconWorkbenchConfiguration();

		// Loads the LANGUAGE FOR THE LABELS OF THE APPLICATION
		ResourceBundle labels = loadLanguageWorkbenchConfiguration();

		// Loads the OUTPUT CONFIGURATION
		loadOutputWorkbenchConfiguration();

		// Loads the EXPLORER CONFIGURATION
		loadExplorerWorkbenchConfiguration(labels);

		// Loads the EDITOR CONFIGURATION
		loadFileEditorWorkbenchConfiguration(labels);

		// Updates the status bar with the lexical
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						labels.getString("s449") + " "
								+ lexiconConfiguration.getName());

		// Loads the GRAMMAR CONFIGURATION
		loadGrammarWorkbenchConfiguration(labels);

		// Sets the DEFAULT CURSOR
		cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
		MainWindow.getInstance().setCursor(cursor);
	}

	/**
	 * Loads the file editor workbench configuration, opening the related files
	 * to the project.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public static void loadFileEditorWorkbenchConfiguration(
			ResourceBundle labels) {

		for (int index = 0; index < MainWindow.getInstance()
				.getProjectConfiguration().getNumFilesFromList(); index++) {

			// Checks if the file really exists
			File file = new File(MainWindow.getInstance()
					.getProjectConfiguration().getFileAt(index).getPath());

			// If the file is not a directory and exists
			if (!MainWindow.getInstance().getProjectConfiguration()
					.getFileAt(index).isDirectory()
					&& file.exists()) {

				TextFile textFile = AcideIOFactory.getInstance().buildFile();
				String text = null;
				text = textFile.load(MainWindow.getInstance()
						.getProjectConfiguration().getFileAt(index).getPath());

				String fileName = null;
				String filePath = MainWindow.getInstance()
						.getProjectConfiguration().getFileAt(index).getPath();

				// Gets the file name
				if (filePath != null) {

					int lastIndexOfSlash = filePath.lastIndexOf("\\");
					if (lastIndexOfSlash == -1)
						lastIndexOfSlash = filePath.lastIndexOf("/");
					fileName = filePath.substring(lastIndexOfSlash + 1,
							filePath.length());
				}

				// If it is the default project or the file is has to be opened
				if (MainWindow.getInstance().getProjectConfiguration()
						.isDefaultProject()
						|| MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(index).isOpened()) {

					// Updates the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setMessage(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(index).getPath());

					// Check if it is a MAIN or COMPILABLE FILE
					int fileType = 0;

					// COMPILABLE
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(index).isCompilableFile()) {

						fileType = 2;

						// Adds the <COMPILABLE> tag in the status bar
						MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(index).getPath()
												+ " <COMPILABLE>");
					}

					// MAIN
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(index).isMainFile()) {

						fileType = 1;

						// Adds the <MAIN> tag in the status bar
						MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(index).getPath()
												+ " <MAIN>");
					}

					// Opens a new tab in the editor
					MainWindow.getInstance().getFileEditorManager()
							.newTab(fileName, filePath, text, true, fileType);

					// Checks if it is marked as a MAIN or COMPILABLE FILE
					for (int i = 0; i < MainWindow.getInstance()
							.getFileEditorManager().getNumFileEditorPanels(); i++) {

						if (MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(i)
								.getAbsolutePath()
								.equals(MainWindow.getInstance()
										.getProjectConfiguration()
										.getFileAt(index).getPath())) {

							// IS COMPILABLE FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(index)
									.isCompilableFile())
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i)
										.setCompilerFile(true);

							// IS MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(index)
									.isMainFile())
								MainWindow.getInstance().getFileEditorManager()
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
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(false);
			} else {

				// If the file does not exist
				if (!file.exists()) {

					// Error message
					JOptionPane.showMessageDialog(null,
							labels.getString("s970")
									+ MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(index).getPath()
									+ labels.getString("s971"), "Error",
							JOptionPane.ERROR_MESSAGE);

					// Removes the file from the project
					MainWindow.getInstance().getProjectConfiguration()
							.removeFileAt(index);

					// The project configuration has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				}
			}
		}

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				// Sets the selected editor
				if (MainWindow.getInstance().getProjectConfiguration()
						.getSelectedEditorIndex() != -1) {

					// Sets the selected file editor
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.setSelectedFileEditorPanelAt(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getSelectedEditorIndex());

					// Sets the focus in the edition area
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().requestFocusInWindow();

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
			currentGrammar = ResourceManager.getInstance().getProperty(
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

		String configurationLanguage = MainWindow.getInstance()
				.getProjectConfiguration().getLanguage();
		AcideLanguage language = AcideLanguage.getInstance();

		// SPANISH
		if (configurationLanguage.equals("spanish"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getSpanish().doClick();

		// ENGLISH
		if (configurationLanguage.equals("english"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getEnglish().doClick();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
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

		// CHECK IF THE EXPLORER HAS TO BE SHOWED
		if (!MainWindow.getInstance().getProjectConfiguration()
				.isExplorerShowed())
			MainWindow.getInstance().getMenu().getView().getShowExplorerPanel()
					.doClick();

		// CHECK IF THE OUTPUT SHELL HAS TO BE SHOWED
		if (!MainWindow.getInstance().getProjectConfiguration().isShellShowed())
			MainWindow.getInstance().getMenu().getView().getShowShellWindow()
					.doClick();

		// Sets the main window size
		MainWindow.getInstance().setSize(
				MainWindow.getInstance().getProjectConfiguration()
						.getWindowWidth(),
				MainWindow.getInstance().getProjectConfiguration()
						.getWindowHeight());

		// Sets the main window location
		MainWindow.getInstance().setLocation(
				MainWindow.getInstance().getProjectConfiguration().getPosX(),
				MainWindow.getInstance().getProjectConfiguration().getPosY());

		// Sets the main window split panel vertical divider location
		MainWindow
				.getInstance()
				.getVerticalSplitPane()
				.setDividerLocation(
						MainWindow.getInstance().getProjectConfiguration()
								.getSplitPaneVerticalDividerLocation());

		// Sets the main window split panel horizontal divider location
		MainWindow
				.getInstance()
				.getHorizontalSplitPane()
				.setDividerLocation(
						MainWindow.getInstance().getProjectConfiguration()
								.getSplitPanelHorizontalDividerLocation());

		// Updates the main window
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
		MainWindow.getInstance().setVisible(true);

		// Not default project
		if (!MainWindow.getInstance().getProjectConfiguration()
				.isDefaultProject()){

			// Enables the project menu
			MainWindow.getInstance().getMenu().enableProjectMenu();
			
			// Enables the open all files menu item
			MainWindow.getInstance().getMenu().getFile().getOpenAllFiles().setEnabled(true);
		}
	}

	/**
	 * Loads the lexicon workbench configuration of ACIDE - A Configurable IDE.
	 * 
	 * @return the lexicon workbench configuration of ACIDE - Configurable IDE.
	 */
	public static LexiconConfiguration loadLexiconWorkbenchConfiguration() {

		// Updates the RESOURCE MANAGER
		ResourceManager.getInstance().setProperty(
				"languagePath",
				MainWindow.getInstance().getProjectConfiguration()
						.getLexicalConfiguration());

		// Gets the lexicon configuration
		LexiconConfiguration lexiconConfiguration = LexiconConfiguration
				.getInstance();
		try {
			lexiconConfiguration.load(ResourceManager.getInstance()
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
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Sets the title with the <empty> tag
				MainWindow.getInstance().setTitle(
						MainWindow.getInstance().getTitle() + " - <empty>");
			} else {

				// Sets all the features for the main window to allow the
				// options for an open project configuration
				String name = MainWindow.getInstance()
						.getProjectConfiguration().getName();

				ExplorerFile explorerFile = new ExplorerFile();
				explorerFile.setPath(name);
				explorerFile.setName(name);
				explorerFile.setParent(null);
				explorerFile.setIsDirectory(true);

				// Enables the explorer menu options
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getAddFile().setEnabled(true);
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getRemoveFile().setEnabled(true);

				// Sets the project title
				MainWindow.getInstance().setTitle(
						labels.getString("s425")
								+ " - "
								+ MainWindow.getInstance()
										.getProjectConfiguration().getName());

				// Builds the EXPLORER TREE with all the associated files
				MainWindow.getInstance().getExplorerPanel().getRoot()
						.removeAllChildren();
				DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
						explorerFile);
				MainWindow.getInstance().getExplorerPanel().getRoot()
						.add(defaultMutableTreeNode);
				ArrayList<DefaultMutableTreeNode> directoryList = new ArrayList<DefaultMutableTreeNode>();

				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// Gets the node
					DefaultMutableTreeNode node = new DefaultMutableTreeNode(
							MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(i));

					// Checks if the file really exists
					File file = new File(MainWindow.getInstance()
							.getProjectConfiguration().getFileAt(i).getPath());

					// If exists
					if (file.exists()) {

						// Directory?
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).isDirectory()) {
							node.setAllowsChildren(true);
							directoryList.add(node);
						} else
							// No children
							node.setAllowsChildren(false);

						if (MainWindow
								.getInstance()
								.getProjectConfiguration()
								.getFileAt(i)
								.getParent()
								.equals(MainWindow.getInstance()
										.getProjectConfiguration().getName())) {
							defaultMutableTreeNode.add(node);
						} else {
							DefaultMutableTreeNode fh = searchDirectoryList(
									directoryList, MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(i).getParent());
							fh.add(node);
						}
					}
				}
				MainWindow.getInstance().getExplorerPanel().getTreeModel().reload();
				MainWindow.getInstance().getExplorerPanel().expandTree();
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getAddFile().setEnabled(true);
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getSaveProject().setEnabled(true);

				// If it has more than 0 files associated
				if (MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList() > 0)

					// Allows to remove files in the EXPLORER menu
					MainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getRemoveFile().setEnabled(true);
				else
					MainWindow.getInstance().getExplorerPanel().getPopupMenu()
							.getRemoveFile().setEnabled(false);

				// Saves the default configuration
				MainWindow.getInstance().getProjectConfiguration()
						.setFirstSave(true);
				MainWindow
						.getInstance()
						.getProjectConfiguration()
						.setPath(
								ResourceManager.getInstance().getProperty(
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
	 * Loads the output configuration and applies it to the Output in the main
	 * window.
	 */
	public static void loadOutputWorkbenchConfiguration() {

		// Updates the RESOURCE MANAGER
		ResourceManager.getInstance().setProperty(
				"outputConfiguration",
				MainWindow.getInstance().getProjectConfiguration()
						.getOutputConfiguration());

		// Gets the output configuration
		OutputConfiguration outputConfiguration = OutputConfiguration
				.getInstance();
		try {
			outputConfiguration.load(ResourceManager.getInstance().getProperty(
					"outputConfiguration"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Sets the shell display options
		MainWindow.getInstance().getOutputPanel().updateShellDisplayOptions();

		// Resets the output
		MainWindow.getInstance().getOutputPanel().resetOutput();
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
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// Deletes all the files associated to the project
		MainWindow.getInstance().getProjectConfiguration().removeFiles();

		// Gets the default configuration file content
		TextFile textFile = new TextFile();
		String fileContent = null;
		String path = null;

		try {

			path = ResourceManager.getInstance().getProperty(
					"defaultAcideProject");
			fileContent = textFile.load(path);

			// If it can't find the file
			if (fileContent == null) {

				// Loads the default file
				fileContent = textFile
						.load("./configuration/project/default.acidePrj");

				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty(
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
	 * Saves the current file editor panel configuration. Closes the new and log tab in the
	 * editor and saves the state of each one of the opened files in the file editor
	 * into the project configuration, whether it is the default project or not.
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

			String file = ResourceManager.getInstance().getProperty(
					"defaultAcideProject");

			// Is default project
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Files which belong to the project
				TextFile textFile = new AcideIOFactory().buildFile();
				MainWindow.getInstance().getProjectConfiguration()
						.removeFiles();

				// Sets the all opened files in the editor
				for (int pos = 0; pos < MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels(); pos++) {

					// Creates the file
					ExplorerFile explorerFile = new ExplorerFile();
					explorerFile.setPath(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(pos)
							.getAbsolutePath());

					// Sets if it is compilable file
					explorerFile.setIsCompilableFile(MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(pos)
							.isCompilerFile());

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
					MainWindow.getInstance().getProjectConfiguration()
							.addFile(explorerFile);
				}

				// Sets the language
				MainWindow
						.getInstance()
						.getProjectConfiguration()
						.setLanguage(
								ResourceManager.getInstance().getProperty(
										"language"));

				// Sets the name
				MainWindow.getInstance().getProjectConfiguration().setName("");

				// Saves the configuration in the file
				textFile.save("./configuration/project/default.acidePrj",
						MainWindow.getInstance().getProjectConfiguration()
								.save());

				// Sets the default project
				ResourceManager.getInstance().setProperty(
						"defaultAcideProject",
						"./configuration/project/default.acidePrj");
			} else {

				// Saves the configuration of the project
				TextFile textFile = new AcideIOFactory().buildFile();
				textFile.save(file, MainWindow.getInstance()
						.getProjectConfiguration().save());
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
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
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
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		// Is the project configuration modified?
		if (MainWindow.getInstance().getProjectConfiguration().isModified()) {

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

					// Checks the opened files in the editor
					int selectedEditor = MainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();
					int numEditors = MainWindow.getInstance()
							.getFileEditorManager().getNumFileEditorPanels();

					// Starts with the last one
					MainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(numEditors - 1);

					for (int cont = numEditors - 1; cont >= 0; cont--) {
						MainWindow.getInstance().getFileEditorManager()
								.setSelectedFileEditorPanelAt(cont);

						// Is the file in the editor modified?
						if (MainWindow.getInstance().getFileEditorManager()
								.isRedButton()) {

							// Asks the user to save the file
							chosenOption = JOptionPane.showConfirmDialog(null,
									labels.getString("s643"),
									labels.getString("s953"),
									JOptionPane.YES_NO_OPTION);

							// If yes
							if (chosenOption == JOptionPane.OK_OPTION) {
								MainWindow.getInstance().getMenu().getFile()
										.saveOrSaveAS();
							}
						}
					}

					// Sets the original selected editor
					MainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(selectedEditor);

					// Saves the output configuration
					OutputConfiguration.getInstance().save();
					// Closes the output
					MainWindow.getInstance().getOutputPanel().executeExitCommand();

					// Updates the configuration
					try {

						// Menu configuration
						String currentMenu = ResourceManager.getInstance()
								.getProperty("currentMenuConfiguration");

						if ((currentMenu.endsWith("lastModified.menuCfg"))
								|| (currentMenu.endsWith("newMenu.menuCfg"))) {
							String previous = ResourceManager.getInstance()
									.getProperty("previousMenuConfiguration");

							// Updates the RESOURCE MANAGER
							ResourceManager.getInstance().setProperty(
									"currentMenuConfiguration", previous);
						}

						// Tool bar configuration
						String currentToolBar = ResourceManager.getInstance()
								.getProperty("currentToolBarConfiguration");
						if ((currentToolBar.endsWith("lastModified.TBcfg"))
								|| currentToolBar.endsWith("newToolBar.TBcfg")) {
							String previous = ResourceManager
									.getInstance()
									.getProperty("previousToolBarConfiguration");

							// Updates the RESOURCE MANAGER
							ResourceManager.getInstance().setProperty(
									"currentToolBarConfiguration", previous);
						}

						// Grammar configuration
						String currentGrammar = ResourceManager.getInstance()
								.getProperty("currentGrammar");
						if ((currentGrammar.endsWith("lastModified.jar"))
								|| (currentGrammar.endsWith("newGrammar.jar"))) {
							String previous = ResourceManager.getInstance()
									.getProperty("previousGrammar");

							// Updates the RESOURCE MANAGER
							ResourceManager.getInstance().setProperty(
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

					// Saves the configuration parameters of the main window
					MainWindow.getInstance().getProjectConfiguration()
							.saveMainWindowParameters();
				}

				// Closes the main window
				System.exit(0);
			}
		} else {

			// Stores the configuration of the files
			saveFileEditorPanelConfiguration();

			// Saves the configuration parameters of the main window
			MainWindow.getInstance().getProjectConfiguration()
					.saveMainWindowParameters();

			// Closes the main window
			System.exit(0);
		}
	}
}
