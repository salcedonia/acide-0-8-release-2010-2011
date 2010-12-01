package es.configuration.project.workbench;

import java.awt.Cursor;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.undo.UndoableEdit;

import language.AcideLanguage;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;
import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.output.OutputConfiguration;
import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

/************************************************************************
 * Loads the workbench configuration of ACIDE - A Configurable IDE from the
 * workbench configuration file.
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
public class AcideWorkbenchConfigurationLoader {

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
	public static void loadMainWindowWorkbenchConfiguration(String configurationFileContent) {

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

		// Loads the OUTPUT CONFIGURATION
		loadOutputWorkbenchConfiguration();

		// Loads the EXPLORER CONFIGURATION
		loadExplorerWorkbenchConfiguration(labels);

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
	public static void loadFileEditorWorkbenchConfiguration(ResourceBundle labels) {

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

				// If the file is already opened
				if (MainWindow.getInstance().getProjectConfiguration()
						.isDefaultProject()
						|| MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(index).isOpened()) {

					// Enables the file menu
					MainWindow.getInstance().getMenu().enableFileMenu();

					// Enables the edit menu
					MainWindow.getInstance().getMenu().enableEditMenu();

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
										.getFileEditorPanelAt(i).setCompilerFile(true);

							// IS MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(index)
									.isMainFile())
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).setMainFile(true);
						}
					}

					// UNDO REDO
					DefaultStyledDocument document = MainWindow.getInstance()
							.getFileEditorManager().getSelectedFileEditorPanel()
							.getSyntaxDocument();

					document.addUndoableEditListener(new UndoableEditListener() {
						/*
						 * (non-Javadoc)
						 * 
						 * @see javax.swing.event.UndoableEditListener#
						 * undoableEditHappened
						 * (javax.swing.event.UndoableEditEvent)
						 */
						public void undoableEditHappened(
								UndoableEditEvent undoableEditEvent) {

							UndoableEdit edit = undoableEditEvent.getEdit();

							if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
									.getType() == DefaultDocumentEvent.EventType.CHANGE))) {

								// Sets the edit property over the selected
								// editor undo manager
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel().getUndoManager()
										.addEdit(undoableEditEvent.getEdit());
							}
						}
					});
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
					
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.setSelectedFileEditorPanelAt(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getSelectedEditorIndex());
					MainWindow
					.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel().getActiveTextEditionArea().requestFocusInWindow();
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
				.getSplitPaneVertical()
				.setDividerLocation(
						MainWindow.getInstance().getProjectConfiguration()
								.getSplitPaneVerticalDividerLocation());

		// Sets the main window split panel horizontal divider location
		MainWindow
				.getInstance()
				.getSplitPaneHorizontal()
				.setDividerLocation(
						MainWindow.getInstance().getProjectConfiguration()
								.getSplitPanelHorizontalDividerLocation());

		// Updates the main window
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
		MainWindow.getInstance().setVisible(true);

		// Not default project
		if (!MainWindow.getInstance().getProjectConfiguration()
				.isDefaultProject())

			// Enables the project menu
			MainWindow.getInstance().getMenu().enableProjectMenu();
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
				MainWindow.getInstance().getExplorer().getPopupMenu()
						.getAddFile().setEnabled(true);
				MainWindow.getInstance().getExplorer().getPopupMenu()
						.getRemoveFile().setEnabled(true);

				// Sets the project title
				MainWindow.getInstance().setTitle(
						labels.getString("s425")
								+ " - "
								+ MainWindow.getInstance()
										.getProjectConfiguration().getName());

				// Builds the EXPLORER TREE with all the associated files
				MainWindow.getInstance().getExplorer().getRoot()
						.removeAllChildren();
				DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
						explorerFile);
				MainWindow.getInstance().getExplorer().getRoot()
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
				MainWindow.getInstance().getExplorer().getTreeModel().reload();
				MainWindow.getInstance().getExplorer().expandTree();
				MainWindow.getInstance().getExplorer().getPopupMenu()
						.getAddFile().setEnabled(true);
				MainWindow.getInstance().getExplorer().getPopupMenu()
						.getSaveProject().setEnabled(true);

				// If it has more than 0 files associated
				if (MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList() > 0)

					// Allows to remove files in the EXPLORER menu
					MainWindow.getInstance().getExplorer().getPopupMenu()
							.getRemoveFile().setEnabled(true);
				else
					MainWindow.getInstance().getExplorer().getPopupMenu()
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
		MainWindow.getInstance().getOutput().updateShellDisplayOptions();

		// Resets the output
		MainWindow.getInstance().getOutput().resetOutput();
	}

	/**
	 * Gets the configuration file content which contains the configuration of
	 * the last opened project whether it is the "empty" configuration or
	 * another.
	 * 
	 * @return the content of the default configuration to load.
	 */
	public static String getConfigurationFileContent() {

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
}
