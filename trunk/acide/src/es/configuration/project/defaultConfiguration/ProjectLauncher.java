package es.configuration.project.defaultConfiguration;

import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.output.OutputConfiguration;
import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.Cursor;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.undo.UndoableEdit;

import language.Language;

import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Loads the last project configuration of ACIDE - A Configurable IDE											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
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
public class ProjectLauncher {

	/**
	 * Searches for a file into the list of files
	 * 
	 * @param list
	 *            list of files
	 * @param fileName
	 *            file name to search for
	 * @return the file itself if it exists, and null in the opposite case
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
	 * the file previously loaded
	 * 
	 * @param configurationFileContent
	 *            configuration file content
	 */
	public static void load(String configurationFileContent) {

		// SET THE WAIT CURSOR
		Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
		MainWindow.getInstance().setCursor(cursor);

		// LOAD THE PROJECT CONFIGURATION
		MainWindow.getInstance().getProjectConfiguration()
				.load(configurationFileContent);

		// LOAD THE MAIN WINDOW CONFIGURATION
		loadMainWindowConfiguration();

		// LOAD THE LEXICON CONFIGURATION
		LexiconConfiguration lexiconConfiguration = loadLexiconConfiguration();

		// LOAD THE LANGUAGE FOR THE LABELS OF THE APPLICATION
		ResourceBundle labels = loadLanguageConfiguration();

		// LOAD THE EDITOR CONFIGURATION
		loadEditorConfiguration(labels);

		// Updates the status bar WITH THE LEXICAL NAME
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						labels.getString("s449") + " "
								+ lexiconConfiguration.getName());

		// SET THE GRAMMAR CONFIGURATION OF THE APPLICATION
		loadSyntacticConfiguration(labels);

		// LOAD THE OUTPUT CONFIGURATION
		loadOutputConfiguration();

		// LOAD THE EXPLORER CONFIGURATION
		loadExplorer(labels);

		// WE SET THE DEFAULT CURSOR ONCE ALL THE COMPONENTS OF THE
		// CONFIGURATION ARE SET
		cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
		MainWindow.getInstance().setCursor(cursor);
	}

	/**
	 * Loads the editor configuration, opening the related files to the project
	 * 
	 * @param labels
	 *            labels to display in the selected language
	 */
	public static void loadEditorConfiguration(ResourceBundle labels) {

		for (int j = 0; j < MainWindow.getInstance().getProjectConfiguration()
				.getNumFilesFromList(); j++) {

			// CHECK IF THE FILE REALLY EXISTS
			File file = new File(MainWindow.getInstance()
					.getProjectConfiguration().getFileAt(j).getPath());

			// IF THE FILE IS NOT A DIRECTORY AND IT EXISTS
			if (!MainWindow.getInstance().getProjectConfiguration()
					.getFileAt(j).isDirectory()
					&& file.exists()) {

				TextFile textFile = IOFactory.getInstance().buildFile();
				String text = null;
				text = textFile.load(MainWindow.getInstance()
						.getProjectConfiguration().getFileAt(j).getPath());

				String fileName = null;
				String filePath = MainWindow.getInstance()
						.getProjectConfiguration().getFileAt(j).getPath();

				// EXTRACT THE NAME OF THE FILE BASED ON THE PATH
				if (filePath != null) {

					// WE COUNT FROM THE LAST APPEARENCE OF THE SYMBOL "/"
					int index = filePath.lastIndexOf("\\");
					if (index == -1)
						index = filePath.lastIndexOf("/");
					fileName = filePath.substring(index + 1, filePath.length());
				}

				// IF THE FILE IS ALREADY OPENED
				if (MainWindow.getInstance().getProjectConfiguration()
						.isDefaultProject()
						|| MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(j).isOpened()) {

					// ENABLE FILE AND EDIT MENUS
					MainWindow.getInstance().getMenu().enableFileMenu();
					MainWindow.getInstance().getMenu().enableEditMenu();

					// Updates the status bar WITH THE NAME OF THE FILE
					MainWindow
							.getInstance()
							.getStatusBar()
							.setMessage(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(j).getPath());

					// CHECK IF IT IS A MAIN OR COMPILABLE FILE
					int fileType = 0;

					// COMPILABLE
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(j).isCompilableFile()) {

						fileType = 2;

						// ADD THE <COMPILABLE> TAG TO THE MESSAGE IN THE STATUS
						// BAR
						MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(j).getPath()
												+ " <COMPILABLE>");
					}
					
					// MAIN
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(j).isMainFile()) {

						fileType = 1;

						// ADD THE <MAIN> TAG TO THE MESSAGE IN THE STATUS BAR
						MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(j).getPath()
												+ " <MAIN>");
					}

					// OPEN THE NEW TAB IN THE EDITOR
					MainWindow.getInstance().getEditorManager()
							.newTab(fileName, filePath, text, true, fileType);

					// CHECK IF IT IS MARKED AS A MAIN OR COMPILABLE FILE
					for (int i = 0; i < MainWindow.getInstance()
							.getEditorManager().getNumEditors(); i++) {

						if (MainWindow
								.getInstance()
								.getEditorManager()
								.getEditorAt(i)
								.getAbsolutePath()
								.equals(MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(j)
										.getPath())) {

							// IS COMPILABLE FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(j)
									.isCompilableFile())
								MainWindow.getInstance().getEditorManager()
										.getEditorAt(i).setCompilerFile(true);

							// IS MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(j)
									.isMainFile())
								MainWindow.getInstance().getEditorManager()
										.getEditorAt(i).setMainFile(true);
						}
					}

					// CONFIGURES THE UNDO REDO MANAGER OF THE APPLICATION
					int selectedEditorIndex = MainWindow.getInstance()
							.getEditorManager().getSelectedEditorIndex();
					DefaultStyledDocument doc = MainWindow.getInstance()
							.getEditorManager().getSelectedEditor()
							.getSyntaxDocument();

					doc.addUndoableEditListener(new UndoableEditListener() {
						/*
						 * (non-Javadoc)
						 * 
						 * @see javax.swing.event.UndoableEditListener#
						 * undoableEditHappened
						 * (javax.swing.event.UndoableEditEvent)
						 */
						public void undoableEditHappened(UndoableEditEvent undoableEditEvent) {

							UndoableEdit edit = undoableEditEvent.getEdit();

							if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
									.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
								MainWindow.getInstance().getMenu().getEdit()
										.getUndoManager()
										.addEdit(undoableEditEvent.getEdit());
							}
						}
					});

					// SET THE CARET POSITION IN THE FIRST POSITION OF THE FILE
					// OPENED
					// IN THE EDITOR
					selectedEditorIndex = MainWindow.getInstance()
							.getEditorManager().getSelectedEditorIndex();
					MainWindow.getInstance().getEditorManager()
							.getEditorAt(selectedEditorIndex).getEditor()
							.setCaretPosition(0);
				}

				// THE PROJECT CONFIGURATION HAS BEEN MODIFIED
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(false);
			} else {

				// IF THE FILE DOES NOT EXISTS
				if (!file.exists()) {

					// DISPLAY A MESSAGE TO THE USER
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s970")
									+ MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(j).getPath()
									+ labels.getString("s971"), "Error",
							JOptionPane.ERROR_MESSAGE);

					// REMOVE THE FILE FROM THE PROJECT
					MainWindow.getInstance().getProjectConfiguration()
							.removeFileAt(j);
					
					// THE PROJECT CONFIGURATION HAS BEEN MODIFIED
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				}
			}
		}
	}

	/**
	 * Loads the syntactic configuration of the application
	 * 
	 * @param labels labels to display in the selected language
	 */
	public static void loadSyntacticConfiguration(ResourceBundle labels) {
		
		String currentGrammar = null;
		String grammarName = null;
		
		try {

			// GET THE NAME
			currentGrammar = PropertiesManager.getProperty("currentGrammar");
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

			// ERROR MESSAGE
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s945"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
		}
	}

	/**
	 * Loads the language configuration
	 * 
	 * @return the language configuration
	 */
	public static ResourceBundle loadLanguageConfiguration() {
		
		String configurationLanguage = MainWindow.getInstance()
				.getProjectConfiguration().getLanguage();
		Language language = Language.getInstance();

		// SPANISH
		if (configurationLanguage.equals("spanish"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getSpanish().doClick();

		// ENGLISH
		if (configurationLanguage.equals("english"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getEnglish().doClick();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		return labels;
	}

	/**
	 * Loads the main window configuration
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

		// CONFIGURES THE SIZE OF THE MAIN WINDOW
		MainWindow.getInstance().setSize(
				MainWindow.getInstance().getProjectConfiguration()
						.getWindowWidth(),
				MainWindow.getInstance().getProjectConfiguration()
						.getWindowHeight());
		MainWindow.getInstance().setLocation(
				MainWindow.getInstance().getProjectConfiguration().getPosX(),
				MainWindow.getInstance().getProjectConfiguration().getPosY());
		MainWindow
				.getInstance()
				.getSplitPaneVertical()
				.setDividerLocation(
						MainWindow.getInstance().getProjectConfiguration()
								.getSplitPaneVerticalDividerLocation());
		MainWindow
				.getInstance()
				.getSplitPaneHorizontal()
				.setDividerLocation(
						MainWindow.getInstance().getProjectConfiguration()
								.getSplitPanelHorizontalDividerLocation());

		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
		MainWindow.getInstance().setVisible(true);

		// NOT DEFAULT PROJECT
		if (!MainWindow.getInstance().getProjectConfiguration()
				.isDefaultProject())
			MainWindow.getInstance().getMenu().enableProjectMenu();
	}

	/**
	 * Loads the lexicon configuration of ACIDE - A Configurable IDE
	 * 
	 * @return the lexicon configuration of ACIDE - Configurable IDE
	 */
	public static LexiconConfiguration loadLexiconConfiguration() {

		PropertiesManager.setProperty("languagePath", MainWindow.getInstance()
				.getProjectConfiguration().getLexicalConfiguration());

		LexiconConfiguration lexiconConfiguration = LexiconConfiguration
				.getInstance();
		try {
			lexiconConfiguration.load(PropertiesManager
					.getProperty("languagePath"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		return lexiconConfiguration;
	}

	/**
	 * Loads the explorer files and builds the explorer in the main window
	 * 
	 * @param labels labels to display in the selected language
	 */
	public static void loadExplorer(ResourceBundle labels) {

		try {

			// DEFAULT PROJECT
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// SET THE TITLE WITH THE <EMPTY> TAG
				MainWindow.getInstance().setTitle(
						MainWindow.getInstance().getTitle() + " - <empty>");
			} else {

				// SET ALL THE FEATURES FOR THE MAIN WINDOW TO ALLOW THE OPTIONS
				// FOR AN OPEN PROJECT CONFIGURATION
				String name = MainWindow.getInstance()
						.getProjectConfiguration().getName();

				ExplorerFile explorerFile = new ExplorerFile();
				explorerFile.setPath(name);
				explorerFile.setName(name);
				explorerFile.setParent(null);
				explorerFile.setIsDirectory(true);

				// ENABLE THE OPTIONS FOR THE EXPLORER
				MainWindow.getInstance().getExplorer().getPopupMenu()
						.getAddFile().setEnabled(true);
				MainWindow.getInstance().getExplorer().getPopupMenu()
						.getRemoveFile().setEnabled(true);

				// SET THE TITLE OF THE PROJECT
				MainWindow.getInstance().setTitle(
						labels.getString("s425")
								+ " - "
								+ MainWindow.getInstance()
										.getProjectConfiguration().getName());

				// BUILD THE TREE OF THE EXPLORER WITH ALL THE FILES ASSOCIATED
				// TO
				// THE PROJECT
				MainWindow.getInstance().getExplorer().getRoot()
						.removeAllChildren();
				DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
						explorerFile);
				MainWindow.getInstance().getExplorer().getRoot()
						.add(defaultMutableTreeNode);
				ArrayList<DefaultMutableTreeNode> directoryList = new ArrayList<DefaultMutableTreeNode>();

				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					DefaultMutableTreeNode node = new DefaultMutableTreeNode(
							MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(i));
					// CHECK IF THE FILE REALLY EXISTS
					File file = new File(MainWindow.getInstance()
							.getProjectConfiguration().getFileAt(i).getPath());

					// IF EXISTS
					if (file.exists()) {

						// IF IT IS DIRECTORY
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).isDirectory()) {
							node.setAllowsChildren(true);
							directoryList.add(node);
						} else
							// NO CHILDREN
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

				// IF IT HAS MORE THAN 0 FILES ASSOCIATED
				if (MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList() > 0)

					// ALLOW THE REMOVE FILE OPTION FOR THE EXPLORER
					MainWindow.getInstance().getExplorer().getPopupMenu()
							.getRemoveFile().setEnabled(true);
				else
					MainWindow.getInstance().getExplorer().getPopupMenu()
							.getRemoveFile().setEnabled(false);

				// STORE THE DEFAULT CONFIGURATION
				MainWindow.getInstance().getProjectConfiguration()
						.setFirstSave(true);
				MainWindow
						.getInstance()
						.getProjectConfiguration()
						.setPath(
								PropertiesManager
										.getProperty("defaultAcideProject"));
			}
		} catch (NumberFormatException exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Loads the output configuration and applies it to the Output in the main
	 * window
	 */
	public static void loadOutputConfiguration() {

		// LOAD THE OUTPUT CONFIGURATION
		PropertiesManager.setProperty("outputConfiguration", MainWindow
				.getInstance().getProjectConfiguration()
				.getOutputConfiguration());

		OutputConfiguration outputConfiguration = OutputConfiguration
				.getInstance();
		try {
			outputConfiguration.load(PropertiesManager
					.getProperty("outputConfiguration"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// WAIT A LITTLE WHILE
	    //try { Thread.sleep(1000); } catch (Exception e) {}
	    
		// SET THE VISUALIZATION OPTIONS
		MainWindow.getInstance().getOutput().updateVisualization();

		// RESET THE OUTPUT
		MainWindow.getInstance().getOutput().resetOutput();
	}

	/**
	 * Gets the configuration file content which contains the
	 * configuration of the last opened project whether it is the "empty"
	 * configuration or another.
	 * 
	 * @return the content of the default configuration to load
	 */
	public static String getConfigurationFileContent() {

		// Gets the language TO DISPLAY
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// DELETE ALL THE FILES ASSOCIATED TO THE PROJECT
		MainWindow.getInstance().getProjectConfiguration().removeFiles();

		// GET THE CONTENT OF THE DEFAULT CONFIGURATION FILE
		TextFile textFile = new TextFile();
		String fileContent = null;
		String path = null;

		try {

			path = PropertiesManager.getProperty("defaultAcideProject");
			fileContent = textFile.load(path);

			// IF IT CAN'T FIND THE FILE
			if (fileContent == null) {

				// LOAD THE DEFAULT FILE BY DEFAULT
				fileContent = textFile
						.load("./configuration/project/default.acidePrj");
				PropertiesManager.setProperty("defaultAcideProject",
						"./configuration/project/default.acidePrj");

				// DISPLAYS A MESSAGE TO THE USER
				JOptionPane.showMessageDialog(null, labels.getString("s960")
						+ path + labels.getString("s959"));
			}
		} catch (Exception exception) {

			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// LOAD THE DEFAULT FILE BY DEFAULT
			fileContent = textFile
					.load("./configuration/project/default.acidePrj");

			// DISPLAYS A MESSAGE TO THE USER
			JOptionPane.showMessageDialog(null, labels.getString("s960") + path
					+ labels.getString("s959"));
		}
		return fileContent;
	}
}
