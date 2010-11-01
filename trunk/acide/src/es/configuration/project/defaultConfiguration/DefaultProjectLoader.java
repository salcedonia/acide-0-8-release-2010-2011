package es.configuration.project.defaultConfiguration;

import es.configuration.lexicon.LexiconConfiguration;
import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.MainWindow;

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

/**
 * Load the last configuration used in the application previously.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class DefaultProjectLoader {

	/**
	 * Search a file into the list of files.
	 * 
	 * @param list
	 *            List of files.
	 * @param fileName
	 *            Name of the file to search for.
	 * 
	 * @return The file itself if it exists, and null in the opposite case.
	 */
	private DefaultMutableTreeNode searchDirectoryList(
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
	 * Get the content of the configuration file which contains the
	 * configuration of the last opened project, whether is the "empty"
	 * configuration or another.
	 * 
	 * @return The content of the default configuration to load.
	 */
	public String getDefaultFileContent() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
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
				fileContent = textFile.load("./configuration/default.acidePrj");
				PropertiesManager.setProperty("defaultAcideProject",
						"./configuration/default.acidePrj");

				// DISPLAYS A MESSAGE TO THE USER
				JOptionPane.showMessageDialog(null, labels.getString("s960")
						+ path + labels.getString("s959"));
			}
		} catch (Exception e) {

			e.printStackTrace();

			// LOAD THE DEFAULT FILE BY DEFAULT
			fileContent = textFile.load("./configuration/default.acidePrj");

			// DISPLAYS A MESSAGE TO THE USER
			JOptionPane.showMessageDialog(null, labels.getString("s960") + path
					+ labels.getString("s959"));
		}
		return fileContent;
	}

	/**
	 * Load the configuration associated to the project based in the content of
	 * the file previously loaded.
	 * 
	 * @param fileContent
	 *            Content of the configuration file.
	 */
	public void load(String fileContent) {

		// SET THE WAIT CURSOR
		Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
		MainWindow.getInstance().setCursor(cursor);

		// LOAD THE PROJECT
		MainWindow.getInstance().getProjectConfiguration().load(fileContent);

		// CHECK IF THE EXPLORER HAS TO BE SHOWED
		if (!MainWindow.getInstance().getProjectConfiguration()
				.isExplorerShowed())
			MainWindow.getInstance().getMenu().getView().getShowBrowser()
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
		MainWindow.getInstance().getSplitPaneVertical().setDividerLocation(
				MainWindow.getInstance().getProjectConfiguration()
						.getSplitPaneVerticalDividerLocation());
		MainWindow.getInstance().getSplitPaneHorizontal().setDividerLocation(
				MainWindow.getInstance().getProjectConfiguration()
						.getSplitPanelHorizontalDividerLocation());

		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
		MainWindow.getInstance().setVisible(true);

		// IF THE NAMES ARE WRONG THEN IT LOADS THE DEFAULT MENU OPTIONS FOR
		// THE PROJECT MENU IN THE MENU BAR
		String project = null;
		try {
			project = PropertiesManager.getProperty("defaultAcideProject");
		} catch (Exception e1) {
			e1.printStackTrace();
		}

		boolean isDefaultProject = true;

		if (!(project.equals("./configuration/default.acidePrj") && MainWindow
				.getInstance().getProjectConfiguration().getName().equals(""))) {

			MainWindow.getInstance().getMenu().enableProjectMenu();
			isDefaultProject = false;
		}

		// LOAD THE LEXICAL CONFIGURATION
		PropertiesManager.setProperty("languagePath", MainWindow.getInstance()
				.getProjectConfiguration().getLexicalConfiguration());

		LexiconConfiguration programmingLanguage = LexiconConfiguration
				.getInstance();
		try {
			programmingLanguage.load(PropertiesManager
					.getProperty("languagePath"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// LOAD THE LANGUAGE FOR THE LABELS OF THE APPLICATION
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
		} catch (NumberFormatException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
		ResourceBundle labels = language.getLabels();

		// OPEN FILES WHICH BELONGS TO THE PROJECT
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
				if (isDefaultProject
						|| MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(j).isOpened()) {

					// ENABLE FILE AND EDIT MENUS
					MainWindow.getInstance().getMenu().enableFileMenu();
					MainWindow.getInstance().getMenu().enableEditMenu();

					// UPDATE THE STATUS BAR WITH THE NAME OF THE FILE
					MainWindow.getInstance().getStatusBar().setMessage(
							MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(j).getPath());

					// CHECK IF IT IS A MAIN OR COMPILABLE FILE
					int fileType = 0;

					// MAIN
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(j).isMainFile()) {

						fileType = 1;

						// ADD THE <MAIN> TAG TO THE MESSAGE IN THE STATUS BAR
						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(j)
										.getPath()
										+ " <MAIN>");
					}

					// COMPILABLE
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(j).isCompilableFile()) {

						fileType = 2;

						// ADD THE <COMPILABLE> TAG TO THE MESSAGE IN THE STATUS
						// BAR
						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(j)
										.getPath()
										+ " <COMPILABLE>");
					}

					// OPEN THE NEW TAB IN THE EDITOR
					MainWindow.getInstance().getEditorBuilder().newTab(
							fileName, filePath, text, true, fileType);

					// CHECK IF IT IS MARKED AS A MAIN OR COMPILABLE FILE
					for (int i = 0; i < MainWindow.getInstance()
							.getEditorBuilder().getNumEditors(); i++) {

						if (MainWindow.getInstance().getEditorBuilder()
								.getEditorAt(i).getAbsolutePath().equals(
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(j).getPath())) {

							// IS COMPILABLE FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(j)
									.isCompilableFile())
								MainWindow.getInstance().getEditorBuilder()
										.getEditorAt(i).setCompilerFile(true);

							// IS MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(j)
									.isMainFile())
								MainWindow.getInstance().getEditorBuilder()
										.getEditorAt(i).setMainFile(true);
						}
					}

					// CONFIGURES THE UNDO REDO MANAGER OF THE APPLICATION
					int selectedEditorIndex = MainWindow.getInstance()
							.getEditorBuilder().getSelectedEditorIndex();
					DefaultStyledDocument doc = MainWindow.getInstance()
							.getEditorBuilder().getSelectedEditor().getSyntaxDocument();

					doc.addUndoableEditListener(new UndoableEditListener() {
						/*
						 * (non-Javadoc)
						 * 
						 * @see javax.swing.event.UndoableEditListener#
						 * undoableEditHappened
						 * (javax.swing.event.UndoableEditEvent)
						 */
						public void undoableEditHappened(UndoableEditEvent evt) {

							UndoableEdit edit = evt.getEdit();

							if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
									.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
								MainWindow.getInstance().getMenu().getEdit()
										.getUndoManager()
										.addEdit(evt.getEdit());
							}
						}
					});

					// SET THE CARET POSITION IN THE FIRST POSITION OF THE FILE
					// OPENED
					// IN THE EDITOR
					selectedEditorIndex = MainWindow.getInstance()
							.getEditorBuilder().getSelectedEditorIndex();
					MainWindow.getInstance().getEditorBuilder().getEditorAt(
							selectedEditorIndex).getEditor()
							.setCaretPosition(0);
				}
				
				MainWindow.getInstance().getProjectConfiguration().setIsModified(false);
			} else {

				// IF THE FILE DOESN'T EXISTS
				if (!file.exists()) {

					// DISPLAY A MESSAGE TO THE USER
					JOptionPane.showMessageDialog(null, labels
							.getString("s970")
							+ MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(j)
									.getPath() + labels.getString("s971"),
							"Error", JOptionPane.ERROR_MESSAGE);
					
					// REMOVE THE FILE FROM THE PROJECT
					MainWindow.getInstance().getProjectConfiguration().removeFileAt(j);
					MainWindow.getInstance().getProjectConfiguration().setIsModified(true);		
				}
			}
		}

		// UPDATES THE STATUS BAR WITH THE LEXICAL NAME
		MainWindow.getInstance().getStatusBar().setMessagelexical(
				labels.getString("s449") + " " + programmingLanguage.getName());

		// SET THE GRAMMAR CONFIGURATION OF THE APPLICATION
		String currentGrammar = null;
		String grammarName = null;
		try {

			// GET THE NAME
			currentGrammar = PropertiesManager.getProperty("currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			grammarName = currentGrammar.substring(index + 1, currentGrammar
					.length() - 4);

			// UPDATES THE STATUS BAR
			MainWindow.getInstance().getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + grammarName);
		} catch (Exception e) {
			
			JOptionPane.showMessageDialog(null, e.getMessage(), labels
					.getString("s945"), JOptionPane.ERROR_MESSAGE);
			Log.getLog().error(e.getMessage());
		}

		// LOAD THE OUTPUT SHELL OF THE APPLICATION
		PropertiesManager.setProperty("execPath", MainWindow.getInstance()
				.getProjectConfiguration().getShellDirectory());
		PropertiesManager.setProperty("exec", MainWindow.getInstance()
				.getProjectConfiguration().getShellPath());
		MainWindow.getInstance().getOutput().resetOutput();
		PropertiesManager.setProperty("exitCommand", MainWindow.getInstance()
				.getProjectConfiguration().getExitCommand());
		PropertiesManager.setProperty("echoCommand", MainWindow.getInstance()
				.getProjectConfiguration().getEchoCommand());

		try {

			// IF IT IS DEFAULT PROJECT
			if (PropertiesManager.getProperty("defaultAcideProject").equals(
					"./configuration/default.acidePrj")) {
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
				MainWindow.getInstance().getExplorer().getPopupMenu().getAddFile().setEnabled(true);
				MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile().setEnabled(true);

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
				MainWindow.getInstance().getExplorer().getRoot().add(
						defaultMutableTreeNode);
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

						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).getParent().equals(
										MainWindow.getInstance()
												.getProjectConfiguration()
												.getName())) {
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
				MainWindow.getInstance().getExplorer().getPopupMenu().getAddFile().setEnabled(true);
				MainWindow.getInstance().getExplorer().getPopupMenu().getSaveProject().setEnabled(true);

				// IF IT HAS MORE THAN 0 FILES ASSOCIATED
				if (MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList() > 0)

					// ALLOW THE REMOVE FILE OPTION FOR THE EXPLORER
					MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile().setEnabled(true);
				else
					MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile().setEnabled(false);

				// STORE THE DEFAULT CONFIGURATION
				MainWindow.getInstance().getProjectConfiguration()
						.setFirstSave(true);
				MainWindow.getInstance().getProjectConfiguration().setPath(
						PropertiesManager.getProperty("defaultAcideProject"));
			}
		} catch (NumberFormatException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}

		// WE SET THE DEFAULT CURSOR ONCE ALL THE COMPONENTS OF THE
		// CONFIGURATION ARE SET
		cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
		MainWindow.getInstance().setCursor(cursor);
	}
}
