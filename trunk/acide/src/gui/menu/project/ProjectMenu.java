package gui.menu.project;

import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.menu.MenuConfiguration;
import es.configuration.toolBar.EditableToolBarCommandList;
import es.explorer.ExplorerFile;
import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.MainWindow;
import gui.menu.configuration.menu.MenuGUI;
import gui.menu.configuration.toolBar.ToolBarCommandGUI;
import gui.toolBarButton.ToolBarCommand;

import java.awt.Cursor;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.undo.UndoableEdit;

import org.apache.log4j.Logger;

import operations.factory.GUIFactory;
import operations.factory.IOFactory;
import operations.log.Log;

import language.Language;
import properties.PropertiesManager;

/**
 * 
 */
public class ProjectMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String NEW_PROJECT = "./resources/icons/menu/project/newProject.png";
	/**
	 * 
	 */
	private static final String OPEN_PROJECT = "./resources/icons/menu/project/openProject.png";
	/**
	 * 
	 */
	private static final String SAVE_PROJECT = "./resources/icons/menu/project/saveProject.png";
	/**
	 * 
	 */
	private static final String NEW_FILE = "./resources/icons/menu/project/newFile.png";
	/**
	 * 
	 */
	private static final String ADD_FILE = "./resources/icons/menu/project/addFile.png";
	/**
	 * 
	 */
	private static final String ADD_FOLDER = "./resources/icons/menu/project/addFolder.png";
	/**
	 * 
	 */
	private static final String DELETE_FILE = "./resources/icons/menu/project/deleteFile.png";
	/**
	 * 
	 */
	private static final String COMPILE = "./resources/icons/menu/project/compile.png";
	/**
	 * 
	 */
	private static final String EXECUTE = "./resources/icons/menu/project/execute.png";
	/**
	 * 
	 */
	private static final String SET_MAIN = "./resources/icons/menu/project/setMain.png";
	/**
	 * 
	 */
	private static final String UNSET_MAIN = "./resources/icons/menu/project/unsetMain.png";
	/**
	 * 
	 */
	private static final String SET_COMPILABLE = "./resources/icons/menu/project/setCompilable.png";
	/**
	 * 
	 */
	private static final String UNSET_COMPILABLE = "./resources/icons/menu/project/unsetCompilable.png";
	/**
	 * 
	 */
	private JMenuItem _newProject;
	/**
	 * 
	 */
	private JMenuItem _openProject;
	/**
	 * 
	 */
	private JMenuItem _saveProject;
	/**
	 * 
	 */
	private JMenuItem _newProjectFile;
	/**
	 * 
	 */
	private JMenuItem _saveAsProject;
	/**
	 * 
	 */
	private JMenuItem _addFile;
	/**
	 * 
	 */
	private JMenuItem _closeProject;
	/**
	 * 
	 */
	private JMenuItem _removeFile;
	/**
	 * 
	 */
	private JMenuItem _deleteFile;
	/**
	 * 
	 */
	private JMenuItem _addFolder;
	/**
	 * 
	 */
	private JMenuItem _removeFolder;
	/**
	 * 
	 */
	private JMenuItem _setCompilable;
	/**
	 * 
	 */
	private JMenuItem _unsetCompilable;
	/**
	 * 
	 */
	private JMenuItem _setMain;
	/**
	 * 
	 */
	private JMenuItem _unsetMain;
	/**
	 * 
	 */
	private JMenuItem _compile;
	/**
	 * 
	 */
	private JMenuItem _execute;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public ProjectMenu() {

		// MENU ITEM
		_newProject = new JMenuItem(new ImageIcon(NEW_PROJECT));
		_openProject = new JMenuItem(new ImageIcon(OPEN_PROJECT));
		_saveProject = new JMenuItem(new ImageIcon(SAVE_PROJECT));
		_saveAsProject = new JMenuItem();
		_newProjectFile = new JMenuItem(new ImageIcon(NEW_FILE));
		_addFile = new JMenuItem(new ImageIcon(ADD_FILE));
		_removeFile = new JMenuItem();
		_deleteFile = new JMenuItem(new ImageIcon(DELETE_FILE));
		_closeProject = new JMenuItem();
		_compile = new JMenuItem(new ImageIcon(COMPILE));
		_execute = new JMenuItem(new ImageIcon(EXECUTE));
		_addFolder = new JMenuItem(new ImageIcon(ADD_FOLDER));
		_removeFolder = new JMenuItem();
		_setMain = new JMenuItem(new ImageIcon(SET_MAIN));
		_unsetMain = new JMenuItem(new ImageIcon(UNSET_MAIN));
		_setCompilable = new JMenuItem(new ImageIcon(SET_COMPILABLE));
		_unsetCompilable = new JMenuItem(new ImageIcon(UNSET_COMPILABLE));

		setLanguageLabels();
	}

	/**
	 * 
	 * @param labels
	 */
	public void setLanguageLabels() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		final ResourceBundle labels = language.getLabels();

		disableMenu();

		// NEW PROJECT
		_newProject.setText(labels.getString("s14"));
		_newProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// OPEN PROJECT
		_openProject.setText(labels.getString("s15"));
		_openProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// SAVE PROJECT
		_saveProject.setText(labels.getString("s16"));
		_saveProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// NEW PROJECT FILE
		_newProjectFile.setText(labels.getString("s947"));

		// ADD FILE
		_addFile.setText(labels.getString("s17"));
		_addFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));

		// REMOVE FILE
		_removeFile.setText(labels.getString("s218"));

		// DELETE FILE
		_deleteFile.setText(labels.getString("s950"));

		// ADD FOLDER
		_addFolder.setText(labels.getString("s219"));

		// REMOVE FOLDER
		_removeFolder.setText(labels.getString("s220"));

		// SAVE AS PROJECT
		_saveAsProject.setText(labels.getString("s926"));

		// CLOSE PROJECT
		_closeProject.setText(labels.getString("s228"));

		// SET COMPILABLE
		_setCompilable.setText(labels.getString("s254"));

		// UNSET COMPILABLE
		_unsetCompilable.setText(labels.getString("s255"));

		// SET MAIN
		_setMain.setText(labels.getString("s256"));

		// UNSET MAIN
		_unsetMain.setText(labels.getString("s952"));

		// COMPILE
		_compile.setText(labels.getString("s18"));
		_compile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.ALT_MASK));

		// EXECUTE
		_execute.setText(labels.getString("s19"));
		_execute.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.ALT_MASK));
	}

	/**
	 * 
	 */
	public void buildMenu() {

		removeAll();

		if (MenuConfiguration.getProject())
			add(_newProject);
		if (MenuConfiguration.getOpenProject())
			add(_openProject);
		if (MenuConfiguration.getCloseProject())
			add(_closeProject);
		if ((MenuConfiguration.getOpenProject()
				|| MenuConfiguration.getProject() || MenuConfiguration
				.getCloseProject())
				&& (MenuConfiguration.getSaveProject() || MenuConfiguration
						.getSaveAsProject()))
			addSeparator();
		if (MenuConfiguration.getSaveProject())
			add(_saveProject);
		if (MenuConfiguration.getSaveAsProject())
			add(_saveAsProject);
		if ((MenuConfiguration.getOpenProject()
				|| MenuConfiguration.getProject()
				|| MenuConfiguration.getSaveProject()
				|| MenuConfiguration.getCloseProject() || MenuConfiguration
				.getSaveAsProject())
				&& (MenuConfiguration.getAddFile()
						|| MenuConfiguration.getRemoveFile()
						|| MenuConfiguration.getAddFolder()
						|| MenuConfiguration.getRemoveFolder()
						|| MenuConfiguration.getDeleteFile() || MenuConfiguration
						.getNewProjectFile()))
			addSeparator();
		if (MenuConfiguration.getNewProjectFile())
			add(_newProjectFile);
		if (MenuConfiguration.getAddFile())
			add(_addFile);
		if (MenuConfiguration.getRemoveFile())
			add(_removeFile);
		if (MenuConfiguration.getDeleteFile())
			add(_deleteFile);
		if (MenuConfiguration.getAddFolder())
			add(_addFolder);
		if (MenuConfiguration.getRemoveFolder())
			add(_removeFolder);
		if ((MenuConfiguration.getOpenProject()
				|| MenuConfiguration.getProject()
				|| MenuConfiguration.getSaveProject()
				|| MenuConfiguration.getSaveAsProject()
				|| MenuConfiguration.getAddFile()
				|| MenuConfiguration.getRemoveFile()
				|| MenuConfiguration.getAddFolder()
				|| MenuConfiguration.getRemoveFolder()
				|| MenuConfiguration.getDeleteFile() || MenuConfiguration
				.getNewProjectFile())
				&& (MenuConfiguration.getCompile()
						|| MenuConfiguration.getExecute()
						|| MenuConfiguration.getSetFile()
						|| MenuConfiguration.getUnsetFile() || MenuConfiguration
						.getSetMain()))
			addSeparator();
		if (MenuConfiguration.getCompile())
			add(_compile);
		if (MenuConfiguration.getExecute())
			add(_execute);
		if ((MenuConfiguration.getOpenProject()
				|| MenuConfiguration.getProject()
				|| MenuConfiguration.getSaveProject()
				|| MenuConfiguration.getSaveAsProject()
				|| MenuConfiguration.getAddFile()
				|| MenuConfiguration.getRemoveFile()
				|| MenuConfiguration.getAddFolder()
				|| MenuConfiguration.getRemoveFolder()
				|| MenuConfiguration.getDeleteFile()
				|| MenuConfiguration.getNewProjectFile()
				|| MenuConfiguration.getCompile() || MenuConfiguration
				.getExecute())
				&& (MenuConfiguration.getSetFile()
						|| MenuConfiguration.getUnsetFile() || MenuConfiguration
						.getSetMain()))
			addSeparator();
		if (MenuConfiguration.getSetFile())
			add(_setCompilable);
		if (MenuConfiguration.getUnsetFile())
			add(_unsetCompilable);
		if (MenuConfiguration.getSetMain())
			add(_setMain);
		if (MenuConfiguration.getUnsetMain())
			add(_unsetMain);
	}

	/**
	 * 
	 */
	public void setListeners() {

		// NEW PROJECT
		_newProject.addActionListener(new NewProjectListener());

		// SAVE PROJECT
		_saveProject.addActionListener(new SaveProjectListener());

		// SAVE AS PROJECT
		_saveAsProject.addActionListener(new SaveAsProjectListener());

		// OPEN PROJECT
		_openProject.addActionListener(new OpenProjectListener());

		// ADD FILE
		_addFile.addActionListener(new AddFileListener());

		// EXECUTE
		_execute.addActionListener(new ExecuteListener());

		// COMPILE
		_compile.addActionListener(new CompileListener());

		// REMOVE FILE
		_removeFile.addActionListener(new RemoveFileListener());

		// CLOSE PROJECT
		_closeProject.addActionListener(new CloseProjectListener());

		// DELETE FILE
		_deleteFile.addActionListener(new DeleteFileListener());

		// NEW PROJECT FILE
		_newProjectFile.addActionListener(new NewProjectFileListener());

		// ADD FOLDER
		_addFolder.addActionListener(new AddFolderListener());

		// REMOVE FOLDER
		_removeFolder.addActionListener(new RemoveFolderListener());

		// SET MAIN
		_setMain.addActionListener(new SetMainListener());

		// UNSET MAIN
		_unsetMain.addActionListener(new UnsetMainListener());

		// SET COMPILABLE
		_setCompilable.addActionListener(new SetCompilableListener());

		// UNSET COMPILABLE
		_unsetCompilable.addActionListener(new UnsetCompilableListener());
	}

	/**
	 * 
	 */
	public void enableMenu() {

		_closeProject.setEnabled(true);
		_saveProject.setEnabled(false);
		_saveAsProject.setEnabled(true);
		_newProjectFile.setEnabled(true);
		_addFile.setEnabled(true);
		_removeFile.setEnabled(false);
		_deleteFile.setEnabled(false);
		_addFolder.setEnabled(true);
		_removeFolder.setEnabled(false);
		_compile.setEnabled(true);
		_execute.setEnabled(true);
		_setMain.setEnabled(false);
		_unsetMain.setEnabled(false);
		_setCompilable.setEnabled(false);
		_unsetCompilable.setEnabled(false);
	}

	/**
	 * 
	 */
	public void disableMenu() {

		_closeProject.setEnabled(false);
		_saveProject.setEnabled(false);
		_saveAsProject.setEnabled(false);
		_newProjectFile.setEnabled(false);
		_addFile.setEnabled(false);
		_removeFile.setEnabled(false);
		_deleteFile.setEnabled(false);
		_addFolder.setEnabled(false);
		_removeFolder.setEnabled(false);
		_compile.setEnabled(false);
		_execute.setEnabled(false);
		_setMain.setEnabled(false);
		_unsetMain.setEnabled(false);
		_setCompilable.setEnabled(false);
		_unsetCompilable.setEnabled(false);
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getNewProject() {
		return _newProject;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveProject() {
		return _saveProject;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getOpenProject() {
		return _openProject;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getNewProjectFile() {
		return _newProjectFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getAddFolder() {
		return _addFolder;
	}

	/**
	 * 
	 * @param addFolder
	 */
	public void setAddFolder(JMenuItem addFolder) {
		_addFolder = addFolder;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCompile() {
		return _compile;
	}

	/**
	 * 
	 * @param compile
	 */
	public void setCompile(JMenuItem compile) {
		_compile = compile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getExecute() {
		return _execute;
	}

	/**
	 * 
	 * @param execute
	 */
	public void setExecute(JMenuItem execute) {
		_execute = execute;
	}

	/**
	 * 
	 * @param removeFile
	 */
	public void setRemoveFile(JMenuItem removeFile) {
		_removeFile = removeFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getRemoveFolder() {
		return _removeFolder;
	}

	/**
	 * 
	 * @param removeFolder
	 */
	public void setRemoveFolder(JMenuItem removeFolder) {
		_removeFolder = removeFolder;
	}

	/**
	 * 
	 * @param openProject
	 */
	public void setOpenProject(JMenuItem openProject) {
		_openProject = openProject;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCloseProject() {
		return _closeProject;
	}

	/**
	 * 
	 * @param closeProject
	 */
	public void setCloseProject(JMenuItem closeProject) {
		_closeProject = closeProject;
	}

	/**
	 * 
	 * @param newProjectFile
	 */
	public void setNewProjectFile(JMenuItem newProjectFile) {
		_newProjectFile = newProjectFile;
	}

	/**
	 * 
	 * @param addFile
	 */
	public void setAddFile(JMenuItem addFile) {
		_addFile = addFile;
	}

	/**
	 * 
	 * @param saveProject
	 */
	public void setSaveProject(JMenuItem saveProject) {
		_saveProject = saveProject;
	}

	/**
	 * 
	 * @param newProject
	 */
	public void setNewProject(JMenuItem newProject) {
		_newProject = newProject;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilable;
	}

	/**
	 * 
	 * @param setFile
	 */
	public void setSetCompilable(JMenuItem setFile) {
		_setCompilable = setFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSetMain() {
		return _setMain;
	}

	/**
	 * 
	 * @param setMain
	 */
	public void setSetMain(JMenuItem setMain) {
		_setMain = setMain;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilable;
	}

	/**
	 * 
	 * @param unsetFile
	 */
	public void setUnsetCompilable(JMenuItem unsetFile) {
		_unsetCompilable = unsetFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveAsProject() {
		return _saveAsProject;
	}

	/**
	 * 
	 * @param saveAsProject
	 */
	public void setSaveAsProject(JMenuItem saveAsProject) {
		_saveAsProject = saveAsProject;
	}

	/**
	 * 
	 * @param deleteFile
	 */
	public void setDeleteFile(JMenuItem deleteFile) {
		_deleteFile = deleteFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getDeleteFile() {
		return _deleteFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMain;
	}

	/**
	 * 
	 * @param unsetMain
	 */
	public void setUnsetMain(JMenuItem unsetMain) {
		_unsetMain = unsetMain;
	}
	
	/**
	 * 
	 */
	class NewProjectListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			GUIFactory ioFactory = GUIFactory.getInstance();
			MainWindow mainWindow = MainWindow.getInstance();
			ResourceBundle labels = Language.getInstance().getLabels();
			boolean cancelSelected = false;

			// IF THE PROJECT IS MODIFIED
			if (mainWindow.getProjectConfiguration().isModified()) {

				// ASK TO THE USER IF HE WANTS TO SAVE IT
				int chosenOption = JOptionPane.showConfirmDialog(null, labels
						.getString("s657"), labels.getString("s953"),
						JOptionPane.YES_NO_CANCEL_OPTION);

				// CANCEL OPTION
				if (chosenOption == JOptionPane.CANCEL_OPTION)
					cancelSelected = true;

				// OK OPTION
				if (chosenOption == JOptionPane.OK_OPTION) {
					mainWindow.getMenu().getProject().getSaveProject().setEnabled(
							true);
					mainWindow.getMenu().getProject().getSaveProject().doClick();
				}
			}

			// SELECTED EDITOR
			int selectedEditor = mainWindow.getEditorBuilder()
					.getSelectedEditorIndex();
			int numEditors = mainWindow.getEditorBuilder().getNumEditors();
			mainWindow.getEditorBuilder().setSelectedEditorAt(numEditors - 1);
			for (int z = numEditors - 1; z >= 0; z--) {
				mainWindow.getEditorBuilder().setSelectedEditorAt(z);

				// IF THE FILE IS MODIFIED
				if (mainWindow.getEditorBuilder().isRedButton()) {

					// ASK TO THE USER IF HE WANTS TO SAVE IT
					int opt = JOptionPane.showConfirmDialog(null, labels
							.getString("s643"), labels.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					// OK OPTION
					if (opt == JOptionPane.OK_OPTION)
						mainWindow.getMenu().getFile().saveOrSaveAS();
				}
			}
			mainWindow.getEditorBuilder().setSelectedEditorAt(selectedEditor);

			// DISPLAYS THE GUI FOR SAVE THE PROJECT
			if (!cancelSelected)
				mainWindow.setProjectGUI(ioFactory.buildProjectGUI());
		}
	}

	/**
	 * 
	 */
	class SaveProjectListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			IOFactory ioFactory = IOFactory.getInstance();
			MainWindow mainWindow = MainWindow.getInstance();
			TextFile textFile = ioFactory.buildFile();

			try {

				// IF THE USER ALREADY CHOSE A PROJECT
				if (!mainWindow.getProjectConfiguration().getName().equals("")) {

					// IF THIS IS NOT THE FIRST TIME THAT THE PROJECT IS SAVED
					// THEN SAVE IT AS
					if (!mainWindow.getProjectConfiguration().isFirstSave()) {
						mainWindow.getMenu().getProject().getSaveAsProject()
								.setEnabled(true);
						mainWindow.getMenu().getProject().getSaveAsProject()
								.doClick();
					} else {

						// GET THE PROPERTIES FROM THE FILE
						String len = PropertiesManager.getProperty("language");
						String currentMenu = PropertiesManager
								.getProperty("currentMenuConfiguration");
						String currentToolBar = PropertiesManager
								.getProperty("currentToolBarConfiguration");
						String currentGrammar = PropertiesManager
								.getProperty("currentGrammar");
						String languagePath = PropertiesManager
								.getProperty("languagePath");
						String exec = PropertiesManager.getProperty("exec");
						String execPath = PropertiesManager.getProperty("execPath");

						// SAVE THE CONFIGURATION
						mainWindow.getProjectConfiguration().setLanguage(len);
						mainWindow.getProjectConfiguration().setMenu(currentMenu);
						mainWindow.getProjectConfiguration().setToolBar(
								currentToolBar);
						mainWindow.getProjectConfiguration()
								.setSyntacticConfiguration(currentGrammar);
						mainWindow.getProjectConfiguration()
								.setLexicalConfiguration(languagePath);
						mainWindow.getProjectConfiguration().setShellPath(exec);
						mainWindow.getProjectConfiguration().setShellDirectory(
								execPath);

						String fileContent = mainWindow.getProjectConfiguration()
								.save();
						textFile.save(mainWindow.getProjectConfiguration()
								.getProjectPath(), fileContent);
						mainWindow.getProjectConfiguration().setIsModified(false);
					}
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * Listener for the open project option.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class OpenProjectListener implements ActionListener {

		/**
		 * Search a file into a list of files.
		 * 
		 * @param list
		 *            List of files.
		 * @param fileName
		 *            File name to search for.
		 * 
		 * @return The node of the tree if exists.
		 */
		private DefaultMutableTreeNode searchDirectoryList(
				ArrayList<DefaultMutableTreeNode> list, String fileName) {

			int pos = 0;
			boolean found = false;

			while (pos < list.size() && !found) {

				DefaultMutableTreeNode temp = list.get(pos);
				ExplorerFile explorerFile = (ExplorerFile) temp.getUserObject();

				if (explorerFile.getName().equals(fileName)) {
					found = true;
					return (DefaultMutableTreeNode) list.get(pos);
				} else
					pos++;
			}
			return null;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			MainWindow mainWindow = MainWindow.getInstance();
			ResourceBundle labels = Language.getInstance().getLabels();
			TextFile textFile = IOFactory.getInstance().buildFile();
			Language language = Language.getInstance();

			boolean cancelOptionSelected = false;

			// SELECT THE EXTENSION FOR THE PROJECT
			String[] ExtPide = new String[] { "acidePrj" };
			textFile.getFileChooser().addChoosableFileFilter(
					new ExtensionFilter(ExtPide, labels.getString("s328")));
			final String file;
			file = textFile.read();

			// IF THE FILE CONTENT IS OK
			if (file != null) {

				// IF THE PROJECT IS MODIFIED
				if (mainWindow.getProjectConfiguration().isModified()) {

					// ASK TO THE USER IF HE WANTS TO SAVE IT
					int chosenOption = JOptionPane.showConfirmDialog(null, labels
							.getString("s657"), labels.getString("s953"),
							JOptionPane.YES_NO_CANCEL_OPTION);

					// CANCEL
					if (chosenOption == JOptionPane.CANCEL_OPTION)
						cancelOptionSelected = true;

					// OK
					if (chosenOption == JOptionPane.OK_OPTION) {
						mainWindow.getMenu().getProject().getSaveProject()
								.setEnabled(true);
						mainWindow.getMenu().getProject().getSaveProject()
								.doClick();
					}
				}

				// IF HE SAID OK
				if (!cancelOptionSelected) {

					// PUT THE WAIT CURSOR
					Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
					mainWindow.setCursor(cursor);
					mainWindow.getStatusBar().setMessage("");

					Thread thread = new Thread() {
						public void run() {
							load(file);
						}
					};
					thread.start();

					// LOAD THE LANGUAGE
					String configurationLanguage = mainWindow
							.getProjectConfiguration().getLanguage();

					// SPANISH
					if (configurationLanguage.equals("spanish"))
						mainWindow.getMenu().getConfiguration().getLanguage()
								.getSpanish().doClick();

					// ENGLISH
					if (configurationLanguage.equals("english"))
						mainWindow.getMenu().getConfiguration().getLanguage()
								.getEnglish().doClick();

					try {
						language.getLanguage(PropertiesManager
								.getProperty("language"));
					} catch (NumberFormatException e2) {
						e2.printStackTrace();
					} catch (Exception e2) {
						e2.printStackTrace();
					}

					// GET THE LABELS TO DISPLAY
					labels = language.getLabels();

					mainWindow.getStatusBar().setMessagelexical(
							labels.getString("s449") + " ");

					// LOAD MENU
					String currentMenu = null;
					try {
						currentMenu = mainWindow.getProjectConfiguration()
								.getMenu();
						boolean[] values = MenuConfiguration
								.loadMenuConfigurationFile(currentMenu);
						MenuConfiguration.setAll(values);
						PropertiesManager.setProperty("currentMenuConfiguration",
								currentMenu);
					} catch (Exception e2) {

						// GET THE NAME
						String currentMenu2;
						int index = currentMenu.lastIndexOf("\\");
						if (index == -1)
							index = currentMenu.lastIndexOf("/");
						currentMenu2 = ".\\configuration\\menu\\"
								+ currentMenu.substring(index + 1, currentMenu
										.length());
						
						boolean[] values = null;

						try {
							values = MenuConfiguration
									.loadMenuConfigurationFile(currentMenu2);
							MenuConfiguration.setAll(values);
							PropertiesManager.setProperty(
									"currentMenuConfiguration", currentMenu2);
							JOptionPane.showMessageDialog(null, labels
									.getString("s956")
									+ currentMenu
									+ labels.getString("s957")
									+ currentMenu2);
						} catch (Exception e1) {

							try {
								values = MenuConfiguration
										.loadMenuConfigurationFile("./configuration/menu/defaultAllOn.menuCfg");
							} catch (Exception e3) {
								e3.printStackTrace();
							}
							MenuConfiguration.setAll(values);
							PropertiesManager.setProperty(
									"currentMenuConfiguration",
									"./configuration/menu/defaultAllOn.menuCfg");
							JOptionPane.showMessageDialog(null, labels
									.getString("s956")
									+ currentMenu + labels.getString("s959"));
						}
					}

					mainWindow.getMenu().buildMenu();
					mainWindow.validate();
					mainWindow.repaint();
					mainWindow.getMenu().getConfiguration().getMenu().getSaveMenu()
							.setEnabled(true);
					MenuGUI.setChangesSaved(true);

					// LOAD GRAMMAR
					try {

						String currentGrammar = mainWindow
								.getProjectConfiguration()
								.getSyntacticConfiguration();

						// GET THE NAME OF THE GRAMMAR
						int index = currentGrammar.lastIndexOf("\\");
						if (index == -1)
							index = currentGrammar.lastIndexOf("/");
						String grammarName = currentGrammar.substring(index + 1,
								currentGrammar.length() - 4);
						
						// UPDATES THE STATUS BAR
						mainWindow.getStatusBar().setMessageGrammar(
								labels.getString("s248") + " " + grammarName);
						PropertiesManager.setProperty("currentGrammar",
								currentGrammar);
					} catch (Exception e1) {

						JOptionPane.showMessageDialog(null, e1.getMessage(), labels
								.getString("s944"), JOptionPane.ERROR_MESSAGE);
					}

					// LOAD SHELL
					PropertiesManager.setProperty("execPath", mainWindow
							.getProjectConfiguration().getShellDirectory());
					PropertiesManager.setProperty("exec", mainWindow
							.getProjectConfiguration().getShellPath());
					MainWindow.getInstance().getOutput().executeExitCommand();
					mainWindow.getOutput().resetOutput();

					// LOAD TOOLBAR
					String currentToolBarConfiguration = null;
					try {
						EditableToolBarCommandList.clear();
						currentToolBarConfiguration = mainWindow
								.getProjectConfiguration().getToolBar();
						EditableToolBarCommandList
								.loadList(currentToolBarConfiguration);
						PropertiesManager.setProperty(
								"currentToolBarConfiguration",
								currentToolBarConfiguration);
					} catch (Exception e2) {

						// GET THE NAME
						String currentToolBarConfiguration2;
						int index = currentToolBarConfiguration.lastIndexOf("\\");
						if (index == -1)
							index = currentToolBarConfiguration.lastIndexOf("/");
						currentToolBarConfiguration2 = ".\\configuration\\toolbar\\"
								+ currentToolBarConfiguration.substring(index + 1,
										currentToolBarConfiguration.length());
						try {
							EditableToolBarCommandList
									.loadList(currentToolBarConfiguration2);
							JOptionPane.showMessageDialog(null, labels
									.getString("s958")
									+ currentToolBarConfiguration
									+ labels.getString("s957")
									+ currentToolBarConfiguration2);
							PropertiesManager.setProperty(
									"currentToolBarConfiguration",
									currentToolBarConfiguration2);
						} catch (Exception e1) {

							try {
								EditableToolBarCommandList
										.loadList("./configuration/toolbar/default.BHcfg");
							} catch (Exception e3) {
								e3.printStackTrace();
							}
							JOptionPane.showMessageDialog(null, labels
									.getString("s958")
									+ currentToolBarConfiguration
									+ labels.getString("s959"));
							PropertiesManager.setProperty(
									"currentToolBarConfiguration",
									"./configuration/toolbar/default.BHcfg");
						}
					}
					ToolBarCommand.buildToolBar();
					ToolBarCommand.buildEditableToolBar();
					mainWindow.validate();
					mainWindow.repaint();
					mainWindow.getMenu().getConfiguration().getToolBar()
							.getSaveToolBar().setEnabled(true);
					ToolBarCommandGUI.setAreChangesSaved(true);

					// LOAD EXPLORER
					mainWindow.getExplorer().getRoot().removeAllChildren();
					ExplorerFile explorerFile = new ExplorerFile();
					explorerFile.setPath(mainWindow.getProjectConfiguration()
							.getName());
					explorerFile.setName(mainWindow.getProjectConfiguration()
							.getName());
					explorerFile.setIsDirectory(true);
					explorerFile.setParent(null);
					mainWindow.setTitle(labels.getString("s425") + " - "
							+ mainWindow.getProjectConfiguration().getName());
					DefaultMutableTreeNode d = new DefaultMutableTreeNode(
							explorerFile);
					d.setAllowsChildren(true);
					mainWindow.getExplorer().getRoot().add(d);
					ArrayList<DefaultMutableTreeNode> listdir = new ArrayList<DefaultMutableTreeNode>();
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {

						DefaultMutableTreeNode h = new DefaultMutableTreeNode(
								mainWindow.getProjectConfiguration().getFileAt(i));
						if (mainWindow.getProjectConfiguration().getFileAt(i)
								.isDirectory()) {
							h.setAllowsChildren(true);
							listdir.add(h);
						} else
							h.setAllowsChildren(false);

						if (mainWindow.getProjectConfiguration().getFileAt(i)
								.getParent().equals(
										mainWindow.getProjectConfiguration()
												.getName())) {
							d.add(h);
						} else {
							DefaultMutableTreeNode fh = searchDirectoryList(
									listdir, mainWindow.getProjectConfiguration()
											.getFileAt(i).getParent());

							fh.add(h);
						}
					}

					mainWindow.getExplorer().getTreeModel().reload();
					mainWindow.getExplorer().expandTree();
					if (mainWindow.getProjectConfiguration().getNumFilesFromList() > 0) {
						mainWindow.getExplorer().getPopupMenu().getRemoveFile()
								.setEnabled(true);
						mainWindow.getExplorer().getPopupMenu().getDeleteFile()
								.setEnabled(true);
					} else {
						mainWindow.getExplorer().getPopupMenu().getRemoveFile()
								.setEnabled(false);
						mainWindow.getExplorer().getPopupMenu().getDeleteFile()
								.setEnabled(false);
					}
					mainWindow.validate();
					mainWindow.repaint();

					if (!mainWindow.getMenu().getView().getShowBrowser()
							.isSelected())
						mainWindow.getExplorer().showExplorer();

					mainWindow.getMenu().getView().getShowBrowser().setSelected(
							true);
					mainWindow.getProjectConfiguration().setIsModified(false);
					mainWindow.getProjectConfiguration().setFirstSave(true);
					mainWindow.getMenu().enableProjectMenu();

					// SET THE DEFAULT CURSOR AGAIN
					cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
					mainWindow.setCursor(cursor);
				}
			}
		}

		/**
		 * 
		 * @param file
		 */
		public void load(String file) {

			String fileContent = null;

			IOFactory ioFactory = IOFactory.getInstance();
			MainWindow mainWindow = MainWindow.getInstance();
			ResourceBundle labels = Language.getInstance().getLabels();
			TextFile textFile = ioFactory.buildFile();
			Language id = Language.getInstance();

			// SET THE WAIT CURSOR
			Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
			mainWindow.setCursor(cursor);

			// SAVE PREVIOUS WINDOW AND PANELS
			mainWindow.getProjectConfiguration().saveMainWindowParameters();

			// SAVE ASOCIATED FILES TO THE PROJECT
			int selectedEditor = mainWindow.getEditorBuilder()
					.getSelectedEditorIndex();
			int numEditors = mainWindow.getEditorBuilder().getNumEditors();
			mainWindow.getEditorBuilder().setSelectedEditorAt(numEditors - 1);
			for (int z = numEditors - 1; z >= 0; z--) {
				mainWindow.getEditorBuilder().setSelectedEditorAt(z);
				if (mainWindow.getEditorBuilder().isRedButton() == true) {
					int opt = JOptionPane.showConfirmDialog(null, labels
							.getString("s643"), labels.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					if (opt == JOptionPane.OK_OPTION) {
						mainWindow.getMenu().getFile().saveOrSaveAS();
					}
				}
			}
			mainWindow.getEditorBuilder().setSelectedEditorAt(selectedEditor);

			// CLOSE EDITORS
			for (int i = 0; i < numEditors; i++) {
				mainWindow.getEditorBuilder().setSelectedEditorAt(0);
				mainWindow.getEditorBuilder().getPane().remove(0);
				mainWindow.getEditorBuilder().getPane().validate();
			}

			fileContent = textFile.load(file);

			PropertiesManager.setProperty("defaultAcideProject", file);
			mainWindow.getProjectConfiguration().setPath(file);
			mainWindow.getProjectConfiguration().removeFiles();
			mainWindow.getProjectConfiguration().load(fileContent);

			// IS EXPLORER SHOWED?
			if (!mainWindow.getProjectConfiguration().isExplorerShowed())
				mainWindow.getMenu().getView().getShowBrowser().doClick();

			// IS SHELL SHOWED?
			if (!mainWindow.getProjectConfiguration().isShellShowed())
				mainWindow.getMenu().getView().getShowShellWindow().doClick();

			// WINDOW PARAMETERS
			mainWindow.setSize(mainWindow.getProjectConfiguration()
					.getWindowWidth(), mainWindow.getProjectConfiguration()
					.getWindowHeight());
			mainWindow.setLocation(mainWindow.getProjectConfiguration().getPosX(),
					mainWindow.getProjectConfiguration().getPosY());

			// LOCATION OF THE SPLIT PANE
			mainWindow.getSplitPaneVertical().setDividerLocation(
					mainWindow.getProjectConfiguration()
							.getSplitPaneVerticalDividerLocation());
			mainWindow.getSplitPaneHorizontal().setDividerLocation(
					mainWindow.getProjectConfiguration()
							.getSplitPanelHorizontalDividerLocation());

			mainWindow.validate();
			mainWindow.repaint();
			mainWindow.setVisible(true);

			// OPEN FILES
			for (int j = 0; j < mainWindow.getProjectConfiguration()
					.getNumFilesFromList(); j++) {

				// IF IS A FILE
				if (!mainWindow.getProjectConfiguration().getFileAt(j)
						.isDirectory()) {

					TextFile ff = IOFactory.getInstance().buildFile();
					String text = ff.load(mainWindow.getProjectConfiguration()
							.getFileAt(j).getPath());

					String name2 = null;
					String file2 = mainWindow.getProjectConfiguration()
							.getFileAt(j).getPath();

					if (file2 != null) {

						// GET THE NAME
						int index = file2.lastIndexOf("\\");
						if (index == -1)
							index = file2.lastIndexOf("/");
						index++;
						name2 = file2.substring(index, file2.length());
					}

					if (mainWindow.getProjectConfiguration().getFileAt(j)
							.isOpened()) {

						mainWindow.getMenu().enableFileMenu();
						mainWindow.getMenu().enableEditMenu();

						// CHECK THE TYPE
						int type = 0;

						// UPDATE THE SATUS BAR
						mainWindow.getStatusBar().setMessage(
								mainWindow.getProjectConfiguration().getFileAt(j)
										.getPath());

						// COMPILABLE FILE
						if (mainWindow.getProjectConfiguration().getFileAt(j)
								.isCompilableFile()) {
							type = 2;

							mainWindow.getStatusBar().setMessage(
									mainWindow.getProjectConfiguration().getFileAt(
											j).getPath()
											+ " <COMPILABLE>");
						}

						// MAIN FILE
						if (mainWindow.getProjectConfiguration().getFileAt(j)
								.isMainFile()) {
							type = 1;

							mainWindow.getStatusBar().setMessage(
									mainWindow.getProjectConfiguration().getFileAt(
											j).getPath()
											+ " <MAIN>");
						}

						mainWindow.getEditorBuilder().newTab(name2, file2, text,
								true, type);
						mainWindow.validate();
						mainWindow.repaint();

						// UNDO REDO MANAGER
						int numEditor = mainWindow.getEditorBuilder()
								.getSelectedEditorIndex();
						DefaultStyledDocument doc = mainWindow.getEditorBuilder()
								.getSelectedEditor().getSyntaxDocument();

						doc.addUndoableEditListener(new UndoableEditListener() {
							/*
							 * (non-Javadoc)
							 * 
							 * @seejavax.swing.event.UndoableEditListener#
							 * undoableEditHappened
							 * (javax.swing.event.UndoableEditEvent)
							 */
							public void undoableEditHappened(UndoableEditEvent evt) {

								MainWindow mainWindow = MainWindow.getInstance();
								UndoableEdit edit = evt.getEdit();
								if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
										.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
									mainWindow.getMenu().getEdit().getUndoManager()
											.addEdit(evt.getEdit());
								}
							}
						});

						// CARET IN THE FIRST POSITION OF THE EDITOR
						numEditor = mainWindow.getEditorBuilder()
								.getSelectedEditorIndex();
						mainWindow.getEditorBuilder().getEditorAt(numEditor)
								.getEditor().setCaretPosition(0);
					}
				}
			}

			// LOAD LEXICAL
			PropertiesManager.setProperty("languagePath", mainWindow
					.getProjectConfiguration().getLexicalConfiguration());

			LexiconConfiguration lexical = LexiconConfiguration.getInstance();
			lexical.load(mainWindow.getProjectConfiguration()
					.getLexicalConfiguration());

			// RESET ALL THE OPENED FILES WITH THE NEW LEXICAL CONFIGURATION
			int numEditors2 = MainWindow.getInstance().getEditorBuilder()
					.getNumEditors();
			for (int i = 0; i < numEditors2; i++)
				MainWindow.getInstance().getEditorBuilder().getEditorAt(i)
						.resetDocument();

			// LOAD LANGUAGE
			String language = mainWindow.getProjectConfiguration().getLanguage();

			// SPANISH
			if (language.equals("spanish"))
				mainWindow.getMenu().getConfiguration().getLanguage().getSpanish()
						.doClick();
			// ENGLISH
			if (language.equals("english"))
				mainWindow.getMenu().getConfiguration().getLanguage().getEnglish()
						.doClick();

			try {
				id.getLanguage(PropertiesManager.getProperty("language"));
			} catch (NumberFormatException e2) {
				e2.printStackTrace();
			} catch (Exception e2) {
				e2.printStackTrace();
			}

			// GET THE LABELS

			labels = id.getLabels();
			mainWindow.getStatusBar().setMessagelexical(
					labels.getString("s449") + " " + lexical.getName());

			// SET THE DEFAULT CURSOR AGAIN
			cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
			mainWindow.setCursor(cursor);
		}
	}

	/**
	 * 
	 */
	class AddFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {

			// GET THE MAIN WINDO
			MainWindow mainWindow = MainWindow.getInstance();

			// BUILD THE TEXT FILE
			TextFile textFile = IOFactory.getInstance().buildFile();

			try {

				// BUILD THE CONTENT
				String fileContent = "";
				fileContent = textFile.read();

				if (fileContent != null) {

					TreePath path = mainWindow.getExplorer().getTree()
							.getSelectionPath();
					DefaultMutableTreeNode filePath;
					ExplorerFile fc;

					if (path != null) {
						filePath = (DefaultMutableTreeNode) path
								.getLastPathComponent();
						fc = (ExplorerFile) filePath.getUserObject();

						if (!fc.isDirectory()) {
							filePath = mainWindow.getExplorer().getRoot()
									.getNextNode();
							fc = (ExplorerFile) filePath.getUserObject();
						}

					} else {
						filePath = mainWindow.getExplorer().getRoot().getNextNode();
						fc = (ExplorerFile) filePath.getUserObject();
					}

					// GET THE NAME
					String name = "";
					int index = fileContent.lastIndexOf("\\");
					if (index == -1)
						index = fileContent.lastIndexOf("/");
					index++;
					name = fileContent.substring(index, fileContent.length());

					ExplorerFile explorerFile = new ExplorerFile();
					explorerFile.setPath(fileContent);
					explorerFile.setName(name);
					explorerFile.setParent(fc.getName());

					boolean isAdded = false;
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration().getFileAt(i)
								.getPath().equals(explorerFile.getPath())) {
							isAdded = true;
						}
					}

					if (!isAdded) {

						mainWindow.getProjectConfiguration().addFile(explorerFile);
						mainWindow.getProjectConfiguration().getFileAt(
								mainWindow.getProjectConfiguration()
										.getNumFilesFromList() - 1).setIsOpened(
								true);

						DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
								explorerFile);
						defaultMutableTreeNode.setAllowsChildren(false);
						filePath.add(defaultMutableTreeNode);
						mainWindow.validate();
						mainWindow.repaint();
						mainWindow.getExplorer().getTreeModel().reload();
						mainWindow.getExplorer().expandTree();
						mainWindow.getExplorer().getPopupMenu().getRemoveFile()
								.setEnabled(true);
						mainWindow.getExplorer().getPopupMenu().getDeleteFile()
								.setEnabled(true);
						mainWindow.getProjectConfiguration().setIsModified(true);
					}

					// OPEN FILE IN THE EDITOR
					boolean isOpened = false;
					for (int i = 0; i < mainWindow.getEditorBuilder()
							.getNumEditors(); i++) {
						if (mainWindow.getEditorBuilder().getEditorAt(i).getAbsolutePath()
								.equals(explorerFile.getPath())) {
							isOpened = true;
						}
					}

					if (!isOpened) {

						TextFile fd = new TextFile();
						int type = 0;

						// UPDATES THE STATUS BAR
						MainWindow.getInstance().getStatusBar().setMessage(
								explorerFile.getPath());

						// OPEN A NEW TAB IN THE EDITOR
						MainWindow.getInstance().getEditorBuilder().newTab(
								explorerFile.getPath(), explorerFile.getPath(),
								fd.load(explorerFile.getPath()), true, type);

						// UNDO REDO
						mainWindow.getMenu().enableFileMenu();
						mainWindow.getMenu().enableEditMenu();
						int selectedEditorIndex = mainWindow.getEditorBuilder()
								.getSelectedEditorIndex();
						Document doc = mainWindow.getEditorBuilder().getEditorAt(
								selectedEditorIndex).getEditor().getDocument();
						doc.addUndoableEditListener(new UndoableEditListener() {
							/*
							 * (non-Javadoc)
							 * 
							 * @seejavax.swing.event.UndoableEditListener#
							 * undoableEditHappened
							 * (javax.swing.event.UndoableEditEvent)
							 */
							public void undoableEditHappened(UndoableEditEvent evt) {

								MainWindow mainWindow = MainWindow.getInstance();
								UndoableEdit edit = evt.getEdit();

								if (edit instanceof DefaultDocumentEvent
										&& ((DefaultDocumentEvent) edit).getType() == DefaultDocumentEvent.EventType.CHANGE) {
									return;
								} else {
									mainWindow.getMenu().getEdit().getUndoManager()
											.addEdit(evt.getEdit());
								}
							}
						});

						// CARET IN THE FIRST POSITION OF THE TEXT
						selectedEditorIndex = mainWindow.getEditorBuilder()
								.getSelectedEditorIndex();
						mainWindow.getEditorBuilder().getEditorAt(
								selectedEditorIndex).getEditor()
								.setCaretPosition(0);
					}
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * 
	 */
	class CloseProjectListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {

			MainWindow mainWindow = MainWindow.getInstance();
			Language language = Language.getInstance();
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e) {
				e.printStackTrace();
			}

			ResourceBundle labels = language.getLabels();
			boolean cancelSelected = false;

			if (mainWindow.getProjectConfiguration().isModified()) {
				int chosenOption = JOptionPane.showConfirmDialog(null, labels
						.getString("s657"), labels.getString("s953"),
						JOptionPane.YES_NO_CANCEL_OPTION);
				if (chosenOption == JOptionPane.CANCEL_OPTION) {
					cancelSelected = true;
				}
				if (chosenOption == JOptionPane.OK_OPTION) {
					mainWindow.getMenu().getProject().getSaveProject().setEnabled(
							true);
					mainWindow.getMenu().getProject().getSaveProject().doClick();
				}
			}
			if (!cancelSelected) {
				mainWindow.getExplorer().getRoot().removeAllChildren();
				mainWindow.getExplorer().getTreeModel().reload();
				mainWindow.getExplorer().getPopupMenu().getAddFile().setEnabled(
						false);
				mainWindow.getExplorer().getPopupMenu().getSaveProject()
						.setEnabled(false);
				mainWindow.getExplorer().getPopupMenu().getRemoveFile().setEnabled(
						false);
				mainWindow.getExplorer().getPopupMenu().getDeleteFile().setEnabled(
						false);
				mainWindow.getProjectConfiguration().saveMainWindowParameters();
				mainWindow.setTitle(labels.getString("s425") + " - <empty>");
				mainWindow.getProjectConfiguration().removeFiles();
				mainWindow.validate();
				mainWindow.repaint();
				PropertiesManager.setProperty("defaultAcideProject",
						"./configuration/default.acidePrj");
				mainWindow.getProjectConfiguration().setName("");
				mainWindow.getProjectConfiguration().setIsModified(false);
				mainWindow.getMenu().getFile().getCloseAllFiles().setEnabled(true);
				mainWindow.getMenu().getFile().getCloseAllFiles().doClick();
				mainWindow.getMenu().deshabilitaMenuProyecto();
				mainWindow.getStatusBar().setMessage("");
			}
		}
	}

	/**
	 * 
	 */
	class ExecuteListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			GUIFactory.getInstance().buildExecutionGUI();
		}
	}

	/**
	 * 
	 */
	class CompileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {

			MainWindow mainWindow = MainWindow.getInstance();
			Runtime runTime = Runtime.getRuntime();
			mainWindow.closeDefaultProject();

			try {
				if (mainWindow.getProjectConfiguration().isCheckCompiler() == true) {

					String fileToCompile = "";

					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration().getFileAt(i)
								.isCompilableFile())
							fileToCompile = fileToCompile
									+ "\""
									+ mainWindow.getProjectConfiguration()
											.getFileAt(i).getPath()
									+ "\""
									+ mainWindow.getProjectConfiguration()
											.getSeparatorFile();
					}
					if (fileToCompile.length() > 0) {
						fileToCompile = fileToCompile.substring(0, fileToCompile
								.length() - 1);
						System.out.println(mainWindow.getProjectConfiguration()
								.getCompilerPath()
								+ " "
								+ mainWindow.getProjectConfiguration()
										.getCompilerArguments()
								+ " "
								+ fileToCompile);
						if (mainWindow.getProjectConfiguration().getCompilerPath() != null)
							runTime.exec(mainWindow.getProjectConfiguration()
									.getCompilerPath()
									+ " "
									+ mainWindow.getProjectConfiguration()
											.getCompilerArguments()
									+ " "
									+ fileToCompile);
					}
				} else {
					String extension = mainWindow.getProjectConfiguration()
							.getFileExtension();
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						ExplorerFile file = mainWindow.getProjectConfiguration()
								.getFileAt(i);
						if (file.isDirectory() == false) {
							String name = file.getPath();
							String ext = name.substring(name.lastIndexOf(".") + 1,
									name.length());
							if (ext.equals(extension)) {
								if (mainWindow.getProjectConfiguration()
										.getCompilerPath() != null) {
									runTime.exec(mainWindow
											.getProjectConfiguration()
											.getCompilerPath()
											+ " "
											+ mainWindow.getProjectConfiguration()
													.getCompilerArguments()
											+ " \""
											+ name + "\"");

								}
							}
						}
					}
				}
			} catch (IOException e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage());
			}
		}
	}

	/**
	 * 
	 */
	class RemoveFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {

			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e) {
				e.printStackTrace();
			}
			ResourceBundle labels = language.getLabels();

			MainWindow mainWindow = MainWindow.getInstance();

			int chosenOption = JOptionPane.showConfirmDialog(null, labels
					.getString("s623"));

			if (chosenOption == JOptionPane.OK_OPTION) {

				Toolkit toolkit = Toolkit.getDefaultToolkit();
				TreePath currentSelection = mainWindow.getExplorer().getTree()
						.getSelectionPath();

				if (currentSelection != null) {
					DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
							.getLastPathComponent());

					ExplorerFile p = (ExplorerFile) currentNode.getUserObject();
					if (!p.isDirectory()) {
						MutableTreeNode parent = (MutableTreeNode) (currentNode
								.getParent());
						if (parent != null) {
							mainWindow.getExplorer().getTreeModel()
									.removeNodeFromParent(currentNode);
							toolkit.beep();
							int cont = -1;
							for (int j = 0; j < mainWindow
									.getProjectConfiguration()
									.getNumFilesFromList(); j++) {
								if (mainWindow.getProjectConfiguration().getFileAt(
										j).getPath().equals(p.getPath())) {
									System.out.println(mainWindow
											.getProjectConfiguration().getFileAt(j)
											.getPath());
									cont = j;
								}
							}

							mainWindow.getProjectConfiguration().removeFileAt(cont);
							mainWindow.getStatusBar().setMessage("");
							int editor = -1;
							for (int z = 0; z < mainWindow.getEditorBuilder()
									.getNumEditors(); z++) {
								if (mainWindow.getEditorBuilder().getEditorAt(z)
										.getAbsolutePath().equals(p.getPath()))
									editor = z;
							}
							if (editor != -1) {

								// Ask for saving the file
								if (mainWindow.getEditorBuilder().isRedButton(
										editor)) {

									int opt = JOptionPane.showConfirmDialog(null,
											labels.getString("s643"), labels
													.getString("s953"),
											JOptionPane.YES_NO_OPTION);

									if (opt == JOptionPane.OK_OPTION) {

										IOFactory fact = IOFactory.getInstance();
										TextFile f2 = fact.buildFile();

										boolean result = f2.save(mainWindow
												.getEditorBuilder().getEditorAt(
														editor).getAbsolutePath(),
												mainWindow.getEditorBuilder()
														.getEditorAt(editor)
														.getText());
										if (result) {
											MainWindow.getInstance()
													.getEditorBuilder()
													.setGreenButtonAt(editor);
										}
									}
								}
							}

							// Close
							mainWindow.getEditorBuilder().getPane().remove(editor);

							if (mainWindow.getEditorBuilder().getPane()
									.getTabCount() == 0) {
								mainWindow.getMenu().disableFileMenu();
								mainWindow.getMenu().disableEditMenu();
							}

						}

						mainWindow.getProjectConfiguration().setIsModified(true);
						return;
					}
					toolkit.beep();
				}
			}

			if (mainWindow.getProjectConfiguration().getNumFilesFromList() > 0) {
				mainWindow.getExplorer().getPopupMenu().getRemoveFile().setEnabled(
						true);
				mainWindow.getExplorer().getPopupMenu().getDeleteFile().setEnabled(
						true);
			} else {
				mainWindow.getExplorer().getPopupMenu().getRemoveFile().setEnabled(
						false);
				mainWindow.getExplorer().getPopupMenu().getDeleteFile().setEnabled(
						false);
			}
		}
	}

	/**
	 * 
	 */
	class DeleteFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {

			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e) {
				e.printStackTrace();
			}

			ResourceBundle labels = language.getLabels();
			MainWindow mainWindow = MainWindow.getInstance();

			int chosenOption = JOptionPane.showConfirmDialog(null, labels
					.getString("s951"));
			if (chosenOption == JOptionPane.OK_OPTION) {

				Toolkit toolkit = Toolkit.getDefaultToolkit();
				TreePath currentSelection = mainWindow.getExplorer().getTree()
						.getSelectionPath();

				if (currentSelection != null) {
					DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
							.getLastPathComponent());

					ExplorerFile p = (ExplorerFile) currentNode.getUserObject();
					if (!p.isDirectory()) {
						MutableTreeNode parent = (MutableTreeNode) (currentNode
								.getParent());
						if (parent != null) {
							mainWindow.getExplorer().getTreeModel()
									.removeNodeFromParent(currentNode);
							toolkit.beep();
							int cont = -1;
							for (int j = 0; j < mainWindow
									.getProjectConfiguration()
									.getNumFilesFromList(); j++) {
								if (mainWindow.getProjectConfiguration().getFileAt(
										j).getPath().equals(p.getPath())) {
									System.out.println(mainWindow
											.getProjectConfiguration().getFileAt(j)
											.getPath());
									cont = j;
								}
							}

							ExplorerFile f = mainWindow.getProjectConfiguration()
									.getFileAt(cont);
							String fileRemove = f.getPath();
							mainWindow.getProjectConfiguration().removeFileAt(cont);

							System.out.println(fileRemove);
							File fi = new File(fileRemove);
							fi.delete();

							mainWindow.getStatusBar().setMessage("");
							mainWindow.getProjectConfiguration()
									.setIsModified(true);

							return;
						}
						toolkit.beep();
					}
				}

				if (mainWindow.getProjectConfiguration().getNumFilesFromList() > 0) {
					mainWindow.getExplorer().getPopupMenu().getRemoveFile()
							.setEnabled(true);
					mainWindow.getExplorer().getPopupMenu().getDeleteFile()
							.setEnabled(true);
				} else {
					mainWindow.getExplorer().getPopupMenu().getRemoveFile()
							.setEnabled(false);
					mainWindow.getExplorer().getPopupMenu().getDeleteFile()
							.setEnabled(false);
				}
			}
		}
	}

	/**
	 * 
	 */
	class NewProjectFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			int selectedEditorIndex = MainWindow.getInstance().getEditorBuilder()
					.getSelectedEditorIndex();

			// CREATES THE FILE
			MainWindow.getInstance().getMenu().getFile().getNewFile().doClick();

			// GET THE LANGUAGE
			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// GET THE LABELS
			ResourceBundle labels = language.getLabels();

			TextFile textFile = IOFactory.getInstance().buildFile();
			String textContent = " ";

			if (MainWindow.getInstance().getEditorBuilder().getNumEditors() == 0) {
				_logger.info(labels.getString("s89"));

			} else {

				MainWindow.getInstance().getMenu().setIsNPF(true);
				textContent = textFile.write();
				MainWindow.getInstance().getMenu().setIsNPF(false);

				if (textContent.equals(" ")) {
					MainWindow.getInstance().getEditorBuilder()
							.setSelectedEditorAt(selectedEditorIndex);
					_logger.info(labels.getString("s92"));
				} else {

					boolean savingResult = textFile.save(textContent, MainWindow
							.getInstance().getEditorBuilder().getSelectedEditor()
							.getText());

					// IF IT COULD SAVE IT
					if (savingResult) {
						_logger.info(labels.getString("s93") + textContent
								+ labels.getString("s94"));

						// SET THE GREEN BUTTON TO THE EDITOR
						MainWindow.getInstance().getEditorBuilder()
								.setGreenButton();

						// GET THE NAME
						int index = textContent.lastIndexOf("\\");
						if (index == -1)
							index = textContent.lastIndexOf("/");
						index++;
						String name = textContent.substring(index, textContent
								.length());

						// SET THE TITLE
						MainWindow
								.getInstance()
								.getEditorBuilder()
								.getPane()
								.setTitleAt(
										MainWindow.getInstance().getEditorBuilder()
												.getPane().getSelectedIndex(), name);

						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setAbsolutePath(textContent);
						MainWindow.getInstance().getEditorBuilder().getPane()
								.setToolTipText(textContent);

						File fich = new File(MainWindow.getInstance()
								.getEditorBuilder().getSelectedEditor().getAbsolutePath());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastChange(
										fich.lastModified());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastChange(fich.length());
					} else {
						_logger.info(labels.getString("s95") + textContent);
					}
				}
			}

			// ADD THE FILE TO THE PROJECT
			try {

				String file = textContent;

				if (file != null && file != " ") {

					TreePath path = MainWindow.getInstance().getExplorer()
							.getTree().getSelectionPath();
					DefaultMutableTreeNode filePath;
					ExplorerFile fc;

					// FOLDER SELECTED
					if (path != null) {

						filePath = (DefaultMutableTreeNode) path
								.getLastPathComponent();
						fc = (ExplorerFile) filePath.getUserObject();

						// FILE SELECTED
						if (!fc.isDirectory()) {// File Selected
							filePath = MainWindow.getInstance().getExplorer()
									.getRoot().getNextNode();
							fc = (ExplorerFile) filePath.getUserObject();
						}

					} else {

						// NOTHING SELECTED
						filePath = MainWindow.getInstance().getExplorer().getRoot()
								.getNextNode();
						fc = (ExplorerFile) filePath.getUserObject();
					}

					// GET THE NAME OF THE NODE
					String name = "";
					int index = file.lastIndexOf("\\");
					if (index == -1)
						index = file.lastIndexOf("/");
					index++;
					name = file.substring(index, file.length());

					ExplorerFile fic = new ExplorerFile();
					fic.setPath(file);
					fic.setName(name);
					fic.setParent(fc.getName());
					MainWindow.getInstance().getProjectConfiguration().addFile(fic);
					MainWindow.getInstance().getProjectConfiguration().getFileAt(
							MainWindow.getInstance().getProjectConfiguration()
									.getNumFilesFromList() - 1).setIsOpened(true);

					DefaultMutableTreeNode d = new DefaultMutableTreeNode(fic);
					d.setAllowsChildren(false);
					filePath.add(d);
					MainWindow.getInstance().validate();
					MainWindow.getInstance().repaint();
					MainWindow.getInstance().getExplorer().getTreeModel().reload();
					MainWindow.getInstance().getExplorer().expandTree();
					MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
							.setEnabled(true);
					MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
							.setEnabled(true);
					MainWindow.getInstance().getProjectConfiguration().setIsModified(true);

					// UPDATES THE STATUS BAR
					MainWindow.getInstance().getStatusBar().setMessage(fic.getPath());
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * 
	 */
	class AddFolderListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {

			Language language = Language.getInstance();
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e) {
				e.printStackTrace();
			}

			final ResourceBundle labels = language.getLabels();

			String folder = JOptionPane.showInputDialog(null, labels
					.getString("s656"));

			MainWindow mainWindow = MainWindow.getInstance();

			if (folder != null) {
				TreePath path = mainWindow.getExplorer().getTree()
						.getSelectionPath();

				DefaultMutableTreeNode filePath;
				ExplorerFile fd;

				if (path != null) {// Selected Folder
					filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
					fd = (ExplorerFile) filePath.getUserObject();

					if (!fd.isDirectory()) {// Selected File
						filePath = mainWindow.getExplorer().getRoot().getNextNode();
						fd = (ExplorerFile) filePath.getUserObject();
					}

				} else {
					filePath = mainWindow.getExplorer().getRoot().getNextNode();
					fd = (ExplorerFile) filePath.getUserObject();
				}

				ExplorerFile explorerFile = new ExplorerFile();
				explorerFile.setPath(folder);
				explorerFile.setName(folder);
				explorerFile.setParent(fd.getName());
				explorerFile.setIsDirectory(true);
				mainWindow.getProjectConfiguration().addFile(explorerFile);
				DefaultMutableTreeNode def = new DefaultMutableTreeNode(
						explorerFile);
				def.setAllowsChildren(true);
				filePath.add(def);
				filePath.setAllowsChildren(true);
				mainWindow.getExplorer().getTreeModel().reload();
				mainWindow.getExplorer().expandTree();
				mainWindow.getProjectConfiguration().setIsModified(true);
			}
		}
	}

	/**
	 * 
	 */
	class RemoveFolderListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {

			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e) {
				e.printStackTrace();
			}

			ResourceBundle labels = language.getLabels();

			MainWindow mainWindow = MainWindow.getInstance();

			int chosenOption = JOptionPane.showConfirmDialog(null, labels
					.getString("s654"));
			if (chosenOption == JOptionPane.OK_OPTION) {

				mainWindow.getProjectConfiguration().setIsModified(true);
				Toolkit toolkit = Toolkit.getDefaultToolkit();
				TreePath currentSelection = mainWindow.getExplorer().getTree()
						.getSelectionPath();
				if (currentSelection != null) {
					DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
							.getLastPathComponent());
					ExplorerFile p = (ExplorerFile) currentNode.getUserObject();
					if (p.isDirectory()) {
						MutableTreeNode parent = (MutableTreeNode) (currentNode
								.getParent());
						if (parent != null) {
							mainWindow.getExplorer().getTreeModel()
									.removeNodeFromParent(currentNode);
							toolkit.beep();
							ArrayList<String> contRemove = new ArrayList<String>();
							if ((currentNode.getDepth() <= 2)
									&& (p.getName().equals(mainWindow
											.getProjectConfiguration().getName()))) {
								mainWindow.getExplorer().getPopupMenu()
										.getAddFile().setEnabled(false);
								mainWindow.getExplorer().getPopupMenu()
										.getSaveProject().setEnabled(false);
								mainWindow.getExplorer().getPopupMenu()
										.getRemoveFile().setEnabled(false);
								mainWindow.getExplorer().getPopupMenu()
										.getDeleteFile().setEnabled(false);
								mainWindow.setTitle(labels.getString("s425")
										+ " - <empty>");
								TextFile f = new TextFile();

								f.save("./configuration/file_acidePrj", "<EMPTY>");
								mainWindow.validate();
								mainWindow.repaint();
								mainWindow.getProjectConfiguration().setName("");
								PropertiesManager.setProperty(
										"defaultAcideProject",
										"./configuration/default.acidePrj");
							}

							int cont = -1;
							for (int j = 0; j < mainWindow
									.getProjectConfiguration()
									.getNumFilesFromList(); j++) {
								if (!p.getName().equals(
										mainWindow.getProjectConfiguration()
												.getName())) {
									if (mainWindow.getProjectConfiguration()
											.getFileAt(j).getName().equals(
													p.getName())) {
										cont = j;

									} else if (mainWindow.getProjectConfiguration()
											.getFileAt(j).getParent().equals(
													p.getName())) {
										if (mainWindow.getProjectConfiguration()
												.getFileAt(j).isDirectory() != true) {
											contRemove.add(mainWindow
													.getProjectConfiguration()
													.getFileAt(j).getPath());
											if (mainWindow
													.getProjectConfiguration()
													.getNumFilesFromList() != 1)
												mainWindow
														.getProjectConfiguration()
														.removeFileAt(j);
											else
												mainWindow
														.getProjectConfiguration()
														.removeFileAt(0);
										} else {
											String dir = mainWindow
													.getProjectConfiguration()
													.getFileAt(j).getName();
											for (int k = j + 1; k < mainWindow
													.getProjectConfiguration()
													.getNumFilesFromList(); k++) {
												if (mainWindow
														.getProjectConfiguration()
														.getFileAt(j).getParent()
														.equals(dir)) {
													contRemove
															.add(mainWindow
																	.getProjectConfiguration()
																	.getFileAt(k)
																	.getPath());

													if (mainWindow
															.getProjectConfiguration()
															.getNumFilesFromList() != 1)
														mainWindow
																.getProjectConfiguration()
																.removeFileAt(k);
													else
														mainWindow
																.getProjectConfiguration()
																.removeFileAt(0);
												}
											}
										}
									}
								}
							}
							if (cont != -1)
								if (mainWindow.getProjectConfiguration()
										.getNumFilesFromList() != 1)
									mainWindow.getProjectConfiguration()
											.removeFileAt(cont);
								else
									mainWindow.getProjectConfiguration()
											.removeFileAt(0);
							int op = JOptionPane.showConfirmDialog(null, labels
									.getString("s655"));
							if (op == JOptionPane.OK_OPTION) {

								for (int j = 0; j < contRemove.size(); j++) {
									File fi = new File(contRemove.get(j));
									if (fi.isFile())
										fi.delete();
								}

							} else
								mainWindow.getStatusBar().setMessage(
										"Option cancel");
							return;

						}
						toolkit.beep();
					}
				}
			}

			if (mainWindow.getProjectConfiguration().getNumFilesFromList() > 0) {
				mainWindow.getExplorer().getPopupMenu().getRemoveFile().setEnabled(
						true);
				mainWindow.getExplorer().getPopupMenu().getDeleteFile().setEnabled(
						true);
			} else {
				mainWindow.getExplorer().getPopupMenu().getRemoveFile().setEnabled(
						false);
				mainWindow.getExplorer().getPopupMenu().getDeleteFile().setEnabled(
						false);
			}
		}
	}

	/**
	 * 
	 */
	class SetMainListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			MainWindow mainWindow = MainWindow.getInstance();

			TreePath path = mainWindow.getExplorer().getTree().getSelectionPath();
			DefaultMutableTreeNode filePath;
			ExplorerFile fc;

			if (path != null) {// Folder selected

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
				fc = (ExplorerFile) filePath.getUserObject();

				if (!fc.isMainFile()) {

					if (!fc.isDirectory()) {// File selected

						for (int i = 0; i < mainWindow.getProjectConfiguration()
								.getFileListSize(); i++) {
							// quit previous main
							if (mainWindow.getProjectConfiguration().getFileAt(i)
									.isMainFile()) {
								mainWindow.getProjectConfiguration().getFileAt(i)
										.setIsMainFile(false);
								mainWindow.getProjectConfiguration().getFileAt(i)
										.setIsCompilableFile(false);
								for (int j = 0; j < mainWindow.getEditorBuilder()
										.getNumEditors(); j++) {
									if (mainWindow
											.getEditorBuilder()
											.getEditorAt(j)
											.getAbsolutePath()
											.equals(
													mainWindow
															.getProjectConfiguration()
															.getFileAt(i).getPath()))
										mainWindow.getEditorBuilder().getPane()
												.setIconAt(j, null);
								}
							}
						}

						fc.setIsMainFile(true);
						fc.setIsCompilableFile(true);
						mainWindow.getProjectConfiguration().setIsModified(true);

						mainWindow.getStatusBar().setMessage(
								fc.getPath() + " <MAIN>");

						// put icon in tab
						for (int j = 0; j < mainWindow.getEditorBuilder()
								.getNumEditors(); j++) {
							if (mainWindow.getEditorBuilder().getEditorAt(j)
									.getAbsolutePath().equals(fc.getPath())) {
								mainWindow
										.getEditorBuilder()
										.getPane()
										.setIconAt(
												j,
												new ImageIcon(
														"./resources/icons/editor/main.PNG"));
							}
						}
					}
				}
			} else {

				String prj = null;
				try {
					prj = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					int editor = mainWindow.getEditorBuilder().getNumEditors();
					if (editor > 0) {
						MainWindow.getInstance().getEditorBuilder().setMainFile();
					}
				}
			}
		}
	}

	/**
	 * 
	 */
	class UnsetMainListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			MainWindow mainWindow = MainWindow.getInstance();

			TreePath path = mainWindow.getExplorer().getTree().getSelectionPath();
			DefaultMutableTreeNode filePath;
			ExplorerFile fc;

			if (path != null) {// Folder Selected

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
				fc = (ExplorerFile) filePath.getUserObject();

				if (fc.isMainFile()) {

					if (!fc.isDirectory()) {// File selected

						fc.setIsMainFile(false);
						fc.setIsCompilableFile(false);
						mainWindow.getProjectConfiguration().setIsModified(true);
						// quit status
						mainWindow.getStatusBar().setMessage(fc.getPath());
						// quit icon in tab
						for (int j = 0; j < mainWindow.getEditorBuilder()
								.getNumEditors(); j++) {
							if (mainWindow.getEditorBuilder().getEditorAt(j)
									.getAbsolutePath().equals(fc.getPath())) {
								mainWindow.getEditorBuilder().getPane().setIconAt(
										j, null);
							}
						}
					}
				}
			} else {// Nothing selected
				// No project
				String prj = null;
				try {
					prj = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					int editor = mainWindow.getEditorBuilder().getNumEditors();
					if (editor > 0) {
						MainWindow.getInstance().getEditorBuilder().unsetMainFile();
					}
				}
			}
		}
	}

	/**
	 * 
	 */
	class SetCompilableListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			MainWindow mainWindow = MainWindow.getInstance();

			TreePath path = mainWindow.getExplorer().getTree().getSelectionPath();
			DefaultMutableTreeNode filePath;
			ExplorerFile fc;

			if (path != null) {// Folder selected

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
				fc = (ExplorerFile) filePath.getUserObject();

				if (!fc.isCompilableFile()
						|| (fc.isCompilableFile() && fc.isMainFile())) {

					if (!fc.isDirectory()) {// File selected

						if (fc.isMainFile())
							fc.setIsMainFile(false);

						fc.setIsCompilableFile(true);
						mainWindow.getProjectConfiguration().setIsModified(true);

						// put icon in tab
						for (int j = 0; j < mainWindow.getEditorBuilder()
								.getNumEditors(); j++) {
							if (mainWindow.getEditorBuilder().getEditorAt(j)
									.getAbsolutePath().equals(fc.getPath())) {
								mainWindow
										.getEditorBuilder()
										.getPane()
										.setIconAt(
												j,
												new ImageIcon(
														"./resources/icons/editor/compilable.PNG"));
								// status
								mainWindow.getStatusBar().setMessage(
										mainWindow.getEditorBuilder()
												.getEditorAt(j).getAbsolutePath()
												+ " <COMPILABLE>");
							}
						}
					}
				}
			} else {// Nothing selected
				// No project
				String prj = null;
				try {
					prj = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					int editor = mainWindow.getEditorBuilder().getNumEditors();
					if (editor > 0) {
						MainWindow.getInstance().getEditorBuilder()
								.setCompilableFile();
					}
				}
			}
		}
	}

	/**
	 * 
	 */
	class UnsetCompilableListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			MainWindow mainWindow = MainWindow.getInstance();

			TreePath path = mainWindow.getExplorer().getTree().getSelectionPath();
			DefaultMutableTreeNode filePath;
			ExplorerFile fc;

			if (path != null) {// hay carpeta seleccionada

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
				fc = (ExplorerFile) filePath.getUserObject();

				if (fc.isCompilableFile() && !fc.isMainFile()) {

					if (!fc.isDirectory()) {// File Selected

						fc.setIsCompilableFile(false);
						mainWindow.getProjectConfiguration().setIsModified(true);
						// quit status
						mainWindow.getStatusBar().setMessage(fc.getPath());
						// quit icon in tab
						for (int j = 0; j < mainWindow.getEditorBuilder()
								.getNumEditors(); j++) {
							if (mainWindow.getEditorBuilder().getEditorAt(j)
									.getAbsolutePath().equals(fc.getPath())) {
								mainWindow.getEditorBuilder().getPane().setIconAt(
										j, null);
							}
						}
					}
				}
			} else {// Nothing selected
				String prj = null;
				try {
					prj = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					int editor = mainWindow.getEditorBuilder().getNumEditors();
					if (editor > 0) {
						MainWindow.getInstance().getEditorBuilder()
								.unsetCompilableFile();
					}
				}
			}
		}
	}

	/**
	 * 
	 */
	class SaveAsProjectListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			try {

				IOFactory fact = IOFactory.getInstance();
				MainWindow mainWindow = MainWindow.getInstance();
				ResourceBundle labels = Language.getInstance().getLabels();
				TextFile f = fact.buildFile();

				if (!mainWindow.getProjectConfiguration().getName().equals("")) {
					// Selects the project extension
					String[] ExtPide = new String[] { "acidePrj" };
					f.getFileChooser().addChoosableFileFilter(
							new ExtensionFilter(ExtPide, labels.getString("s328")));

					String file = f.write();
					String len = PropertiesManager.getProperty("language");
					String currentMenu = PropertiesManager
							.getProperty("currentMenuConfiguration");
					String currentTB = PropertiesManager
							.getProperty("currentToolBarConfiguration");
					mainWindow.getProjectConfiguration().setLanguage(len);
					mainWindow.getProjectConfiguration().setMenu(currentMenu);
					mainWindow.getProjectConfiguration().setToolBar(currentTB);

					if (!file.contains(".acidePrj"))
						file = file + ".acidePrj";
					mainWindow.getProjectConfiguration().setPath(file);
					String cad = mainWindow.getProjectConfiguration().save();
					f.save(mainWindow.getProjectConfiguration().getProjectPath(),
							cad);
					mainWindow.getProjectConfiguration().setFirstSave(true);
					PropertiesManager.setProperty("defaultAcideProject", file);
					PropertiesManager.setProperty("defaultPath", file);
					mainWindow.getProjectConfiguration().setIsModified(false);
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}
}