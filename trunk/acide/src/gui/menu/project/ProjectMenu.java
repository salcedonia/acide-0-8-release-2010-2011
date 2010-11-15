package gui.menu.project;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;
import es.configuration.menu.MenuConfiguration;
import gui.menu.project.listeners.AddFileListener;
import gui.menu.project.listeners.AddFolderListener;
import gui.menu.project.listeners.CloseProjectListener;
import gui.menu.project.listeners.CompileListener;
import gui.menu.project.listeners.DeleteFileListener;
import gui.menu.project.listeners.ExecuteListener;
import gui.menu.project.listeners.NewProjectFileListener;
import gui.menu.project.listeners.NewProjectListener;
import gui.menu.project.listeners.OpenProjectListener;
import gui.menu.project.listeners.RemoveFileListener;
import gui.menu.project.listeners.RemoveFolderListener;
import gui.menu.project.listeners.SaveAsProjectListener;
import gui.menu.project.listeners.SaveProjectListener;
import gui.menu.project.listeners.SetCompilableListener;
import gui.menu.project.listeners.SetMainListener;
import gui.menu.project.listeners.UnsetCompilableListener;
import gui.menu.project.listeners.UnsetMainListener;

/************************************************************************																
 * Project menu of ACIDE - A Configurable IDE											
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
 * @see JMenu																													
 ***********************************************************************/
public class ProjectMenu extends JMenu {

	/**
	 * Serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the new project menu item
	 */
	private static final String NEW_PROJECT = "./resources/icons/menu/project/newProject.png";
	/**
	 * Image file for the open project menu item
	 */
	private static final String OPEN_PROJECT = "./resources/icons/menu/project/openProject.png";
	/**
	 * Image file for the save project menu item
	 */
	private static final String SAVE_PROJECT = "./resources/icons/menu/project/saveProject.png";
	/**
	 * image file for the new file menu item
	 */
	private static final String NEW_FILE = "./resources/icons/menu/project/newFile.png";
	/**
	 * Image file for the add file menu item
	 */
	private static final String ADD_FILE = "./resources/icons/menu/project/addFile.png";
	/**
	 * Image file for the add folder menu item
	 */
	private static final String ADD_FOLDER = "./resources/icons/menu/project/addFolder.png";
	/**
	 * Image file for the delete file menu item
	 */
	private static final String DELETE_FILE = "./resources/icons/menu/project/deleteFile.png";
	/**
	 * Image file for the compile menu item
	 */
	private static final String COMPILE = "./resources/icons/menu/project/compile.png";
	/**
	 * Image file for the execute menu item
	 */
	private static final String EXECUTE = "./resources/icons/menu/project/execute.png";
	/**
	 * Image file for the set main menu item
	 */
	private static final String SET_MAIN = "./resources/icons/menu/project/setMain.png";
	/**
	 * Image file for the unset main menu item
	 */
	private static final String UNSET_MAIN = "./resources/icons/menu/project/unsetMain.png";
	/**
	 * Image file for the set compilable menu item
	 */
	private static final String SET_COMPILABLE = "./resources/icons/menu/project/setCompilable.png";
	/**
	 * Image file for the unset compilable project menu item
	 */
	private static final String UNSET_COMPILABLE = "./resources/icons/menu/project/unsetCompilable.png";
	/**
	 * New project
	 */
	private JMenuItem _newProject;
	/**
	 * Open project
	 */
	private JMenuItem _openProject;
	/**
	 * Save project
	 */
	private JMenuItem _saveProject;
	/**
	 * New project file menu item
	 */
	private JMenuItem _newProjectFile;
	/**
	 * Save as project menu item
	 */
	private JMenuItem _saveAsProject;
	/**
	 * Add file menu item
	 */
	private JMenuItem _addFile;
	/**
	 * Close project menu item
	 */
	private JMenuItem _closeProject;
	/**
	 * Remove file menu item
	 */
	private JMenuItem _removeFile;
	/**
	 * Delete file menu item
	 */
	private JMenuItem _deleteFile;
	/**
	 * Add folder menu item
	 */
	private JMenuItem _addFolder;
	/**
	 * Remove folder menu item
	 */
	private JMenuItem _removeFolder;
	/**
	 * Set compilable menu item
	 */
	private JMenuItem _setCompilable;
	/**
	 * Unset compilable menu item
	 */
	private JMenuItem _unsetCompilable;
	/**
	 * Set main menu item
	 */
	private JMenuItem _setMain;
	/**
	 * Unset main menu item
	 */
	private JMenuItem _unsetMain;
	/**
	 * Compile menu item
	 */
	private JMenuItem _compile;
	/**
	 * Execute menu item
	 */
	private JMenuItem _execute;

	/**
	 * Class constructor
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
	 * Sets the labels to display in the selected language
	 * 
	 * @param labels labels to display in the selected language
	 */
	public void setLanguageLabels() {

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
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
	 * Builds the project menu
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
						|| MenuConfiguration.getSetCompiledFile()
						|| MenuConfiguration.getUnsetCompilableFile() || MenuConfiguration
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
				&& (MenuConfiguration.getSetCompiledFile()
						|| MenuConfiguration.getUnsetCompilableFile() || MenuConfiguration
						.getSetMain()))
			addSeparator();
		if (MenuConfiguration.getSetCompiledFile())
			add(_setCompilable);
		if (MenuConfiguration.getUnsetCompilableFile())
			add(_unsetCompilable);
		if (MenuConfiguration.getSetMain())
			add(_setMain);
		if (MenuConfiguration.getUnsetMain())
			add(_unsetMain);
	}

	/**
	 * Sets the project menu Listeners
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
	 * Enables the project menu
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
	 * Disables the project menu
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
	 * Returns the new project menu item
	 * 
	 * @return the new project menu item
	 */
	public JMenuItem getNewProject() {
		return _newProject;
	}

	/**
	 * Returns the save project menu item
	 * 
	 * @return the save project menu item
	 */
	public JMenuItem getSaveProject() {
		return _saveProject;
	}

	/**
	 * Returns the open project menu item
	 * 
	 * @return the open project menu item
	 */
	public JMenuItem getOpenProject() {
		return _openProject;
	}

	/**
	 * Returns the new project file menu item
	 * 
	 * @return the new project file menu item
	 */
	public JMenuItem getNewProjectFile() {
		return _newProjectFile;
	}

	/**
	 * Returns the add file menu item
	 * 
	 * @return the add file menu item
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * Returns the add folder menu item
	 * 
	 * @return the add folder menu item
	 */
	public JMenuItem getAddFolder() {
		return _addFolder;
	}

	/**
	 * Sets a new value to the add folder menu item
	 * 
	 * @param addFolder new value to set
	 */
	public void setAddFolder(JMenuItem addFolder) {
		_addFolder = addFolder;
	}

	/**
	 * Returns the compile menu item
	 * 
	 * @return the compile menu item
	 */
	public JMenuItem getCompile() {
		return _compile;
	}

	/**
	 * Sets a new value to the compile menu item
	 * 
	 * @param compile new value to set
	 */
	public void setCompile(JMenuItem compile) {
		_compile = compile;
	}

	/**
	 * Returns the execute menu item
	 * 
	 * @return the execute menu item
	 */
	public JMenuItem getExecute() {
		return _execute;
	}

	/**
	 * Sets a new value to the execute menu item
	 * 
	 * @param execute new value to set
	 */
	public void setExecute(JMenuItem execute) {
		_execute = execute;
	}

	/**
	 * Sets a new value to the remove file menu item
	 * 
	 * @param removeFile new value to set
	 */
	public void setRemoveFile(JMenuItem removeFile) {
		_removeFile = removeFile;
	}

	/**
	 * Returns the remove folder menu item
	 * 
	 * @return the remove folder menu item
	 */
	public JMenuItem getRemoveFolder() {
		return _removeFolder;
	}

	/**
	 * Sets a new value to the remove folder menu item
	 * 
	 * @param removeFolder new value to set
	 */
	public void setRemoveFolder(JMenuItem removeFolder) {
		_removeFolder = removeFolder;
	}

	/**
	 * Sets a new value to the open project menu item
	 * 
	 * @param openProject new value to set
	 */
	public void setOpenProject(JMenuItem openProject) {
		_openProject = openProject;
	}

	/**
	 * Returns the remove file menu item
	 * 
	 * @return the remove file menu item
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * Returns the close project menu item
	 * 
	 * @return the close project menu item
	 */
	public JMenuItem getCloseProject() {
		return _closeProject;
	}

	/**
	 * Sets a new value to the close project menu item
	 * 
	 * @param closeProject new value to set
	 */
	public void setCloseProject(JMenuItem closeProject) {
		_closeProject = closeProject;
	}

	/**
	 * Sets a new value to the new project menu item
	 * 
	 * @param newProjectFile new value to set
	 */
	public void setNewProjectFile(JMenuItem newProjectFile) {
		_newProjectFile = newProjectFile;
	}

	/**
	 * Sets a new value to the add file menu item
	 * 
	 * @param addFile new value to set
	 */
	public void setAddFile(JMenuItem addFile) {
		_addFile = addFile;
	}

	/**
	 * Sets a new value to the save project menu item
	 * 
	 * @param saveProject new value to set
	 */
	public void setSaveProject(JMenuItem saveProject) {
		_saveProject = saveProject;
	}

	/**
	 * Sets a new value to the new project menu item
	 *  
	 * @param newProject new value to set
	 */
	public void setNewProject(JMenuItem newProject) {
		_newProject = newProject;
	}

	/**
	 * Returns the set compilable menu item
	 * 
	 * @return the set compilable menu item
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilable;
	}

	/**
	 * Sets a new value to the set compilable menu item
	 * 
	 * @param setCompilable new value to set
	 */
	public void setSetCompilable(JMenuItem setCompilable) {
		_setCompilable = setCompilable;
	}

	/**
	 * Returns the set main menu item
	 * 
	 * @return the set main menu item
	 */
	public JMenuItem getSetMain() {
		return _setMain;
	}

	/**
	 * Sets a new value to the set main menu item
	 * 
	 * @param setMain new value to set
	 */
	public void setSetMain(JMenuItem setMain) {
		_setMain = setMain;
	}

	/**
	 * Returns the unset compilable menu item
	 * 
	 * @return the unset compilable menu item
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilable;
	}

	/**
	 * Sets a new value to the unset compilable menu item
	 * 
	 * @param unsetFile new value to set
	 */
	public void setUnsetCompilable(JMenuItem unsetFile) {
		_unsetCompilable = unsetFile;
	}

	/**
	 * Returns the save as project menu item
	 * 
	 * @return the save as project menu item
	 */
	public JMenuItem getSaveAsProject() {
		return _saveAsProject;
	}

	/**
	 * Sets a new value to the save as project menu item
	 * 
	 * @param saveAsProject new value to set
	 */
	public void setSaveAsProject(JMenuItem saveAsProject) {
		_saveAsProject = saveAsProject;
	}

	/**
	 * Sets a new value to the delete file menu item
	 * 
	 * @param deleteFile new value to set
	 */
	public void setDeleteFile(JMenuItem deleteFile) {
		_deleteFile = deleteFile;
	}

	/**
	 * Returns the delete file menu item
	 * 
	 * @return the delete file menu item
	 */
	public JMenuItem getDeleteFile() {
		return _deleteFile;
	}

	/**
	 * Returns the unset main menu item
	 * 
	 * @return the unset main menu item
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMain;
	}

	/**
	 * Sets a new value to the unset main menu item
	 * 
	 * @param unsetMain new value to set
	 */
	public void setUnsetMain(JMenuItem unsetMain) {
		_unsetMain = unsetMain;
	}
}