package gui.menuBar.projectMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;
import es.configuration.menu.MenuConfiguration;
import gui.menuBar.projectMenu.listeners.AddFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.AddFolderMenuItemListener;
import gui.menuBar.projectMenu.listeners.CloseProjecMenuItemtListener;
import gui.menuBar.projectMenu.listeners.CompileMenuItemListener;
import gui.menuBar.projectMenu.listeners.DeleteFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.ExecuteMenuItemListener;
import gui.menuBar.projectMenu.listeners.NewProjectFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.NewProjectMenuItemListener;
import gui.menuBar.projectMenu.listeners.OpenProjectMenuItemListener;
import gui.menuBar.projectMenu.listeners.RemoveFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.RemoveFolderMenuItemListener;
import gui.menuBar.projectMenu.listeners.SaveAsProjectMenuItemListener;
import gui.menuBar.projectMenu.listeners.SaveProjectMenuItemListener;
import gui.menuBar.projectMenu.listeners.SetCompilableFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.SetMainFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.UnsetCompilableFileMenuItemListener;
import gui.menuBar.projectMenu.listeners.UnsetMainFileMenuItemListener;

/************************************************************************																
 * Project menu of ACIDE - A Configurable IDE.											
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
	 * Project menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * New project menu item name.
	 */
	public static final String NEW_PROJECT_NAME = "New Project";
	/**
	 * Open project menu item name.
	 */
	public static final String OPEN_PROJECT_NAME = "Open Project";
	/**
	 * Save project menu item name.
	 */
	public static final String SAVE_PROJECT_NAME = "Save Project";
	/**
	 * Save project as menu item image icon.
	 */
	public static final String SAVE_PROJECT_AS_NAME = "Save Project As";
	/**
	 * New project file menu item name.
	 */
	public static final String NEW_PROJECT_FILE_NAME = "New Project File";
	/**
	 * Add file menu item name.
	 */
	public static final String ADD_FILE_NAME = "Add File";
	/**
	 * Add folder menu item name.
	 */
	public static final String ADD_FOLDER_NAME = "Add Folder";
	/**
	 * Remove folder menu item name.
	 */
	public static final String REMOVE_FOLDER_NAME = "Remove Folder";
	/**
	 * Remove file menu item name.
	 */
	public static final String REMOVE_FILE_NAME = "Remove File";
	/**
	 * Delete file menu item name.
	 */
	public static final String DELETE_FILE_NAME = "Delete File";
	/**
	 * Close project menu item name.
	 */
	public static final String CLOSE_PROJECT_NAME = "Close Project";
	/**
	 * Compile menu item name.
	 */
	public static final String COMPILE_NAME = "Compile";
	/**
	 * Execute menu item name.
	 */
	public static final String EXECUTE_NAME = "Execute";
	/**
	 * Set main file menu item name.
	 */
	public static final String SET_MAIN_FILE_NAME = "Set Main File";
	/**
	 * Unset main file menu item name.
	 */
	public static final String UNSET_MAIN_FILE_NAME = "Unset Main File";
	/**
	 * Set compilable file menu item name.
	 */
	public static final String SET_COMPILABLE_FILE_NAME = "Set Compilable File";
	/**
	 * Unset compilable file menu item name.
	 */
	public static final String UNSET_COMPILABLE_FILE_NAME = "Unset Compilable File";
	/**
	 * New project menu item image icon.
	 */
	private static final ImageIcon NEW_PROJECT_IMAGE = new ImageIcon("./resources/icons/menu/project/newProject.png");
	/**
	 * Open project menu item image icon.
	 */
	private static final ImageIcon OPEN_PROJECT_IMAGE = new ImageIcon("./resources/icons/menu/project/openProject.png");
	/**
	 * Save project menu item image icon.
	 */
	private static final ImageIcon SAVE_PROJECT_IMAGE = new ImageIcon("./resources/icons/menu/project/saveProject.png");
	/**
	 * New project file menu item image icon.
	 */
	private static final ImageIcon NEW_PROJECT_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/newFile.png");
	/**
	 * Add file menu item image icon.
	 */
	private static final ImageIcon ADD_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/addFile.png");
	/**
	 * Add folder menu item image icon.
	 */
	private static final ImageIcon ADD_FOLDER_IMAGE = new ImageIcon("./resources/icons/menu/project/addFolder.png");
	/**
	 * Delete file menu item main icon.
	 */
	private static final ImageIcon DELETE_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/deleteFile.png");
	/**
	 * Compile menu item image icon.
	 */
	private static final ImageIcon COMPILE_IMAGE = new ImageIcon("./resources/icons/menu/project/compile.png");
	/**
	 * Execute menu item image icon.
	 */
	private static final ImageIcon EXECUTE_IMAGE = new ImageIcon("./resources/icons/menu/project/execute.png");
	/**
	 * Set main menu item image icon.
	 */
	private static final ImageIcon SET_MAIN_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/setMain.png");
	/**
	 * Unset main menu item image icon.
	 */
	private static final ImageIcon UNSET_MAIN_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/unsetMain.png");
	/**
	 * Set compilable menu item image icon.
	 */
	private static final ImageIcon SET_COMPILABLE_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/setCompilable.png");
	/**
	 * Unset compilable project menu item image icon.
	 */
	private static final ImageIcon UNSET_COMPILABLE_FILE_IMAGE = new ImageIcon("./resources/icons/menu/project/unsetCompilable.png");
	/**
	 * New project menu item.
	 */
	private JMenuItem _newProject;
	/**
	 * Open project menu item.
	 */
	private JMenuItem _openProject;
	/**
	 * Save project menu item.
	 */
	private JMenuItem _saveProject;
	/**
	 * New project file menu item.
	 */
	private JMenuItem _newProjectFile;
	/**
	 * Save as project menu item.
	 */
	private JMenuItem _saveProjectAs;
	/**
	 * Add file menu item.
	 */
	private JMenuItem _addFile;
	/**
	 * Close project menu item.
	 */
	private JMenuItem _closeProject;
	/**
	 * Remove file menu item.
	 */
	private JMenuItem _removeFile;
	/**
	 * Delete file menu item.
	 */
	private JMenuItem _deleteFile;
	/**
	 * Add folder menu item.
	 */
	private JMenuItem _addFolder;
	/**
	 * Remove folder menu item.
	 */
	private JMenuItem _removeFolder;
	/**
	 * Set compilable menu item.
	 */
	private JMenuItem _setCompilableFile;
	/**
	 * Unset compilable menu item.
	 */
	private JMenuItem _unsetCompilableFile;
	/**
	 * Set main menu item.
	 */
	private JMenuItem _setMainFile;
	/**
	 * Unset main menu item.
	 */
	private JMenuItem _unsetMainFile;
	/**
	 * Compile menu item.
	 */
	private JMenuItem _compile;
	/**
	 * Execute menu item.
	 */
	private JMenuItem _execute;

	/**
	 * Creates a new project menu.
	 */
	public ProjectMenu() {

		// MENU ITEM
		_newProject = new JMenuItem(NEW_PROJECT_IMAGE);
		_openProject = new JMenuItem(OPEN_PROJECT_IMAGE);
		_saveProject = new JMenuItem(SAVE_PROJECT_IMAGE);
		_saveProjectAs = new JMenuItem();
		_newProjectFile = new JMenuItem(NEW_PROJECT_FILE_IMAGE);
		_addFile = new JMenuItem(ADD_FILE_IMAGE);
		_removeFile = new JMenuItem();
		_deleteFile = new JMenuItem(DELETE_FILE_IMAGE);
		_closeProject = new JMenuItem();
		_compile = new JMenuItem(COMPILE_IMAGE);
		_execute = new JMenuItem(EXECUTE_IMAGE);
		_addFolder = new JMenuItem(ADD_FOLDER_IMAGE);
		_removeFolder = new JMenuItem();
		_setMainFile = new JMenuItem(SET_MAIN_FILE_IMAGE);
		_unsetMainFile = new JMenuItem(UNSET_MAIN_FILE_IMAGE);
		_setCompilableFile = new JMenuItem(SET_COMPILABLE_FILE_IMAGE);
		_unsetCompilableFile = new JMenuItem(UNSET_COMPILABLE_FILE_IMAGE);

		setLanguageLabels();
	}

	/**
	 * Sets the labels to display in the selected language.
	 * 
	 * @param labels labels to display in the selected language.
	 */
	public void setLanguageLabels() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
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
		_saveProjectAs.setText(labels.getString("s926"));

		// CLOSE PROJECT
		_closeProject.setText(labels.getString("s228"));

		// SET COMPILABLE
		_setCompilableFile.setText(labels.getString("s254"));

		// UNSET COMPILABLE
		_unsetCompilableFile.setText(labels.getString("s255"));

		// SET MAIN
		_setMainFile.setText(labels.getString("s256"));

		// UNSET MAIN
		_unsetMainFile.setText(labels.getString("s952"));

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
	 * Builds the project menu.
	 */
	public void buildMenu() {

		removeAll();

		// NEW PROJECT
		if (MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME))
			add(_newProject);
		
		// OPEN PROJECT
		if (MenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME))
			add(_openProject);
		
		// CLOSE PROJECT
		if (MenuConfiguration.getInstance().getIsDisplayed(CLOSE_PROJECT_NAME))
			add(_closeProject);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(CLOSE_PROJECT_NAME))
				&& (MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME) 
						|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME)))
			addSeparator();
		
		// SAVE PROJECT
		if (MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME))
			add(_saveProject);
		
		// SAVE PROJECT AS
		if (MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME))
			add(_saveProjectAs);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(CLOSE_PROJECT_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME))
				&& (MenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME) 
						|| MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME)))
			addSeparator();
		
		// NEW PROJECT FILE
		if (MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME))
			add(_newProjectFile);
		
		// ADD FILE
		if (MenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME))
			add(_addFile);
		
		// REMOVE FILE
		if (MenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME))
			add(_removeFile);
		
		// DELETE FILE
		if (MenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME))
			add(_deleteFile);
		
		// ADD FOLDER
		if (MenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME))
			add(_addFolder);
		
		// REMOVE FOLDER
		if (MenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME))
			add(_removeFolder);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME))
				&& (MenuConfiguration.getInstance().getIsDisplayed(COMPILE_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(EXECUTE_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(SET_COMPILABLE_FILE_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(UNSET_COMPILABLE_FILE_NAME) 
						|| MenuConfiguration.getInstance().getIsDisplayed(SET_MAIN_FILE_NAME)))
			addSeparator();
		
		// COMPILE
		if (MenuConfiguration.getInstance().getIsDisplayed(COMPILE_NAME))
			add(_compile);
		
		// EXECUTE
		if (MenuConfiguration.getInstance().getIsDisplayed(EXECUTE_NAME))
			add(_execute);
		
		// SEPARATOR
		if ((MenuConfiguration.getInstance().getIsDisplayed(OPEN_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(SAVE_PROJECT_AS_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(ADD_FILE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(REMOVE_FILE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(ADD_FOLDER_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(REMOVE_FOLDER_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(DELETE_FILE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(NEW_PROJECT_FILE_NAME)
				|| MenuConfiguration.getInstance().getIsDisplayed(COMPILE_NAME) 
				|| MenuConfiguration.getInstance().getIsDisplayed(EXECUTE_NAME))
				&& (MenuConfiguration.getInstance().getIsDisplayed(SET_COMPILABLE_FILE_NAME)
						|| MenuConfiguration.getInstance().getIsDisplayed(UNSET_COMPILABLE_FILE_NAME) 
						|| MenuConfiguration.getInstance().getIsDisplayed(SET_MAIN_FILE_NAME)))
			addSeparator();
		
		// SET COMPILABLE FILE
		if (MenuConfiguration.getInstance().getIsDisplayed(SET_COMPILABLE_FILE_NAME))
			add(_setCompilableFile);
		
		// UNSET COMPILABLE FILE
		if (MenuConfiguration.getInstance().getIsDisplayed(UNSET_COMPILABLE_FILE_NAME))
			add(_unsetCompilableFile);
		
		// SET MAIN FILE
		if (MenuConfiguration.getInstance().getIsDisplayed(SET_MAIN_FILE_NAME))
			add(_setMainFile);
		
		// UNSET MAIN FILE
		if (MenuConfiguration.getInstance().getIsDisplayed(UNSET_MAIN_FILE_NAME))
			add(_unsetMainFile);
	}

	/**
	 * Sets the project menu listeners.
	 */
	public void setListeners() {

		// NEW PROJECT
		_newProject.addActionListener(new NewProjectMenuItemListener());

		// SAVE PROJECT
		_saveProject.addActionListener(new SaveProjectMenuItemListener());

		// SAVE AS PROJECT
		_saveProjectAs.addActionListener(new SaveAsProjectMenuItemListener());

		// OPEN PROJECT
		_openProject.addActionListener(new OpenProjectMenuItemListener());

		// ADD FILE
		_addFile.addActionListener(new AddFileMenuItemListener());

		// EXECUTE
		_execute.addActionListener(new ExecuteMenuItemListener());

		// COMPILE
		_compile.addActionListener(new CompileMenuItemListener());

		// REMOVE FILE
		_removeFile.addActionListener(new RemoveFileMenuItemListener());

		// CLOSE PROJECT
		_closeProject.addActionListener(new CloseProjecMenuItemtListener());

		// DELETE FILE
		_deleteFile.addActionListener(new DeleteFileMenuItemListener());

		// NEW PROJECT FILE
		_newProjectFile.addActionListener(new NewProjectFileMenuItemListener());

		// ADD FOLDER
		_addFolder.addActionListener(new AddFolderMenuItemListener());

		// REMOVE FOLDER
		_removeFolder.addActionListener(new RemoveFolderMenuItemListener());

		// SET MAIN
		_setMainFile.addActionListener(new SetMainFileMenuItemListener());

		// UNSET MAIN
		_unsetMainFile.addActionListener(new UnsetMainFileMenuItemListener());

		// SET COMPILABLE
		_setCompilableFile.addActionListener(new SetCompilableFileMenuItemListener());

		// UNSET COMPILABLE
		_unsetCompilableFile.addActionListener(new UnsetCompilableFileMenuItemListener());
	}

	/**
	 * Enables the project menu.
	 */
	public void enableMenu() {

		_closeProject.setEnabled(true);
		_saveProject.setEnabled(false);
		_saveProjectAs.setEnabled(true);
		_newProjectFile.setEnabled(true);
		_addFile.setEnabled(true);
		_removeFile.setEnabled(false);
		_deleteFile.setEnabled(false);
		_addFolder.setEnabled(true);
		_removeFolder.setEnabled(false);
		_compile.setEnabled(true);
		_execute.setEnabled(true);
		_setMainFile.setEnabled(false);
		_unsetMainFile.setEnabled(false);
		_setCompilableFile.setEnabled(false);
		_unsetCompilableFile.setEnabled(false);
	}

	/**
	 * Disables the project menu
	 */
	public void disableMenu() {

		_closeProject.setEnabled(false);
		_saveProject.setEnabled(false);
		_saveProjectAs.setEnabled(false);
		_newProjectFile.setEnabled(false);
		_addFile.setEnabled(false);
		_removeFile.setEnabled(false);
		_deleteFile.setEnabled(false);
		_addFolder.setEnabled(false);
		_removeFolder.setEnabled(false);
		_compile.setEnabled(false);
		_execute.setEnabled(false);
		_setMainFile.setEnabled(false);
		_unsetMainFile.setEnabled(false);
		_setCompilableFile.setEnabled(false);
		_unsetCompilableFile.setEnabled(false);
	}

	/**
	 * Returns the new project menu item.
	 * 
	 * @return the new project menu item.
	 */
	public JMenuItem getNewProject() {
		return _newProject;
	}

	/**
	 * Returns the save project menu item.
	 * 
	 * @return the save project menu item.
	 */
	public JMenuItem getSaveProject() {
		return _saveProject;
	}

	/**
	 * Returns the open project menu item.
	 * 
	 * @return the open project menu item.
	 */
	public JMenuItem getOpenProject() {
		return _openProject;
	}

	/**
	 * Returns the new project file menu item.
	 * 
	 * @return the new project file menu item.
	 */
	public JMenuItem getNewProjectFile() {
		return _newProjectFile;
	}

	/**
	 * Returns the add file menu item.
	 * 
	 * @return the add file menu item.
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * Returns the add folder menu item.
	 * 
	 * @return the add folder menu item.
	 */
	public JMenuItem getAddFolder() {
		return _addFolder;
	}

	/**
	 * Sets a new value to the add folder menu item.
	 * 
	 * @param addFolder new value to set.
	 */
	public void setAddFolder(JMenuItem addFolder) {
		_addFolder = addFolder;
	}

	/**
	 * Returns the compile menu item.
	 * 
	 * @return the compile menu item.
	 */
	public JMenuItem getCompile() {
		return _compile;
	}

	/**
	 * Sets a new value to the compile menu item.
	 * 
	 * @param compile new value to set.
	 */
	public void setCompile(JMenuItem compile) {
		_compile = compile;
	}

	/**
	 * Returns the execute menu item.
	 * 
	 * @return the execute menu item.
	 */
	public JMenuItem getExecute() {
		return _execute;
	}

	/**
	 * Sets a new value to the execute menu item.
	 * 
	 * @param execute new value to set.
	 */
	public void setExecute(JMenuItem execute) {
		_execute = execute;
	}

	/**
	 * Sets a new value to the remove file menu item.
	 * 
	 * @param removeFile new value to set.
	 */
	public void setRemoveFile(JMenuItem removeFile) {
		_removeFile = removeFile;
	}

	/**
	 * Returns the remove folder menu item.
	 * 
	 * @return the remove folder menu item.
	 */
	public JMenuItem getRemoveFolder() {
		return _removeFolder;
	}

	/**
	 * Sets a new value to the remove folder menu item.
	 * 
	 * @param removeFolder new value to set.
	 */
	public void setRemoveFolder(JMenuItem removeFolder) {
		_removeFolder = removeFolder;
	}

	/**
	 * Sets a new value to the open project menu item.
	 * 
	 * @param openProject new value to set.
	 */
	public void setOpenProject(JMenuItem openProject) {
		_openProject = openProject;
	}

	/**
	 * Returns the remove file menu item.
	 * 
	 * @return the remove file menu item.
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * Returns the close project menu item.
	 * 
	 * @return the close project menu item.
	 */
	public JMenuItem getCloseProject() {
		return _closeProject;
	}

	/**
	 * Sets a new value to the close project menu item.
	 * 
	 * @param closeProject new value to set.
	 */
	public void setCloseProject(JMenuItem closeProject) {
		_closeProject = closeProject;
	}

	/**
	 * Sets a new value to the new project menu item.
	 * 
	 * @param newProjectFile new value to set.
	 */
	public void setNewProjectFile(JMenuItem newProjectFile) {
		_newProjectFile = newProjectFile;
	}

	/**
	 * Sets a new value to the add file menu item.
	 * 
	 * @param addFile new value to set.
	 */
	public void setAddFile(JMenuItem addFile) {
		_addFile = addFile;
	}

	/**
	 * Sets a new value to the save project menu item.
	 * 
	 * @param saveProject new value to set.
	 */
	public void setSaveProject(JMenuItem saveProject) {
		_saveProject = saveProject;
	}

	/**
	 * Sets a new value to the new project menu item.
	 *  
	 * @param newProject new value to set.
	 */
	public void setNewProject(JMenuItem newProject) {
		_newProject = newProject;
	}

	/**
	 * Returns the set compilable menu item.
	 * 
	 * @return the set compilable menu item.
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilableFile;
	}

	/**
	 * Sets a new value to the set compilable menu item.
	 * 
	 * @param setCompilable new value to set.
	 */
	public void setSetCompilable(JMenuItem setCompilable) {
		_setCompilableFile = setCompilable;
	}

	/**
	 * Returns the set main menu item.
	 * 
	 * @return the set main menu item.
	 */
	public JMenuItem getSetMain() {
		return _setMainFile;
	}

	/**
	 * Sets a new value to the set main menu item.
	 * 
	 * @param setMain new value to set.
	 */
	public void setSetMain(JMenuItem setMain) {
		_setMainFile = setMain;
	}

	/**
	 * Returns the unset compilable menu item.
	 * 
	 * @return the unset compilable menu item.
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilableFile;
	}

	/**
	 * Sets a new value to the unset compilable menu item.
	 * 
	 * @param unsetFile new value to set.
	 */
	public void setUnsetCompilable(JMenuItem unsetFile) {
		_unsetCompilableFile = unsetFile;
	}

	/**
	 * Returns the save as project menu item.
	 * 
	 * @return the save as project menu item.
	 */
	public JMenuItem getSaveAsProject() {
		return _saveProjectAs;
	}

	/**
	 * Sets a new value to the save as project menu item.
	 * 
	 * @param saveAsProject new value to set.
	 */
	public void setSaveAsProject(JMenuItem saveAsProject) {
		_saveProjectAs = saveAsProject;
	}

	/**
	 * Sets a new value to the delete file menu item.
	 * 
	 * @param deleteFile new value to set.
	 */
	public void setDeleteFile(JMenuItem deleteFile) {
		_deleteFile = deleteFile;
	}

	/**
	 * Returns the delete file menu item.
	 * 
	 * @return the delete file menu item.
	 */
	public JMenuItem getDeleteFile() {
		return _deleteFile;
	}

	/**
	 * Returns the unset main menu item.
	 * 
	 * @return the unset main menu item.
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMainFile;
	}

	/**
	 * Sets a new value to the unset main menu item.
	 * 
	 * @param unsetMain new value to set.
	 */
	public void setUnsetMain(JMenuItem unsetMain) {
		_unsetMainFile = unsetMain;
	}
}