package es.configuration.project;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.util.ArrayList;

import operations.log.AcideLog;
import resources.ResourceManager;


/************************************************************************																
 * Project configuration of ACIDE - A Configurable IDE.											
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
public class AcideProjectConfiguration {

	/**
	 * Project name.
	 */
	private String _name;
	/**
	 * Project path.
	 */
	private String _path;
	/**
	 * Lexicon configuration.
	 */
	private String _lexiconConfiguration;
	/**
	 * Syntactic configuration.
	 */
	private String _syntacticConfiguration;
	/**
	 * Output configuration.
	 */
	private String _outputConfiguration;
	/**
	 * Compiler path.
	 */
	private String _compilerPath;
	/**
	 * Arguments for the compiler.
	 */
	private String _compilerArguments;
	/**
	 * Flag that indicates if the compiler is marked or not.
	 */
	private boolean _checkCompiler;
	/**
	 * Separator file for the compiler.
	 */
	private String _separatorFile;
	/**
	 * File extensions valid for the project.
	 */
	private String _fileExtension;
	/**
	 * Language of the application.
	 */
	private String _language;
	/**
	 * Menu configuration.
	 */
	private String _menuConfiguration;
	/**
	 * Tool Bar configuration.
	 */
	private String _toolBarConfiguration;
	/**
	 * Flag that indicates if it is the first time that the configuration has
	 * been saved or not.
	 */
	private boolean _isFirstSave;
	/**
	 * Flag that indicates if the configuration has been modified or not.
	 */
	private boolean _isModified;
	/**
	 * Flag that indicates if the Explorer is showed or not.
	 */
	private boolean _isExplorerShowed;
	/**
	 * Flag that indicates if the Shell Output is showed or not.
	 */
	private boolean _isShellShowed;
	/**
	 * Window width.
	 */
	private int _windowWidth;
	/**
	 * Window height.
	 */
	private int _windowHeight;
	/**
	 * X position for the window.
	 */
	private int _posX;
	/**
	 * Y position for the window.
	 */
	private int _posY;
	/**
	 * Panel Width.
	 */
	private int _splitPaneVerticalDividerLocation;
	/**
	 * Panel Height.
	 */
	private int _splitPaneHorizontalDividerLocation;
	/**
	 * File list of the files which belongs to the project.
	 */
	private ArrayList<ExplorerFile> _fileList;
	/**
	 * Selected editor index.
	 */
	private int _selectedEditorIndex;

	/**
	 * Creates a new project configuration.
	 */
	public AcideProjectConfiguration() {
		_fileList = new ArrayList<ExplorerFile>();
	}

	/**
	 * Saves the project configuration in a string.
	 * 
	 * @return the file content with the project configuration to be saved.
	 */
	public String save() {

		String fileContent = "";
		fileContent = fileContent + _name + "\n";
		fileContent = fileContent + _lexiconConfiguration + "\n";
		fileContent = fileContent + _syntacticConfiguration + "\n";
		fileContent = fileContent + _compilerPath + "\n";
		fileContent = fileContent + _compilerArguments + "\n";
		fileContent = fileContent + _outputConfiguration + "\n";
		fileContent = fileContent + _language + "\n";
		fileContent = fileContent + _menuConfiguration + "\n";
		fileContent = fileContent + _toolBarConfiguration + "\n";

		// If the explorer is shown
		if (MainWindow.getInstance().getMenu().getView().getShowExplorerPanel()
				.isSelected())
			_isExplorerShowed = true;
		else
			_isExplorerShowed = false;
		fileContent = fileContent + isExplorerShowed() + "\n";

		// If the shell is shown
		if (MainWindow.getInstance().getMenu().getView().getShowShellWindow()
				.isSelected())
			_isShellShowed = true;
		else
			_isShellShowed = false;
		fileContent = fileContent + isShellShowed() + "\n";

		// MAIN WINDOW WIDTH
		fileContent = fileContent + MainWindow.getInstance().getWidth() + "\n";
		
		// MAIN WINDOW HEIGHT
		fileContent = fileContent + MainWindow.getInstance().getHeight() + "\n";
		
		// MAIN WINDOW X
		fileContent = fileContent + MainWindow.getInstance().getX() + "\n";
		
		// MAIN WINDOW Y
		fileContent = fileContent + MainWindow.getInstance().getY() + "\n";
		
		// MAIN WINDOW VERTICAL SPLIT PANEL
		fileContent = fileContent
				+ MainWindow.getInstance().getSplitPaneVertical()
						.getDividerLocation() + "\n";
		
		// MAIN WINDOW HORIZONTAL SPLIT PANEL
		fileContent = fileContent
				+ MainWindow.getInstance().getSplitPaneHorizontal()
						.getDividerLocation() + "\n";

		// SELECTED EDITOR
		
		// Avoids the new and the log tab if they are displayed and selects the previous
		// opened tab
		int selectedEditorIndex = MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanelIndex() ;
		

		// if(MainWindow.getInstance().getFileEditorManager().getSelectedEditor().isNewFile()){
		//
		// selectedEditorIndex--;
		//
		// if(selectedEditorIndex != -1 &&
		// MainWindow.getInstance().getFileEditorManager().getEditorAt(selectedEditorIndex).isLogFile())
		// selectedEditorIndex--;
		// }
		// else
		// if(MainWindow.getInstance().getFileEditorManager().getSelectedEditor().isLogFile()){
		//
		// selectedEditorIndex--;
		//
		// if(selectedEditorIndex != -1 &&
		// MainWindow.getInstance().getFileEditorManager().getEditorAt(selectedEditorIndex).isNewFile())
		// selectedEditorIndex--;
		// }
					
		fileContent = fileContent
		+ selectedEditorIndex + "\n";
		
		// Files associated to the project
		fileContent = fileContent + getNumFilesFromList() + "\n";

		for (int i = 0; i < _fileList.size(); i++) {
			ExplorerFile f = (ExplorerFile) _fileList.get(i);
			fileContent = fileContent + f.getPath() + "\n" + f.getName() + "\n"
					+ f.getParent() + "\n" + f.isDirectory() + "\n"
					+ f.isCompilableFile() + "\n" + f.isMainFile() + "\n"
					+ f.isOpened() + "\n";
		}

		return fileContent;
	}

	/**
	 * Saves the Main Window configuration parameters in the configuration file.
	 */
	public void saveMainWindowParameters() {

		// Not default project
		if (!isDefaultProject()) {

			// Gets the project configuration to overwriting it
			String fileContent = null;
			String path = null;
			TextFile textFile = new TextFile();

			try {
				path = ResourceManager.getInstance().getProperty("defaultAcideProject");
				fileContent = textFile.load(path);
			} catch (Exception exception) {
				
				// Updates the Log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Splits the text in different lines
			String[] lines = fileContent.split("\n");

			// MAIN WINDOW WIDTH
			lines[11] = Integer.toString(MainWindow.getInstance().getWidth());
			
			// MAIN WINDOW HEIGHT
			lines[12] = Integer.toString(MainWindow.getInstance().getHeight());
			
			// MAIN WINDOW X
			lines[13] = Integer.toString(MainWindow.getInstance().getX());
			
			// MAIN WINDOW Y
			lines[14] = Integer.toString(MainWindow.getInstance().getY());
			
			// MAIN WINDOW VERTICAL SPLIT PANEL
			lines[15] = Integer.toString(MainWindow.getInstance()
					.getSplitPaneVertical().getDividerLocation());
			
			// MAIN WINDOW HORIZONTAL SPLIT PANEL
			lines[16] = Integer.toString(MainWindow.getInstance()
					.getSplitPaneHorizontal().getDividerLocation());

			// MAIN WINDOW SELECTED EDITOR
			lines[17] = Integer.toString(MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex());
			
			// Rebuilds the file content
			String newFileContent = "";
			for (int i = 0; i < lines.length; i++)
				newFileContent = newFileContent + lines[i] + "\n";

			// Saves the text in the file
			try {
				textFile.save(
						ResourceManager.getInstance().getProperty("defaultAcideProject"),
						newFileContent);
			} catch (Exception exception) {
				
				// Updates the Log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * Returns true if the current project is
	 * "./configuration/project/default.acidePrj" or the name is "".
	 * 
	 * @return true if it has the default project and false in other case.
	 */
	public boolean isDefaultProject() {

		// Gets the project configuration
		String project = null;
		try {
			project = ResourceManager.getInstance().getProperty("defaultAcideProject");
		} catch (Exception exception) {
			
			// Updates the Log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		return project.equals("./configuration/project/default.acidePrj")
				&& MainWindow.getInstance().getProjectConfiguration().getName()
						.equals("");
	}

	/**
	 * Loads the project configuration from the file content given as a
	 * parameter.
	 * 
	 * @param fileContent
	 *            file content which contains all the project configuration to
	 *            load.
	 */
	public void load(String fileContent) {

		int initialPosition = 0;
		int finalPosition = 0;

		// PROJECT NAME PATH
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_name = fileContent.substring(initialPosition, finalPosition);

		// LEXICAL CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_lexiconConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// GRAMMAR CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_syntacticConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// COMPILER PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_compilerPath = fileContent.substring(initialPosition, finalPosition);

		// COMPILER ARGUMENTS
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_compilerArguments = fileContent.substring(initialPosition,
				finalPosition);

		// OUTPUT CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_outputConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// LANGUAGE OF THE APPLICATION
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_language = fileContent.substring(initialPosition, finalPosition);

		// MENU CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_menuConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// TOOLBAR CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_toolBarConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// IS EXPLORER SHOWED
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		String cond = fileContent.substring(initialPosition, finalPosition);
		if (cond.equals("true"))
			_isExplorerShowed = true;
		else
			_isExplorerShowed = false;

		// IS SHELL SHOWED
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		cond = fileContent.substring(initialPosition, finalPosition);
		if (cond.equals("true"))
			_isShellShowed = true;
		else
			_isShellShowed = false;

		// WINDOW WIDTH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		cond = fileContent.substring(initialPosition, finalPosition);
		_windowWidth = Integer.parseInt(cond);

		// WINDOW HEIGHT
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		cond = fileContent.substring(initialPosition, finalPosition);
		_windowHeight = Integer.parseInt(cond);

		// POSITION X WINDOW
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		cond = fileContent.substring(initialPosition, finalPosition);
		_posX = Integer.parseInt(cond);

		// POSITION Y WINDOW
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		cond = fileContent.substring(initialPosition, finalPosition);
		_posY = Integer.parseInt(cond);

		// PANEL WIDTH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		cond = fileContent.substring(initialPosition, finalPosition);
		_splitPaneVerticalDividerLocation = Integer.parseInt(cond);

		// PANEL HEIGHT
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		cond = fileContent.substring(initialPosition, finalPosition);
		_splitPaneHorizontalDividerLocation = Integer.parseInt(cond);

		// SELECTED EDITOR
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		cond = fileContent.substring(initialPosition, finalPosition);
		_selectedEditorIndex = Integer.parseInt(cond);
		
		// RELATED FILES
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		String numFiles = fileContent.substring(initialPosition, finalPosition);

		initialPosition = finalPosition + 1;

		boolean isCompilable;
		boolean isMain;
		String name;
		String path;
		String parent;
		boolean isDirectory;
		boolean isOpened;

		_fileList.clear();

		for (int i = 0; i < Integer.parseInt(numFiles); i++) {

			ExplorerFile explorerFile = new ExplorerFile();
			finalPosition = fileContent.indexOf("\n", initialPosition);
			path = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;
			finalPosition = fileContent.indexOf("\n", initialPosition);
			name = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;
			finalPosition = fileContent.indexOf("\n", initialPosition);
			parent = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;
			finalPosition = fileContent.indexOf("\n", initialPosition);
			if (Boolean.parseBoolean(fileContent.substring(initialPosition,
					finalPosition)) == true)
				isDirectory = true;
			else
				isDirectory = false;
			initialPosition = finalPosition + 1;
			finalPosition = fileContent.indexOf("\n", initialPosition);
			if (Boolean.parseBoolean(fileContent.substring(initialPosition,
					finalPosition)) == true)
				isCompilable = true;
			else
				isCompilable = false;
			initialPosition = finalPosition + 1;
			finalPosition = fileContent.indexOf("\n", initialPosition);
			if (Boolean.parseBoolean(fileContent.substring(initialPosition,
					finalPosition)) == true)
				isMain = true;
			else
				isMain = false;
			initialPosition = finalPosition + 1;
			finalPosition = fileContent.indexOf("\n", initialPosition);
			if (Boolean.parseBoolean(fileContent.substring(initialPosition,
					finalPosition)) == true)
				isOpened = true;
			else
				isOpened = false;
			initialPosition = finalPosition + 1;
			explorerFile.setIsMainFile(isMain);
			explorerFile.setIsCompilableFile(isCompilable);
			explorerFile.setPath(path);
			explorerFile.setParent(parent);
			explorerFile.setName(name);
			explorerFile.setIsDirectory(isDirectory);
			explorerFile.setIsOpened(isOpened);
			_fileList.add(explorerFile);
		}
	}

	/**
	 * Returns the project name.
	 * 
	 * @return the project name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Returns the project path.
	 * 
	 * @return the project path.
	 */
	public String getProjectPath() {
		return _path;
	}

	/**
	 * Returns the file list size.
	 * 
	 * @return the file list size.
	 */
	public int getFileListSize() {
		return _fileList.size();
	}

	/**
	 * Returns the compiler path.
	 * 
	 * @return the compiler path.
	 */
	public String getCompilerPath() {
		return _compilerPath;
	}

	/**
	 * Returns the compiler arguments.
	 * 
	 * @return the compiler arguments.
	 */
	public String getCompilerArguments() {
		return _compilerArguments;
	}

	/**
	 * Returns the file from a list in the position given as a parameter.
	 * 
	 * @param index
	 *            position of the file.
	 * @return the file from a list in the position given as a parameter.
	 * @see ExplorerFile
	 */
	public ExplorerFile getFileAt(int index) {
		return _fileList.get(index);
	}

	/**
	 * Returns the lexical configuration.
	 * 
	 * @return the lexical configuration.
	 */
	public String getLexicalConfiguration() {
		return _lexiconConfiguration;
	}

	/**
	 * Sets a new value to the name given as a parameter.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Sets a new value to the path given as a parameter.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * Sets a new value to the compiler path given as a parameter.
	 * 
	 * @param compilerPath
	 *            new value to set.
	 */
	public void setCompilerPath(String compilerPath) {
		_compilerPath = compilerPath;
	}

	/**
	 * Sets a new value to the compiler arguments given as a parameter.
	 * 
	 * @param compilerArguments
	 *            new value to set.
	 */
	public void setCompilerArguments(String compilerArguments) {
		_compilerArguments = compilerArguments;
	}

	/**
	 * Adds a file to the list.
	 * 
	 * @param explorerFile
	 *            new file to add.
	 */
	public void addFile(ExplorerFile explorerFile) {
		_fileList.add(explorerFile);
	}

	/**
	 * Returns the number of files from the list of files.
	 * 
	 * @return the number of files from the list of files.
	 */
	public int getNumFilesFromList() {
		return _fileList.size();
	}

	/**
	 * Removes all the files from the list.
	 */
	public void removeFiles() {
		_fileList.clear();
	}

	/**
	 * Removes a file at the position of the list given as a parameter.
	 * 
	 * @param position
	 *            position of the file to remove.
	 */
	public void removeFileAt(int position) {
		_fileList.remove(position);
	}

	/**
	 * Returns the syntactic configuration.
	 * 
	 * @return the syntactic configuration.
	 */
	public String getSyntacticConfiguration() {
		return _syntacticConfiguration;
	}

	/**
	 * Sets a new value to the syntactic configurable.
	 * 
	 * @param syntacticConfiguration
	 *            new value to set.
	 */
	public void setSyntacticConfiguration(String syntacticConfiguration) {
		_syntacticConfiguration = syntacticConfiguration;
	}

	/**
	 * Returns the is check compiler flag.
	 * 
	 * @return the is check compiler flag.
	 */
	public boolean isCheckCompiler() {
		return _checkCompiler;
	}

	/**
	 * Sets a new value to the is check compiler flag.
	 * 
	 * @param checkCompiler
	 *            new value to set.
	 */
	public void setCheckCompiler(boolean checkCompiler) {
		_checkCompiler = checkCompiler;
	}

	/**
	 * Returns the separator file.
	 * 
	 * @return the separator file.
	 */
	public String getSeparatorFile() {
		return _separatorFile;
	}

	/**
	 * Sets a new value to the separator file.
	 * 
	 * @param separatorFile
	 *            new value to set.
	 */
	public void setSeparatorFile(String separatorFile) {
		_separatorFile = separatorFile;
	}

	/**
	 * Returns the file extension.
	 * 
	 * @return the file extension.
	 */
	public String getFileExtension() {
		return _fileExtension;
	}

	/**
	 * Sets a new value to the file extension.
	 * 
	 * @param fileExtension
	 *            new value to set.
	 */
	public void setFileExtension(String fileExtension) {
		_fileExtension = fileExtension;
	}

	/**
	 * Returns the is first save flag.
	 * 
	 * @return the is first save flag.
	 */
	public boolean isFirstSave() {
		return _isFirstSave;
	}

	/**
	 * Sets a new value to the is first save flag.
	 * 
	 * @param firstSave
	 *            new value to set.
	 */
	public void setFirstSave(boolean firstSave) {
		_isFirstSave = firstSave;
	}

	/**
	 * Returns the menu.
	 * 
	 * @return the menu.
	 */
	public String getMenu() {
		return _menuConfiguration;
	}

	/**
	 * Sets a new value to the menu.
	 * 
	 * @param menu
	 *            new value to set.
	 */
	public void setMenu(String menu) {
		_menuConfiguration = menu;
	}

	/**
	 * Returns the tool bar.
	 * 
	 * @return the tool bar.
	 */
	public String getToolBar() {
		return _toolBarConfiguration;
	}

	/**
	 * Sets a new value to the tool bar.
	 * 
	 * @param toolBar
	 *            new value to set.
	 */
	public void setToolBar(String toolBar) {
		_toolBarConfiguration = toolBar;
	}

	/**
	 * Returns the language.
	 * 
	 * @return the language.
	 */
	public String getLanguage() {
		return _language;
	}

	/**
	 * Sets a new value to the language.
	 * 
	 * @param language
	 *            new value to set.
	 */
	public void setLanguage(String language) {
		_language = language;
	}

	/**
	 * Returns the is modified flag.
	 * 
	 * @return the is modified flag.
	 */
	public boolean isModified() {
		return _isModified;
	}

	/**
	 * Sets a new value to the is modified flag.
	 * 
	 * @param isModified
	 *            new value to set.
	 */
	public void setIsModified(boolean isModified) {
		_isModified = isModified;
	}

	/**
	 * Returns the window height.
	 * 
	 * @return the window height.
	 */
	public int getWindowHeight() {
		return _windowHeight;
	}

	/**
	 * Sets a new value for the window height.
	 * 
	 * @param windowHeight
	 *            new value to set.
	 */
	public void setWindowHeight(int windowHeight) {
		_windowHeight = windowHeight;
	}

	/**
	 * Returns the x position of the window.
	 * 
	 * @return the x position of the window.
	 */
	public int getPosX() {
		return _posX;
	}

	/**
	 * Sets a new value to the x position of the window.
	 * 
	 * @param posX
	 *            new value to set.
	 */
	public void setPosX(int posX) {
		_posX = posX;
	}

	/**
	 * Returns the y position of the window.
	 * 
	 * @return the y position of the window.
	 */
	public int getPosY() {
		return _posY;
	}

	/**
	 * Sets a new value to the y position of the window.
	 * 
	 * @param posY
	 *            new value to set.
	 */
	public void setPosY(int posY) {
		_posY = posY;
	}

	/**
	 * Returns the showed shell flag.
	 * 
	 * @return the showed shell flag.
	 */
	public boolean isShellShowed() {
		return _isShellShowed;
	}

	/**
	 * Sets a new value to the showed shell flag.
	 * 
	 * @param isShellShowed
	 *            new value to set.
	 */
	public void setIsShellShowed(boolean isShellShowed) {
		_isShellShowed = isShellShowed;
	}

	/**
	 * Returns the explorer showed flag.
	 * 
	 * @return the explorer showed flag.
	 */
	public boolean isExplorerShowed() {
		return _isExplorerShowed;
	}

	/**
	 * Sets a new value to the flag of the explorer showed.
	 * 
	 * @param isExplorerShowed
	 *            new value to set.
	 */
	public void setIsExplorerShowed(boolean isExplorerShowed) {
		_isExplorerShowed = isExplorerShowed;
	}

	/**
	 * Returns the window width.
	 * 
	 * @return the window width.
	 */
	public int getWindowWidth() {
		return _windowWidth;
	}

	/**
	 * Sets a new value for the window width.
	 * 
	 * @param windowWidth
	 *            new value to set.
	 */
	public void setWindowWidth(int windowWidth) {
		_windowWidth = windowWidth;
	}

	/**
	 * Returns the splitPaneHorizontal location.
	 * 
	 * @return the splitPaneHorizontal location.
	 */
	public int getSplitPanelHorizontalDividerLocation() {
		return _splitPaneHorizontalDividerLocation;
	}

	/**
	 * Sets a new value for the splitPaneHorizontalDividerLocation.
	 * 
	 * @param splitPaneHorizontalDividerLocation
	 *            new value to set.
	 */
	public void setSplitPaneHorizontalDividerLocation(
			int splitPaneHorizontalDividerLocation) {
		_splitPaneHorizontalDividerLocation = splitPaneHorizontalDividerLocation;
	}

	/**
	 * Returns the splitPaneVertical location.
	 * 
	 * @return the splitPaneVertical location.
	 */
	public int getSplitPaneVerticalDividerLocation() {
		return _splitPaneVerticalDividerLocation;
	}

	/**
	 * Sets a new value for the splitPaneVerticalDividerLocation.
	 * 
	 * @param splitPaneVerticalDividerLocation
	 *            new value to set.
	 */
	public void setSplitPaneVerticalDividerLocation(
			int splitPaneVerticalDividerLocation) {
		_splitPaneVerticalDividerLocation = splitPaneVerticalDividerLocation;
	}

	/**
	 * Sets a new value for the lexicalConfiguration.
	 * 
	 * @param lexicalConfiguration
	 *            new value to set.
	 */
	public void setLexicalConfiguration(String lexicalConfiguration) {
		_lexiconConfiguration = lexicalConfiguration;
	}

	/**
	 * Sets a new value to the output configuration.
	 * 
	 * @param outputConfiguration
	 *            new value to set.
	 */
	public void setOutputConfiguration(String outputConfiguration) {
		_outputConfiguration = outputConfiguration;
	}

	/**
	 * Returns the output configuration.
	 * 
	 * @return the output configuration.
	 */
	public String getOutputConfiguration() {
		return _outputConfiguration;
	}

	/**
	 * Returns the selected editor index.
	 * 
	 * @return the selected editor index.
	 */
	public int getSelectedEditorIndex() {
		return _selectedEditorIndex;
	}
}