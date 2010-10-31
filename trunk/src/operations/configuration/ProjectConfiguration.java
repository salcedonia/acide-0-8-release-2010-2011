package operations.configuration;

import es.text.TextFile;
import gui.MainWindow;
import java.util.ArrayList;

import properties.PropertiesManager;

/**
 * Store and handle all the information about the configuration of the project.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ProjectConfiguration {

	/**
	 * Project name.
	 */
	private String _name;
	/**
	 * Project path.
	 */
	private String _path;
	/**
	 * Lexical configuration.
	 */
	private String _lexicalConfiguration;
	/**
	 * Syntactic configuration.
	 */
	private String _syntacticConfiguration;
	/**
	 * Compiler path.
	 */
	private String _compilerPath;
	/**
	 * Arguments for the compiler.
	 */
	private String _compilerArguments;
	/**
	 * Shell path.
	 */
	private String _shellPath;
	/**
	 * Echo command to display in the output.
	 */
	private String _echoCommand;
	/**
	 * Exit command for the output.
	 */
	private String _exitCommand;
	/**
	 * Shell directory.
	 */
	private String _shellDirectory;
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
	private String _menu;
	/**
	 * Tool Bar configuration.
	 */
	private String _toolBar;
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
	 * Constructor of the class.
	 */
	public ProjectConfiguration() {
		_fileList = new ArrayList<ExplorerFile>();
	}

	/**
	 * Save the project configuration in a string.
	 * 
	 * @return The file content with the project configuration to be saved.
	 */
	public String save() {

		MainWindow mainWindow = MainWindow.getInstance();

		String fileContent = "";
		fileContent = fileContent + getName() + "\n";
		fileContent = fileContent + _lexicalConfiguration + "\n";
		fileContent = fileContent + _syntacticConfiguration + "\n";
		fileContent = fileContent + getCompilerPath() + "\n";
		fileContent = fileContent + getCompilerArguments() + "\n";
		fileContent = fileContent + getShellPath() + "\n";
		fileContent = fileContent + getShellDirectory() + "\n";
		fileContent = fileContent + getLanguage() + "\n";
		fileContent = fileContent + getMenu() + "\n";
		fileContent = fileContent + getToolBar() + "\n";

		// IF THE EXPLORER IS SHOWN
		if (mainWindow.getMenu().getView().getShowBrowser().isSelected())
			_isExplorerShowed = true;
		else
			_isExplorerShowed = false;
		fileContent = fileContent + isExplorerShowed() + "\n";

		// IF THE SHELL IS SHOWN.
		if (mainWindow.getMenu().getView().getShowShellWindow().isSelected())
			_isShellShowed = true;
		else
			_isShellShowed = false;
		fileContent = fileContent + isShellShowed() + "\n";

		// MAIN WINDOW PARAMETERS
		fileContent = fileContent + mainWindow.getWidth() + "\n";
		fileContent = fileContent + mainWindow.getHeight() + "\n";
		fileContent = fileContent + mainWindow.getX() + "\n";
		fileContent = fileContent + mainWindow.getY() + "\n";
		fileContent = fileContent
				+ mainWindow.getSplitPaneVertical().getDividerLocation() + "\n";
		fileContent = fileContent
				+ mainWindow.getSplitPaneHorizontal().getDividerLocation()
				+ "\n";

		// FILES ASSOCIATED TO THE PROJECT
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
	 * Save the Main Window configuration parameters in the configuration file.
	 */
	public void saveMainWindowParameters() {

		// IF THERE IS A OPENED PROJECT
		if (!isDefaultProject()) {

			// GET THE CONFIGURATION OF THE PROJECT FOR OVERWRITTING IT
			MainWindow mainWindow = MainWindow.getInstance();
			String fileContent = null;
			String path = null;
			TextFile textFile = new TextFile();

			try {
				path = PropertiesManager.getProperty("defaultAcideProject");
				fileContent = textFile.load(path);
			} catch (Exception e) {
				e.printStackTrace();
			}

			// SPLIT THE TEXT IN DIFFERENT LINES
			String[] lines = fileContent.split("\n");

			// SUBSTITUTE THE VALUES FOR THE NEW ONES
			lines[12] = Integer.toString(mainWindow.getWidth());
			lines[13] = Integer.toString(mainWindow.getHeight());
			lines[14] = Integer.toString(mainWindow.getX());
			lines[15] = Integer.toString(mainWindow.getY());
			lines[16] = Integer.toString(mainWindow.getSplitPaneVertical()
					.getDividerLocation());
			lines[17] = Integer.toString(mainWindow.getSplitPaneHorizontal()
					.getDividerLocation());

			// REBUILD THE FILE CONTENT
			String newFileContent = "";
			for (int i = 0; i < lines.length; i++)
				newFileContent = newFileContent + lines[i] + "\n";

			// SAVE TEXT IN FILE
			try {
				textFile.save(
						PropertiesManager.getProperty("defaultAcideProject"),
						newFileContent);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Return true if the current project is "./configuration/default.acidePrj" and the 
	 * name is "".
	 * 
	 * @return True if it has the default project and false in other case.
	 */
	public boolean isDefaultProject(){
		
		// GET THE PROJECT CONFIGURATION
		String project = null;
		try {
			project = PropertiesManager.getProperty("defaultAcideProject");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		return project.equals("./configuration/default.acidePrj") || MainWindow
				.getInstance().getProjectConfiguration().getName().equals("");
	}
	
	/**
	 * Load the project configuration from the file content given as a
	 * parameter.
	 * 
	 * @param fileContent
	 *            File content which contains all the project configuration to
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
		_lexicalConfiguration = fileContent.substring(initialPosition,
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

		// SHELL PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_shellPath = fileContent.substring(initialPosition, finalPosition);

		// SHELL DIRECTORY
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_shellDirectory = fileContent.substring(initialPosition, finalPosition);

		// LANGUAGE OF THE APPLICATION
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_language = fileContent.substring(initialPosition, finalPosition);

		// MENU CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_menu = fileContent.substring(initialPosition, finalPosition);

		// TOOLBAR CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_toolBar = fileContent.substring(initialPosition, finalPosition);

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
	 * Returns the name.
	 * 
	 * @return The name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Returns the project path.
	 * 
	 * @return The project path.
	 */
	public String getProjectPath() {
		return _path;
	}


	/**
	 * Returns the file list size.
	 * 
	 * @return The file list size.
	 */
	public int getFileListSize() {
		return _fileList.size();
	}

	/**
	 * Returns the compiler path.
	 * 
	 * @return The compiler path.
	 */
	public String getCompilerPath() {
		return _compilerPath;
	}

	/**
	 * Returns the shell path.
	 * 
	 * @return The shell path.
	 */
	public String getShellPath() {
		return _shellPath;
	}

	/**
	 * Returns the compiler arguments.
	 * 
	 * @return The compiler arguments.
	 */
	public String getCompilerArguments() {
		return _compilerArguments;
	}

	/**
	 * Returns the file from a list in the position given as a parameter.
	 * 
	 * @param index
	 *            Position of the file.
	 * 
	 * @return The file from a list in the position given as a parameter.
	 */
	public ExplorerFile getFileAt(int index) {
		return _fileList.get(index);
	}

	/**
	 * Returns the lexical configuration.
	 * 
	 * @return The lexical configuration.
	 */
	public String getLexicalConfiguration() {
		return _lexicalConfiguration;
	}

	/**
	 * Set a new value to the name given as a parameter.
	 * 
	 * @param name
	 *            New value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Set a new value to the path given as a parameter.
	 * 
	 * @param path
	 *            New value to set.
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * Set a new value to the compiler path given as a parameter.
	 * 
	 * @param path
	 *            New value to set.
	 */
	public void setCompilerPath(String path) {
		_compilerPath = path;
	}

	/**
	 * Set a new value to the compiler arguments given as a parameter.
	 * 
	 * @param a
	 *            New value to set.
	 */
	public void setCompilerArguments(String a) {
		_compilerArguments = a;
	}

	/**
	 * Set a new value to the shell path.
	 * 
	 * @param s
	 *            New value to set.
	 */
	public void setShellPath(String s) {
		_shellPath = s;
	}

	/**
	 * Add a file to the list.
	 * 
	 * @param f
	 *            New file to add.
	 */
	public void addFile(ExplorerFile f) {
		_fileList.add(f);
	}

	/**
	 * Returns the number of files from the list of files.
	 * 
	 * @return The number of files from the list of files.
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
	 * @param pos
	 *            Position of the file to remove.
	 */
	public void removeFileAt(int pos) {
		_fileList.remove(pos);
	}

	/**
	 * Returns the syntactic configuration.
	 * 
	 * @return The syntactic configuration.
	 */
	public String getSyntacticConfiguration() {
		return _syntacticConfiguration;
	}

	/**
	 * Set a new value to the syntactic configurable.
	 * 
	 * @param syntacticConfiguration
	 *            New value to set.
	 */
	public void setSyntacticConfiguration(String syntacticConfiguration) {
		_syntacticConfiguration = syntacticConfiguration;
	}

	/**
	 * Returns the is check compiler flag.
	 * 
	 * @return The is check compiler flag.
	 */
	public boolean isCheckCompiler() {
		return _checkCompiler;
	}

	/**
	 * Set a new value to the is check compiler flag.
	 * 
	 * @param checkCompiler
	 *            New value to set.
	 */
	public void setCheckCompiler(boolean checkCompiler) {
		_checkCompiler = checkCompiler;
	}

	/**
	 * Returns the separator file.
	 * 
	 * @return The separator file.
	 */
	public String getSeparatorFile() {
		return _separatorFile;
	}

	/**
	 * Set a new value to the separator file.
	 * 
	 * @param separatorFile
	 *            New value to set.
	 */
	public void setSeparatorFile(String separatorFile) {
		_separatorFile = separatorFile;
	}

	/**
	 * Returns the file extension.
	 * 
	 * @return The file extension.
	 */
	public String getFileExtension() {
		return _fileExtension;
	}

	/**
	 * Set a new value to the file extension.
	 * 
	 * @param fileExtension
	 *            New value to set.
	 */
	public void setFileExtension(String fileExtension) {
		_fileExtension = fileExtension;
	}

	/**
	 * Returns the is first save flag.
	 * 
	 * @return The is first save flag.
	 */
	public boolean isFirstSave() {
		return _isFirstSave;
	}

	/**
	 * Set a new value to the is first save flag.
	 * 
	 * @param firstSave
	 *            New value to set.
	 */
	public void setFirstSave(boolean firstSave) {
		_isFirstSave = firstSave;
	}

	/**
	 * Returns the menu.
	 * 
	 * @return The menu.
	 */
	public String getMenu() {
		return _menu;
	}

	/**
	 * Set a new value to the menu.
	 * 
	 * @param menu
	 *            New value to set.
	 */
	public void setMenu(String menu) {
		_menu = menu;
	}

	/**
	 * Returns the tool bar.
	 * 
	 * @return The tool bar.
	 */
	public String getToolBar() {
		return _toolBar;
	}

	/**
	 * Set a new value to the tool bar.
	 * 
	 * @param toolBar
	 *            New value to set.
	 */
	public void setToolBar(String toolBar) {
		_toolBar = toolBar;
	}

	/**
	 * Returns the language.
	 * 
	 * @return The language.
	 */
	public String getLanguage() {
		return _language;
	}

	/**
	 * Set a new value to the language.
	 * 
	 * @param language
	 *            New value to set.
	 */
	public void setLanguage(String language) {
		_language = language;
	}

	/**
	 * Return the is modified flag.
	 * 
	 * @return The is modified flag.
	 */
	public boolean isModified() {
		return _isModified;
	}

	/**
	 * Set a new value to the is modified flag.
	 * 
	 * @param isModified
	 *            New value to set.
	 */
	public void setIsModified(boolean isModified) {
		_isModified = isModified;
	}

	/**
	 * Set a new value to the echo command.
	 * 
	 * @param echoCommand
	 *            New value to set.
	 */
	public void setEchoCommand(String echoCommand) {
		_echoCommand = echoCommand;
	}

	/**
	 * Set a new value to exit command.
	 * 
	 * @param exitCommand
	 *            New value to set.
	 */
	public void setExitCommand(String exitCommand) {
		_exitCommand = exitCommand;
	}

	/**
	 * Set a new value to the shell directory.
	 * 
	 * @param shellDirectory
	 *            New value to set.
	 */
	public void setShellDirectory(String shellDirectory) {
		_shellDirectory = shellDirectory;
	}

	/**
	 * Returns the echo command.
	 * 
	 * @return The echo command.
	 */
	public String getEchoCommand() {
		return _echoCommand;
	}

	/**
	 * Returns the exit command.
	 * 
	 * @return The exit command.
	 */
	public String getExitCommand() {
		return _exitCommand;
	}

	/**
	 * Returns the shell directory.
	 * 
	 * @return The shell directory.
	 */
	public String getShellDirectory() {
		return _shellDirectory;
	}

	/**
	 * Returns the window height.
	 * 
	 * @return The window height.
	 */
	public int getWindowHeight() {
		return _windowHeight;
	}

	/**
	 * Set a new value for the window height.
	 * 
	 * @param windowHeight
	 *            New value to set.
	 */
	public void setWindowHeight(int windowHeight) {
		_windowHeight = windowHeight;
	}

	/**
	 * Returns the x position of the window.
	 * 
	 * @return The x position of the window.
	 */
	public int getPosX() {
		return _posX;
	}

	/**
	 * Set a new value to the x position of the window.
	 * 
	 * @param posX
	 *            New value to set.
	 */
	public void setPosX(int posX) {
		_posX = posX;
	}

	/**
	 * Returns the y position of the window.
	 * 
	 * @return The y position of the window.
	 */
	public int getPosY() {
		return _posY;
	}

	/**
	 * Set a new value to the y position of the window.
	 * 
	 * @param posY
	 *            New value to set.
	 */
	public void setPosY(int posY) {
		_posY = posY;
	}

	/**
	 * Returns the showed shell flag.
	 * 
	 * @return The showed shell flag.
	 */
	public boolean isShellShowed() {
		return _isShellShowed;
	}

	/**
	 * Set a new value to the showed shell flag.
	 * 
	 * @param isShellShowed
	 *            New value to set.
	 */
	public void setIsShellShowed(boolean isShellShowed) {
		_isShellShowed = isShellShowed;
	}

	/**
	 * Returns the explorer showed flag.
	 * 
	 * @return The explorer showed flag.
	 */
	public boolean isExplorerShowed() {
		return _isExplorerShowed;
	}

	/**
	 * Set a new value to the flag of the explorer showed.
	 * 
	 * @param isExplorerShowed
	 *            New value to set.
	 */
	public void setIsExplorerShowed(boolean isExplorerShowed) {
		_isExplorerShowed = isExplorerShowed;
	}

	/**
	 * Returns the window width.
	 * 
	 * @return The window width.
	 */
	public int getWindowWidth() {
		return _windowWidth;
	}

	/**
	 * Set a new value for the window width.
	 * 
	 * @param windowWidth
	 *            New value to set.
	 */
	public void setWindowWidth(int windowWidth) {
		_windowWidth = windowWidth;
	}

	/**
	 * Returns the splitPaneHorizontal location.
	 * 
	 * @return The splitPaneHorizontal location.
	 */
	public int getSplitPanelHorizontalDividerLocation() {
		return _splitPaneHorizontalDividerLocation;
	}

	/**
	 * Set a new value for the splitPaneHorizontalDividerLocation.
	 * 
	 * @param splitPaneHorizontalDividerLocation
	 *            New value to set.
	 */
	public void setSplitPaneHorizontalDividerLocation(
			int splitPaneHorizontalDividerLocation) {
		_splitPaneHorizontalDividerLocation = splitPaneHorizontalDividerLocation;
	}

	/**
	 * Returns the splitPaneVertical location.
	 * 
	 * @return The splitPaneVertical location.
	 */
	public int getSplitPaneVerticalDividerLocation() {
		return _splitPaneVerticalDividerLocation;
	}

	/**
	 * Set a new value for the splitPaneVerticalDividerLocation.
	 * 
	 * @param splitPaneVerticalDividerLocation
	 *            New value to set.
	 */
	public void setSplitPaneVerticalDividerLocation(
			int splitPaneVerticalDividerLocation) {
		_splitPaneVerticalDividerLocation = splitPaneVerticalDividerLocation;
	}

	/**
	 * Set a new value for the lexicalConfiguration.
	 * 
	 * @param lexicalConfiguration
	 *            New value to set.
	 */
	public void setLexicalConfiguration(String lexicalConfiguration) {
		_lexicalConfiguration = lexicalConfiguration;
	}
}