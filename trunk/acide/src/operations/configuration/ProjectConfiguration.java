package operations.configuration;

import es.text.TextFile;
import gui.MainWindow;
import java.util.ArrayList;

import properties.PropertiesManager;

/**
 *
 */
public class ProjectConfiguration {

	/**
	 * 
	 */
	private String _name;
	/**
	 * 
	 */
	private String _path;
	/**
	 * 
	 */
	private String _languageConfiguration;
	/**
	 * 
	 */
	private String _grammarConfiguration;
	/**
	 * 
	 */
	private String _compilerPath;
	/**
	 * 
	 */
	private String _compilerArguments;
	/**
	 * 
	 */
	private String _shellPath;
	/**
	 * 
	 */
	private String _echoCommand;
	/**
	 * 
	 */
	private String _exitCommand;
	/**
	 * 
	 */
	private String _shellDir;
	/**
	 * 
	 */
	private boolean _checkCompiler;
	/**
	 * 
	 */
	private String _separatorFile;
	/**
	 * 
	 */
	private String _fileExtension;
	/**
	 * 
	 */
	private String _language;
	/**
	 * 
	 */
	private String _currentMenu;
	/**
	 * 
	 */
	private String _currentToolBar;
	/**
	 * 
	 */
	private boolean _firstSave;
	/**
	 * 
	 */
	private boolean _modified;
	/**
	 * 
	 */
	private boolean _explorer;
	/**
	 * 
	 */
	private boolean _shell;
	/**
	 * 
	 */
	private int _widthWindow;
	/**
	 * 
	 */
	private int _heightWindow;
	/**
	 * 
	 */
	private int _posX;
	/**
	 * 
	 */
	private int _posY;
	/**
	 * Panels
	 */
	private int _width1;
	/**
	 * Panels
	 */
	private int _height1;
	/**
	 * 
	 */
	private ArrayList<ExplorerFile> _fileList;
	/**
	 * 
	 */
	private String _numFiles;

	/**
	 * Constructor of the class.
	 */
	public ProjectConfiguration() {
		_fileList = new ArrayList<ExplorerFile>();
	}

	/**
	 * 
	 * 
	 * @return String
	 */
	public String save() {

		MainWindow mainWindow = MainWindow.getInstance();
		String text = "";
		text = text + getName() + "\n";
		text = text + _languageConfiguration + "\n";
		text = text + _grammarConfiguration + "\n";
		text = text + getCompilerPath() + "\n";
		text = text + getCompilerArguments() + "\n";
		text = text + getShellPath() + "\n";
		text = text + getShellDir() + "\n";
		text = text + getLanguage() + "\n";
		text = text + getCurrentMenu() + "\n";
		text = text + getCurrentToolBar() + "\n";

		if (mainWindow.getMenu().getView().getShowBrowser().isSelected())
			_explorer = true;
		else
			_explorer = false;
		text = text + isExplorer() + "\n";
		if (mainWindow.getMenu().getView().getShowShellWindow().isSelected())
			_shell = true;
		else
			_shell = false;
		text = text + isShell() + "\n";
		text = text + mainWindow.getWidth() + "\n";
		text = text + mainWindow.getHeight() + "\n";
		text = text + mainWindow.getX() + "\n";
		text = text + mainWindow.getY() + "\n";
		text = text + mainWindow.getSplitPaneVertical().getDividerLocation()
				+ "\n";
		text = text + mainWindow.getSplitPaneHorizontal().getDividerLocation()
				+ "\n";

		text = text + getNumFilesFromList() + "\n";

		// Add the associated files
		for (int i = 0; i < _fileList.size(); i++) {
			ExplorerFile f = (ExplorerFile) _fileList.get(i);
			text = text + f.getPath() + "\n" + f.getName() + "\n"
					+ f.getParent() + "\n" + f.isDirectory() + "\n"
					+ f.isSetFile() + "\n" + f.isMainFile() + "\n"
					+ f.isOpened() + "\n";
		}
		return text;
	}

	/**
	 * 
	 * @return String
	 */
	public void save2() {

		String project = null;
		try {
			project = PropertiesManager.getProperty("defaultAcideProject");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		if (!(project.equals("./configuration/default.acidePrj") && MainWindow
				.getInstance().getProjectConfiguration().getName().equals(""))) {

			// Project
			MainWindow mainWindow = MainWindow.getInstance();
			String t = null;
			String path = null;
			TextFile f = new TextFile();

			try {
				path = PropertiesManager.getProperty("defaultAcideProject");
				t = f.load(path);
			} catch (Exception e) {
				e.printStackTrace();
			}

			// Split the text in different lines
			String[] lines = t.split("\n");

			// Substitute values
			lines[12] = Integer.toString(mainWindow.getWidth());
			lines[13] = Integer.toString(mainWindow.getHeight());
			lines[14] = Integer.toString(mainWindow.getX());
			lines[15] = Integer.toString(mainWindow.getY());
			lines[16] = Integer.toString(mainWindow.getSplitPaneVertical()
					.getDividerLocation());
			lines[17] = Integer.toString(mainWindow.getSplitPaneHorizontal()
					.getDividerLocation());

			// Rebuild text
			String t2 = "";
			for (int i = 0; i < lines.length; i++) {
				t2 = t2 + lines[i] + "\n";
			}

			// Save text in file
			try {
				f.save(PropertiesManager.getProperty("defaultAcideProject"), t2);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * 
	 * @param t
	 */
	public void load(String t) {

		int cont = 0;
		int contf = 0;

		contf = t.indexOf("\n", cont);
		_name = t.substring(cont, contf);
		cont = contf + 1;

		// language path
		contf = t.indexOf("\n", cont);
		_languageConfiguration = t.substring(cont, contf);
//		if (!_languageConfiguration.equals("null")) {
//			ProgrammingLanguage l = ProgrammingLanguage.getInstance();
//			String leng = "./configuration/lexical/" + _languageConfiguration
//					+ ".xml";
//			l.load(leng);
//		}
		cont = contf + 1;

		// grammar path
		contf = t.indexOf("\n", cont);
		_grammarConfiguration = t.substring(cont, contf);
		cont = contf + 1;

		// path exec
		contf = t.indexOf("\n", cont);
		_compilerPath = t.substring(cont, contf);
		cont = contf + 1;

		// arguments
		contf = t.indexOf("\n", cont);
		_compilerArguments = t.substring(cont, contf);
		cont = contf + 1;

		// load path shell
		contf = t.indexOf("\n", cont);
		_shellPath = t.substring(cont, contf);
		cont = contf + 1;

		// load dir shell
		contf = t.indexOf("\n", cont);
		_shellDir = t.substring(cont, contf);
		cont = contf + 1;

		// load language
		contf = t.indexOf("\n", cont);
		_language = t.substring(cont, contf);
		cont = contf + 1;

		// load current menu
		contf = t.indexOf("\n", cont);
		_currentMenu = t.substring(cont, contf);
		cont = contf + 1;

		// load current ToolBar
		contf = t.indexOf("\n", cont);
		_currentToolBar = t.substring(cont, contf);
		cont = contf + 1;

		// Explorer
		contf = t.indexOf("\n", cont);
		String cond = t.substring(cont, contf);
		if (cond.equals("true"))
			_explorer = true;
		else
			_explorer = false;
		cont = contf + 1;

		// Shell
		contf = t.indexOf("\n", cont);
		cond = t.substring(cont, contf);
		if (cond.equals("true"))
			_shell = true;
		else
			_shell = false;
		cont = contf + 1;

		// Width
		contf = t.indexOf("\n", cont);
		cond = t.substring(cont, contf);
		_widthWindow = Integer.parseInt(cond);
		cont = contf + 1;

		// Height
		contf = t.indexOf("\n", cont);
		cond = t.substring(cont, contf);
		_heightWindow = Integer.parseInt(cond);
		cont = contf + 1;

		// Position x Window
		contf = t.indexOf("\n", cont);
		cond = t.substring(cont, contf);
		_posX = Integer.parseInt(cond);
		cont = contf + 1;

		// Position y Window
		contf = t.indexOf("\n", cont);
		cond = t.substring(cont, contf);
		_posY = Integer.parseInt(cond);
		cont = contf + 1;

		// Width1
		contf = t.indexOf("\n", cont);
		cond = t.substring(cont, contf);
		_width1 = Integer.parseInt(cond);
		cont = contf + 1;
		// Height1
		contf = t.indexOf("\n", cont);
		cond = t.substring(cont, contf);
		_height1 = Integer.parseInt(cond);
		cont = contf + 1;

		// Load Related Files
		contf = t.indexOf("\n", cont);
		_numFiles = t.substring(cont, contf);
		cont = contf + 1;

		boolean check;
		boolean main;
		String name;
		String path;
		String parent;
		boolean dir;
		boolean opened;

		_fileList.clear();
				
		for (int i = 0; i < Integer.parseInt(getNumFiles()); i++) {
			
			ExplorerFile fi = new ExplorerFile();
			contf = t.indexOf("\n", cont);
			path = t.substring(cont, contf);
			cont = contf + 1;
			contf = t.indexOf("\n", cont);
			name = t.substring(cont, contf);
			cont = contf + 1;
			contf = t.indexOf("\n", cont);
			parent = t.substring(cont, contf);
			cont = contf + 1;
			contf = t.indexOf("\n", cont);
			if (Boolean.parseBoolean(t.substring(cont, contf)) == true)
				dir = true;
			else
				dir = false;
			cont = contf + 1;
			contf = t.indexOf("\n", cont);
			if (Boolean.parseBoolean(t.substring(cont, contf)) == true)
				check = true;
			else
				check = false;
			cont = contf + 1;
			contf = t.indexOf("\n", cont);
			if (Boolean.parseBoolean(t.substring(cont, contf)) == true)
				main = true;
			else
				main = false;
			cont = contf + 1;
			contf = t.indexOf("\n", cont);
			if (Boolean.parseBoolean(t.substring(cont, contf)) == true)
				opened = true;
			else
				opened = false;
			cont = contf + 1;
			fi.setMainFile(main);
			fi.setSetFile(check);
			fi.setPath(path);
			fi.setParent(parent);
			fi.setName(name);
			fi.setDirectory(dir);
			fi.setOpened(opened);
			_fileList.add(fi);
		}
	}

	/**
	 * 
	 * @return
	 */
	public String getName() {
		return _name;
	}

	/**
	 * 
	 * @return
	 */
	public String getProjectPath() {
		return _path;
	}

	/**
	 * 
	 * @return
	 */
	public String getNumFiles() {
		if (_numFiles != null && !_numFiles.matches("null"))
			return _numFiles;
		return "0";
	}

	/**
	 * 
	 * @return
	 */
	public int getFileListSize() {
		return _fileList.size();
	}

	/**
	 * 
	 * @param numFiles
	 */
	public void setNumFiles(String numFiles) {
		_numFiles = numFiles;
	}

	/**
	 * 
	 * @return
	 */
	public String getCompilerPath() {
		return _compilerPath;
	}

	/**
	 * 
	 * @return
	 */
	public String getShellPath() {
		return _shellPath;
	}

	/**
	 * 
	 * @return
	 */
	public String getCompilerArguments() {
		return _compilerArguments;
	}

	/**
	 * 
	 * @param index
	 * @return
	 */
	public ExplorerFile getFile(int index) {
		return _fileList.get(index);
	}

	/**
	 * 
	 * @return
	 */
	public String getLanguageConfiguration() {
		return _languageConfiguration;
	}

	/**
	 * 
	 * @param np
	 */
	public void setName(String np) {
		_name = np;
	}

	/**
	 * 
	 * @param pp
	 */
	public void setPath(String pp) {
		_path = pp;
	}

	/**
	 * 
	 * @param pe
	 */
	public void setCompilerPath(String pe) {
		_compilerPath = pe;
	}

	/**
	 * 
	 * @param a
	 */
	public void setCompilerArguments(String a) {
		_compilerArguments = a;
	}

	/**
	 * 
	 * @param s
	 */
	public void setShellPath(String s) {
		_shellPath = s;
	}

	/**
	 * 
	 * @param f
	 */
	public void setFile(ExplorerFile f) {
		_fileList.add(f);
	}

	/**
	 * 
	 * @return
	 */
	public int getNumFilesFromList() {
		return _fileList.size();
	}

	/**
	 * 
	 */
	public void removeFiles() {
		_fileList.clear();
	}

	/**
	 * 
	 * @param cont
	 */
	public void removeFileAt(int cont) {
		_fileList.remove(cont);
	}

	/**
	 * 
	 * @return
	 */
	public String getGrammarConfiguration() {
		return _grammarConfiguration;
	}

	/**
	 * 
	 * @param grammarConfig
	 */
	public void setGrammarConfiguration(String grammarConfig) {
		_grammarConfiguration = grammarConfig;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCheckCompiler() {
		return _checkCompiler;
	}

	/**
	 * 
	 * @param checkCompiler
	 */
	public void setCheckCompiler(boolean checkCompiler) {
		_checkCompiler = checkCompiler;
	}

	/**
	 * 
	 * @return
	 */
	public String getSeparatorFile() {
		return _separatorFile;
	}

	/**
	 * 
	 * @param separatorFile
	 */
	public void setSeparatorFile(String separatorFile) {
		_separatorFile = separatorFile;
	}

	/**
	 * 
	 * @return
	 */
	public String getExtensionFile() {
		return _fileExtension;
	}

	/**
	 * 
	 * @param extensionFile
	 */
	public void setFileExtension(String extensionFile) {
		_fileExtension = extensionFile;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isFirstSave() {
		return _firstSave;
	}

	/**
	 * 
	 * @param firstSave
	 */
	public void setFirstSave(boolean firstSave) {
		_firstSave = firstSave;
	}

	/**
	 * 
	 * @return
	 */
	public String getCurrentMenu() {
		return _currentMenu;
	}

	/**
	 * 
	 * @param currentMenu
	 */
	public void setCurrentMenu(String currentMenu) {
		_currentMenu = currentMenu;
	}

	/**
	 * 
	 * @return
	 */
	public String getCurrentToolBar() {
		return _currentToolBar;
	}

	/**
	 * 
	 * @param currentToolBar
	 */
	public void setCurrentToolBar(String currentToolBar) {
		_currentToolBar = currentToolBar;
	}

	/**
	 * 
	 * @return
	 */
	public String getLanguage() {
		return _language;
	}

	/**
	 * 
	 * @param language
	 */
	public void setLanguage(String language) {
		_language = language;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isModified() {
		return _modified;
	}

	/**
	 * 
	 * @param b
	 */
	public void setModified(boolean b) {
		_modified = b;
	}

	/**
	 * 
	 * @param string
	 */
	public void setEchoCommand(String string) {
		_echoCommand = string;
	}

	/**
	 * 
	 * @param text
	 */
	public void setExitCommand(String text) {
		_exitCommand = text;
	}

	/**
	 * 
	 * @param text
	 */
	public void setShellDir(String text) {
		_shellDir = text;
	}

	/**
	 * 
	 * @return
	 */
	public String getEchoCommand() {
		return _echoCommand;
	}

	/**
	 * 
	 * @return
	 */
	public String getExitCommand() {
		return _exitCommand;
	}

	/**
	 * 
	 * @return
	 */
	public String getShellDir() {
		return _shellDir;
	}

	/**
	 * 
	 * @return
	 */
	public int getHeightWindow() {
		return _heightWindow;
	}

	/**
	 * 
	 * @param heightWindow
	 */
	public void setHeightWindow(int heightWindow) {
		_heightWindow = heightWindow;
	}

	/**
	 * 
	 * @return
	 */
	public int getPosX() {
		return _posX;
	}

	/**
	 * 
	 * @param posx
	 */
	public void setPosX(int posx) {
		_posX = posx;
	}

	/**
	 * 
	 * @return
	 */
	public int getPosY() {
		return _posY;
	}

	/**
	 * 
	 * @param posy
	 */
	public void setPosY(int posy) {
		_posY = posy;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isShell() {
		return _shell;
	}

	/**
	 * 
	 * @param shell
	 */
	public void setShell(boolean shell) {
		_shell = shell;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isExplorer() {
		return _explorer;
	}

	/**
	 * 
	 * @param e
	 */
	public void setExplorer(boolean e) {
		_explorer = e;
	}

	/**
	 * 
	 * @return
	 */
	public int getWidthWindow() {
		return _widthWindow;
	}

	/**
	 * 
	 * @param widthWindow
	 */
	public void setWidthWindow(int widthWindow) {
		_widthWindow = widthWindow;
	}

	/**
	 * 
	 * @return
	 */
	public int getHeight1() {
		return _height1;
	}

	/**
	 * 
	 * @param h
	 */
	public void setHeight1(int h) {
		_height1 = h;
	}

	/**
	 * 
	 * @return
	 */
	public int getWidth1() {
		return _width1;
	}

	/**
	 * 
	 * @param h
	 */
	public void setWidth1(int h) {
		_width1 = h;
	}

	/**
	 * 
	 * @param languageConfiguration
	 */
	public void setLanguageConfiguration(String languageConfiguration) {
		_languageConfiguration = languageConfiguration;
	}
}