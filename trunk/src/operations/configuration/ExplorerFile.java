package operations.configuration;

import javax.swing.tree.TreePath;

/**
 * Handle the explorer files of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ExplorerFile {

	/**
	 * File path.
	 */
	private String _path;
	/**
	 * Flag that indicates if the file is a compilable file or not.
	 */
	private boolean _isCompilableFile;
	/**
	 * Flag that indicates if the file is a main file or not.
	 */
	private boolean _isMainFile;
	/**
	 * Name of the file.
	 */
	private String _name;
	/**
	 * Parent of the file.
	 */
	private String _parent;
	/**
	 * Flag that indicates if the file is a directory or not.
	 */
	private boolean _isDirectory;
	/**
	 * Flag that indicates if the file is opened or not.
	 */
	private boolean _isOpened;
	/**
	 * Tree path.
	 */
	private TreePath _treePath;

	/**
	 * Constructor of the class.
	 */
	public ExplorerFile() {

	}

	/**
	 * Get the file parent.
	 * 
	 * @return The file parent.
	 */
	public String getParent() {
		return _parent;
	}

	/**
	 * Set a new value to the file parent.
	 * 
	 * @param parent
	 *            New value to set.
	 */
	public void setParent(String parent) {
		_parent = parent;
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
	 * Set a new value to the name.
	 * 
	 * @param name
	 *            New value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the string of the class.
	 */
	public String toString() {
		return _name;
	}

	/**
	 * Set a new value to the string of the class.
	 * 
	 * @param string
	 *            New value to set.
	 */
	public void setString(String string) {
		_name = string;
	}

	/**
	 * Return the flag that indicates if it is the main file or not.
	 * 
	 * @return The flag that indicates if it is the main file or not.
	 */
	public boolean isMainFile() {
		return _isMainFile;
	}

	/**
	 * Set a new value to the flag of main file.
	 * 
	 * @param isMainFile
	 *            New value to set.
	 */
	public void setIsMainFile(boolean isMainFile) {
		_isMainFile = isMainFile;
	}

	/**
	 * Returns the path.
	 * 
	 * @return The path.
	 */
	public String getPath() {
		return _path;
	}

	/**
	 * Set a new value to the path.
	 * 
	 * @param path
	 *            New value to set.
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * Return the flag that indicates if it is a compilable file or not.
	 * 
	 * @return The flag that indicates if it is a compilable file or not.
	 */
	public boolean isCompilableFile() {
		return _isCompilableFile;
	}

	/**
	 * Set a new value to the flag of compilable file.
	 * 
	 * @param isCompilableFile
	 *            New value to set.
	 */
	public void setIsCompilableFile(boolean isCompilableFile) {
		_isCompilableFile = isCompilableFile;
	}

	/**
	 * Returns the flag that indicates if it is directory or not.
	 * 
	 * @return The flag that indicates if it is directory or not.
	 */
	public boolean isDirectory() {
		return _isDirectory;
	}

	/**
	 * Set a new value to the flag of directory
	 * 
	 * @param isDirectory
	 *            New value to set.
	 */
	public void setIsDirectory(boolean isDirectory) {
		_isDirectory = isDirectory;
	}

	/**
	 * Returns the file path.
	 * 
	 * @return The file path.
	 */
	public String getFilePath() {
		
		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");	
		return _path.substring(0, index);
	}

	/**
	 * Returns the file extension.
	 * 
	 * @return The file extension.
	 */
	public String getFileExt() {
		return _path.substring(_path.lastIndexOf(".") + 1);
	}

	/**
	 * Returns the file name.
	 * 
	 * @return The file name.
	 */
	public String getFileName() {
		
		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");	
		return _path.substring(index + 1,
				_path.lastIndexOf("."));
	}

	/**
	 * Return the last path component.
	 * 
	 * @return The last path component.
	 */
	public String getLastPathComponent() {
		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");	
		return _path.substring(index + 1);
	}

	/**
	 * Set a new value to the flag of opened file.
	 * 
	 * @param isOpened
	 *            New value to set.
	 */
	public void setIsOpened(boolean isOpened) {
		_isOpened = isOpened;
	}

	/**
	 * Returns the flag that indicates if the file is opened or not.
	 * 
	 * @return The flag that indicates if the file is opened or not.
	 */
	public boolean isOpened() {
		return _isOpened;
	}

	/**
	 * Returns the tree path.
	 * 
	 * @return The tree path.
	 */
	public TreePath getTreePath() {
		return _treePath;
	}
}
