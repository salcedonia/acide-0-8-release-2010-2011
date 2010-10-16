package operations.configuration;

import javax.swing.tree.TreePath;

/**
 * 
 */
public class ExplorerFile {
	
	/**
	 * 
	 */
	private String _path;
	/**
	 * 
	 */
	private boolean _isSetFile;
	/**
	 * 
	 */
	private boolean _isMainFile;
	/**
	 * 
	 */
	private String _name;
	/**
	 * 
	 */
	private String _parent;
	/**
	 * 
	 */
	private boolean _isDirectory;
	/**
	 *
	 */	
	private boolean _opened;
	/**
	 * 
	 */
	private TreePath _treePath;

	/**
	 * Constructor of the class.
	 */
	public ExplorerFile() {

	}
	
	/**
	 * 
	 * @return
	 */
	public String getParent() {
		return _parent;
	}

	/**
	 * 
	 * @param parent
	 */
	public void setParent(String parent) {
		_parent = parent;
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
	 * @param name
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * 
	 */
	public String toString() {

		return _name;
	}

	/**
	 * 
	 * @param s
	 */
	public void setString(String s) {
		_name = s;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isMainFile() {
		return _isMainFile;
	}

	/**
	 * 
	 * @param mainFile
	 */
	public void setMainFile(boolean mainFile) {
		_isMainFile = mainFile;
	}

	/**
	 * 
	 * @return
	 */
	public String getPath() {
		return _path;
	}

	/**
	 * 
	 * @param path
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isSetFile() {
		return _isSetFile;
	}

	/**
	 * 
	 * @param setFile
	 */
	public void setSetFile(boolean setFile) {
		_isSetFile = setFile;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isDirectory() {
		return _isDirectory;
	}

	/**
	 * 
	 * @param directory
	 */
	public void setDirectory(boolean directory) {
		_isDirectory = directory;
	}

	/**
	 * 
	 * @return
	 */
	public String getFilePath() {
		String filePath;

		filePath = _path.substring(0, _path.lastIndexOf("\\"));

		return filePath;
	}

	/**
	 * 
	 * @return
	 */
	public String getFileExt() {
		String fileExt;

		fileExt = _path.substring(_path.lastIndexOf(".") + 1);

		return fileExt;
	}

	/**
	 * 
	 * @return
	 */
	public String getFileName() {
		String fileExt;

		fileExt = _path.substring(_path.lastIndexOf("\\") + 1,
				_path.lastIndexOf("."));

		return fileExt;
	}

	/**
	 * 
	 * @return
	 */
	public String getLastPathComponent() {
		String s;

		s = _path.substring(_path.lastIndexOf("\\") + 1);

		return s;
	}

	/**
	 * 
	 * @param b
	 */
	public void setOpened(boolean b) {
		_opened = b;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isOpened() {
		return _opened;
	}

	/**
	 * 
	 * @return
	 */
	public TreePath getTreePath() {
		return _treePath;
	}
}
