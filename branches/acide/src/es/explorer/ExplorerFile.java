package es.explorer;

import javax.swing.tree.TreePath;

/************************************************************************																
 * Explorer files of ACIDE - A Configurable IDE											
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
public class ExplorerFile {

	/**
	 * File path
	 */
	private String _path;
	/**
	 * Flag that indicates if the file is a compilable file or not
	 */
	private boolean _isCompilableFile;
	/**
	 * Flag that indicates if the file is a main file or not
	 */
	private boolean _isMainFile;
	/**
	 * File name
	 */
	private String _name;
	/**
	 * File parent
	 */
	private String _parent;
	/**
	 * Flag that indicates if the file is a directory or not
	 */
	private boolean _isDirectory;
	/**
	 * Flag that indicates if the file is opened or not
	 */
	private boolean _isOpened;
	/**
	 * Tree path
	 */
	private TreePath _treePath;

	/**
	 * Class constructor
	 */
	public ExplorerFile() {

	}

	/**
	 * Gets the file parent
	 * 
	 * @return the file parent
	 */
	public String getParent() {
		return _parent;
	}

	/**
	 * Sets a new value to the file parent
	 * 
	 * @param parent
	 *            new value to set
	 */
	public void setParent(String parent) {
		_parent = parent;
	}

	/**
	 * Returns the name
	 * 
	 * @return the name
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the name
	 * 
	 * @param name
	 *            new value to set
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the class string
	 * 
	 * @return the class string
	 */
	public String toString() {
		return _name;
	}

	/**
	 * Sets a new value to the string of the class
	 * 
	 * @param string
	 *            new value to set
	 */
	public void setString(String string) {
		_name = string;
	}

	/**
	 * Returns the flag that indicates if it is the main file or not
	 * 
	 * @return the flag that indicates if it is the main file or not
	 */
	public boolean isMainFile() {
		return _isMainFile;
	}

	/**
	 * Sets a new value to the flag of main file
	 * 
	 * @param isMainFile
	 *            new value to set
	 */
	public void setIsMainFile(boolean isMainFile) {
		_isMainFile = isMainFile;
	}

	/**
	 * Returns the path
	 * 
	 * @return the path
	 */
	public String getPath() {
		return _path;
	}

	/**
	 * Sets a new value to the path
	 * 
	 * @param path
	 *            new value to set
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * Returns the flag that indicates if it is a compilable file or not
	 * 
	 * @return the flag that indicates if it is a compilable file or not
	 */
	public boolean isCompilableFile() {
		return _isCompilableFile;
	}

	/**
	 * Sets a new value to the flag of compilable file
	 * 
	 * @param isCompilableFile
	 *            new value to set
	 */
	public void setIsCompilableFile(boolean isCompilableFile) {
		_isCompilableFile = isCompilableFile;
	}

	/**
	 * Returns the flag that indicates if it is directory or not
	 * 
	 * @return the flag that indicates if it is directory or not
	 */
	public boolean isDirectory() {
		return _isDirectory;
	}

	/**
	 * Sets a new value to the flag of directory
	 * 
	 * @param isDirectory
	 *            new value to set
	 */
	public void setIsDirectory(boolean isDirectory) {
		_isDirectory = isDirectory;
	}

	/**
	 * Returns the file path
	 * 
	 * @return the file path
	 */
	public String getFilePath() {

		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");
		return _path.substring(0, index);
	}

	/**
	 * Returns the file extension
	 * 
	 * @return the file extension
	 */
	public String getFileExt() {
		return _path.substring(_path.lastIndexOf(".") + 1);
	}

	/**
	 * Returns the file name
	 * 
	 * @return the file name
	 */
	public String getFileName() {

		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");
		return _path.substring(index + 1, _path.lastIndexOf("."));
	}

	/**
	 * Returns the last path component
	 * 
	 * @return the last path component
	 */
	public String getLastPathComponent() {
		int index = _path.lastIndexOf("\\");
		if (index == -1)
			index = _path.lastIndexOf("/");
		return _path.substring(index + 1);
	}

	/**
	 * Sets a new value to the flag of opened file
	 * 
	 * @param isOpened
	 *            new value to set
	 */
	public void setIsOpened(boolean isOpened) {
		_isOpened = isOpened;
	}

	/**
	 * Returns the flag that indicates if the file is opened or not
	 * 
	 * @return the flag that indicates if the file is opened or not
	 */
	public boolean isOpened() {
		return _isOpened;
	}

	/**
	 * Returns the tree path
	 * 
	 * @return the tree path
	 */
	public TreePath getTreePath() {
		return _treePath;
	}
}
