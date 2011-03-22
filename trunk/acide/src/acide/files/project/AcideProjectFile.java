/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package acide.files.project;

import javax.swing.tree.TreePath;

/**
 * ACIDE - A Configurable IDE project file.
 * 
 * @version 0.8
 */
public class AcideProjectFile {

	/**
	 * ACIDE - A Configurable IDE project file path.
	 */
	private String _absolutePath;
	/**
	 * ACIDE - A Configurable IDE project file name.
	 */
	private String _name;
	/**
	 * ACIDE - A Configurable IDE project file parent.
	 */
	private String _parent;
	/**
	 * ACIDE - A Configurable IDE project file flag that indicates if the file
	 * is a directory or not.
	 */
	private boolean _isDirectory;
	/**
	 * ACIDE - A Configurable IDE project file flag that indicates if the file
	 * is opened or not.
	 */
	private boolean _isOpened;
	/**
	 * ACIDE - A Configurable IDE project file flag that indicates if the file
	 * is a compilable file or not.
	 */
	private boolean _isCompilableFile;
	/**
	 * ACIDE - A Configurable IDE project file flag that indicates if the file
	 * is a main file or not.
	 */
	private boolean _isMainFile;
	/**
	 * ACIDE - A Configurable IDE project file tree path.
	 */
	private TreePath _treePath;

	/**
	 * Creates a new ACIDE - A Configurable IDE project file.
	 */
	public AcideProjectFile() {

	}

	/**
	 * Gets the ACIDE - A Configurable project file parent.
	 * 
	 * @return the ACIDE - A Configurable project file parent.
	 */
	public String getParent() {
		return _parent;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable project file parent.
	 * 
	 * @param parent
	 *            new value to set.
	 */
	public void setParent(String parent) {
		_parent = parent;
	}

	/**
	 * Returns the ACIDE - A Configurable project file name.
	 * 
	 * @return the ACIDE - A Configurable project file name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable project file name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the ACIDE - A Configurable project file class string.
	 * 
	 * @return the ACIDE - A Configurable project file class string.
	 */
	public String toString() {
		return _name;
	}

	/**
	 * Sets a new value to the string of the ACIDE - A Configurable project file
	 * class.
	 * 
	 * @param string
	 *            new value to set.
	 */
	public void setString(String string) {
		_name = string;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file is main file flag.
	 * 
	 * @return the ACIDE - A Configurable IDE project file is main file flag.
	 */
	public boolean isMainFile() {
		return _isMainFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project file is main
	 * file flag.
	 * 
	 * @param isMainFile
	 *            new value to set.
	 */
	public void setIsMainFile(boolean isMainFile) {
		_isMainFile = isMainFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file absolute path.
	 * 
	 * @return the ACIDE - A Configurable IDE project file absolute path.
	 */
	public String getAbsolutePath() {
		return _absolutePath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project file absolute
	 * path.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setAbsolutePath(String path) {
		_absolutePath = path;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file is compilable file
	 * flag.
	 * 
	 * @return the ACIDE - A Configurable IDE project file is compilable file
	 *         flag.
	 */
	public boolean isCompilableFile() {
		return _isCompilableFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project file is
	 * compilable file flag.
	 * 
	 * @param isCompilableFile
	 *            new value to set.
	 */
	public void setIsCompilableFile(boolean isCompilableFile) {
		_isCompilableFile = isCompilableFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file is directory flag.
	 * 
	 * @return the ACIDE - A Configurable IDE project file is directory flag.
	 */
	public boolean isDirectory() {
		return _isDirectory;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project file is
	 * directory flag.
	 * 
	 * @param isDirectory
	 *            new value to set.
	 */
	public void setIsDirectory(boolean isDirectory) {
		_isDirectory = isDirectory;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file relative path.
	 * 
	 * @return the ACIDE - A Configurable IDE project file relative path.
	 */
	public String getRelativePath() {

		int index = _absolutePath.lastIndexOf("\\");
		if (index == -1)
			index = _absolutePath.lastIndexOf("/");
		return _absolutePath.substring(0, index);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file extension.
	 * 
	 * @return the ACIDE - A Configurable IDE project file extension.
	 */
	public String getFileExtension() {
		return _absolutePath.substring(_absolutePath.lastIndexOf(".") + 1);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file name extracted from
	 * the absolute path.
	 * 
	 * @return the ACIDE - A Configurable IDE project file name extracted from
	 *         the absolute path.
	 */
	public String getFileName() {

		int index = _absolutePath.lastIndexOf("\\");
		if (index == -1)
			index = _absolutePath.lastIndexOf("/");
		return _absolutePath.substring(index + 1,
				_absolutePath.lastIndexOf("."));
	}

	/**
	 * Returns the ACIDE - A Configurable IDE last project file path component.
	 * 
	 * @return the ACIDE - A Configurable IDE last project file path component.
	 */
	public String getLastPathComponent() {
		int index = _absolutePath.lastIndexOf("\\");
		if (index == -1)
			index = _absolutePath.lastIndexOf("/");
		return _absolutePath.substring(index + 1);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project file is opened
	 * file flag.
	 * 
	 * @param isOpened
	 *            new value to set.
	 */
	public void setIsOpened(boolean isOpened) {
		_isOpened = isOpened;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file is opened flag.
	 * 
	 * @return the ACIDE - A Configurable IDE project file is opened flag.
	 */
	public boolean isOpened() {
		return _isOpened;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project file tree path.
	 * 
	 * @return the ACIDE - A Configurable IDE project file tree path.
	 */
	public TreePath getTreePath() {
		return _treePath;
	}

	/**
	 * Returns the type of file.
	 * 
	 * @return the type of file.
	 */
	public AcideProjectFileType getType() {
		
		if(!_isMainFile && !_isCompilableFile)
			return AcideProjectFileType.NORMAL;
		
		if(!_isMainFile && _isCompilableFile)
			return AcideProjectFileType.COMPILABLE;
		
		if(_isMainFile && _isCompilableFile)
			return AcideProjectFileType.MAIN;
		
		return null;
	}
}
