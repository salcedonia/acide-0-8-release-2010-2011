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
package acide.configuration.project;

import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.toolBarPanel.menuBarToolBar.AcideMenuBarToolBar;

import java.io.File;
import java.util.ArrayList;

import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE project configuration.
 * 
 * @version 0.8
 * @see AcideProjectFile
 */
public class AcideProjectConfiguration {

	/**
	 * ACIDE - A Configurable IDE project configuration unique class instance.
	 */
	private static AcideProjectConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE project configuration project name.
	 */
	private String _name;
	/**
	 * ACIDE - A Configurable IDE project configuration project path.
	 */
	private String _path;
	/**
	 * ACIDE - A Configurable IDE project configuration console configuration.
	 */
	private String _consoleConfiguration;
	/**
	 * ACIDE - A Configurable IDE project configuration file editor
	 * configuration.
	 */
	private String _fileEditorConfiguration;
	/**
	 * ACIDE - A Configurable IDE project configuration window configuration.
	 */
	private String _windowConfiguration;
	/**
	 * Language of the application.
	 */
	private String _languageConfiguration;
	/**
	 * Menu configuration.
	 */
	private String _menuConfiguration;
	/**
	 * Tool Bar configuration.
	 */
	private String _toolBarConfiguration;
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
	 * Flag that indicates if it is the first time that the configuration has
	 * been saved or not.
	 */
	private boolean _isFirstSave;
	/**
	 * Flag that indicates if the project configuration has been modified or
	 * not.
	 */
	private boolean _isModified;
	/**
	 * File list of the files which belongs to the project.
	 */
	private ArrayList<AcideProjectFile> _fileList;

	/**
	 * Creates a new project configuration.
	 */
	public AcideProjectConfiguration() {
		_fileList = new ArrayList<AcideProjectFile>();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration unique class
	 *         instance.
	 */
	public static AcideProjectConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideProjectConfiguration();
		return _instance;
	}

	/**
	 * Saves the project configuration in a string.
	 * 
	 * @return the file content with the project configuration to be saved.
	 */
	public String save() {

		// Builds the file content
		String fileContent = "";
		
		// Adds the name
		fileContent = fileContent + _name + "\n";
		
		// Adds the path
		fileContent = fileContent + _path + "\n";
		
		// Adds the window configuration
		fileContent = fileContent + _windowConfiguration + "\n";
		
		// Adds the compiler path
		fileContent = fileContent + _compilerPath + "\n";
		
		// Adds the compiler arguments
		fileContent = fileContent + _compilerArguments + "\n";
		
		// Adds the console configuration
		fileContent = fileContent + _consoleConfiguration + "\n";
		
		// Adds the language configuration
		fileContent = fileContent + _languageConfiguration + "\n";
		
		// Adds the menu configuration
		fileContent = fileContent + _menuConfiguration + "\n";
		
		// Adds the tool bar configuration
		fileContent = fileContent + _toolBarConfiguration + "\n";
		
		// Adds the file editor
		fileContent = fileContent + _fileEditorConfiguration + "\n";

		// Adds the number of files associated
		fileContent = fileContent + _fileList.size() + "\n";

		for (int index = 0; index < _fileList.size(); index++) {

			// Gets the ACIDE - A Configurable file from the list
			AcideProjectFile file = (AcideProjectFile) _fileList.get(index);
			
			// Adds its information
			fileContent = fileContent + file.getAbsolutePath() + "\n"
					+ file.getName() + "\n" + file.getParent() + "\n"
					+ file.isDirectory() + "\n" + file.isCompilableFile()
					+ "\n" + file.isMainFile() + "\n" + file.isOpened() + "\n";
		}

		// Returns the file content
		return fileContent;
	}

	/**
	 * Returns true if the current project is
	 * "./configuration/project/default.acidePrj" or the name is "".
	 * 
	 * @return true if it has the default project and false in other case.
	 */
	public boolean isDefaultProject() {

		String projectConfiguration = null;
		try {

			// Gets the ACIDE - A Configurable IDE project configuration
			projectConfiguration = AcideResourceManager.getInstance()
					.getProperty("projectConfiguration");
		} catch (Exception exception) {

			// Updates the Log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		return projectConfiguration
				.matches("./configuration/project/default.acidePrj")
				&& AcideProjectConfiguration.getInstance().getName().equals("");
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

		// Gets the project name
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_name = fileContent.substring(initialPosition, finalPosition);

		// Gets the project path
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_path = fileContent.substring(initialPosition, finalPosition);

		// Gets the window configuration
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_windowConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the compiler path
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_compilerPath = fileContent.substring(initialPosition, finalPosition);

		// Gets the compiler arguments
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_compilerArguments = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the console configuration
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consoleConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the language configuration
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_languageConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the menu configuration
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_menuConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the tool bar configuration
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_toolBarConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the file editor configuration
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_fileEditorConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the number of files of the project
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		String numFiles = fileContent.substring(initialPosition, finalPosition);
		initialPosition = finalPosition + 1;

		boolean isCompilableFile;
		boolean isMainFile;
		String name;
		String path;
		String parent;
		boolean isDirectory;
		boolean isOpened;

		// Clears the file list
		_fileList.clear();

		for (int index = 0; index < Integer.parseInt(numFiles); index++) {

			// Creates the ACIDE - A Configurable IDE file
			AcideProjectFile file = new AcideProjectFile();

			// Gets the absolute path
			finalPosition = fileContent.indexOf("\n", initialPosition);
			path = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;

			// Gets the name
			finalPosition = fileContent.indexOf("\n", initialPosition);
			name = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;

			// Gets the parent
			finalPosition = fileContent.indexOf("\n", initialPosition);
			parent = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;

			// Gets the is directory flag
			finalPosition = fileContent.indexOf("\n", initialPosition);
			isDirectory = Boolean.parseBoolean(fileContent.substring(
					initialPosition, finalPosition));
			initialPosition = finalPosition + 1;

			// Gets the is compilable flag
			finalPosition = fileContent.indexOf("\n", initialPosition);
			isCompilableFile = Boolean.parseBoolean(fileContent.substring(
					initialPosition, finalPosition));
			initialPosition = finalPosition + 1;

			// Gets the is main flag
			finalPosition = fileContent.indexOf("\n", initialPosition);
			isMainFile = Boolean.parseBoolean(fileContent.substring(
					initialPosition, finalPosition));
			initialPosition = finalPosition + 1;

			// Gets the is opened flag
			finalPosition = fileContent.indexOf("\n", initialPosition);
			isOpened = Boolean.parseBoolean(fileContent.substring(
					initialPosition, finalPosition));
			initialPosition = finalPosition + 1;

			// Updates the ACIDE - A Configurable IDE file with the info
			
			// Sets the is main flag
			file.setIsMainFile(isMainFile);
			
			// Sets the is compilable flag
			file.setIsCompilableFile(isCompilableFile);
			
			// Sets the absolute path
			file.setAbsolutePath(path);
			
			// Sets the parent
			file.setParent(parent);
			
			// Sets the name
			file.setName(name);
			
			// Sets the is directory flag
			file.setIsDirectory(isDirectory);
			
			// Sets the is opened flag
			file.setIsOpened(isOpened);

			// Always add the folders as they do not exist
			if (file.isDirectory())
				// Adds the file to the list
				_fileList.add(file);
			else {

				// Checks if exists
				File externalFile = new File(path);
				if (externalFile.exists()) {

					// Adds the file to the list
					_fileList.add(file);
				} else {

					// Displays an error message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s970")
							+ path
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s971"), "Error",
							JOptionPane.ERROR_MESSAGE);

					// The configuration has been modified
					_isModified = true;
				}
			}
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration project
	 * name.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration project
	 *         name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration project
	 * path.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration project
	 *         path.
	 */
	public String getProjectPath() {
		return _path;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration file list
	 * size.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration file list
	 *         size.
	 */
	public int getFileListSize() {
		return _fileList.size();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration compiler
	 * path.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration compiler
	 *         path.
	 */
	public String getCompilerPath() {
		return _compilerPath;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration compiler
	 * arguments.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration compiler
	 *         arguments.
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
	 * @see AcideProjectFile
	 */
	public AcideProjectFile getFileAt(int index) {
		return _fileList.get(index);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * project name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Sets a new value to the the ACIDE - A Configurable IDE project
	 * configuration project path.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * compiler path.
	 * 
	 * @param compilerPath
	 *            new value to set.
	 */
	public void setCompilerPath(String compilerPath) {
		_compilerPath = compilerPath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * compiler arguments.
	 * 
	 * @param compilerArguments
	 *            new value to set.
	 */
	public void setCompilerArguments(String compilerArguments) {
		_compilerArguments = compilerArguments;
	}

	/**
	 * Adds a file to the ACIDE - A Configurable IDE project configuration file
	 * list.
	 * 
	 * @param projectFile
	 *            new file to add.
	 */
	public void addFile(AcideProjectFile projectFile) {
		_fileList.add(projectFile);
	}

	/**
	 * Returns the number of files from the ACIDE - A Configurable IDE project
	 * configuration file list.
	 * 
	 * @return the number of files from the ACIDE - A Configurable IDE project
	 *         configuration file list.
	 */
	public int getNumberOfFilesFromList() {
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
	 * Removes a file from the list which matches with the one given as a
	 * parameter.
	 * 
	 * @param name
	 *            file name to be removed.
	 */
	public void removeFileAt(String name) {
		_fileList.remove(name);
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
	 * Returns the the ACIDE - A Configurable IDE project configuration
	 * separator file.
	 * 
	 * @return the the ACIDE - A Configurable IDE project configuration
	 *         separator file.
	 */
	public String getSeparatorFile() {
		return _separatorFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * separator file.
	 * 
	 * @param separatorFile
	 *            new value to set.
	 */
	public void setSeparatorFile(String separatorFile) {
		_separatorFile = separatorFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration file
	 * extension.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration file
	 *         extension.
	 */
	public String getFileExtension() {
		return _fileExtension;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * file extension.
	 * 
	 * @param fileExtension
	 *            new value to set.
	 */
	public void setFileExtension(String fileExtension) {
		_fileExtension = fileExtension;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration is first
	 * save flag.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration is first
	 *         save flag.
	 */
	public boolean isFirstSave() {
		return _isFirstSave;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * is first save flag.
	 * 
	 * @param firstSave
	 *            new value to set.
	 */
	public void setFirstSave(boolean firstSave) {
		_isFirstSave = firstSave;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration menu
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration menu
	 *         configuration.
	 */
	public String getMenuConfiguration() {
		return _menuConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * menu configuration.
	 * 
	 * @param menuConfiguration
	 *            new value to set.
	 */
	public void setMenuConfiguration(String menuConfiguration) {
		_menuConfiguration = menuConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration tool bar
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration tool bar
	 *         configuration.
	 */
	public String getToolBarConfiguration() {
		return _toolBarConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * tool bar configuration.
	 * 
	 * @param toolBarConfiguration
	 *            new value to set.
	 */
	public void setToolBarConfiguration(String toolBarConfiguration) {
		_toolBarConfiguration = toolBarConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration language
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration language
	 *         configuration.
	 */
	public String getLanguageConfiguration() {
		return _languageConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * language configuration.
	 * 
	 * @param languageConfiguration
	 *            new value to set.
	 */
	public void setLanguageConfiguration(String languageConfiguration) {
		_languageConfiguration = languageConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration is project modified
	 * flag.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration is project modified
	 *         flag.
	 */
	public boolean isModified() {
		return _isModified;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * is project modified flag.
	 * 
	 * @param isProjectModified
	 *            new value to set.
	 */
	public void setIsModified(boolean isProjectModified) {
		
		// Stores the flag
		_isModified = isProjectModified;
		
		// Updates the save project in the static tool bar
		AcideMenuBarToolBar.getInstance().updateSaveProjectButtonState(isProjectModified);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * console configuration.
	 * 
	 * @param consoleConfiguration
	 *            new value to set.
	 */
	public void setOutputConfiguration(String consoleConfiguration) {
		_consoleConfiguration = consoleConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration console
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration console
	 *         configuration.
	 */
	public String getConsoleConfiguration() {
		return _consoleConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration file editor
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration file editor
	 *         configuration.
	 */
	public String getFileEditorConfiguration() {
		return _fileEditorConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * file editor configuration.
	 * 
	 * @param fileEditorConfiguration
	 *            new value to set.
	 */
	public void setFileEditorConfiguration(String fileEditorConfiguration) {
		_fileEditorConfiguration = fileEditorConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration window
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration window
	 *         configuration.
	 */
	public String getWindowConfiguration() {
		return _windowConfiguration;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * window configuration.
	 * 
	 * @param windowConfiguration
	 *            new value to set.
	 */
	public void setWindowConfiguration(String windowConfiguration) {
		_windowConfiguration = windowConfiguration;
	}

	/**
	 * Returns the file from the file list which absolute path matches with the
	 * path given as a parameter.
	 * 
	 * @param absolutePath
	 *            absolute path to compare with.
	 * 
	 * @return the file from the file list which absolute path matches with the
	 *         path given as a parameter.
	 */
	public AcideProjectFile getFileAt(String absolutePath) {

		for (int index = 0; index < _fileList.size(); index++)
			if (_fileList.get(index).getAbsolutePath().equals(absolutePath))
				return _fileList.get(index);

		return null;
	}

	/**
	 * Returns the index of the file from the file list which absolute path
	 * matches with the path given as a parameter.
	 * 
	 * @param absolutePath
	 *            absolute path to compare with.
	 * 
	 * @return the index of the file from the file list. If it is not at the
	 *         list then returns -1.
	 */
	public int getIndexOfFile(String filePath) {

		for (int index = 0; index < _fileList.size(); index++)
			if (_fileList.get(index).getAbsolutePath().equals(filePath))
				return index;
		return -1;
	}
	
	/**
	 * <p>
	 * Check if the current project has been modified. If so, asks to the user
	 * if he wants to save it. In the process stores the
	 * </p>
	 * 
	 * @return true if the options OK or NO have not been selected and false in
	 *         other case.
	 */
	public boolean askForSavingProject() {

		boolean isCancelSelected = false;

		// Is the project configuration modified
		if (AcideProjectConfiguration.getInstance().isModified()) {

			// Do you want to save it?
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s657"), AcideLanguageManager
							.getInstance().getLabels().getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is OK
			if (returnValue == JOptionPane.OK_OPTION) {

				// Enables the save project menu item
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveProjectMenuItem().setEnabled(true);

				// Save the project
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveProjectMenuItem().doClick();
			} else {

				// If it is not NO
				if (returnValue != JOptionPane.NO_OPTION)
					isCancelSelected = true;
			}
		}

		return isCancelSelected;
	}
}