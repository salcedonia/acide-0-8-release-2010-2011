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

import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.Color;
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
	 * ACIDE - A Configurable IDE project configuration default path.
	 */
	public static final String DEFAULT_PATH = "./configuration/project/default.acideProject";
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
	 * Shell path associated to the project.
	 */
	private String _consolePanelShellPath;
	/**
	 * Shell directory associated to the project.
	 */
	private String _consolePanelShellDirectory;
	/**
	 * Indicates if the command fired has to be displayed in the console.
	 */
	private boolean _consolePanelIsEchoCommand;
	/**
	 * ACIDE - A Configurable IDE console panel configuration console exit
	 * command.
	 */
	private String _consolePanelExitCommand;
	/**
	 * ACIDE - A Configurable IDE console panel configuration font name.
	 */
	private String _consolePanelFontName;
	/**
	 * ACIDE - A Configurable IDE console panel configuration font style.
	 */
	private int _consolePanelFontStyle;
	/**
	 * ACIDE - A Configurable IDE console panel configuration font size.
	 */
	private int _consolePanelFontSize;
	/**
	 * ACIDE - A Configurable IDE console panel configuration foreground color.
	 */
	private Color _consolePanelForegroundColor;
	/**
	 * ACIDE - A Configurable IDE console panel configuration background color.
	 */
	private Color _consolePanelBackgroundColor;
	/**
	 * Flag that indicates if the explorer panel is showed or not.
	 */
	private boolean _isExplorerPanelShowed;
	/**
	 * Flag that indicates if the console panel is showed or not.
	 */
	private boolean _isConsolePanelShowed;
	/**
	 * ACIDE - A Configurable IDE main window width.
	 */
	private int _width;
	/**
	 * ACIDE - A Configurable IDE main window height.
	 */
	private int _height;
	/**
	 * ACIDE - A Configurable IDE main window x coordinate.
	 */
	private int _xCoordinate;
	/**
	 * ACIDE - A Configurable IDE main window y coordinate.
	 */
	private int _yCoordinate;
	/**
	 * Vertical split pane divider location in the ACIDE - A Configurable IDE
	 * main window.
	 */
	private int _verticalSplitPaneDividerLocation;
	/**
	 * Horizontal split pan divider location in the ACIDE - A Configurable IDE
	 * main window.
	 */
	private int _horizontalSplitPaneDividerLocation;
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

		// Adds the compiler path
		fileContent = fileContent + _compilerPath + "\n";

		// Adds the compiler arguments
		fileContent = fileContent + _compilerArguments + "\n";

		// Adds the console panel shell path
		fileContent = fileContent + _consolePanelShellPath + "\n";

		// Adds the console panel shell directory
		fileContent = fileContent + _consolePanelShellDirectory + "\n";

		// Adds the console panel exit command
		fileContent = fileContent + _consolePanelExitCommand + "\n";

		// Adds the console panel is echo command
		fileContent = fileContent + _consolePanelIsEchoCommand + "\n";

		// Adds the console panel foreground color
		fileContent = fileContent
				+ Integer.toString(_consolePanelForegroundColor.getRGB())
				+ "\n";

		// Adds the console panel background color
		fileContent = fileContent
				+ Integer.toString(_consolePanelBackgroundColor.getRGB())
				+ "\n";

		// Adds the console panel font name
		fileContent = fileContent + _consolePanelFontName + "\n";

		// Adds the console panel font style
		fileContent = fileContent + _consolePanelFontStyle + "\n";

		// Adds the console panel font size
		fileContent = fileContent + _consolePanelFontSize + "\n";

		// Adds the is explorer panel showed configuration
		fileContent = fileContent + _isExplorerPanelShowed + "\n";

		// Adds the is console panel showed configuration
		fileContent = fileContent + _isConsolePanelShowed + "\n";

		// Adds the width
		fileContent = fileContent + _width + "\n";

		// Adds the height
		fileContent = fileContent + _height + "\n";

		// Adds the x coordinate
		fileContent = fileContent + _xCoordinate + "\n";

		// Adds the y coordinate
		fileContent = fileContent + _yCoordinate + "\n";

		// Adds the vertical split pane divider location
		fileContent = fileContent + _verticalSplitPaneDividerLocation + "\n";

		// Adds the horizontal split pane divider location
		fileContent = fileContent + _horizontalSplitPaneDividerLocation + "\n";

		// Adds the language configuration
		fileContent = fileContent + _languageConfiguration + "\n";

		// Adds the menu configuration
		fileContent = fileContent + _menuConfiguration + "\n";

		// Adds the tool bar configuration
		fileContent = fileContent + _toolBarConfiguration + "\n";

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
	 * "./configuration/project/default.acideProject" or the name is "".
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

		return projectConfiguration.matches(DEFAULT_PATH)
				&& AcideProjectConfiguration.getInstance().getName().equals("");
	}

	/**
	 * Loads the project configuration from the file content given as a
	 * parameter.
	 * 
	 * @param filePath
	 *            configuration file path which contains all the project
	 *            configuration to load.
	 */
	public void load(String filePath) {

		// Deletes all the files associated to the project
		AcideProjectConfiguration.getInstance().removeFiles();

		String fileContent = null;

		// Loads its content
		fileContent = AcideFileManager.getInstance().load(filePath);

		// If it can't find the file
		if (fileContent == null) {

			// Loads the default file
			fileContent = AcideFileManager.getInstance().load(
					AcideProjectConfiguration.DEFAULT_PATH);

			// Updates the ACIDE - A Configurable IDE project configuration
			AcideResourceManager.getInstance().setProperty(
					"projectConfiguration",
					AcideProjectConfiguration.DEFAULT_PATH);

			// Displays an error message
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s960")
							+ filePath
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s959"));
		}

		int initialPosition = 0;
		int finalPosition = 0;

		// Gets the project name
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_name = fileContent.substring(initialPosition, finalPosition);

		// Gets the project path
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_path = fileContent.substring(initialPosition, finalPosition);

		// Gets the compiler path
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_compilerPath = fileContent.substring(initialPosition, finalPosition);

		// Gets the compiler arguments
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_compilerArguments = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the console panel shell path
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelShellPath = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the console panel shell directory
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelShellDirectory = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the console panel exit command
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelExitCommand = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the console panel is echo command
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelIsEchoCommand = Boolean.parseBoolean(fileContent
				.substring(initialPosition, finalPosition));

		// Gets the console panel foreground color
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelForegroundColor = new Color(Integer.parseInt(fileContent
				.substring(initialPosition, finalPosition)));

		// Gets the console panel background color
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelBackgroundColor = new Color(Integer.parseInt(fileContent
				.substring(initialPosition, finalPosition)));

		// Gets the console panel font name
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelFontName = fileContent.substring(initialPosition,
				finalPosition);

		// Gets the console panel font style
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelFontStyle = Integer.parseInt(fileContent.substring(
				initialPosition, finalPosition));

		// Gets the console panel font size
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consolePanelFontSize = Integer.parseInt(fileContent.substring(
				initialPosition, finalPosition));

		// Gets the is explorer panel showed flag
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_isExplorerPanelShowed = Boolean.parseBoolean(fileContent.substring(
				initialPosition, finalPosition));

		// Gets the is console panel showed flag
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_isConsolePanelShowed = Boolean.parseBoolean(fileContent.substring(
				initialPosition, finalPosition));

		// Gets the ACIDE - A Configurable IDE main window width
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_width = Integer.parseInt(fileContent.substring(initialPosition,
				finalPosition));

		// Gets the ACIDE - A Configurable IDE main window height
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_height = Integer.parseInt(fileContent.substring(initialPosition,
				finalPosition));

		// Gets the ACIDE - A Configurable IDE main window x coordinate
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_xCoordinate = Integer.parseInt(fileContent.substring(initialPosition,
				finalPosition));

		// Gets the ACIDE - A Configurable IDE main window y coordinate
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_yCoordinate = Integer.parseInt(fileContent.substring(initialPosition,
				finalPosition));

		// Gets the ACIDE - A Configurable IDE main window vertical split pane
		// divider location
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_verticalSplitPaneDividerLocation = Integer.parseInt(fileContent
				.substring(initialPosition, finalPosition));

		// Gets the ACIDE - A Configurable IDE main window horizontal split pane
		// divider location
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_horizontalSplitPaneDividerLocation = Integer.parseInt(fileContent
				.substring(initialPosition, finalPosition));

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
	 * Asks for saving the project configuration to the user.
	 * 
	 * @return false if the cancel option has been selected and true in other
	 *         case.
	 */
	public boolean askForSavingProjectConfiguration() {

		// Are the project configuration modified
		if (AcideProjectConfiguration.getInstance().isModified()) {

			// Ask the user to save the configuration
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s657"), AcideLanguageManager
							.getInstance().getLabels().getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is not the cancel or the closed option
			if (returnValue != JOptionPane.CANCEL_OPTION
					&& returnValue != JOptionPane.CLOSED_OPTION) {

				// If it is yes
				if (returnValue == JOptionPane.YES_OPTION) {

					// If it is not the default project
					if (!AcideProjectConfiguration.getInstance()
							.isDefaultProject()) {

						// Enables the menu
						AcideMainWindow.getInstance().getMenu()
								.getProjectMenu().getSaveProjectMenuItem()
								.setEnabled(true);

						// Saves the project
						AcideMainWindow.getInstance().getMenu()
								.getProjectMenu().getSaveProjectMenuItem()
								.doClick();
					}
				}
			} else
				return false;
		}

		return true;
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
	 * Returns the ACIDE - A Configurable IDE project configuration is project
	 * modified flag.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration is project
	 *         modified flag.
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

		// Updates the save project in the menu bar tool bar
		AcideMainWindow.getInstance().getToolBarPanel().getMenuBarToolBar()
				.updateSaveProjectButtonState(isProjectModified);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration shell path.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration shell path.
	 */
	public String getShellPath() {
		return _consolePanelShellPath;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE project configuration shell
	 * directory.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration shell
	 *         directory.
	 */
	public String getShellDirectory() {
		return _consolePanelShellDirectory;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE project configuration
	 * shell path.
	 * 
	 * @param shellPath
	 *            new value to set.
	 */
	public void setShellPath(String shellPath) {
		_consolePanelShellPath = shellPath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration font name.
	 * 
	 * @param fontName
	 *            new value to set.
	 */
	public void setFontName(String fontName) {
		_consolePanelFontName = fontName;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration font
	 * name.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration font
	 *         name.
	 */
	public String getFontName() {
		return _consolePanelFontName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration font size.
	 * 
	 * @param fontSize
	 *            new value to set.
	 */
	public void setFontSize(int fontSize) {
		_consolePanelFontSize = fontSize;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration font
	 * size.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration font
	 *         size.
	 */
	public int getFontSize() {
		return _consolePanelFontSize;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration font
	 * style.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration font
	 *         style.
	 */
	public int getFontStyle() {
		return _consolePanelFontStyle;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration font style.
	 * 
	 * @param fontStyle
	 *            new value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_consolePanelFontStyle = fontStyle;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration
	 * foreground color.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration
	 *         foreground color.
	 */
	public Color getForegroundColor() {
		return _consolePanelForegroundColor;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration foreground color.
	 * 
	 * @param foregroundColor
	 *            new value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_consolePanelForegroundColor = foregroundColor;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration
	 * background color.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration
	 *         background color.
	 */
	public Color getBackgroundColor() {
		return _consolePanelBackgroundColor;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration background color.
	 * 
	 * @param backgroundColor
	 *            new value to set.
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_consolePanelBackgroundColor = backgroundColor;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration is
	 * echo command flag value.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration is
	 *         echo command flag value.
	 */
	public boolean getIsEchoCommand() {
		return _consolePanelIsEchoCommand;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration is echo command flag.
	 * 
	 * @param isEchoCommand
	 *            new value to set.
	 */
	public void setIsEchoCommand(boolean isEchoCommand) {
		_consolePanelIsEchoCommand = isEchoCommand;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration exit
	 * command.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration exit
	 *         command.
	 */
	public String getExitCommand() {
		return _consolePanelExitCommand;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration exit command.
	 * 
	 * @param exitCommand
	 *            new value to set.
	 */
	public void setExitCommand(String exitCommand) {
		_consolePanelExitCommand = exitCommand;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration shell directory.
	 * 
	 * @param shellDirectory
	 *            new value to set.
	 */
	public void setShellDirectory(String shellDirectory) {
		_consolePanelShellDirectory = shellDirectory;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE window configuration window
	 * height.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration window
	 *         height.
	 */
	public int getWindowHeight() {
		return _height;
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE window configuration
	 * window height.
	 * 
	 * @param height
	 *            new value to set.
	 */
	public void setWindowHeight(int height) {
		_height = height;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE window configuration window x
	 * coordinate.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration window x
	 *         coordinate.
	 */
	public int getXCoordinate() {
		return _xCoordinate;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE window configuration
	 * window x coordinate.
	 * 
	 * @param xCoordinate
	 *            new value to set.
	 */
	public void setXCoordinate(int xCoordinate) {
		_xCoordinate = xCoordinate;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE window configuration window y
	 * coordinate.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration window y
	 *         coordinate.
	 */
	public int getYCoordinate() {
		return _yCoordinate;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE window configuration y
	 * coordinate of the window.
	 * 
	 * @param yCoordinate
	 *            new value to set.
	 */
	public void setYCoordinate(int yCoordinate) {
		_yCoordinate = yCoordinate;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE window configuration is console
	 * panel showed flag.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration is console
	 *         panel showed flag.
	 */
	public boolean isConsolePanelShowed() {
		return _isConsolePanelShowed;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE window configuration
	 * is console panel showed flag.
	 * 
	 * @param isConsolePanelShowed
	 *            new value to set.
	 */
	public void setIsConsolePanelShowed(boolean isConsolePanelShowed) {
		_isConsolePanelShowed = isConsolePanelShowed;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE window configuration is explorer
	 * panel showed flag.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration is explorer
	 *         panel showed flag.
	 */
	public boolean isExplorerPanelShowed() {
		return _isExplorerPanelShowed;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE window configuration
	 * is explorer panel showed flag.
	 * 
	 * @param isExplorerPanelShowed
	 *            new value to set.
	 */
	public void setIsExplorerPanelShowed(boolean isExplorerPanelShowed) {
		_isExplorerPanelShowed = isExplorerPanelShowed;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE window configuration window width.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration window width.
	 */
	public int getWindowWidth() {
		return _width;
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE window configuration
	 * window width.
	 * 
	 * @param width
	 *            new value to set.
	 */
	public void setWindowWidth(int width) {
		_width = width;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE window configuration horizontal
	 * split pane divider location.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration horizontal
	 *         split pane divider location.
	 */
	public int getHorizontalSplitPanelDividerLocation() {
		return _horizontalSplitPaneDividerLocation;
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE window configuration
	 * horizontal split pane divider location.
	 * 
	 * @param horizontalSplitPaneDividerLocation
	 *            new value to set.
	 */
	public void setHorizontalSplitPaneDividerLocation(
			int horizontalSplitPaneDividerLocation) {
		_horizontalSplitPaneDividerLocation = horizontalSplitPaneDividerLocation;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE window configuration vertical
	 * split pane divider location.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration vertical
	 *         split pane divider location.
	 */
	public int getVerticalSplitPaneDividerLocation() {
		return _verticalSplitPaneDividerLocation;
	}

	/**
	 * Sets a new value for the ACIDE - A Configurable IDE window configuration
	 * vertical split pane divider location.
	 * 
	 * @param verticalSplitPaneDividerLocation
	 *            new value to set.
	 */
	public void setVerticalSplitPaneDividerLocation(
			int verticalSplitPaneDividerLocation) {
		_verticalSplitPaneDividerLocation = verticalSplitPaneDividerLocation;
	}
}