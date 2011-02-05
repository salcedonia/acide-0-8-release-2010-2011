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
package es.configuration.project;

import es.project.AcideProjectFile;

import java.io.File;
import java.util.ArrayList;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;

import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE project configuration.
 * 
 * @version 0.8
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
	 * ACIDE - A Configurable IDE project configuration lexicon configuration.
	 */
	private String _lexiconConfiguration;
	/**
	 * ACIDE - A Configurable IDE project configuration grammar configuration.
	 */
	private String _grammarConfiguration;
	/**
	 * ACIDE - A Configurable IDE project configuration console configuration.
	 */
	private String _consoleConfiguration;
	/**
	 * ACIDE - A Configurable IDE project configuration file editor configuration.
	 */
	private String _fileEditorConfiguration;
	/**
	 * ACIDE - A Configurable IDE project configuration window configuration.
	 */
	private String _windowConfiguration;
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
	 * Flag that indicates if the configuration has been modified or not.
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
	 * Returns the ACIDE - A Configurable IDE project configuration unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE project configuration unique class instance.
	 */
	public static AcideProjectConfiguration getInstance(){
		
		if(_instance == null)
			_instance = new AcideProjectConfiguration();
		return _instance;
	}
	
	/**
	 * Saves the project configuration in a string.
	 * 
	 * @return the file content with the project configuration to be saved.
	 */
	public String save() {

		String fileContent = "";
		fileContent = fileContent + _name + "\n";
		fileContent = fileContent + _path + "\n";
		fileContent = fileContent + _windowConfiguration + "\n";
		fileContent = fileContent + _lexiconConfiguration + "\n";
		fileContent = fileContent + _grammarConfiguration + "\n";
		fileContent = fileContent + _compilerPath + "\n";
		fileContent = fileContent + _compilerArguments + "\n";
		fileContent = fileContent + _consoleConfiguration + "\n";
		fileContent = fileContent + _language + "\n";
		fileContent = fileContent + _menuConfiguration + "\n";
		fileContent = fileContent + _toolBarConfiguration + "\n";
		fileContent = fileContent + _fileEditorConfiguration + "\n";

		// FILES ASSOCIATED TO THE PROJECT
		fileContent = fileContent + _fileList.size() + "\n";

		for (int index = 0; index < _fileList.size(); index++) {
			
			// Gets the ACIDE - A Configurable file from the list
			AcideProjectFile file = (AcideProjectFile) _fileList.get(index);
			fileContent = fileContent + file.getAbsolutePath() + "\n" + file.getName() + "\n"
					+ file.getParent() + "\n" + file.isDirectory() + "\n"
					+ file.isCompilableFile() + "\n" + file.isMainFile() + "\n"
					+ file.isOpened() + "\n";
		}

		return fileContent;
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
			project = AcideResourceManager.getInstance().getProperty(
					"defaultAcideProject");
		} catch (Exception exception) {

			// Updates the Log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		return project.matches("./configuration/project/default.acidePrj")
				&& AcideProjectConfiguration.getInstance().getName()
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

		// PROJECT NAME
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_name = fileContent.substring(initialPosition, finalPosition);
		
		// PROJECT PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_path = fileContent.substring(initialPosition, finalPosition);
		
		// WINDOW CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_windowConfiguration = fileContent.substring(initialPosition,
				finalPosition);
		
		// LEXICON CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_lexiconConfiguration = fileContent.substring(initialPosition,
				finalPosition);

		// GRAMMAR CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_grammarConfiguration = fileContent.substring(initialPosition,
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

		// CONSOLE CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_consoleConfiguration = fileContent.substring(initialPosition,
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

		// FILE EDITOR CONFIGURATION PATH
		initialPosition = finalPosition + 1;
		finalPosition = fileContent.indexOf("\n", initialPosition);
		_fileEditorConfiguration = fileContent.substring(initialPosition,
				finalPosition);
		
		// NUM FILES
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
			
			// ABSOLUTE PATH
			finalPosition = fileContent.indexOf("\n", initialPosition);
			path = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;
			
			// NAME
			finalPosition = fileContent.indexOf("\n", initialPosition);
			name = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;
			
			// PARENT
			finalPosition = fileContent.indexOf("\n", initialPosition);
			parent = fileContent.substring(initialPosition, finalPosition);
			initialPosition = finalPosition + 1;
			
			// IS DIRECTORY
			finalPosition = fileContent.indexOf("\n", initialPosition);
			if (Boolean.parseBoolean(fileContent.substring(initialPosition,
					finalPosition)) == true)
				isDirectory = true;
			else
				isDirectory = false;
			initialPosition = finalPosition + 1;
			
			// IS COMPILABLE FILE
			finalPosition = fileContent.indexOf("\n", initialPosition);
			if (Boolean.parseBoolean(fileContent.substring(initialPosition,
					finalPosition)) == true)
				isCompilableFile = true;
			else
				isCompilableFile = false;
			initialPosition = finalPosition + 1;
			
			// IS MAIN FILE
			finalPosition = fileContent.indexOf("\n", initialPosition);
			if (Boolean.parseBoolean(fileContent.substring(initialPosition,
					finalPosition)) == true)
				isMainFile = true;
			else
				isMainFile = false;
			initialPosition = finalPosition + 1;
			
			// IS OPENED
			finalPosition = fileContent.indexOf("\n", initialPosition);
			if (Boolean.parseBoolean(fileContent.substring(initialPosition,
					finalPosition)) == true)
				isOpened = true;
			else
				isOpened = false;
			initialPosition = finalPosition + 1;
			
			// Updates the ACIDE - A Configurable IDE file with the info
			file.setIsMainFile(isMainFile);
			file.setIsCompilableFile(isCompilableFile);
			file.setAbsolutePath(path);
			file.setParent(parent);
			file.setName(name);
			file.setIsDirectory(isDirectory);
			file.setIsOpened(isOpened);
			
			// Checks if exists
			File externalFile = new File(path);
			if(externalFile.exists()){
				
				// Adds the file to the list
				_fileList.add(file);
			}
			else{
				
				// Error message
				JOptionPane.showMessageDialog(null,
						AcideLanguageManager.getInstance().getLabels().getString("s970")
								+ AcideProjectConfiguration.getInstance()
										.getFileAt(index).getAbsolutePath()
								+ AcideLanguageManager.getInstance().getLabels().getString("s971"), "Error",
						JOptionPane.ERROR_MESSAGE);
				
				// The configuration has been modified
				_isModified = true;
			}
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
	 * @see AcideProjectFile
	 */
	public AcideProjectFile getFileAt(int index) {
		return _fileList.get(index);
	}

	/**
	 * Returns the lexicon configuration.
	 * 
	 * @return the lexicon configuration.
	 */
	public String getLexiconConfiguration() {
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
	 * @param projectFile
	 *            new file to add.
	 */
	public void addFile(AcideProjectFile projectFile) {
		_fileList.add(projectFile);
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
	 * Removes a file from the list which matches with the one given as a parameter.
	 * 
	 * @param name
	 *            file name to be removed.
	 */
	public void removeFileAt(String name) {
		_fileList.remove(name);
	}
	
	/**
	 * Returns the syntactic configuration.
	 * 
	 * @return the syntactic configuration.
	 */
	public String getGrammarConfiguration() {
		return _grammarConfiguration;
	}

	/**
	 * Sets a new value to the syntactic configurable.
	 * 
	 * @param syntacticConfiguration
	 *            new value to set.
	 */
	public void setGrammarConfiguration(String syntacticConfiguration) {
		_grammarConfiguration = syntacticConfiguration;
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
	public String getToolBarConfiguration() {
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
	 * Sets a new value for the lexicon configuration.
	 * 
	 * @param lexiconConfiguration
	 *            new value to set.
	 */
	public void setLexiconConfiguration(String lexiconConfiguration) {
		_lexiconConfiguration = lexiconConfiguration;
	}

	/**
	 * Sets a new value to the console configuration.
	 * 
	 * @param consoleConfiguration
	 *            new value to set.
	 */
	public void setOutputConfiguration(String consoleConfiguration) {
		_consoleConfiguration = consoleConfiguration;
	}

	/**
	 * Returns the console configuration.
	 * 
	 * @return the console configuration.
	 */
	public String getConsoleConfiguration() {
		return _consoleConfiguration;
	}
	
	/**
	 * Returns the file editor configuration.
	 * 
	 * @return the file editor configuration.
	 */
	public String getFileEditorConfiguration() {
		return _fileEditorConfiguration;
	}

	/**
	 * Sets a new value to the file editor configuration.
	 * 
	 * @param fileEditorConfiguration new value to set.
	 */
	public void setFileEditorConfiguration(String fileEditorConfiguration) {
		_fileEditorConfiguration = fileEditorConfiguration;
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE window configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE window configuration.
	 */
	public String getWindowConfiguration() {
		return _windowConfiguration;
	}
	
	/**
	 * Sets a new value to the ACIDE - A Configurable IDE window configuration.
	 * 
	 * @param windowConfiguration new value to set.
	 */
	public void setWindowConfiguration(String windowConfiguration){
		_windowConfiguration = windowConfiguration;
	}
	
	/**
	 * Returns the file from the file list which absolute path matches with
	 * the path given as a parameter.
	 * 
	 * @param absolutePath absolute path to compare with.
	 * 
	 * @return the file from the file list which absolute path matches with
	 * the path given as a parameter.
	 */
	public AcideProjectFile getFileAt(String absolutePath) {
	
		for(int index = 0; index < _fileList.size(); index++)
			if(_fileList.get(index).getAbsolutePath().equals(absolutePath))
				return _fileList.get(index);
		
		return null;
	}
}