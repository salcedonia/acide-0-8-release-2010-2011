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
package es.configuration.fileEditor;

import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import language.AcideLanguageManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import operations.fileEditor.FileEditorPanelConfiguration;
import operations.fileEditor.FileEditorPanelConfigurationList;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file editor configuration.
 * 
 * @version 0.8
 */
public class AcideFileEditorConfiguration {

	/**
	 * ACIDE - A Configurable IDE file editor configuration unique class
	 * instance.
	 */
	private static AcideFileEditorConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE file editor configuration file list which
	 * contains the list of opened files in the configuration file.
	 */
	private FileEditorPanelConfigurationList _fileEditorPanelConfigurationList;
	/**
	 * ACIDE - A Configurable IDE file editor configuration selected file editor
	 * panel selected index.
	 */
	private int _selectedFileEditorPanelIndex;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor configuration.
	 */
	public AcideFileEditorConfiguration() {

		// Creates the file editor panel configuration list
		_fileEditorPanelConfigurationList = new FileEditorPanelConfigurationList();
		_selectedFileEditorPanelIndex = -1;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration unique
	 * class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration unique
	 *         class instance.
	 */
	public static AcideFileEditorConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideFileEditorConfiguration();
		return _instance;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration file
	 * list.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration file
	 *         list.
	 */
	public FileEditorPanelConfigurationList getFileList() {
		return _fileEditorPanelConfigurationList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration file list.
	 * 
	 * @param fileList
	 *            new value to set.
	 */
	public void setFileList(FileEditorPanelConfigurationList fileList) {
		_fileEditorPanelConfigurationList = fileList;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager configuration
	 * selected file editor panel index.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager configuration
	 *         selected file editor panel index.
	 */
	public int getSelectedFileEditorPanelIndex() {
		return _selectedFileEditorPanelIndex;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor manager
	 * configuration selected file editor panel index.
	 * 
	 * @param selectedFileEditorPanelIndex
	 *            new value to set.
	 */
	public void setSelectedFileEditorPanelIndex(int selectedFileEditorPanelIndex) {
		_selectedFileEditorPanelIndex = selectedFileEditorPanelIndex;
	}

	/**
	 * Load the ACIDE - A Configurable IDE file editor manager configuration
	 * from an XML file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void load(String configurationFilePath) {

		// If the name is already set by the user
		if ((configurationFilePath != null)
				&& (!configurationFilePath.trim().equalsIgnoreCase(""))) {
			try {

				XStream x = new XStream(new DomDriver());
				FileInputStream f = new FileInputStream(configurationFilePath);

				// Gets the file editor manager configuration
				AcideFileEditorConfiguration fileEditorManagerConfiguration = (AcideFileEditorConfiguration) x
						.fromXML(f);

				// FILE LIST
				FileEditorPanelConfigurationList fileEditorPanelConfigurationList = fileEditorManagerConfiguration
						.getFileList();

				// SELECTED FILE EDITOR PANEL INDEX
				int selectedFileEditorPanelIndex = fileEditorManagerConfiguration
						.getSelectedFileEditorPanelIndex();

				f.close();

				// FILE LIST
				_fileEditorPanelConfigurationList = fileEditorPanelConfigurationList;

				// SELECTED FILE EDITOR PANEL INDEX
				_selectedFileEditorPanelIndex = selectedFileEditorPanelIndex;

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"fileEditorConfiguration", configurationFilePath);

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s990"));
				exception.printStackTrace();

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"fileEditorConfiguration", configurationFilePath);
			}
		}
	}

	/**
	 * Saves the ACIDE - A Configurable IDE file editor manager configuration in
	 * a XML file and returns true if the operation was succeed or false in
	 * other case.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean save() {

		// FILE LIST
		_fileEditorPanelConfigurationList = new FileEditorPanelConfigurationList();

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// If there are opened file editors
		if (selectedFileEditorPanelIndex != -1) {

			// Analyzes all the opened file editor panels
			for (int index = 0; index < MainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// Gets the selected file editor panel
				AcideFileEditorPanel fileEditorPanel = MainWindow.getInstance()
						.getFileEditorManager().getFileEditorPanelAt(index);

				// Creates the file editor panel configuration for the current
				// file editor panel
				FileEditorPanelConfiguration fileEditorPanelConfiguration = new FileEditorPanelConfiguration();

				// PATH
				fileEditorPanelConfiguration.setPath(fileEditorPanel
						.getAbsolutePath());

				// CARET POSITION
				fileEditorPanelConfiguration.setCaretPosition(fileEditorPanel
						.getActiveTextEditionArea().getCaretPosition());

				// TYPE
				String type = "Normal";
				if (fileEditorPanel.isCompilableFile())
					type = "Compilable";
				if (fileEditorPanel.isMainFile())
					type = "Main";
				fileEditorPanelConfiguration.setType(type);

				// Inserts the file editor panel configuration into the list
				_fileEditorPanelConfigurationList
						.insertFileEditorPanelConfiguration(fileEditorPanelConfiguration);
			}
		}

		// SELECTED FILE EDITOR PANEL INDEX
		_selectedFileEditorPanelIndex = selectedFileEditorPanelIndex;

		// Creates the xStream object to handle XML files
		XStream xStream = new XStream();

		try {

			// Saves the file
			FileOutputStream file = new FileOutputStream(
					"./configuration/fileEditor/configuration.xml");

			// Saves the content into XML format
			xStream.toXML(this, file);

			// Closes the file
			file.close();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
			return false;
		}

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty("fileEditorConfiguration",
				"./configuration/fileEditor/configuration.xml");

		return true;
	}

	/**
	 * Returns the number of files from the list.
	 * 
	 * @return the number of files from the list.
	 */
	public int getNumFilesFromList() {
		return _fileEditorPanelConfigurationList.getSize();
	}

	/**
	 * Returns the file editor panel configuration at the position in the list
	 * given as a parameter.
	 * 
	 * @param index
	 *            position to get.
	 * 
	 * @return the file editor panel configuration at the position in the list
	 *         given as a parameter.
	 */
	public FileEditorPanelConfiguration getFileAt(int index) {
		return _fileEditorPanelConfigurationList
				.getFileEditorPanelConfigurationAt(index);
	}
}
