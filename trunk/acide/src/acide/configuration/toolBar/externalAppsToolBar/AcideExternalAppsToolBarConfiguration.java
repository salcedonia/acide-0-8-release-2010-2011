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
package acide.configuration.toolBar.externalAppsToolBar;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE external applications tool bar configuration.
 * 
 * @version 0.8
 */
public class AcideExternalAppsToolBarConfiguration {

	/**
	 * ACIDE - A Configurable IDE tool bar console panel tool bar configuration
	 * end mark constant.
	 */
	private static final String END_MARK = "// End Of External Applications Tool Bar Button Configuration";
	/**
	 * ACIDE - A Configurable IDE external applications tool bar configuration
	 * final list.
	 */
	private ArrayList<AcideExternalAppsToolBarButtonConf> _finalList;
	/**
	 * ACIDE - A Configurable IDE external applications tool bar configuration
	 * temporal list.
	 */
	private ArrayList<AcideExternalAppsToolBarButtonConf> _temporalList;

	/**
	 * Creates a new ACIDE - A Configurable IDE external applications tool bar
	 * configuration.
	 */
	public AcideExternalAppsToolBarConfiguration() {

		// Creates the final list
		_finalList = new ArrayList<AcideExternalAppsToolBarButtonConf>();

		// Creates the temporal list
		_temporalList = new ArrayList<AcideExternalAppsToolBarButtonConf>();
	}

	/**
	 * Loads the ACIDE - A Configurable IDE external applications tool bar
	 * configuration from a file given as a parameter.
	 * 
	 * @param filePath
	 *            configuration file path.
	 * @throws IOException
	 *             console
	 */
	public ArrayList<AcideExternalAppsToolBarButtonConf> load(String filePath)
			throws IOException {

		// Clears the external application button configuration list
		ArrayList<AcideExternalAppsToolBarButtonConf> list = new ArrayList<AcideExternalAppsToolBarButtonConf>();

		// Reads the file line by line
		BufferedReader bufferedReader = new BufferedReader(new FileReader(
				filePath));
		String line = null;

		// Creates the external application button
		AcideExternalAppsToolBarButtonConf externalApplicationButton = new AcideExternalAppsToolBarButtonConf();

		// Full read commands. It increases when it finds the mark //
		int count = 0;

		// Puts the starting line of the external applications button
		// configuration
		while ((line = bufferedReader.readLine()) != null
				&& !line.startsWith("// End Of Console Panel Tool Bar Button Configuration")) {
		}

		while ((line = bufferedReader.readLine()) != null) {

			// It skips the empty lines
			if (!line.isEmpty()) {

				if (!line.startsWith("//")) {

					// Gets the last index of the equal symbol
					int lastIndexOfEqual = line.lastIndexOf("=");

					// Sets the name
					if (line.startsWith("name"))
						externalApplicationButton.setName(line.substring(
								lastIndexOfEqual + 2, line.length()));

					// Sets the path
					if (line.startsWith("path"))
						externalApplicationButton.setPath(line.substring(
								lastIndexOfEqual + 2, line.length()));

					// Sets the hint text
					if (line.startsWith("hintText"))
						externalApplicationButton.setHintText(line.substring(
								lastIndexOfEqual + 2, line.length()));

					// Sets the icon
					if (line.startsWith("icon"))
						externalApplicationButton.setIcon(line.substring(
								lastIndexOfEqual + 2, line.length()));
				} else {

					count++;

					// It has read one full command
					if (count > 1) {

						// Adds the shell command to the list
						list.add(externalApplicationButton);

						// Creates a new object for the next one
						externalApplicationButton = new AcideExternalAppsToolBarButtonConf();
					}

					// If it is the end of the console panel tool bar button
					// configuration
					if (line.startsWith(END_MARK))
						return list;
				}
			}
		}

		return list;
	}

	/**
	 * Saves the ACIDE - A Configurable IDE external applications tool bar
	 * configuration lists into a file.
	 * 
	 * @param filePath
	 *            file path.
	 * @param list
	 *            list to be saved.
	 * @return true if the result succeed and false in other case.
	 */
	public boolean save(String filePath,
			ArrayList<AcideExternalAppsToolBarButtonConf> list) {

		// Generates the file content
		String fileContent = generateFileContent(list);

		// Appends the file content in the file path
		return appendTextInFile(filePath, fileContent);
	}

	/**
	 * <p>
	 * Appends the file content to the file path content.
	 * </p>
	 * <p>
	 * When this method has been invoked in
	 * {@link AcideToolBarConfigurationWindow}, the file defined in the given
	 * parameter <b>file path</b> contains already the configuration of the
	 * console panel tool bar, so it is mandatory to append the file content and
	 * not to overwrite the file.
	 * </p>
	 * 
	 * @param filePath
	 *            file path.
	 * @param fileContent
	 *            file content to append.
	 * 
	 * @return true if the operation succeed and false in other case.
	 */
	private boolean appendTextInFile(String filePath, String fileContent) {

		BufferedWriter bufferedWriter = null;
		try {

			// Creates the buffered writer
			bufferedWriter = new BufferedWriter(new FileWriter(filePath, true));

			// Appends the text in the file path
			bufferedWriter.append(fileContent);
		} catch (IOException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			return false;

		} finally {
			if (bufferedWriter != null) {
				try {

					// Closes the buffered writer
					bufferedWriter.close();
				} catch (IOException exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();

					return false;
				}
			}
		}

		return true;
	}

	/**
	 * Generates the file content to be stored afterwards in the specified file
	 * path in the save method.
	 * 
	 * @param list
	 *            ACIDE - A Configurable IDE external applications tool bar
	 *            button configuration list.
	 * @return the file content to be stored afterwards in the specified file
	 *         path in the save method.
	 */
	private String generateFileContent(
			ArrayList<AcideExternalAppsToolBarButtonConf> list) {
		String fileContent = "";

		for (AcideExternalAppsToolBarButtonConf buttonConfiguration : list) {

			// Gets the name
			String name = buttonConfiguration.getName();
			fileContent += "// Command " + name + "\n";
			fileContent += "name = " + name + "\n";

			// Gets the path text
			String applicationPath = buttonConfiguration.getPath();
			fileContent += "path = " + applicationPath + "\n";

			// Gets the hint text
			String hintText = buttonConfiguration.getHintText();
			fileContent += "hintText = " + hintText + "\n";

			// Gets the icon
			String icon = buttonConfiguration.getIcon();
			fileContent += "icon = " + icon + "\n";

			// Adds a blank line
			fileContent += "\n";
		}

		// Adds the end mark
		fileContent += END_MARK;
		return fileContent;
	}

	/**
	 * Saves the ACIDE - A Configurable IDE external applications tool bar
	 * configuration final list into the configuration file.
	 * 
	 * @param path
	 *            file path.
	 * @return true if the result succeed and false in other case.
	 */
	public boolean saveFinalList(String path) {
		return save(path, _finalList);
	}

	/**
	 * Saves the ACIDE - A Configurable IDE external applications tool bar
	 * configuration temporal list into a file.
	 * 
	 * @param path
	 *            file path.
	 * @return true if the result succeed and false in other case.
	 */
	public boolean saveTemporalList(String path) {
		return save(path, _temporalList);
	}

	/**
	 * Loads the ACIDE - A Configurable IDE external applications tool bar
	 * configuration final list from the configuration file.
	 * 
	 * @param path
	 *            configuration file path.
	 * @throws Exception
	 *             when something wrong occurs.
	 */
	public void loadFinalList(String path) throws Exception {
		_finalList = load(path);
	}

	/**
	 * Loads the ACIDE - A Configurable IDE external applications tool bar
	 * configuration temporal list from the configuration file.
	 * 
	 * @param path
	 *            configuration file path.
	 * @throws Exception
	 *             when something wrong occurs.
	 */
	public void loadTemporalList(String path) throws Exception {
		_temporalList = load(path);
	}

	/**
	 * Loads the ACIDE - A Configurable IDE external applications tool bar
	 * configuration final and temporal list from the tool bar file.
	 * 
	 * @param filePath
	 *            tool bar file.
	 * 
	 * @throws Exception
	 *             when something unusual occurs.
	 */
	public void loadLists(String filePath) throws Exception {

		// Loads the final list
		loadFinalList(filePath);

		// Loads the temporal list
		loadTemporalList(filePath);
	}

	/**
	 * Adds a button configuration given as a parameter to the ACIDE - A
	 * Configurable IDE external applications button configuration final list.
	 * 
	 * @param buttonConfiguration
	 *            button configuration to add.
	 * @return true if the result was succeed and false in other case.
	 */
	public boolean addButtonConfiguration(
			AcideExternalAppsToolBarButtonConf buttonConfiguration) {
		return _finalList.add(buttonConfiguration);
	}

	/**
	 * Removes a button configuration given as a parameter from the ACIDE - A
	 * Configurable IDE external applications tool bar configuration final list.
	 * 
	 * @param buttonConfiguration
	 *            button configuration to remove.
	 * @return true if the result was succeed and false in other case.
	 */
	public boolean removeButtonConfiguration(
			AcideExternalAppsToolBarButtonConf buttonConfiguration) {
		return _finalList.remove(buttonConfiguration);
	}

	/**
	 * Returns the ACIDE - ACIDE - A Configurable IDE external applications tool
	 * bar configuration final list size.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar
	 *         configuration final list size.
	 */
	public int getSize() {
		return _finalList.size();
	}

	/**
	 * Returns the button configuration at the position of the ACIDE - A
	 * Configurable IDE external applications tool bar configuration final list
	 * given as a parameter.
	 * 
	 * @param position
	 *            position to return.
	 * @return the button configuration at the position of the ACIDE - A
	 *         Configurable IDE external applications tool bar configuration
	 *         final list given as a parameter.
	 */
	public AcideExternalAppsToolBarButtonConf getButtonConfigurationAt(
			int position) {
		return (AcideExternalAppsToolBarButtonConf) _finalList.get(position);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications tool bar
	 * configuration final list.
	 * 
	 * @return the ACIDE - ACIDE - A Configurable IDE external applications tool
	 *         bar configuration final list.
	 */
	public ArrayList<AcideExternalAppsToolBarButtonConf> getFinalList() {
		return _finalList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE external applications
	 * tool bar configuration final list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setFinalList(ArrayList<AcideExternalAppsToolBarButtonConf> list) {

		// Clears the final list
		_finalList.clear();

		// Adds all the button configurations to it
		for (AcideExternalAppsToolBarButtonConf buttonConfiguration : list)
			_finalList.add(buttonConfiguration);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE external applications
	 * tool bar configuration temporal list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setTemporalList(
			ArrayList<AcideExternalAppsToolBarButtonConf> list) {

		// Clears the temporal list
		_temporalList.clear();

		// Adds all the button configurations to it
		for (AcideExternalAppsToolBarButtonConf buttonConfiguration : list)
			_temporalList.add(buttonConfiguration);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications tool bar
	 * configuration temporal list.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar
	 *         configuration temporal list.
	 */
	public ArrayList<AcideExternalAppsToolBarButtonConf> getTemporalList() {
		return _temporalList;
	}
}
