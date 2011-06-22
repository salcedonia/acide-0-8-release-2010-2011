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
package acide.configuration.toolBar.consolePanelToolBar;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;

import acide.gui.toolBarPanel.consolePanelToolBar.utils.AcideParameterType;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE tool bar console panel tool bar configuration.
 * 
 * @version 0.8
 */
public class AcideConsolePanelToolBarConfiguration {

	/**
	 * ACIDE - A Configurable IDE tool bar console panel tool bar configuration
	 * end mark constant.
	 */
	private static final String END_MARK = "// End Of Console Panel Tool Bar Button Configuration";
	/**
	 * ACIDE - A Configurable IDE tool bar console panel tool bar configuration
	 * final list.
	 */
	private ArrayList<AcideConsolePanelToolBarButtonConf> _finalList;
	/**
	 * ACIDE - A Configurable IDE tool bar console panel tool bar configuration
	 * temporal list.
	 */
	private ArrayList<AcideConsolePanelToolBarButtonConf> _temporalList;

	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar console panel tool bar
	 * configuration.
	 */
	public AcideConsolePanelToolBarConfiguration() {

		// Creates the final list
		_finalList = new ArrayList<AcideConsolePanelToolBarButtonConf>();

		// Creates the temporal list
		_temporalList = new ArrayList<AcideConsolePanelToolBarButtonConf>();
	}

	/**
	 * Loads the ACIDE - A Configurable IDE console panel tool bar configuration
	 * from a file given as a parameter.
	 * 
	 * @param filePath
	 *            configuration file path.
	 * @throws IOException
	 */
	public ArrayList<AcideConsolePanelToolBarButtonConf> load(String filePath)
			throws IOException {

		// Creates the console panel tool bar button configuration list
		ArrayList<AcideConsolePanelToolBarButtonConf> list = new ArrayList<AcideConsolePanelToolBarButtonConf>();

		// Creates and initializes the variables
		String parameterTypeString;
		AcideParameterType parameterType = AcideParameterType.NONE;

		// Reads the file line by line
		BufferedReader bufferedReader = new BufferedReader(new FileReader(
				filePath));
		String line = null;

		// Creates the console panel tool bar button configuration
		AcideConsolePanelToolBarButtonConf buttonConfiguration = new AcideConsolePanelToolBarButtonConf();

		// Full read buttonConfigurations. It increases when it finds the mark
		// "//"
		int count = 0;

		while ((line = bufferedReader.readLine()) != null) {

			// It skips the empty lines
			if (!line.isEmpty()) {

				if (!line.startsWith("//")) {

					int lastIndexOfEqual = line.lastIndexOf("=");

					// Sets the name
					if (line.startsWith("name"))
						buttonConfiguration.setName(line.substring(
								lastIndexOfEqual + 2, line.length()));

					// Sets the action
					if (line.startsWith("action"))
						buttonConfiguration.setAction(line.substring(
								lastIndexOfEqual + 2, line.length()));

					// Sets the hint text
					if (line.startsWith("hintText"))
						buttonConfiguration.setHintText(line.substring(
								lastIndexOfEqual + 2, line.length()));

					// Sets the icon
					if (line.startsWith("icon"))
						buttonConfiguration.setIcon(line.substring(
								lastIndexOfEqual + 2, line.length()));

					// Sets the parameter type
					if (line.startsWith("parameterType")) {

						parameterTypeString = line.substring(
								lastIndexOfEqual + 2, line.length());
						if (parameterTypeString.matches("NONE"))
							parameterType = AcideParameterType.NONE;
						if (parameterTypeString.matches("TEXT"))
							parameterType = AcideParameterType.TEXT;
						if (parameterTypeString.matches("FILE"))
							parameterType = AcideParameterType.FILE;
						if (parameterTypeString.matches("DIRECTORY"))
							parameterType = AcideParameterType.DIRECTORY;
						buttonConfiguration.setParameterType(parameterType);
					}

					// Sets the is executed in system shell flag
					if (line.startsWith("isExecutedInSytemShell"))
						buttonConfiguration.setIsExecutedInSystemShell(Boolean
								.parseBoolean(line.substring(
										lastIndexOfEqual + 2, line.length())));
				} else {

					count++;
					// It has read one full button configuration
					if (count > 1) {

						// Adds the button configuration to the list
						list.add(buttonConfiguration);

						// Creates a new object for the next one
						buttonConfiguration = new AcideConsolePanelToolBarButtonConf();
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
	 * Saves the console panel tool bar button configuration list content into
	 * the configuration file given as a parameter.
	 * 
	 * @param filePath
	 *            configuration file path.
	 * @param list
	 *            list to save.
	 * @return true if it succeed and false in other case.
	 */
	public boolean save(String filePath,
			ArrayList<AcideConsolePanelToolBarButtonConf> list) {

		// Generates the file content
		String fileContent = generateFileContent(list);

		// Writes the file content into the file path
		return writeFileContentInFile(filePath, fileContent);
	}

	/**
	 * Writes the file content into the file path given as a parameter.
	 * 
	 * @param filePath
	 *            file path.
	 * @param fileContent
	 *            file content to append.
	 * 
	 * @return true if the operation succeed and false in other case.
	 */
	private boolean writeFileContentInFile(String filePath, String fileContent) {

		try {

			// Creates the print writer with the file
			PrintWriter printerWriter = new PrintWriter(new BufferedWriter(
					new FileWriter(filePath)));

			// Prints the file content into it
			printerWriter.print(fileContent);

			// Closes the print writer
			printerWriter.close();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s310")
							+ filePath);

			return true;
		} catch (IOException exception) {

			// Updates the log
			AcideLog.getLog().error(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s311")
							+ filePath);
			exception.printStackTrace();

			return false;
		}
	}

	/**
	 * Generates the file content to be stored afterwards in the specified file
	 * path in the save method.
	 * 
	 * @param list
	 *            ACIDE - A Configurable IDE console panel tool bar button
	 *            configuration list.
	 * @return the file content to be stored afterwards in the specified file
	 *         path in the save method.
	 */
	private String generateFileContent(
			ArrayList<AcideConsolePanelToolBarButtonConf> list) {
		String fileContent = "";

		for (AcideConsolePanelToolBarButtonConf buttonConfiguration : list) {

			// Gets the name
			String name = buttonConfiguration.getName();
			fileContent += "// buttonConfiguration " + name + "\n";
			fileContent += "name = " + name + "\n";

			// Gets the action
			String action = buttonConfiguration.getAction();
			fileContent += "action = " + action + "\n";

			// Gets the hint text
			String hintText = buttonConfiguration.getHintText();
			fileContent += "hintText = " + hintText + "\n";

			// Gets the icon
			String icon = buttonConfiguration.getIcon();
			fileContent += "icon = " + icon + "\n";

			// Gets the parameter type
			AcideParameterType parameterType = buttonConfiguration
					.getParameterType();

			switch (parameterType) {

			case NONE:
				fileContent += "parameterType = NONE\n";
				break;
			case TEXT:
				fileContent += "parameterType = TEXT\n";
				break;
			case FILE:
				fileContent += "parameterType = FILE\n";
				break;
			case DIRECTORY:
				fileContent += "parameterType = DIRECTORY\n";
				break;
			}

			// Gets the is executed in system shell
			boolean isExecutedInSytemShell = buttonConfiguration
					.isExecutedInSystemShell();
			fileContent += "isExecutedInSytemShell = " + isExecutedInSytemShell
					+ "\n";

			// Adds a blank line
			fileContent += "\n";
		}

		// Adds the end mark
		fileContent += END_MARK + "\n\n";
		return fileContent;
	}

	/**
	 * Saves the ACIDE - A Configurable IDE tool bar console panel tool bar
	 * configuration final list into a file.
	 * 
	 * @param path
	 *            file path.
	 * @return true if the result was succeed and false in other case.
	 */
	public boolean saveFinalList(String path) {
		return save(path, _finalList);
	}

	/**
	 * Saves the ACIDE - A Configurable IDE tool bar console panel tool bar
	 * configuration temporal list into a file.
	 * 
	 * @param path
	 *            file path.
	 * @return true if the result was succeed and false in other case.
	 */
	public boolean saveTemporalList(String path) {
		return save(path, _temporalList);
	}

	/**
	 * Loads the ACIDE - A Configurable IDE tool bar console panel tool bar
	 * configuration final list from the tool bar file.
	 * 
	 * @param path
	 *            tool bar file path.
	 * @throws Exception
	 *             when something wrong occurs.
	 */
	public void loadFinalList(String path) throws Exception {
		_finalList = load(path);
	}

	/**
	 * Loads the ACIDE - A Configurable IDE tool bar console panel tool bar
	 * configuration temporal list from the tool bar file.
	 * 
	 * @param path
	 *            tool bar file path.
	 * @throws Exception
	 *             when something wrong occurs.
	 */
	public void loadTemporalList(String path) throws Exception {
		_temporalList = load(path);
	}

	/**
	 * Loads the ACIDE - A Configurable IDE tool bar console panel tool bar
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
	 * Adds a buttonConfiguration given as a parameter to the ACIDE - A
	 * Configurable IDE tool bar console buttonConfiguration configuration final
	 * list.
	 * 
	 * @param buttonConfiguration
	 *            button configuration to add.
	 * @return true if the result succeed and false in other case.
	 */
	public boolean addbuttonConfiguration(
			AcideConsolePanelToolBarButtonConf buttonConfiguration) {
		return _finalList.add(buttonConfiguration);
	}

	/**
	 * Removes a buttonConfiguration given as a parameter from the ACIDE - A
	 * Configurable IDE tool bar console panel tool bar configuration final
	 * list.
	 * 
	 * @param buttonConfiguration
	 *            button configuration to remove.
	 * @return true if the result succeed and false in other case.
	 */
	public boolean removebuttonConfiguration(
			AcideConsolePanelToolBarButtonConf buttonConfiguration) {
		return _finalList.remove(buttonConfiguration);
	}

	/**
	 * Returns the ACIDE - ACIDE - A Configurable IDE tool bar console panel
	 * tool bar configuration final list size.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar console panel tool bar
	 *         configuration final list size.
	 */
	public int getSize() {
		return _finalList.size();
	}

	/**
	 * Returns the button configuration from the final list at the position
	 * given as a parameter.
	 * 
	 * @param index
	 *            index to return.
	 * @return the button configuration from the final list at the position
	 *         given as a parameter.
	 */
	public AcideConsolePanelToolBarButtonConf getButtonConfigurationAt(int index) {
		return (AcideConsolePanelToolBarButtonConf) _finalList.get(index);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar console panel tool bar
	 * configuration final list.
	 * 
	 * @return the ACIDE - ACIDE - A Configurable IDE tool bar console panel
	 *         tool bar configuration final list.
	 */
	public ArrayList<AcideConsolePanelToolBarButtonConf> getFinalList() {
		return _finalList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar console panel
	 * tool bar configuration final list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setFinalList(ArrayList<AcideConsolePanelToolBarButtonConf> list) {

		// Clears the final list
		_finalList.clear();

		// Adds all the button configurations to it
		for (AcideConsolePanelToolBarButtonConf buttonConfiguration : list)
			_finalList.add(buttonConfiguration);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar console panel
	 * tool bar configuration temporal list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setTemporalList(
			ArrayList<AcideConsolePanelToolBarButtonConf> list) {

		// Clears the temporal list
		_temporalList.clear();

		// Adds all the button configurations to it
		for (AcideConsolePanelToolBarButtonConf buttonConfiguration : list)
			_temporalList.add(buttonConfiguration);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar console panel tool bar
	 * configuration temporal list.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar console panel tool bar
	 *         configuration temporal list.
	 */
	public ArrayList<AcideConsolePanelToolBarButtonConf> getTemporalList() {
		return _temporalList;
	}
}
