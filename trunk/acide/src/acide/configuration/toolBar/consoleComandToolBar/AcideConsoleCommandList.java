package acide.configuration.toolBar.consoleComandToolBar;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;

import acide.files.AcideFileManager;
import acide.gui.toolBarPanel.consoleCommandToolBar.utils.AcideParameterType;

/**
 * Shell tool bar command list of ACIDE - A Configurable IDE.
 * 
 * @version 0.8
 */
public class AcideConsoleCommandList {

	/**
	 * Shell command final list.
	 */
	private static ArrayList<AcideConsoleCommand> _finalList;
	/**
	 * Shell command list to store temporally the loaded values from the
	 * configuration file.
	 */
	private static ArrayList<AcideConsoleCommand> _temporalList;

	/**
	 * Creates a new shell command list. Creates both final and temporal lists.
	 */
	public AcideConsoleCommandList() {

		_finalList = new ArrayList<AcideConsoleCommand>();
		_temporalList = new ArrayList<AcideConsoleCommand>();
	}

	/**
	 * Clears the shell command lists.
	 */
	public static void clear() {

		_finalList = new ArrayList<AcideConsoleCommand>();
		_temporalList = new ArrayList<AcideConsoleCommand>();
	}

	/**
	 * Saves the list into a file.
	 * 
	 * @param path
	 *            file path.
	 * @param temporalList
	 * 			  temporal list to be loaded.
	 * @return true if the result was succeed and false in other case.
	 */
	public static boolean save(String path,
			ArrayList<AcideConsoleCommand> temporalList) {

		String textContent = "";

		for (AcideConsoleCommand shellCommand : temporalList) {

			// NAME
			String name = shellCommand.getName();
			textContent += "// Command " + name + "\n";
			textContent += "name = " + name + "\n";
			
			// ACTION
			String action = shellCommand.getAction();
			textContent += "action = " + action + "\n";
			
			// HINT TEXT
			String hintText = shellCommand.getHintText();
			textContent += "hintText = " + hintText + "\n";
			
			// ICON
			String icon = shellCommand.getIcon();
			textContent += "icon = " + icon + "\n";

			// EXTRA PARAMETER
			AcideParameterType parameterType = shellCommand.getParameterType();
			
			switch (parameterType) {

			case NONE:
				textContent += "parameterType = NONE\n";
				break;
			case TEXT:
				textContent += "parameterType = TEXT\n";
				break;
			case FILE:
				textContent += "parameterType = FILE\n";
				break;
			case DIRECTORY:
				textContent += "parameterType = DIRECTORY\n";
				break;
			}

			textContent += "\n";
		}

		textContent += "// End";

		// Saves the content and returns the updated file
		return AcideFileManager.getInstance().write(path, textContent);
	}

	/**
	 * Saves the final list into a file.
	 * 
	 * @param path
	 *            file path.
	 * @return true if the result was succeed and false in other case.
	 */
	public static boolean saveFinalList(String path) {
		return save(path, _finalList);
	}

	/**
	 * Saves the temporal list into a file.
	 * 
	 * @param path
	 *            file path.
	 * @return true if the result was succeed and false in other case.
	 */
	public static boolean saveTemporalList(String path) {
		return save(path, _temporalList);
	}

	/**
	 * Loads the list from a file.
	 * 
	 * @param path
	 *            file path.
	 * @param temporalList
	 * 			  temporal list to be loaded.
	 * @throws Exception
	 *             IO Exception.
	 */
	public static void load(String path,
			ArrayList<AcideConsoleCommand> temporalList) throws Exception {

		// Clears the shell command list
		temporalList.clear();

		// Creates and initializes the variables
		String parameterTypeString;
		AcideParameterType parameterType = AcideParameterType.NONE;

		// Reads the file line by line
		BufferedReader bufferedReader = new BufferedReader(new FileReader(path));
		String line = null;

		AcideConsoleCommand shellCommand = new AcideConsoleCommand();

		// Full read commands. It increases when it finds the mark //
		int count = 0;

		while ((line = bufferedReader.readLine()) != null) {

			// It skips the empty lines
			if (!line.isEmpty()) {

				if (!line.startsWith("//")) {

					int lastIndexOfParentesis = line.lastIndexOf("=");

					// NAME
					if (line.startsWith("name"))
						shellCommand.setName(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// ACTION
					if (line.startsWith("action"))
						shellCommand.setAction(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// HINT TEXT
					if (line.startsWith("hintText"))
						shellCommand.setHintText(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// ICON
					if (line.startsWith("icon"))
						shellCommand.setIcon(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// PARAMETER TYPE
					if (line.startsWith("parameterType")) {

						parameterTypeString = line.substring(
								lastIndexOfParentesis + 2, line.length());
						if (parameterTypeString.matches("NONE"))
							parameterType = AcideParameterType.NONE;
						if (parameterTypeString.matches("TEXT"))
							parameterType = AcideParameterType.TEXT;
						if (parameterTypeString.matches("FILE"))
							parameterType = AcideParameterType.FILE;
						if (parameterTypeString.matches("DIRECTORY"))
							parameterType = AcideParameterType.DIRECTORY;
						shellCommand.setParameterType(parameterType);
					}
				} else {

					count++;
					// It has read one full command
					if (count > 1) {

						// Adds the shell command to the list
						temporalList.add(shellCommand);

						// Creates a new object for the next one
						shellCommand = new AcideConsoleCommand();
					}
				}
			}
		}
	}

	/**
	 * Loads the shell command list from the tool bar file.
	 * 
	 * @param path
	 *            tool bar file path.
	 * @throws Exception
	 *             when something wrong occurs.
	 */
	public static void loadFinalList(String path) throws Exception {
		load(path, _finalList);
	}

	/**
	 * Loads the shell command temporal list from the tool bar file.
	 * 
	 * @param path
	 *            tool bar file path.
	 * @throws Exception
	 *             when something wrong occurs.
	 */
	public static void loadTemporalList(String path) throws Exception {
		load(path, _temporalList);
	}

	/**
	 * Adds a command given as a parameter to the shell command final list.
	 * 
	 * @param command
	 *            command to add.
	 * @return true if the result was succeed and false in other case.
	 */
	public static boolean addCommand(AcideConsoleCommand command) {
		return _finalList.add(command);
	}

	/**
	 * Removes a command given as a parameter from the shell command final list.
	 * 
	 * @param command
	 *            command to remove.
	 * @return true if the result was succeed and false in other case.
	 */
	public static boolean removeCommand(AcideConsoleCommand command) {
		return _finalList.remove(command);
	}

	/**
	 * Returns the shell command final list size.
	 * 
	 * @return the shell command final list size.
	 */
	public static int getSize() {
		return _finalList.size();
	}

	/**
	 * Returns the shell command at the position given as a parameter, from the
	 * final list
	 * 
	 * @param position
	 *            position to return.
	 * @return the shell command at the position of the list given as a
	 *         parameter.
	 */
	public static AcideConsoleCommand getShellCommandAt(int position) {
		return (AcideConsoleCommand) _finalList.get(position);
	}

	/**
	 * Returns the shell command final list.
	 * 
	 * @return the shell command final list.
	 */
	public static ArrayList<AcideConsoleCommand> getFinalList() {
		return _finalList;
	}

	/**
	 * Sets a new value to the shell command final list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public static void setFinalList(ArrayList<AcideConsoleCommand> list) {

		_finalList.clear();

		for (AcideConsoleCommand shellCommand : list)
			_finalList.add(shellCommand);
	}

	/**
	 * Sets a new value to the shell command temporal list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public static void setTemporalList(ArrayList<AcideConsoleCommand> list) {

		_temporalList.clear();

		for (AcideConsoleCommand shellCommand : list)
			_temporalList.add(shellCommand);
	}

	/**
	 * Returns the shell command temporal list.
	 * 
	 * @return the shell command temporal list.
	 */
	public static ArrayList<AcideConsoleCommand> getTemporalList() {
		return _temporalList;
	}
}
