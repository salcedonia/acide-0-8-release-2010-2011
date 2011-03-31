package acide.configuration.toolBar.consoleComandToolBar;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;

import acide.files.AcideFileManager;
import acide.gui.toolBarPanel.consoleCommandToolBar.utils.AcideParameterType;

/**
 * ACIDE - A Configurable IDE tool bar console command configuration.
 * 
 * @version 0.8
 */
public class AcideConsoleCommandConfiguration {

	/**
	 * ACIDE - A Configurable IDE tool bar console command configuration unique
	 * class instance.
	 */
	private static AcideConsoleCommandConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE tool bar console command configuration final
	 * list.
	 */
	private ArrayList<AcideConsoleCommand> _finalList;
	/**
	 * ACIDE - A Configurable IDE tool bar console command configuration
	 * temporal list.
	 */
	private ArrayList<AcideConsoleCommand> _temporalList;

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar console command
	 * configuration unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar console command
	 *         configuration unique class instance.
	 */
	public static AcideConsoleCommandConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideConsoleCommandConfiguration();

		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar console command
	 * configuration.
	 */
	public AcideConsoleCommandConfiguration() {

		_finalList = new ArrayList<AcideConsoleCommand>();
		_temporalList = new ArrayList<AcideConsoleCommand>();
	}

	/**
	 * Clears the ACIDE - A Configurable IDE tool bar console command
	 * configuration lists.
	 */
	public void clear() {

		_finalList = new ArrayList<AcideConsoleCommand>();
		_temporalList = new ArrayList<AcideConsoleCommand>();
	}

	/**
	 * Saves the ACIDE - A Configurable IDE tool bar console command
	 * configuration lists into a file.
	 * 
	 * @param path
	 *            file path.
	 * @param temporalList
	 *            temporal list to be loaded.
	 * @return true if the result was succeed and false in other case.
	 */
	public static boolean save(String path,
			ArrayList<AcideConsoleCommand> temporalList) {

		String textContent = "";

		for (AcideConsoleCommand shellCommand : temporalList) {

			// Gets the name
			String name = shellCommand.getName();
			textContent += "// Command " + name + "\n";
			textContent += "name = " + name + "\n";

			// Gets the action
			String action = shellCommand.getAction();
			textContent += "action = " + action + "\n";

			// Gets the hint text
			String hintText = shellCommand.getHintText();
			textContent += "hintText = " + hintText + "\n";

			// Gets the icon
			String icon = shellCommand.getIcon();
			textContent += "icon = " + icon + "\n";

			// Gets the parameter type
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
	 * Saves the ACIDE - A Configurable IDE tool bar console command
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
	 * Saves the ACIDE - A Configurable IDE tool bar console command
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
	 * Loads the ACIDE - A Configurable IDE tool bar console command
	 * configuration lists from a file.
	 * 
	 * @param path
	 *            file path.
	 * @param temporalList
	 *            temporal list to be loaded.
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

		// Creates the console command
		AcideConsoleCommand consoleCommand = new AcideConsoleCommand();

		// Full read commands. It increases when it finds the mark //
		int count = 0;

		while ((line = bufferedReader.readLine()) != null) {

			// It skips the empty lines
			if (!line.isEmpty()) {

				if (!line.startsWith("//")) {

					int lastIndexOfParentesis = line.lastIndexOf("=");

					// Sets the name
					if (line.startsWith("name"))
						consoleCommand.setName(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// Sets the action
					if (line.startsWith("action"))
						consoleCommand.setAction(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// Sets the hint text
					if (line.startsWith("hintText"))
						consoleCommand.setHintText(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// Sets the icon
					if (line.startsWith("icon"))
						consoleCommand.setIcon(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// Sets the parameter type
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
						consoleCommand.setParameterType(parameterType);
					}
				} else {

					count++;
					// It has read one full command
					if (count > 1) {

						// Adds the shell command to the list
						temporalList.add(consoleCommand);

						// Creates a new object for the next one
						consoleCommand = new AcideConsoleCommand();
					}
				}
			}
		}
	}

	/**
	 * Loads the ACIDE - A Configurable IDE tool bar console command
	 * configuration final list from the tool bar file.
	 * 
	 * @param path
	 *            tool bar file path.
	 * @throws Exception
	 *             when something wrong occurs.
	 */
	public void loadFinalList(String path) throws Exception {
		load(path, _finalList);
	}

	/**
	 * Loads the ACIDE - A Configurable IDE tool bar console command
	 * configuration temporal list from the tool bar file.
	 * 
	 * @param path
	 *            tool bar file path.
	 * @throws Exception
	 *             when something wrong occurs.
	 */
	public void loadTemporalList(String path) throws Exception {
		load(path, _temporalList);
	}

	/**
	 * Adds a command given as a parameter to the ACIDE - A Configurable IDE
	 * tool bar console command configuration final list.
	 * 
	 * @param command
	 *            command to add.
	 * @return true if the result was succeed and false in other case.
	 */
	public boolean addCommand(AcideConsoleCommand command) {
		return _finalList.add(command);
	}

	/**
	 * Removes a command given as a parameter from the ACIDE - A Configurable
	 * IDE tool bar console command configuration final list.
	 * 
	 * @param command
	 *            command to remove.
	 * @return true if the result was succeed and false in other case.
	 */
	public boolean removeCommand(AcideConsoleCommand command) {
		return _finalList.remove(command);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar console command
	 * configuration final list size.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar console command
	 *         configuration final list size.
	 */
	public int getSize() {
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
	public AcideConsoleCommand getShellCommandAt(int position) {
		return (AcideConsoleCommand) _finalList.get(position);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar console command
	 * configuration final list.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar console command
	 *         configuration final list.
	 */
	public ArrayList<AcideConsoleCommand> getFinalList() {
		return _finalList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar console
	 * command configuration final list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setFinalList(ArrayList<AcideConsoleCommand> list) {

		_finalList.clear();

		for (AcideConsoleCommand shellCommand : list)
			_finalList.add(shellCommand);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar console
	 * command configuration temporal list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public void setTemporalList(ArrayList<AcideConsoleCommand> list) {

		_temporalList.clear();

		for (AcideConsoleCommand shellCommand : list)
			_temporalList.add(shellCommand);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar console command
	 * configuration temporal list.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar console command
	 *         configuration temporal list.
	 */
	public ArrayList<AcideConsoleCommand> getTemporalList() {
		return _temporalList;
	}
}
