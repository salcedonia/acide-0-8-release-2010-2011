package es.configuration.toolBar.shellComandToolBar;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Vector;

import es.text.TextFile;
import gui.toolBarPanel.shellCommandToolBar.ParameterType;

/************************************************************************
 * Shell tool bar command list of ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
public class ShellCommandList {

	/**
	 * Shell command final list.
	 */
	private static Vector<ShellCommand> _finalList;
	/**
	 * Shell command list to store temporally the loaded values from the
	 * configuration file.
	 */
	private static Vector<ShellCommand> _temporalList;

	/**
	 * Creates a new shell command list. Creates both final and temporal lists.
	 */
	public ShellCommandList() {

		_finalList = new Vector<ShellCommand>();
		_temporalList = new Vector<ShellCommand>();
	}

	/**
	 * Clears the shell command lists.
	 */
	public static void clear() {

		_finalList = new Vector<ShellCommand>();
		_temporalList = new Vector<ShellCommand>();
	}

	/**
	 * Saves the list into a file.
	 * 
	 * @param path
	 *            file path.
	 * @return true if the result was succeed and false in other case.
	 */
	public static boolean save(String path, Vector<ShellCommand> list) {

		String textContent = "";

		for (ShellCommand shellCommand : list) {

			// NAME
			String name = shellCommand.getName();

			// ACTION
			String action = shellCommand.getAction();

			// HINT TEXT
			String hintText = shellCommand.getHintText();

			// HAS ICON
			boolean hasIcon = shellCommand.getHasIcon();

			// ICON
			String icon = shellCommand.getIcon();

			// EXTRA PARAMETER
			ParameterType parameterType = shellCommand.getParameterType();

			textContent += "// Command " + name + "\n";
			textContent += "name = " + name + "\n";
			textContent += "action = " + action + "\n";
			textContent += "hintText = " + hintText + "\n";
			textContent += "hasIcon = " + hasIcon + "\n";
			textContent += "icon = " + icon + "\n";
			textContent += "parameterType = " + parameterType.toString() + "\n";
			textContent += "\n";
		}

		textContent += "// End";
			
		// Creates the text file
		TextFile textFile = new TextFile();

		// Saves the content and returns the updated file
		return textFile.save(path, textContent);
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
	 * @throws Exception
	 *             IO Exception.
	 */
	public static void load(String path, Vector<ShellCommand> list)
			throws Exception {

		// Clears the shell command list
		list.removeAllElements();

		// Creates and initializes the variables
		String hasIconString = null;
		boolean hasIconBoolean = false;
		String parameterTypeString;
		ParameterType parameterType = ParameterType.NONE;

		// Reads the file line by line
		BufferedReader bufferedReader = new BufferedReader(new FileReader(path));
		String line = null;

		ShellCommand shellCommand = new ShellCommand();

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

					// HAS ICON
					if (line.startsWith("hasIcon")) {

						hasIconString = line.substring(
								lastIndexOfParentesis + 2, line.length());
						if (hasIconString.matches("true"))
							hasIconBoolean = true;
						if (hasIconString.matches("false"))
							hasIconBoolean = false;
						
						shellCommand.setHasIcon(hasIconBoolean);
					}
					// ICON
					if (line.startsWith("icon"))
						shellCommand.setIcon(line.substring(
								lastIndexOfParentesis + 2, line.length()));

					// PARAMETER TYPE
					if (line.startsWith("parameterType")) {

						parameterTypeString = line.substring(
								lastIndexOfParentesis + 2, line.length());
						if (parameterTypeString.matches("NONE"))
							parameterType = ParameterType.NONE;
						if (parameterTypeString.matches("TEXT"))
							parameterType = ParameterType.TEXT;
						if (parameterTypeString.matches("FILE"))
							parameterType = ParameterType.FILE;
						if (parameterTypeString.matches("DIRECTORY"))
							parameterType = ParameterType.DIRECTORY;
						shellCommand
								.setParameterType(parameterType);
					}
				} else {

					count++;
					// It has read one full command
					if (count > 1) {

						// Adds the shell command to the list
						list.add(shellCommand);

						// Creates a new object for the next one
						shellCommand = new ShellCommand();
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
	public static boolean addCommand(ShellCommand command) {
		return _finalList.add(command);
	}

	/**
	 * Removes a command given as a parameter from the shell command final list.
	 * 
	 * @param command
	 *            command to remove.
	 * @return true if the result was succeed and false in other case.
	 */
	public static boolean removeCommand(ShellCommand command) {
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
	public static ShellCommand getShellCommandAt(int position) {
		return (ShellCommand) _finalList.get(position);
	}

	/**
	 * Returns the shell command final list.
	 * 
	 * @return the shell command final list.
	 */
	public static Vector<ShellCommand> getFinalList() {
		return _finalList;
	}

	/**
	 * Sets a new value to the shell command final list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public static void setFinalList(Vector<ShellCommand> list) {
		
		_finalList.clear();
		
		for(ShellCommand shellCommand : list)
			_finalList.add(shellCommand);
	}

	/**
	 * Sets a new value to the shell command temporal list.
	 * 
	 * @param list
	 *            new value to set.
	 */
	public static void setTemporalList(Vector<ShellCommand> list) {

		_temporalList.clear();
		
		for(ShellCommand shellCommand : list)
			_temporalList.add(shellCommand);
	}

	/**
	 * Returns the shell command temporal list.
	 * 
	 * @return the shell command temporal list.
	 */
	public static Vector<ShellCommand> getTemporalList() {
		return _temporalList;
	}
}
