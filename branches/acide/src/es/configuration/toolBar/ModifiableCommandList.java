package es.configuration.toolBar;

import java.util.ResourceBundle;
import java.util.Vector;

import operations.log.Log;

import language.Language;
import properties.PropertiesManager;

import es.text.TextFile;

/************************************************************************																
 * Modifiable tool bar command list of ACIDE - A Configurable IDE											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
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
public class ModifiableCommandList {

	/**
	 * Command list
	 */
	private static Vector<ModifiableCommand> _commandList;
	/**
	 * Command list to store temporally the loaded values from the configuration
	 * file
	 */
	private static Vector<ModifiableCommand> _commandAuxList;

	/**
	 * Class constructor
	 */
	public ModifiableCommandList() {

		_commandList = new Vector<ModifiableCommand>();
		_commandAuxList = new Vector<ModifiableCommand>();
	}

	/**
	 * Clears the command list
	 */
	public static void clear() {
		_commandList = new Vector<ModifiableCommand>();
		_commandAuxList = new Vector<ModifiableCommand>();
	}

	/**
	 * Saves the list into a file
	 * 
	 * @param path
	 *            file path
	 * @return true if the result was succeed and false in other case
	 */
	public static boolean saveList(String path) {

		String textContent = "";
		for (int i = 0; i < _commandList.size(); i++) {

			ModifiableCommand icon = (ModifiableCommand) _commandList.get(i);
			String name = icon.getName();
			String command = icon.getCommand();
			String txtHelp = icon.getHelpText();
			String image;

			if (icon.getHasIcon()) {
				image = icon.getIcon();
				textContent += "]N] " + name + " ]C] " + command + " ]T] "
						+ txtHelp + " ]B] true ]I] " + image + " ]F]\n";
			} else
				textContent += "]N] " + name + " ]C] " + command + " ]T] "
						+ txtHelp + " ]B] false ]I] ]F]\n";
		}
		TextFile f = new TextFile();
		return f.save(path, textContent);
	}

	/**
	 * Saves the temporal list into a file
	 * 
	 * @param path
	 *            file path
	 * @return true if the result was succeed and false in other case
	 */
	public static boolean saveAuxList(String path) {

		String textContent = "";

		for (int i = 0; i < _commandAuxList.size(); i++) {

			ModifiableCommand modifiableCommand = _commandAuxList.get(i);
			String name = modifiableCommand.getName();
			String command = modifiableCommand.getCommand();
			String txtHelp = modifiableCommand.getHelpText();
			String icon;
			if (modifiableCommand.getHasIcon()) {
				icon = modifiableCommand.getIcon();
				textContent += "]N] " + name + " ]C] " + command + " ]T] "
						+ txtHelp + " ]B] true ]I] " + icon + " ]F]\n";
			} else
				textContent += "]N] " + name + " ]C] " + command + " ]T] "
						+ txtHelp + " ]B] false ]I] ]F]\n";
		}
		TextFile f = new TextFile();
		return f.save(path, textContent);
	}

	/**
	 * Loads the list from a file
	 * 
	 * @param path
	 *            file path
	 * @throws Exception
	 *             IO Exception
	 */
	public static void loadList(String path) throws Exception {

		_commandList.removeAllElements();

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		TextFile textFile = new TextFile();
		String textContent = textFile.load(path);

		char c;
		int indexAux = 0;
		String name;
		String command;
		String textHelp;
		String icon;

		for (int index = 0; index < textContent.length(); index++) {
			index = textContent.indexOf("]N]");
			if (index == -1) {
				/*
				 * If we don't find the string ]N] and indexAux = 0 means that
				 * we are at the beginning of the file and is not a valid format
				 */
				if (indexAux == 0)
					throw new Exception(labels.getString("s129"));
				// NO MORE COMMANDS IN THE FILE
				else
					index = textContent.length();
			} else {
				// NAME
				index += 4;
				textContent = textContent.substring(index);
				indexAux = textContent.indexOf("]C]");
				if (indexAux == -1)
					throw new Exception(labels.getString("s129"));
				name = textContent.substring(0, indexAux - 1);
				index = indexAux;
				// COMMAND
				index += 4;
				textContent = textContent.substring(index);
				indexAux = textContent.indexOf("]T]");
				if (indexAux == -1)
					throw new Exception(labels.getString("s129"));
				command = textContent.substring(0, indexAux - 1);
				index = indexAux;
				// HELP TEXT
				index += 4;
				textContent = textContent.substring(index);
				indexAux = textContent.indexOf("]B]");
				if (indexAux == -1)
					throw new Exception(labels.getString("s129"));
				textHelp = textContent.substring(0, indexAux - 1);
				index = indexAux;
				// HAS ICON
				index += 4;
				c = textContent.charAt(index);

				// HAS ICON?
				if (c == 't') {
					index = textContent.indexOf("]I]");
					if (index == -1)
						throw new Exception(labels.getString("s129"));
					index += 4;
					textContent = textContent.substring(index);
					indexAux = textContent.indexOf("]F]");
					if (indexAux == -1)
						throw new Exception(labels.getString("s129"));
					icon = textContent.substring(0, indexAux - 1);
					index = indexAux;
					_commandList.add(new ModifiableCommand(name, command,
							textHelp, true, icon));
					// MORE ICONS?
					if (textContent.indexOf("]N]") == -1)
						index = textContent.length();
				}
				// NO ICON?
				else if (c == 'f') {
					_commandList.add(new ModifiableCommand(name, command,
							textHelp));
					// MORE ICONS?
					if (textContent.indexOf("]N]") == -1)
						index = textContent.length();
				} else
					throw new Exception(labels.getString("s129"));
			}
		}
	}

	/**
	 * Loads the temporal list from a file
	 * 
	 * @param path
	 *            file path
	 * @throws Exception
	 *             IO Exception
	 */
	public static void loadAuxList(String path) throws Exception {

		_commandAuxList.removeAllElements();

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		TextFile textFile = new TextFile();
		String fileContent = textFile.load(path);

		char c;
		int indexAux = 0;
		String name;
		String command;
		String helpText;
		String icon;

		for (int index = 0; index < fileContent.length(); index++) {

			index = fileContent.indexOf("]N]");

			if (index == -1) {

				if (indexAux == 0)
					throw new Exception(labels.getString("s129"));
				else
					index = fileContent.length();
			} else {
				// NAME
				index += 4;
				fileContent = fileContent.substring(index);
				indexAux = fileContent.indexOf("]C]");

				if (indexAux == -1)
					throw new Exception(labels.getString("s129"));
				name = fileContent.substring(0, indexAux - 1);
				index = indexAux;

				// COMMAND
				index += 4;
				fileContent = fileContent.substring(index);
				indexAux = fileContent.indexOf("]T]");

				if (indexAux == -1)
					throw new Exception(labels.getString("s129"));
				command = fileContent.substring(0, indexAux - 1);
				index = indexAux;

				// HELP TEXT
				index += 4;
				fileContent = fileContent.substring(index);
				indexAux = fileContent.indexOf("]B]");

				if (indexAux == -1)
					throw new Exception(labels.getString("s129"));
				helpText = fileContent.substring(0, indexAux - 1);
				index = indexAux;

				// HAS ICON
				index += 4;
				c = fileContent.charAt(index);

				// HAS ICON?
				if (c == 't') {

					index = fileContent.indexOf("]I]");
					if (index == -1)
						throw new Exception(labels.getString("s129"));
					index += 4;
					fileContent = fileContent.substring(index);

					indexAux = fileContent.indexOf("]F]");

					if (indexAux == -1)
						throw new Exception(labels.getString("s129"));
					icon = fileContent.substring(0, indexAux - 1);
					index = indexAux;

					_commandAuxList.add(new ModifiableCommand(name, command,
							helpText, true, icon));
				}
				// No ICON?
				else if (c == 'f')
					_commandAuxList.add(new ModifiableCommand(name, command,
							helpText));
				else
					throw new Exception(labels.getString("s129"));
			}
		}
	}

	/**
	 * Adds a command given as a parameter to the list
	 * 
	 * @param command
	 *            command to add
	 * @return true if the result was succeed and false in other case
	 */
	public static boolean addCommand(ModifiableCommand command) {
		return _commandList.add(command);
	}

	/**
	 * Removes a command given as a parameter from the list
	 * 
	 * @param command
	 *            command to remove
	 * @return true if the result was succeed and false in other case
	 */
	public static boolean removeCommand(ModifiableCommand command) {
		return _commandList.remove(command);
	}

	/**
	 * Returns the list size
	 * 
	 * @return the list size
	 */
	public static int getSize() {
		return _commandList.size();
	}

	/**
	 * Returns the command at the position of the list given as a parameter
	 * 
	 * @param position
	 *            position to return
	 * @return the command at the position of the list given as a parameter
	 */
	public static ModifiableCommand getCommandAt(int position) {
		return (ModifiableCommand) _commandList.get(position);
	}

	/**
	 * Returns the modifiable command list
	 * 
	 * @return the modifiable command list
	 */
	public static Vector<ModifiableCommand> getCommandList() {
		return _commandList;
	}

	/**
	 * Sets a new modifiable command list
	 * 
	 * @param list
	 *            new modifiable command list to set
	 */
	public static void setList(Vector<ModifiableCommand> list) {
		_commandList = list;
	}

	/**
	 * Sets a new temporal modifiable command list
	 * 
	 * @param list
	 *            new temporal modifiable command list to set
	 */
	public static void setAuxList(Vector<ModifiableCommand> list) {
		_commandAuxList = list;
	}

	/**
	 * Returns the temporal modifiable command list
	 * 
	 * @return the temporal modifiable command list
	 */
	public static Vector<ModifiableCommand> getAuxList() {
		return _commandAuxList;
	}
}
