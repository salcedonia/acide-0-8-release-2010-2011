package operations.configuration;

import java.util.ResourceBundle;
import java.util.Vector;

import properties.PropertiesManager;

import language.Language;
import es.text.TextFile;

/**
 * Handles the configurable tool bar command list.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class EditableToolBarCommandList {
	
	/**
	 * Command list.
	 */
	private static Vector <EditableToolBarCommand> _commandList;
	/**
	 * Command list to store temporally the loaded values from the file.
	 */
	private static Vector <EditableToolBarCommand> _commandAuxList;
	
	/**
	 * Constructor of the class.
	 */
	public EditableToolBarCommandList() {
		
		_commandList = new Vector <EditableToolBarCommand>();		
		_commandAuxList = new Vector <EditableToolBarCommand>();
	}
	
	/**
	 * Clear the command list.
	 */
	public static void clear() {
		_commandList = new Vector <EditableToolBarCommand>();		
		_commandAuxList = new Vector <EditableToolBarCommand>();
	}
	
	/**
	 * Save the list into a file.
	 * 
	 * @param path File path.
	 *            
	 * @return True if the result was succeed and false in other case.
	 */	
	public static boolean saveList(String path) {
		
		String textContent = "";
		for(int i = 0; i < _commandList.size(); i++) {
			
			EditableToolBarCommand icon = (EditableToolBarCommand) _commandList.get(i);
			String name = icon.getName();
			String command = icon.getCommand();
			String txtHelp = icon.getHelpText();
			String image;
			if(icon.getHasIcon()) {
				image = icon.getIcon(); 
				textContent += "]N] " + name + " ]C] " + command + " ]T] " + txtHelp + " ]B] true ]I] " + image + " ]F]\n"; 			
			}
			else textContent += "]N] " + name + " ]C] " + command + " ]T] " + txtHelp + " ]B] false ]I] ]F]\n";			 
		} 
		TextFile f = new TextFile();
		return f.save(path,textContent);
	}
	
	/**
	 * Save the temporal list into a file.
	 * 
	 * @param path File path.
	 *           
	 * @return True if the result was succeed and false in other case.
	 */
	public static boolean saveAuxList(String path) {
		
		String textContent = "";
		
		for(int i = 0; i < _commandAuxList.size(); i++) {
			
			EditableToolBarCommand editableToolBarCommand = _commandAuxList.get(i);
			String name = editableToolBarCommand.getName();
			String command = editableToolBarCommand.getCommand();
			String txtHelp = editableToolBarCommand.getHelpText();
			String icon;
			if(editableToolBarCommand.getHasIcon()) {
				icon = editableToolBarCommand.getIcon(); 
				textContent += "]N] " + name + " ]C] " + command + " ]T] " + txtHelp + " ]B] true ]I] " + icon + " ]F]\n"; 			
			}
			else textContent += "]N] " + name + " ]C] " + command + " ]T] " + txtHelp + " ]B] false ]I] ]F]\n";			 
		} 
		TextFile f = new TextFile();
		return f.save(path,textContent);
	}
	
	/**
	 * Load the list from a file.
	 * 
	 * @param path File path.
	 * 
	 * @throws Exception IO Exception.
	 */
	public static void loadList(String path) throws Exception {
		
		_commandList.removeAllElements();
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		TextFile textFile = new TextFile();
		String textContent = textFile.load(path);
		
		char c;
		int indexAux = 0;
		String name;
		String command;
		String textHelp;
		String icon;
		
		for(int index = 0; index < textContent.length(); index++) {
			index = textContent.indexOf("]N]");
			if(index == -1) {
				/*
				 * If we don't find the string ]N] and indexAux = 0 
				 * means that we are at the beginning of the file
				 * and is not a valid format
				 */ 
				if(indexAux == 0) throw new Exception(labels.getString("s129"));
				// NO MORE COMMANDS IN THE FILE
				else index = textContent.length();
			}
			else {
				// NAME
				index += 4;
				textContent = textContent.substring(index);
				indexAux = textContent.indexOf("]C]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				name = textContent.substring(0, indexAux - 1);
				index = indexAux;
				// COMMAND
				index += 4;
				textContent = textContent.substring(index);
				indexAux = textContent.indexOf("]T]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				command = textContent.substring(0,indexAux - 1);
				index = indexAux;
				// HELP TEXT
				index += 4;
				textContent = textContent.substring(index);
				indexAux = textContent.indexOf("]B]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				textHelp = textContent.substring(0,indexAux - 1);
				index = indexAux;
				// HAS ICON
				index += 4;
				c = textContent.charAt(index);
		
				// HAS ICON?
				if(c == 't') {
					index = textContent.indexOf("]I]");
					if(index == -1) throw new Exception(labels.getString("s129"));
					index += 4;
					textContent = textContent.substring(index);
					indexAux = textContent.indexOf("]F]");
					if(indexAux == -1) throw new Exception(labels.getString("s129"));
					icon = textContent.substring(0,indexAux - 1);
					index = indexAux;
					_commandList.add(new EditableToolBarCommand(name,command,textHelp,true,icon));
					// MORE ICONS?
					if(textContent.indexOf("]N]") == -1) index = textContent.length();
				}
				// NO ICON?
				else if (c == 'f') {
					_commandList.add(new EditableToolBarCommand(name,command,textHelp));
					// MORE ICONS?
					if(textContent.indexOf("]N]") == -1) index = textContent.length();
				}
				else throw new Exception(labels.getString("s129"));
			}
		}
	}
	

	/**
	 * Load the temporal list from a file.
	 * 
	 * @param path File path.
	 * 
	 * @throws Exception IO Exception.
	 */
	public static void loadAuxList(String path) throws Exception {
		
		_commandAuxList.removeAllElements();
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		TextFile textFile = new TextFile();
		String bhTxt = textFile.load(path);
		
		char c;
		int indexAux = 0;
		String name;
		String command;
		String helpText;
		String icon;
		
		for(int index = 0; index < bhTxt.length(); index++) {
			
			index = bhTxt.indexOf("]N]");
			
			if(index == -1) {
	
				if(indexAux == 0) 
					throw new Exception(labels.getString("s129"));
				else 
					index = bhTxt.length();
			}
			else {
				// NAME
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]C]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				name = bhTxt.substring(0, indexAux - 1);
				index = indexAux;
				// COMMAND
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]T]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				command = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// HELP TEXT
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]B]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				helpText = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// HAS ICON
				index += 4;
				c = bhTxt.charAt(index);

				// HAS ICON?
				if(c == 't') {
					index = bhTxt.indexOf("]I]");
					if(index == -1) throw new Exception(labels.getString("s129"));
					index += 4;
					bhTxt = bhTxt.substring(index);
					indexAux = bhTxt.indexOf("]F]");
					if(indexAux == -1) throw new Exception(labels.getString("s129"));
					icon = bhTxt.substring(0,indexAux - 1);
					index = indexAux;
					_commandAuxList.add(new EditableToolBarCommand(name,command,helpText,true,icon));
				}
				// No ICON?
				else if (c == 'f') {
					_commandAuxList.add(new EditableToolBarCommand(name,command,helpText));
				}
				else throw new Exception(labels.getString("s129"));
			}
		}
	}
	
	/**
	 * Adds a command given as a parameter to the list.
	 * 
	 * @param command Command to add.
	 * 
	 * @return True if the result was succeed and false in other case.
	 */
	public static boolean addCommand(EditableToolBarCommand command) {
		return _commandList.add(command);
	}
	
	/**
	 * Removes a command given as a parameter from the list.
	 * 
	 * @param command Command to remove.
	 * 
	 * @return True if the result was succeed and false in other case.
	 */
	public static boolean removeCommand(EditableToolBarCommand command) {
		return _commandList.remove(command);
	}
	
	/**
	 * Returns the list size.
	 * 
	 * @return The list size.
	 */
	public static int getSize() {
		return _commandList.size();
	}
	
	/**
	 * Returns the command at the position of the list given as a parameter.
	 * 
	 * @param i Position.
	 * 
	 * @return The command at the position of the list given as a parameter.
	 */
	public static EditableToolBarCommand getCommandAt(int i) {
		return (EditableToolBarCommand) _commandList.get(i);
	}

	/**
	 * Returns the list.
	 * 
	 * @return The list.
	 */
	public static Vector<EditableToolBarCommand> getCommandList() {
		return _commandList;
	}

	/**
	 * Set a new list.
	 * 
	 * @param list New list to set.
	 */
	public static void setList(Vector<EditableToolBarCommand> list) {
		_commandList = list;
	}
	
	/**
	 * Set a new temporal list.
	 * 
	 * @param list New temporal list to set.
	 */
	public static void setAuxList(Vector<EditableToolBarCommand> list) {
		_commandAuxList = list;
	}
	
	/**
	 * Returns the temporal list.
	 * 
	 * @return The temporal list.
	 */
	public static Vector<EditableToolBarCommand> getAuxList() {
		return _commandAuxList;
	}
}
