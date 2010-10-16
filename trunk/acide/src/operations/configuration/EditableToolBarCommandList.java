package operations.configuration;

import java.util.ResourceBundle;
import java.util.Vector;

import properties.PropertiesManager;

import language.Language;
import es.text.TextFile;

/**
 * 
 */
public class EditableToolBarCommandList {
	
	/**
	 * 
	 */
	private static Vector <EditableToolBarCommand> _commandList;
	/**
	 * 
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
	 * 
	 */
	public static void clear() {
		_commandList = new Vector <EditableToolBarCommand>();		
		_commandAuxList = new Vector <EditableToolBarCommand>();
	}
	
	/**
	 * 
	 * 
	 * @param path
	 *            
	 * @return 
	 */	
	public static boolean saveList(String path) {
		
		String txt = "";
		for(int i = 0; i < _commandList.size(); i++) {
			
			EditableToolBarCommand icon = (EditableToolBarCommand) _commandList.get(i);
			String name = icon.getName();
			String command = icon.getCommand();
			String txtHelp = icon.getHelpText();
			String image;
			if(icon.getHasIcon()) {
				image = icon.getIcon(); 
				txt += "]N] " + name + " ]C] " + command + " ]T] " + txtHelp + " ]B] true ]I] " + image + " ]F]\n"; 			
			}
			else txt += "]N] " + name + " ]C] " + command + " ]T] " + txtHelp + " ]B] false ]I] ]F]\n";			 
		} 
		TextFile f = new TextFile();
		return f.save(path,txt);
	}
	
	/**
	 * 
	 * @param path           
	 * @return 
	 */
	public static boolean saveAuxList(String path) {
		String txt = "";
		for(int i = 0; i < _commandAuxList.size(); i++) {
			EditableToolBarCommand icon = (EditableToolBarCommand) _commandAuxList.get(i);
			String name = icon.getName();
			String command = icon.getCommand();
			String txtHelp = icon.getHelpText();
			String image;
			if(icon.getHasIcon()) {
				image = icon.getIcon(); 
				txt += "]N] " + name + " ]C] " + command + " ]T] " + txtHelp + " ]B] true ]I] " + image + " ]F]\n"; 			
			}
			else txt += "]N] " + name + " ]C] " + command + " ]T] " + txtHelp + " ]B] false ]I] ]F]\n";			 
		} 
		TextFile f = new TextFile();
		return f.save(path,txt);
	}
	
	/**
	 * 
	 * @param ruta 
	 * @throws Exception
	 */
	public static void loadList(String ruta) throws Exception {
		
		_commandList.removeAllElements();
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		TextFile f = new TextFile();
		String bhTxt = f.load(ruta);
		char c;
		int indexAux = 0;
		String name;
		String command;
		String txtHelp;
		String image;
		for(int index = 0; index < bhTxt.length(); index++) {
			index = bhTxt.indexOf("]N]");
			if(index == -1) {
				/*
				 * If we don't find the string ]N] and indexAux = 0 
				 * means that we are at the beginning of the file
				 * and is not a valid format
				 */ 
				if(indexAux == 0) throw new Exception(labels.getString("s129"));
				// No more commands in the file
				else index = bhTxt.length();
			}
			else {
				// Name
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]C]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				name = bhTxt.substring(0, indexAux - 1);
				index = indexAux;
				// Command
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]T]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				command = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// Help Text
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]B]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				txtHelp = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// Has Icon
				index += 4;
				c = bhTxt.charAt(index);
		
				// If has image
				if(c == 't') {
					index = bhTxt.indexOf("]I]");
					if(index == -1) throw new Exception(labels.getString("s129"));
					index += 4;
					bhTxt = bhTxt.substring(index);
					indexAux = bhTxt.indexOf("]F]");
					if(indexAux == -1) throw new Exception(labels.getString("s129"));
					image = bhTxt.substring(0,indexAux - 1);
					index = indexAux;
					_commandList.add(new EditableToolBarCommand(name,command,txtHelp,true,image));
					// Any more icons?
					if(bhTxt.indexOf("]N]") == -1) index = bhTxt.length();
				}
				// No image
				else if (c == 'f') {
					_commandList.add(new EditableToolBarCommand(name,command,txtHelp));
					// Any more icons?
					if(bhTxt.indexOf("]N]") == -1) index = bhTxt.length();
				}
				else throw new Exception(labels.getString("s129"));
			}
		}
	}
	

	/**
	 * 
	 * @param path
	 * @throws Exception
	 */
	public static void loadAuxList(String path) throws Exception {
		_commandAuxList.removeAllElements();
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		TextFile f = new TextFile();
		String bhTxt = f.load(path);
		char c;
		int indexAux = 0;
		String name;
		String command;
		String helpText;
		String image;
		for(int index = 0; index < bhTxt.length(); index++) {
			
			index = bhTxt.indexOf("]N]");
			
			if(index == -1) {
	
				if(indexAux == 0) 
					throw new Exception(labels.getString("s129"));
				else 
					index = bhTxt.length();
			}
			else {
				// Name
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]C]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				name = bhTxt.substring(0, indexAux - 1);
				index = indexAux;
				// Command
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]T]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				command = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// Help text
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]B]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				helpText = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// Has image
				index += 4;
				c = bhTxt.charAt(index);

				if(c == 't') {
					index = bhTxt.indexOf("]I]");
					if(index == -1) throw new Exception(labels.getString("s129"));
					index += 4;
					bhTxt = bhTxt.substring(index);
					indexAux = bhTxt.indexOf("]F]");
					if(indexAux == -1) throw new Exception(labels.getString("s129"));
					image = bhTxt.substring(0,indexAux - 1);
					index = indexAux;
					_commandAuxList.add(new EditableToolBarCommand(name,command,helpText,true,image));
				}
				// No image
				else if (c == 'f') {
					_commandAuxList.add(new EditableToolBarCommand(name,command,helpText));
				}
				else throw new Exception(labels.getString("s129"));
			}
		}
	}
	
	/**
	 * 
	 * @param command
	 * @return
	 */
	public static boolean addCommand(EditableToolBarCommand command) {
		return _commandList.add(command);
	}
	
	/**
	 * 
	 * @param command
	 * @return
	 */
	public static boolean removeCommand(EditableToolBarCommand command) {
		return _commandList.remove(command);
	}
	
	/**
	 * 
	 * @return
	 */
	public static int getSize() {
		return _commandList.size();
	}
	
	/**
	 * 
	 * @param i
	 * @return
	 */
	public static EditableToolBarCommand getCommandAt(int i) {
		return (EditableToolBarCommand) _commandList.get(i);
	}

	/**
	 * 
	 * @return
	 */
	public static Vector<EditableToolBarCommand> getCommandList() {
		return _commandList;
	}

	/**
	 * 
	 * @param list
	 */
	public static void setList(Vector<EditableToolBarCommand> list) {
		_commandList = list;
	}
	
	/**
	 * 
	 * @param list
	 */
	public static void setAuxList(Vector<EditableToolBarCommand> list) {
		_commandAuxList = list;
	}
	
	/**
	 * 
	 * @return
	 */
	public static Vector<EditableToolBarCommand> getAuxList() {
		return _commandAuxList;
	}
}
