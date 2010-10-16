package operations.configuration;

/**
 * 
 */
public class EditableToolBarCommand {
	/**
	 *
	 */
	private String _name;
	/**
	 * 
	 */
	private String _command;
	/**
	 * 
	 */
	private String _helpText;
	/**
	 * 
	 */
	private String _icon;
	/**
	 * 
	 */
	private boolean _hasIcon;

	/**
	 * Constructor of the class.
	 */
	public EditableToolBarCommand() {
		_name = "";
		_command = "";
		_helpText = "";
		_hasIcon = false;
	}

	/**
	 *
	 *
	 * @param name
	 * @param txt 
	 */
	public EditableToolBarCommand(String name, String txt) {
		_name = name;
		_command = "";
		_helpText = txt;
		_hasIcon = false;
	}
	
	/**
	 * 
	 * 
	 * @param name 
	 * @param comand 
	 * @param txt 
	 */
	public EditableToolBarCommand(String name, String comand, String txt) {
		_name = name;
		_command = comand;
		_helpText = txt;
		_hasIcon = false;
	}
	
	/**
	 * 
	 * 
	 * @param name
	 * @param comand
	 * @param txt 
	 * @param tiene 
	 * @param image
	 */
	public EditableToolBarCommand(String name, String comand, String txt, boolean tiene, String image) {
		_name = name;
		_command = comand;
		_helpText = txt;
		_icon = image;
		_hasIcon = tiene;
	}

	/**
	 * 
	 * @return
	 */
	public String getCommand() {
		return _command;
	}

	/**
	 * 
	 * @param command
	 */
	public void setCommand(String command) {
		_command = command;
	}

	/**
	 * 
	 * @return
	 */
	public String getHelpText() {
		return _helpText;
	}

	/**
	 * 
	 * @param text
	 */
	public void setHelpText(String text) {
		_helpText = text;
	}

	/**
	 * 
	 * @return
	 */
	public String getName() {
		return _name;
	}

	/**
	 * 
	 * @param name
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * 
	 * @return
	 */
	public String getIcon() {
		return _icon;
	}

	/**
	 * 
	 * @param icon
	 */
	public void setIcon(String icon) {
		_icon = icon;
	}

	/**
	 * 
	 * @return
	 */
	public boolean getHasIcon() {
		return _hasIcon;
	}

	/**
	 * 
	 * @param hasIcon
	 */
	public void setHasIcon(boolean hasIcon) {
		_hasIcon = hasIcon;
	}
}
