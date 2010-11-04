package es.configuration.toolBar;

/**
 * Handles the configurable tool bar commands.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class EditableToolBarCommand {
	/**
	 * Name of the command.
	 */
	private String _name;
	/**
	 * Command itself.
	 */
	private String _command;
	/**
	 * Hint text.
	 */
	private String _helpText;
	/**
	 * Icon to display.
	 */
	private String _icon;
	/**
	 * Flag that indicates if it has icon or not.
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
	 * Constructor of the class.
	 * 
	 * @param name
	 *            Command name.
	 * @param helpText
	 *            Help text.
	 */
	public EditableToolBarCommand(String name, String helpText) {
		_name = name;
		_command = "";
		_helpText = helpText;
		_hasIcon = false;
	}

	/**
	 * Constructor of the class.
	 * 
	 * @param name
	 *            Command name.
	 * @param command
	 *            Command itself.
	 * @param helpText
	 *            Help text.
	 */
	public EditableToolBarCommand(String name, String command, String helpText) {
		_name = name;
		_command = command;
		_helpText = helpText;
		_hasIcon = false;
	}

	/**
	 * Constructor of the class.
	 * 
	 * @param name
	 *            Command name.
	 * @param command
	 *            Command itself.
	 * @param helpText
	 *            Help text.
	 * @param hasIcon
	 *            Indicates if the configurable tool bar command has icon or
	 *            not.
	 * @param icon
	 *            The icon itself.
	 */
	public EditableToolBarCommand(String name, String comand, String txt,
			boolean hasIcon, String icon) {
		_name = name;
		_command = comand;
		_helpText = txt;
		_icon = icon;
		_hasIcon = hasIcon;
	}

	/**
	 * Returns the command itself.
	 * 
	 * @return The command itself.
	 */
	public String getCommand() {
		return _command;
	}

	/**
	 * Set a new value to the command.
	 * 
	 * @param command
	 *            New value to set.
	 */
	public void setCommand(String command) {
		_command = command;
	}

	/**
	 * Returns the help text.
	 * 
	 * @return The help text.
	 */
	public String getHelpText() {
		return _helpText;
	}

	/**
	 * Set a new value to the help text.
	 * 
	 * @param text
	 *            New value to set.
	 */
	public void setHelpText(String text) {
		_helpText = text;
	}

	/**
	 * Returns the name.
	 * 
	 * @return The name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Set a new value to the name.
	 * 
	 * @param name
	 *            New value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the icon.
	 * 
	 * @return The icon.
	 */
	public String getIcon() {
		return _icon;
	}

	/**
	 * Set a new value to icon.
	 * 
	 * @param icon
	 *            New value to set.
	 */
	public void setIcon(String icon) {
		_icon = icon;
	}

	/**
	 * Returns the flag for the icon.
	 * 
	 * @return The flag for the icon.
	 */
	public boolean getHasIcon() {
		return _hasIcon;
	}

	/**
	 * Set a new value to the flag for the icon.
	 * 
	 * @param hasIcon
	 *            New value to set.
	 */
	public void setHasIcon(boolean hasIcon) {
		_hasIcon = hasIcon;
	}
}
