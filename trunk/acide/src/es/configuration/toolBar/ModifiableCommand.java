package es.configuration.toolBar;

/************************************************************************																
 * Handles the configurable tool bar commands of ACIDE - A Configurable IDE											
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
public class ModifiableCommand {
	/**
	 * Command name
	 */
	private String _name;
	/**
	 * Command itself
	 */
	private String _command;
	/**
	 * Hint text
	 */
	private String _helpText;
	/**
	 * Icon to display
	 */
	private String _icon;
	/**
	 * Flag that indicates if it has icon or not
	 */
	private boolean _hasIcon;

	/**
	 * Class constructor
	 */
	public ModifiableCommand() {
		_name = "";
		_command = "";
		_helpText = "";
		_hasIcon = false;
	}

	/**
	 * Class constructor
	 * 
	 * @param name
	 *            command name
	 * @param helpText
	 *            help text
	 */
	public ModifiableCommand(String name, String helpText) {
		_name = name;
		_command = "";
		_helpText = helpText;
		_hasIcon = false;
	}

	/**
	 * Class constructor
	 * 
	 * @param name
	 *            command name
	 * @param command
	 *            command itself
	 * @param helpText
	 *            help text
	 */
	public ModifiableCommand(String name, String command, String helpText) {
		_name = name;
		_command = command;
		_helpText = helpText;
		_hasIcon = false;
	}

	/**
	 * Class constructor
	 * 
	 * @param name
	 *            command name
	 * @param command
	 *            command itself
	 * @param helpText
	 *            help text
	 * @param hasIcon
	 *            indicates if the configurable tool bar command has icon or
	 *            not
	 * @param icon
	 *            the icon itself
	 */
	public ModifiableCommand(String name, String comand, String txt,
			boolean hasIcon, String icon) {
		_name = name;
		_command = comand;
		_helpText = txt;
		_icon = icon;
		_hasIcon = hasIcon;
	}

	/**
	 * Returns the command itself
	 * 
	 * @return the command itself
	 */
	public String getCommand() {
		return _command;
	}

	/**
	 * Sets a new value to the command
	 * 
	 * @param command
	 *            new value to set
	 */
	public void setCommand(String command) {
		_command = command;
	}

	/**
	 * Returns the help text
	 * 
	 * @return the help text
	 */
	public String getHelpText() {
		return _helpText;
	}

	/**
	 * Sets a new value to the help text
	 * 
	 * @param text
	 *            new value to set
	 */
	public void setHelpText(String text) {
		_helpText = text;
	}

	/**
	 * Returns the name
	 * 
	 * @return the name
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the name
	 * 
	 * @param name
	 *            new value to set
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the icon
	 * 
	 * @return the icon
	 */
	public String getIcon() {
		return _icon;
	}

	/**
	 * Sets a new value to icon
	 * 
	 * @param icon
	 *            new value to set
	 */
	public void setIcon(String icon) {
		_icon = icon;
	}

	/**
	 * Returns the flag for the icon
	 * 
	 * @return the flag for the icon
	 */
	public boolean getHasIcon() {
		return _hasIcon;
	}

	/**
	 * Sets a new value to the flag for the icon
	 * 
	 * @param hasIcon
	 *            new value to set
	 */
	public void setHasIcon(boolean hasIcon) {
		_hasIcon = hasIcon;
	}
}
