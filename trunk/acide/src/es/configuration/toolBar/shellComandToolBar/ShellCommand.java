package es.configuration.toolBar.shellComandToolBar;

import gui.toolBarPanel.shellCommandToolBar.ParameterType;

/************************************************************************																
 * Handles the shell tool bar command of ACIDE - A Configurable IDE.											
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
public class ShellCommand {
	/**
	 * Shell command name.
	 */
	private String _name;
	/**
	 * Shell command itself.
	 */
	private String _action;
	/**
	 * Shell command hint text.
	 */
	private String _hintText;
	/**
	 * Shell command icon path to display.
	 */
	private String _icon;
	/**
	 * Flag that indicates if it has icon or not.
	 */
	private boolean _hasIcon;
	/**
	 * Shell command parameter type.
	 */
	private ParameterType _parameterType;

	/**
	 * Creates a new sell tool bar command.
	 */
	public ShellCommand() {
		_name = "";
		_action = "";
		_hintText = "";
		_hasIcon = false;
		_parameterType = ParameterType.NONE;
	}

	/**
	 * Creates a new sell tool bar command with a new name and hint text
	 * given as parameters.
	 * 
	 * @param name
	 *            shell command name.
	 * @param hintText
	 *            shell command hint text.
	 * @param parameterType shell command parameter type.
	 */
	public ShellCommand(String name, String hintText, ParameterType parameterType) {
		_name = name;
		_action = "";
		_hintText = hintText;
		_hasIcon = false;
		_parameterType = parameterType;
	}

	/**
	 * Creates a new sell tool bar command with a new name, action and hint text
	 * given as parameters.
	 * 
	 * @param name
	 *            shell command name.
	 * @param action
	 *            shell command action.
	 * @param hintText
	 *            shell command hint text.
	 * @param parameterType shell command extra parameter flag.
	 */
	public ShellCommand(String name, String action, String hintText, ParameterType parameterType) {
		_name = name;
		_action = action;
		_hintText = hintText;
		_hasIcon = false;
		_parameterType = parameterType;
	}

	/**
	 * Creates a new sell tool bar command with a new name, action and hint text
	 * given as parameters.
	 * 
	 * @param name
	 *            shell command name.
	 * @param action
	 *            shell command action.
	 * @param hintText
	 *            shell command hint text.
	 */
	public ShellCommand(String name, String action, String hintText) {
		_name = name;
		_action = action;
		_hintText = hintText;
		_hasIcon = false;
		_parameterType = ParameterType.NONE;
	}
	
	/**
	 * Creates a new sell tool bar command with a new name, action, hint text
	 * and icon given as parameters.
	 * 
	 * @param name
	 *            shell command name.
	 * @param action
	 *            shell command action.
	 * @param hintText
	 *            shell command hint text.
	 * @param hasIcon
	 *            indicates if the configurable tool bar command has icon or
	 *            not.
	 * @param icon
	 *            shell command icon.
	 * @param parameterType shell command extra parameter flag.
	 */
	public ShellCommand(String name, String action, String hintText,
			boolean hasIcon, String icon, ParameterType parameterType) {
		_name = name;
		_action = action;
		_hintText = hintText;
		_icon = icon;
		_hasIcon = hasIcon;
		_parameterType = parameterType;
	}
	
	/**
	 * Creates a new sell tool bar command with a new name, action, hint text
	 * and icon given as parameters.
	 * 
	 * @param name
	 *            shell command name.
	 * @param action
	 *            shell command action.
	 * @param hintText
	 *            shell command hint text.
	 * @param hasIcon
	 *            indicates if the configurable tool bar command has icon or
	 *            not.
	 * @param icon
	 *            shell command icon.
	 */
	public ShellCommand(String name, String action, String hintText,
			boolean hasIcon, String icon) {
		_name = name;
		_action = action;
		_hintText = hintText;
		_icon = icon;
		_hasIcon = hasIcon;
		_parameterType = ParameterType.NONE;
	}

	/**
	 * Returns the shell command action.
	 * 
	 * @return the shell command action.
	 */
	public String getAction() {
		return _action;
	}

	/**
	 * Sets a new value to the shell command action.
	 * 
	 * @param action
	 *            new value to set.
	 */
	public void setAction(String action) {
		_action = action;
	}

	/**
	 * Returns the shell command hint text.
	 * 
	 * @return the shell command hint text.
	 */
	public String getHintText() {
		return _hintText;
	}

	/**
	 * Sets a new value to the shell command hint text.
	 * 
	 * @param hintText
	 *            new value to set.
	 */
	public void setHintText(String hintText) {
		_hintText = hintText;
	}

	/**
	 * Returns the shell command name.
	 * 
	 * @return the shell command name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the shell command name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the shell command icon.
	 * 
	 * @return the shell command icon.
	 */
	public String getIcon() {
		return _icon;
	}

	/**
	 * Sets a new value to the shell command icon.
	 * 
	 * @param icon
	 *            new value to set.
	 */
	public void setIcon(String icon) {
		_icon = icon;
	}

	/**
	 * Returns the flag for the icon.
	 * 
	 * @return the flag for the icon.
	 */
	public boolean getHasIcon() {
		return _hasIcon;
	}

	/**
	 * Sets a new value to the flag for the icon.
	 * 
	 * @param hasIcon
	 *            new value to set.
	 */
	public void setHasIcon(boolean hasIcon) {
		_hasIcon = hasIcon;
	}

	/**
	 * Returns the shell command parameter type.
	 * 
	 * @return the shell command parameter type.
	 */
	public ParameterType getParameterType() {
		return _parameterType;
	}
	
	/**
	 * Sets a new value to the shell command parameter type.
	 * 
	 * @param parameterType new value to set.
	 */
	public void setParameterType(ParameterType parameterType){
		_parameterType = parameterType;
	}
}
