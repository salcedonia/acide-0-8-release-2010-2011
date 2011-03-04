/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package es.configuration.toolBar.consoleComandToolBar;

import gui.toolBarPanel.consoleCommandToolBar.utils.AcideParameterType;

/**
 * ACIDE - A Configurable IDE console command.
 * 
 * @version 0.8
 */
public class ConsoleCommand {

	/**
	 * ACIDE - A Configurable IDE console command number of parameters.
	 */
	public static final int NUMBER_OF_PARAMETERS = 5;
	/**
	 * ACIDE - A Configurable IDE console command name.
	 */
	private String _name;
	/**
	 * ACIDE - A Configurable IDE console command itself.
	 */
	private String _action;
	/**
	 * ACIDE - A Configurable IDE console command hint text.
	 */
	private String _hintText;
	/**
	 * ACIDE - A Configurable IDE console command icon path to display.
	 */
	private String _icon;
	/**
	 * ACIDE - A Configurable IDE console command parameter type.
	 */
	private AcideParameterType _parameterType;

	/**
	 * Creates a new ACIDE - A Configurable IDE console command.
	 */
	public ConsoleCommand() {
		_name = "";
		_action = "";
		_hintText = "";
		_icon = "";
		_parameterType = AcideParameterType.NONE;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE console command with a new name
	 * and hint text given as parameters.
	 * 
	 * @param name
	 *            ACIDE - A Configurable IDE console command name.
	 * @param hintText
	 *            ACIDE - A Configurable IDE console command hint text.
	 * @param parameterType
	 *            ACIDE - A Configurable IDE console command parameter type.
	 */
	public ConsoleCommand(String name, String hintText,
			AcideParameterType parameterType) {
		_name = name;
		_action = "";
		_hintText = hintText;
		_icon = "";
		_parameterType = parameterType;
	}

	/**
	 * Creates a ACIDE - A Configurable IDE console command with a new name,
	 * action and hint text given as parameters.
	 * 
	 * @param name
	 *            ACIDE - A Configurable IDE console command name.
	 * @param action
	 *            ACIDE - A Configurable IDE console command action.
	 * @param hintText
	 *            ACIDE - A Configurable IDE console command hint text.
	 * @param parameterType
	 *            ACIDE - A Configurable IDE console command extra parameter
	 *            flag.
	 */
	public ConsoleCommand(String name, String action, String hintText,
			AcideParameterType parameterType) {
		_name = name;
		_action = action;
		_hintText = hintText;
		_icon = "";
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
	public ConsoleCommand(String name, String action, String hintText) {
		_name = name;
		_action = action;
		_hintText = hintText;
		_icon = "";
		_parameterType = AcideParameterType.NONE;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE console command with a new name,
	 * action, hint text and icon given as parameters.
	 * 
	 * @param name
	 *            ACIDE - A Configurable IDE console command name.
	 * @param action
	 *            ACIDE - A Configurable IDE console command action.
	 * @param hintText
	 *            ACIDE - A Configurable IDE console command hint text.
	 * @param icon
	 *            ACIDE - A Configurable IDE console command icon.
	 * @param parameterType
	 *            ACIDE - A Configurable IDE console command parameter type.
	 */
	public ConsoleCommand(String name, String action, String hintText,
			String icon, AcideParameterType parameterType) {
		_name = name;
		_action = action;
		_hintText = hintText;
		_icon = icon;
		_parameterType = parameterType;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE console command with a new name,
	 * action, hint text and icon given as parameters.
	 * 
	 * @param name
	 *            ACIDE - A Configurable IDE console command name.
	 * @param action
	 *            ACIDE - A Configurable IDE console command action.
	 * @param hintText
	 *            ACIDE - A Configurable IDE console command hint text.
	 * @param icon
	 *            ACIDE - A Configurable IDE console command icon.
	 */
	public ConsoleCommand(String name, String action, String hintText,
			String icon) {
		_name = name;
		_action = action;
		_hintText = hintText;
		_icon = icon;
		_parameterType = AcideParameterType.NONE;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console command action.
	 * 
	 * @return the ACIDE - A Configurable IDE console command action.
	 */
	public String getAction() {
		return _action;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console command
	 * action.
	 * 
	 * @param action
	 *            new value to set.
	 */
	public void setAction(String action) {
		_action = action;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console command hint text.
	 * 
	 * @return the ACIDE - A Configurable IDE console command hint text.
	 */
	public String getHintText() {
		return _hintText;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console command hint
	 * text.
	 * 
	 * @param hintText
	 *            new value to set.
	 */
	public void setHintText(String hintText) {
		_hintText = hintText;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console command name.
	 * 
	 * @return the ACIDE - A Configurable IDE console command name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console command name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console command icon.
	 * 
	 * @return the ACIDE - A Configurable IDE console command icon.
	 */
	public String getIcon() {
		return _icon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console command icon.
	 * 
	 * @param icon
	 *            new value to set.
	 */
	public void setIcon(String icon) {
		_icon = icon;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console command has icon flag.
	 * 
	 * @return the ACIDE - A Configurable IDE console command has icon flag.
	 */
	public boolean getHasIcon() {
		return !_icon.matches("");
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console command parameter type.
	 * 
	 * @return the ACIDE - A Configurable IDE console command parameter type.
	 */
	public AcideParameterType getParameterType() {
		return _parameterType;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console command
	 * parameter type.
	 * 
	 * @param parameterType
	 *            new value to set.
	 */
	public void setParameterType(AcideParameterType parameterType) {
		_parameterType = parameterType;
	}
}
