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
package acide.configuration.toolBar.consolePanelToolBar;

import acide.gui.toolBarPanel.consolePanelToolBar.utils.AcideParameterType;

/**
 * ACIDE - A Configurable IDE console panel tool bar button configuration.
 * 
 * @version 0.8
 */
public class AcideConsolePanelToolBarButtonConf {

	/**
	 * ACIDE - A Configurable IDE console panel tool bar button configuration
	 * number of parameters.
	 */
	public static final int NUMBER_OF_PARAMETERS = 6;
	/**
	 * ACIDE - A Configurable IDE console panel tool bar button configuration
	 * name.
	 */
	private String _name;
	/**
	 * ACIDE - A Configurable IDE console panel tool bar button configuration
	 * action.
	 */
	private String _action;
	/**
	 * ACIDE - A Configurable IDE console panel tool bar button configuration
	 * hint text.
	 */
	private String _hintText;
	/**
	 * ACIDE - A Configurable IDE console panel tool bar button configuration
	 * icon path to display.
	 */
	private String _icon;
	/**
	 * ACIDE - A Configurable IDE console panel tool bar button configuration
	 * parameter type.
	 */
	private AcideParameterType _parameterType;
	/**
	 * <p>
	 * ACIDE - A Configurable IDE console panel tool bar button configuration is
	 * executed in system shell flag.
	 * </p>
	 * <p>
	 * A command can be sent to the current loaded shell in ACIDE - A
	 * Configurable IDE or can be sent to the Operative System Shell to be
	 * executed.
	 * </p>
	 * <p>
	 * True -> The command is executed in the Operative System shell and False
	 * -> The command is executed in the ACIDE - A Configurable loaded shell.
	 * </p>
	 */
	private boolean _isExecutedInSystemShell;

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel tool bar button
	 * configuration.
	 */
	public AcideConsolePanelToolBarButtonConf() {

		// Sets the name by default
		_name = "";

		// Sets the action by default
		_action = "";

		// Sets the hint text by default
		_hintText = "";

		// Sets the icon by default
		_icon = "";

		// Sets the parameter type by default
		_parameterType = AcideParameterType.NONE;

		// Sets the is executed in system shell flag by default
		_isExecutedInSystemShell = false;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel tool bar button
	 * configuration with a new name, action, hint text and icon given as
	 * parameters.
	 * 
	 * @param name
	 *            ACIDE - A Configurable IDE console panel tool bar button
	 *            configuration name.
	 * @param action
	 *            ACIDE - A Configurable IDE console panel tool bar button
	 *            configuration action.
	 * @param hintText
	 *            ACIDE - A Configurable IDE console panel tool bar button
	 *            configuration hint text.
	 * @param icon
	 *            ACIDE - A Configurable IDE console panel tool bar button
	 *            configuration icon.
	 * @param parameterType
	 *            ACIDE - A Configurable IDE console panel tool bar button
	 *            configuration parameter type.
	 * @param isExecutedInSystemShell
	 *            ACIDE - A Configurable IDE console panel tool bar button
	 *            configuration is executed in system shell flag.
	 */
	public AcideConsolePanelToolBarButtonConf(String name, String action,
			String hintText, String icon, AcideParameterType parameterType,
			boolean isExecutedInSystemShell) {

		// Stores the name
		_name = name;

		// Stores the action
		_action = action;

		// Stores the hint text
		_hintText = hintText;

		// Stores the icon
		_icon = icon;

		// Stores the parameter type
		_parameterType = parameterType;

		// Stores the is executed in system shell flag
		_isExecutedInSystemShell = isExecutedInSystemShell;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel tool bar button
	 * configuration action.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel tool bar button
	 *         configuration action.
	 */
	public String getAction() {
		return _action;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel tool bar
	 * button configuration action.
	 * 
	 * @param action
	 *            new value to set.
	 */
	public void setAction(String action) {
		_action = action;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel tool bar button
	 * configuration hint text.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel tool bar button
	 *         configuration hint text.
	 */
	public String getHintText() {
		return _hintText;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel tool bar
	 * button configuration hint text.
	 * 
	 * @param hintText
	 *            new value to set.
	 */
	public void setHintText(String hintText) {
		_hintText = hintText;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel tool bar button
	 * configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel tool bar button
	 *         configuration name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel tool bar
	 * button configuration name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel tool bar button
	 * configuration icon.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel tool bar button
	 *         configuration icon.
	 */
	public String getIcon() {
		return _icon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel tool bar
	 * button configuration icon.
	 * 
	 * @param icon
	 *            new value to set.
	 */
	public void setIcon(String icon) {
		_icon = icon;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel tool bar button
	 * configuration has icon flag.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel tool bar button
	 *         configuration has icon flag.
	 */
	public boolean getHasIcon() {
		return !_icon.matches("");
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel tool bar button
	 * configuration parameter type.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel tool bar button
	 *         configuration parameter type.
	 */
	public AcideParameterType getParameterType() {
		return _parameterType;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel tool bar
	 * button configuration parameter type.
	 * 
	 * @param parameterType
	 *            new value to set.
	 */
	public void setParameterType(AcideParameterType parameterType) {
		_parameterType = parameterType;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel tool bar button
	 * configuration is executed in system shell flag.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel tool bar button
	 *         configuration is executed in system shell flag.
	 */
	public boolean isExecutedInSystemShell() {
		return _isExecutedInSystemShell;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel tool bar
	 * button configuration is executed in system shell flag.
	 * 
	 * @param isExecutedInSystemShell
	 *            new value to set.
	 */
	public void setIsExecutedInSystemShell(boolean isExecutedInSystemShell) {
		_isExecutedInSystemShell = isExecutedInSystemShell;
	}
}
