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
package acide.configuration.menu;

/**
 * ACIDE - A Configurable IDE Menu item information. It is used to be stored
 * into an ArrayList which contents all the menu items.
 * 
 * @version 0.8
 */
public class AcideMenuItemInformation {

	/**
	 * ACIDE - A Configurable IDE Menu item information menu item name.
	 */
	private String _name;
	/**
	 * ACIDE - A Configurable IDE Menu item information menu item flag that
	 * indicates if the menu element has to be displayed in the ACIDE - A
	 * Configurable IDE menu bar.
	 */
	private boolean _isDisplayed;

	/**
	 * Creates a new menu element information with a new name and isDisplayed
	 * flag given as parameters.
	 * 
	 * @param name
	 *            new name.
	 * @param isDisplayed
	 *            new flag value.
	 */
	public AcideMenuItemInformation(String name, boolean isDisplayed) {
		_name = name;
		_isDisplayed = isDisplayed;
	}

	/**
	 * Returns the menu item information name.
	 * 
	 * @return the menu item information name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the menu item information name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the menu item information is displayed flag.
	 * 
	 * @return the menu item information is displayed flag.
	 */
	public boolean getIsDisplayed() {
		return _isDisplayed;
	}

	/**
	 * Sets a new value to the menu item information is displayed flag.
	 * 
	 * @param isDisplayed
	 *            new value to set.
	 */
	public void setIsDisplayed(boolean isDisplayed) {
		_isDisplayed = isDisplayed;
	}
}
