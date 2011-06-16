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
package acide.gui.consolePanel.utils;

import java.util.ArrayList;

import javax.swing.JPanel;

/**
 * ACIDE - A Configurable IDE console panel command record.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideConsolePanelCommandRecord extends ArrayList<String> {

	/**
	 * ACIDE - A Configurable IDE console panel command record serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console panel command record current index.
	 */
	private int _currentIndex;
	/**
	 * ACIDE - A Configurable IDE console panel command record maximum index
	 * value.
	 */
	private int _maximumIndex;

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel command record.
	 */
	public AcideConsolePanelCommandRecord() {

		// The command record current index is 0
		_currentIndex = 0;

		// The command record maximum index is 0
		_maximumIndex = 0;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel command record
	 * current index.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel command record
	 *         current index.
	 */
	public int getCurrentIndex() {
		return _currentIndex;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel command
	 * record current index.
	 * 
	 * @param currentIndex
	 *            new value to set.
	 */
	public void setCurrentIndex(int currentIndex) {
		_currentIndex = currentIndex;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel command record
	 * maximum index.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel command record
	 *         maximum index.
	 */
	public int getMaximumIndex() {
		return _maximumIndex;
	}

	/**
	 * Sets the ACIDE - A Configurable IDE console panel command record maximum
	 * index.
	 * 
	 * @param maximumIndex
	 *            new value to set.
	 */
	public void setMaximumIndex(int maximumIndex) {
		_maximumIndex = maximumIndex;
	}
}
