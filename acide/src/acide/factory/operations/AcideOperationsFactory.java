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
package acide.factory.operations;

import acide.gui.menuBar.editMenu.utils.AcideSearchEngine;
import acide.gui.menuBar.fileMenu.utils.AcidePrinterManager;

import javax.swing.text.JTextComponent;

/**
 * ACIDE - A Configurable IDE operations factory.
 * 
 * Builds the most relevant operations of ACIDE - A Configurable IDE.
 * 
 * @version 0.8
 */
public class AcideOperationsFactory {

	/**
	 * ACIDE - A Configurable IDE operations factory unique class instance.
	 */
	private static AcideOperationsFactory _instance;

	/**
	 * Returns the ACIDE - A Configurable IDE operations factory unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE operations factory unique class instance.
	 * @see AcideOperationsFactory
	 */
	public static AcideOperationsFactory getInstance() {
		if (_instance == null)
			_instance = new AcideOperationsFactory();
		return _instance;
	}

	/**
	 * Builds the search class of ACIDE - A Configurable IDE.
	 * 
	 * @return The search class of ACIDE - A Configurable IDE.
	 * @see AcideSearchEngine
	 */
	public AcideSearchEngine buildSearch() {
		return new AcideSearchEngine();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE printer manager.
	 * 
	 * @param component
	 *            text component to print.
	 * @param page
	 *            indicates if the page number has to be showed on the printed
	 *            page or not.
	 * @param date
	 *            indicates if the date has to be showed on the printed page or
	 *            not.
	 * @return the printer manager of ACIDE - A Configurable IDE.
	 * @see AcidePrinterManager
	 */
	public AcidePrinterManager buildPrinterManager(JTextComponent component,
			boolean page, boolean date) {
		return new AcidePrinterManager(component, page, date);
	}
}
