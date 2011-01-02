/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
package operations.factory;

import es.text.TextFile;
 
/**																
 * ACIDE - A Configurable IDE IOFactory.
 * 
 * Builds the Input/Output objects of ACIDE - A Configurable IDE.											
 *					
 * @version 0.8																														
 */
public class AcideIOFactory {
	
	/**
	 * ACIDE - A Configurable IDE IOFactory unique class instance.
	 */
	private static AcideIOFactory _instance;

	/**
	 * Returns the ACIDE - A Configurable IDE IOFactory unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE IOFactoryunique class instance.
	 */
	public static AcideIOFactory getInstance() {
		if (_instance == null)
			_instance = new AcideIOFactory();
		return _instance;
	}

	/**
	 * Builds a text file.
	 * 
	 * @return the new text file.
	 */
	public TextFile buildFile() {
		return new TextFile();
	}
}
