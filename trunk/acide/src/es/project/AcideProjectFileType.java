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
package es.project;

import java.io.Serializable;

/**																
 * ACIDE - A Configurable IDE project file type.
 * 
 * Defines the different ACIDE - A Configurable IDE project file types available which are:
 *   - Normal file.
 *   - Compilable file.
 *   - Main file.
 *					
 * @version 0.8																													
 */
public enum AcideProjectFileType implements Serializable{

	/**
	 * Normal ACIDE - A Configurable IDE project file type.
	 */
	NORMAL,
	/**
	 * Compilable ACIDE - A Configurable IDE project file type.
	 */
	COMPILABLE,
	/**
	 * Main ACIDE - A Configurable IDE project file type.
	 */
	MAIN;
}
