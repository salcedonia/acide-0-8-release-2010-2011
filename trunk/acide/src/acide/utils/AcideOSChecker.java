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
package acide.utils;

/**
 * <p>
 * ACIDE - A Configurable IDE operative system checker.
 * </p>
 * <p>
 * Has the methods to get the OS in which the application is being currently run.
 * </p>
 * 
 * @version 0.8
 */
public class AcideOSChecker{
	
	/**
	 * ACIDE - A Configurable IDE operative system checker unique class instance.
	 */
	private static AcideOSChecker _instance;
	
	/**
	 * Returns the ACIDE - A Configurable IDE operative system checker unique class instance.
	 * 
	 * @return The ACIDE - A Configurable IDE operative system checker unique class instance.
	 */
	public AcideOSChecker getInstance(){
		
		if (_instance == null)
			_instance = new AcideOSChecker();
		return _instance;
	}
	 
	/**
	 * Checks if the current OS is WINDOWS.
	 * 
	 * @return true if the current OS is WINDOWS and false in other case.
	 */
	public static boolean isWindows(){
 
		String os = System.getProperty("os.name").toLowerCase();
	    return (os.indexOf( "win" ) >= 0); 
	}
 
	 
	/**
	 * Checks if the current OS is MAC OS.
	 * 
	 * @return true if the current OS is MAC OS and false in other case.
	 */
	public static boolean isMac(){
 
		String os = System.getProperty("os.name").toLowerCase();
	    return (os.indexOf( "mac" ) >= 0); 
	}
 
	 
	/**
	 * Checks if the current OS is UNIX or LINUX.
	 * 
	 * @return true if the current OS is UNIX or LINUX and false in other case.
	 */
	public static boolean isUnixOrLinux(){
 
		String os = System.getProperty("os.name").toLowerCase();
	    return (os.indexOf( "nix") >=0 || os.indexOf( "nux") >=0);
	}
}