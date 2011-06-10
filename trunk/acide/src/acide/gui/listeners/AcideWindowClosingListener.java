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
package acide.gui.listeners;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE window closing listener.
 * 
 * This listener is used by all the ACIDE - A Configurable IDE windows.
 * 
 * @version 0.8
 * @see WindowAdapter
 */
public class AcideWindowClosingListener extends WindowAdapter{
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
	 */
	@Override
	public void windowClosing(WindowEvent windowEvent) {
		
		// Enables the main window
		AcideMainWindow.getInstance().setEnabled(true);

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
	}
}
