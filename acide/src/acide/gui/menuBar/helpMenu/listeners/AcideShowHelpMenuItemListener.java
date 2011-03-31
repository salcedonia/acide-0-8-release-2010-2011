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
package acide.gui.menuBar.helpMenu.listeners;

import acide.configuration.project.AcideProjectConfiguration;

import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE help menu show help menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideShowHelpMenuItemListener implements ActionListener {

	/**
	 * Help URL of the help file in Spanish.
	 */
	private final static String SPANISH_HELP_URL = "resources/help/ayuda.txt";
	/**
	 * Help URL of the help file in English.
	 */
	private final static String ENGLISH_HELP_URL = "resources/help/help.txt";

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		String path = "";

		try {

			if (AcideProjectConfiguration.getInstance()
					.getLanguageConfiguration().equals("spanish"))
				// SPANISH USER GUIDE
				path = SPANISH_HELP_URL;
			else
				// ENGLISH USER'S GUIDE
				path = ENGLISH_HELP_URL;

			Desktop.getDesktop().open(new File(path));

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());

			// HELP GUIDE NOT FOUND
			JOptionPane.showMessageDialog(null, AcideLanguageManager
					.getInstance().getLabels().getString("s969")
					+ path, AcideLanguageManager.getInstance().getLabels()
					.getString("s945"), JOptionPane.ERROR_MESSAGE);
		}
		// GUIFactory.getInstance().buildHelp();
		/*
		 * try { Desktop.getDesktop().browse(new URI(HELP_URL)); } catch
		 * (IOException e) { e.printStackTrace(); } catch (URISyntaxException e)
		 * { e.printStackTrace(); }
		 */
	}
}
