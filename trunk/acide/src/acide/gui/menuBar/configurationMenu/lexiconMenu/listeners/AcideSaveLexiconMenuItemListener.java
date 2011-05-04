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
package acide.gui.menuBar.configurationMenu.lexiconMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE lexicon menu save lexicon menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveLexiconMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the lexicon configuration path
		String path = AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getPath();

		// If it is ok
		if (!path.equals(" ")) {

			// Gets the lexicon name
			int index = path.lastIndexOf("\\");
			if (index == -1)
				index = path.lastIndexOf("/");
			String name = path.substring(index + 1, path.length());

			if (name.contains(".")) {
				index = name.lastIndexOf(".");
				name = name.substring(0, index);
			}

			// Saves it
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getLexiconConfiguration()
					.save(name, false);

		} else {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s92"));
		}
	}
}
