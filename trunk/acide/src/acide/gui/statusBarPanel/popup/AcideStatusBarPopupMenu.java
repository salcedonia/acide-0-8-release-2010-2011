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
package acide.gui.statusBarPanel.popup;

import acide.gui.mainWindow.AcideMainWindow;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import acide.resources.AcideResourceManager;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE status bar popup menu.
 * 
 * @version 0.8
 * @see JPopupMenu
 */
public class AcideStatusBarPopupMenu extends JPopupMenu {

	/**
	 * ACIDE - A Configurable IDE status bar popup menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE status bar popup menu copy menu item.
	 */
	private JMenuItem _copyMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE status bar popup menu.
	 */
	public AcideStatusBarPopupMenu() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		_copyMenuItem = new JMenuItem(labels.getString("s187"));
		_copyMenuItem.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Gets the text from the status message label
				String selection = AcideMainWindow.getInstance().getStatusBar()
						.getStatusMessage().getText();
				
				// Puts it into the system clipboard
				StringSelection data = new StringSelection(selection);
				Clipboard clipboard = Toolkit.getDefaultToolkit()
						.getSystemClipboard();
				clipboard.setContents(data, data);
			}
		});
		add(_copyMenuItem);
	}
}
