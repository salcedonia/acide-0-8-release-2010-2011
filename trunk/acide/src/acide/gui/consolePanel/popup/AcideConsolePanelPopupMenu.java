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
package acide.gui.consolePanel.popup;

import acide.factory.gui.AcideGUIFactory;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;


import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE console panel popup menu.
 * 
 * @version 0.8
 * @see JPopupMenu
 * @see JMenuItem
 */
public class AcideConsolePanelPopupMenu extends JPopupMenu {

	/**
	 * ACIDE - A Configurable IDE console panel popup menu class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu copy menu item image
	 * icon.
	 */
	private final static ImageIcon COPY_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/copy.png");
	/**
	 * ACIDE - A Configurable IDE console panel popup menu paste menu item image
	 * icon.
	 */
	private final static ImageIcon PASTE_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/paste.png");
	/**
	 * ACIDE - A Configurable IDE console panel popup menu cut menu item image
	 * icon.
	 */
	private final static ImageIcon CUT_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/cut.png");
	/**
	 * ACIDE - A Configurable IDE console panel popup menu shell display options
	 * menu item image icon.
	 */
	private final static ImageIcon CONSOLE_DISPLAY_OPTIONS_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/console/consoleDisplayOptions.png");
	/**
	 * ACIDE - A Configurable IDE console panel popup menu copy menu item.
	 */
	private JMenuItem _copy;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu cut menu item.
	 */
	private JMenuItem _cut;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu paste menu item.
	 */
	private JMenuItem _paste;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu shell display options
	 * menu item.
	 */
	private JMenuItem _consoleDisplayOptions;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu reset menu item.
	 */
	private JMenuItem _reset;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu control+C menu item.
	 */
	private JMenuItem _controlC;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu clear console buffer
	 * menu item.
	 */
	private JMenuItem _clearConsoleBuffer;

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel popup menu.
	 */
	public AcideConsolePanelPopupMenu() {

		// CONSOLE DISPLAY OPTIONS
		_consoleDisplayOptions = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s986"),
				CONSOLE_DISPLAY_OPTIONS_IMAGE);
		add(_consoleDisplayOptions);
		addSeparator();

		// COPY
		_copy = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s187"), COPY_IMAGE);
		add(_copy);

		// CUT
		_cut = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s188"), CUT_IMAGE);
		add(_cut);

		// PASTE
		_paste = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s189"), PASTE_IMAGE);
		add(_paste);
		addSeparator();

		// CONTROL + C
		_controlC = new JMenuItem("Ctrl-C");
		add(_controlC);

		// RESET
		_reset = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s987"));
		add(_reset);

		// CLEAR CONSOLE BUFFER
		_clearConsoleBuffer = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s999"));
		add(_clearConsoleBuffer);

		// Sets the listeners of the window components
		setListeners();
	}

	/**
	 * Sets the listeners of the window components.
	 */
	public void setListeners() {

		// CONSOLE DISPLAY OPTIONS
		_consoleDisplayOptions
				.addActionListener(new ConsoleDisplayOptionsMenuItemAction());

		// COPY
		_copy.addActionListener(new CopyMenuItemAction());

		// CUT
		_cut.addActionListener(new CutMenuItemAction());

		// PASTE
		_paste.addActionListener(new PasteMenuItemAction());

		// CONTROL + C
		_controlC.addActionListener(new ControlCMenuItemAction());

		// RESET
		_reset.addActionListener(new ResetMenuItemAction());

		// CLEAR CONSOLE BUFFER
		_clearConsoleBuffer
				.addActionListener(new ClearConsoleBufferMenuItemAction());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel popup menu copy menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel popup menu copy menu
	 *         item.
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel popup menu copy menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel popup menu copy menu
	 *         item.
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel popup menu paste
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel popup menu paste
	 *         menu item.
	 */
	public JMenuItem getPaste() {
		return _paste;
	}

	/**
	 * ACIDE - A Configurable IDE console panel popup menu console display
	 * options menu item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ConsoleDisplayOptionsMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Shows the console display options window
			AcideGUIFactory.getInstance()
					.buildAcideConsoleDisplayOptionsWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE console panel popup menu console copy menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CopyMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Copy
			AcideMainWindow.getInstance().getConsolePanel().getTextPane().copy();
		}
	}

	/**
	 * ACIDE - A Configurable IDE console panel popup menu console cut menu item
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CutMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If the selection start position is after the prompt position
			if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.getSelectionStart() >= AcideMainWindow.getInstance()
					.getConsolePanel().getPromptCaretPosition())

				// Cuts
				AcideMainWindow.getInstance().getConsolePanel().getTextPane().cut();
		}
	}

	/**
	 * ACIDE - A Configurable IDE console panel popup menu console paste
	 * menu item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class PasteMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If the selection start position is after the prompt position
			if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.getSelectionStart() >= AcideMainWindow.getInstance()
					.getConsolePanel().getPromptCaretPosition())

				// Pastes
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.paste();
		}
	}

	/**
	 * ACIDE - A Configurable IDE console panel popup menu console control + c
	 * menu item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ControlCMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Kill the process
			AcideMainWindow.getInstance().getConsolePanel().killShellProcess();

			// Resets the console again
			AcideMainWindow.getInstance().getConsolePanel().resetConsole();
		}
	}

	/**
	 * ACIDE - A Configurable IDE console panel popup menu console reset menu
	 * item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ResetMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			AcideMainWindow.getInstance().getConsolePanel().resetConsole();
		}
	}

	/**
	 * ACIDE - A Configurable IDE console panel popup menu console clear console
	 * buffer menu item action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ClearConsoleBufferMenuItemAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Clears the console buffer
			AcideMainWindow.getInstance().getConsolePanel().clearConsoleBuffer();
		}
	}
}
