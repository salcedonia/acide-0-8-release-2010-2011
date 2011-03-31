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
	private JMenuItem _copyMenuItem;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu cut menu item.
	 */
	private JMenuItem _cutMenuItem;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu paste menu item.
	 */
	private JMenuItem _pasteMenuItem;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu shell display options
	 * menu item.
	 */
	private JMenuItem _consoleDisplayOptionsMenuItem;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu reset menu item.
	 */
	private JMenuItem _resetMenuItem;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu control+C menu item.
	 */
	private JMenuItem _controlCMenuItem;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu clear console buffer
	 * menu item.
	 */
	private JMenuItem _clearConsoleBufferMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel popup menu.
	 */
	public AcideConsolePanelPopupMenu() {

		// Creates the console display options menu item
		_consoleDisplayOptionsMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s986"),
				CONSOLE_DISPLAY_OPTIONS_IMAGE);
		
		// Adds the console display options to the popup menu
		add(_consoleDisplayOptionsMenuItem);
		
		// Adds a separator to the popup menu
		addSeparator();

		// Creates the copy menu item
		_copyMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s187"), COPY_IMAGE);
		
		// Adds the copy menu item to the popup menu
		add(_copyMenuItem);

		// Creates the cut menu item
		_cutMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s188"), CUT_IMAGE);
		
		// Adds the cut menu item to the popup menu
		add(_cutMenuItem);

		// Creates the paste menu item
		_pasteMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s189"), PASTE_IMAGE);
		
		// Adds the paste menu item to the popup menu
		add(_pasteMenuItem);
		
		// Adds a separator to the popup menu
		addSeparator();

		// Creates the controlC menu item
		_controlCMenuItem = new JMenuItem("Ctrl-C");
		
		// Adds the controlC menu item to the popup menu
		add(_controlCMenuItem);

		// Creates the reset menu item
		_resetMenuItem = new JMenuItem(AcideLanguageManager.getInstance().getLabels()
				.getString("s987"));
		
		// Adds the reset menu item to the popup menu
		add(_resetMenuItem);

		// Create the clear console buffer menu item
		_clearConsoleBufferMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s999"));
		
		// Adds the clear console buffer menu item to the popup menu
		add(_clearConsoleBufferMenuItem);

		// Sets the listeners of the window components
		setListeners();
	}

	/**
	 * Sets the listeners of the window components.
	 */
	public void setListeners() {

		// Sets the console display options menu item action listener
		_consoleDisplayOptionsMenuItem
				.addActionListener(new ConsoleDisplayOptionsMenuItemAction());

		// Sets the copy menu item action listener
		_copyMenuItem.addActionListener(new CopyMenuItemAction());

		// Sets the cut menu item action listener
		_cutMenuItem.addActionListener(new CutMenuItemAction());

		// Sets the paste menu item action listener
		_pasteMenuItem.addActionListener(new PasteMenuItemAction());

		// Sets the controlC menu item action listener
		_controlCMenuItem.addActionListener(new ControlCMenuItemAction());

		// Sets the reset menu item action listener
		_resetMenuItem.addActionListener(new ResetMenuItemAction());

		// Sets the clear console buffer menu item action listener
		_clearConsoleBufferMenuItem
				.addActionListener(new ClearConsoleBufferMenuItemAction());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel popup menu copy menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel popup menu copy menu
	 *         item.
	 */
	public JMenuItem getCopyMenuItem() {
		return _copyMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel popup menu copy menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel popup menu copy menu
	 *         item.
	 */
	public JMenuItem getCutMenuItem() {
		return _cutMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel popup menu paste
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel popup menu paste
	 *         menu item.
	 */
	public JMenuItem getPasteMenuItem() {
		return _pasteMenuItem;
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
