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
package gui.consolePanel.popup;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import operations.factory.AcideGUIFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

import language.AcideLanguageManager;

/**																
 * ACIDE - A Configurable IDE console panel popup menu.										
 *					
 * @version 0.8
 * @see JPopupMenu	
 * @see JMenuItem																													
 */
public class AcideConsolePanelPopupMenu extends JPopupMenu {

	/**
	 * Acide console panel popup menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the copy menu item.
	 */
	private final static ImageIcon COPY_IMAGE = new ImageIcon("./resources/icons/menu/edit/copy.png");
	/**
	 * Image file for the paste menu item.
	 */
	private final static ImageIcon PASTE_IMAGE = new ImageIcon("./resources/icons/menu/edit/paste.png");
	/**
	 * image file for the cut menu item.
	 */
	private final static ImageIcon CUT_IMAGE = new ImageIcon("./resources/icons/menu/edit/cut.png");
	/**
	 * Shell display options menu item image icon.
	 */
	private final static ImageIcon CONSOLE_DISPLAY_OPTIONS_IMAGE = new ImageIcon("./resources/icons/menu/configuration/console/consoleDisplayOptions.png");
	/**
	 * Copy menu item.
	 */
	private JMenuItem _copy;
	/**
	 * Cut menu item.
	 */
	private JMenuItem _cut;
	/**
	 * Paste menu item.
	 */
	private JMenuItem _paste;
	/**
	 * Shell display options menu item.
	 */
	private JMenuItem _consoleDisplayOptions;
	/**
	 * Reset menu item.
	 */
	private JMenuItem _reset;
	/**
	 * Control+C menu item.
	 */
	private JMenuItem _controlC;
	/**
	 * Clear console buffer menu item.
	 */
	private JMenuItem _clearConsoleBuffer;

	/**
	 * Creates a new Acide console panel popup menu.
	 */
	public AcideConsolePanelPopupMenu() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// CONSOLE DISPLAY OPTIONS
		_consoleDisplayOptions = new JMenuItem(labels.getString("s986"), CONSOLE_DISPLAY_OPTIONS_IMAGE);
		_consoleDisplayOptions.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				AcideGUIFactory.getInstance().buildAcideConsoleDisplayOptionsWindow();
			}
		});
		add(_consoleDisplayOptions);
		addSeparator();

		// COPY
		_copy = new JMenuItem(labels.getString("s187"), COPY_IMAGE);
		_copy.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getConsolePanel().getTextPane().copy();
			}
		});
		add(_copy);

		// CUT
		_cut = new JMenuItem(labels.getString("s188"), CUT_IMAGE);
		_cut.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				if (MainWindow.getInstance().getConsolePanel().getTextPane()
						.getSelectionStart() >= MainWindow.getInstance()
						.getConsolePanel().getPromptCaretPosition())
					MainWindow.getInstance().getConsolePanel().getTextPane()
							.cut();
			}
		});
		add(_cut);

		// PASTE
		_paste = new JMenuItem(labels.getString("s189"), PASTE_IMAGE);
		_paste.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				if (MainWindow.getInstance().getConsolePanel().getTextPane()
						.getSelectionStart() >= MainWindow.getInstance()
						.getConsolePanel().getPromptCaretPosition())
					MainWindow.getInstance().getConsolePanel().getTextPane()
							.paste();
			}
		});
		add(_paste);
		addSeparator();

		// CONTROL + C
		_controlC = new JMenuItem("Ctrl-C");
		_controlC.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// SEND THE CTRL-C TO THE OUTPUT WRITER
				//MainWindow.getInstance().getOutput()
					//	.sendCommandToOutput(Character.toString((char) 3));
			}
		});
		add(_controlC);

		// RESET
		_reset = new JMenuItem(labels.getString("s987"));
		_reset.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getConsolePanel().resetConsole();
			}
		});
		add(_reset);
		
		// CLEAR CONSOLE BUFFER
		_clearConsoleBuffer = new JMenuItem(labels.getString("s999"));
		_clearConsoleBuffer.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getConsolePanel().clearConsoleBuffer();
			}
		});
		add(_clearConsoleBuffer);
	}

	/**
	 * Returns the copy menu item
	 * 
	 * @return the copy menu item
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Returns the copy menu item
	 * 
	 * @return the copy menu item
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Returns the paste menu item
	 * 
	 * @return the paste menu item
	 */
	public JMenuItem getPaste() {
		return _paste;
	}
}
