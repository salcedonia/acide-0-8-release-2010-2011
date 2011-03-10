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
package acide.gui.toolBarPanel.consoleCommandToolBar;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.configuration.toolBar.consoleComandToolBar.AcideConsoleCommand;
import acide.configuration.toolBar.consoleComandToolBar.AcideConsoleCommandList;
import acide.gui.mainWindow.AcideMainWindow;


import acide.resources.AcideResourceManager;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE console command tool bar.
 * 
 * Its buttons executes console commands in the ACIDE - A Configurable IDE
 * console panel.
 * 
 * @version 0.8
 * @see ArrayList
 */
public class AcideConsoleCommandToolBar extends ArrayList<Component> {

	/**
	 * ACIDE - A Configurable IDE console command tool bar class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console command tool bar unique class
	 * instance.
	 */
	private static AcideConsoleCommandToolBar _instance;

	/**
	 * Creates a new ACIDE - A Configurable IDE console command tool bar.
	 */
	public AcideConsoleCommandToolBar() {
		super();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console command tool bar unique
	 * class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE console command tool bar unique
	 *         class instance.
	 */
	public static AcideConsoleCommandToolBar getInstance() {

		if (_instance == null)
			_instance = new AcideConsoleCommandToolBar();
		return _instance;
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console command tool bar with the
	 * tool bar project configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE console command tool bar.
	 */
	public AcideConsoleCommandToolBar build() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the message
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s130"));

		// Removes all the buttons
		clear();

		// Separator
		add(Box.createRigidArea(new Dimension(10, 10)));
		
		// Adds all the buttons
		JButton button;
		for (int command = 0; command < AcideConsoleCommandList.getSize(); command++) {

			final AcideConsoleCommand newCommand = AcideConsoleCommandList
					.getShellCommandAt(command);

			if (newCommand.getHasIcon())
				button = new JButton(new ImageIcon(newCommand.getIcon()));
			else if (!(newCommand.getName().equals("")))
				button = new JButton(newCommand.getName());
			else
				button = new JButton((new Integer(command + 1)).toString());
			if (!(newCommand.getHintText().equals("")))
				button.setToolTipText(newCommand.getHintText());
			
			// Adds the button to the tool bar
			add(button);

			// LISTENER
			button.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see
				 * java.awt.event.ActionListener#actionPerformed(java.awt.event
				 * .ActionEvent)
				 */
				@Override
				public void actionPerformed(ActionEvent actionEvent) {

					JFileChooser fileChooser = new JFileChooser();
					int returnValue = 0;

					// ACTION
					switch (newCommand.getParameterType()) {

					case NONE:
						AcideMainWindow.getInstance().getConsolePanel()
								.executeCommand(newCommand.getAction(), "");
						break;
					case TEXT:

						// Ask to the user for the text
						String text = JOptionPane.showInputDialog(null,
								labels.getString("s1009"));

						AcideMainWindow.getInstance().getConsolePanel()
								.executeCommand(newCommand.getAction(), text);
						break;
					case FILE:

						// Ask to the user for the file
						fileChooser
								.setFileSelectionMode(JFileChooser.FILES_ONLY);
						returnValue = fileChooser.showOpenDialog(null);

						if (returnValue == JFileChooser.APPROVE_OPTION) {

							AcideMainWindow
									.getInstance()
									.getConsolePanel()
									.executeCommand(
											newCommand.getAction(),
											fileChooser.getSelectedFile()
													.getAbsolutePath());
						}

						break;
					case DIRECTORY:

						// Ask to the user for the directory
						fileChooser
								.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
						returnValue = fileChooser.showOpenDialog(null);

						if (returnValue == JFileChooser.APPROVE_OPTION) {

							AcideMainWindow
									.getInstance()
									.getConsolePanel()
									.executeCommand(
											newCommand.getAction(),
											fileChooser.getSelectedFile()
													.getAbsolutePath());
						}

						break;
					}

					// Sets the caret position at the end of the text of the
					// console panel
					AcideMainWindow
							.getInstance()
							.getConsolePanel()
							.getTextPane()
							.setCaretPosition(
									AcideMainWindow.getInstance().getConsolePanel()
											.getTextPane().getDocument()
											.getLength());

					// Sets the focus in the console panel
					AcideMainWindow.getInstance().getConsolePanel().getTextPane()
							.requestFocusInWindow();
				}
			});
			
			add(Box.createRigidArea(new Dimension(5, 5)));
		}
		// Updates the log
		AcideLog.getLog().info(labels.getString("s131"));

		return this;
	}
}
