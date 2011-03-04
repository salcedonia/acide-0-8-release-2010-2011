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
package gui.consolePanel.listeners;

import gui.consolePanel.AcideConsolePanel;
import gui.mainWindow.MainWindow;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.text.BadLocationException;

import operations.log.AcideLog;

/**
 * ACIDE - A Configurable IDE console panel keyboard listener.
 * 
 * @version 0.8
 * @see KeyListener
 */
public class AcideConsolePanelKeyboardListener implements KeyListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyTyped(KeyEvent keyEvent) {

		// Gets the console panel
		AcideConsolePanel consolePanel = MainWindow.getInstance()
				.getConsolePanel();

		// If the console pane text pane is initialized
		if (consolePanel.getTextPane() != null) {

			// If the caret position is behind the prompt caret position
			if (consolePanel.getTextPane().getCaretPosition() < consolePanel
					.getPromptCaretPosition())

				// Ignores the key
				keyEvent.consume();
			else
				// Updates the selection size
				consolePanel
						.setSelectionSize(consolePanel.getSelectionSize() + 1);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		// If the console panel text pane is initialized
		if (MainWindow.getInstance().getConsolePanel().getTextPane() != null) {

			if (!MainWindow.getInstance().getConsolePanel()
					.getTextPaneContent().matches("")) {

				// If the caret is in the limit with the prompt
				// and the key is left, up or down
				if ((MainWindow.getInstance().getConsolePanel().getTextPane()
						.getCaretPosition() == MainWindow.getInstance()
						.getConsolePanel().getPromptCaretPosition())
						&& ((keyEvent.getKeyCode() == KeyEvent.VK_LEFT)
								|| (keyEvent.getKeyCode() == 8) || keyEvent
								.getKeyCode() == KeyEvent.VK_UP))
					// Consumes the key
					keyEvent.consume();

				// If the caret is behind the prompt
				if (MainWindow.getInstance().getConsolePanel().getTextPane()
						.getSelectionStart() < MainWindow.getInstance()
						.getConsolePanel().getPromptCaretPosition())

					// Consumes the key
					keyEvent.consume();

				// Gets the command
				String command = (String) MainWindow
						.getInstance()
						.getConsolePanel()
						.getTextPane()
						.getText()
						.subSequence(
								MainWindow.getInstance().getConsolePanel()
										.getPromptCaretPosition(),
								MainWindow.getInstance().getConsolePanel()
										.getDefaultStyledDocument().getLength());

				switch (keyEvent.getKeyCode()) {

				case KeyEvent.VK_ENTER:

					if (keyEvent.getKeyChar() == '\n') {

						if (MainWindow.getInstance().getConsolePanel()
								.getProcessThread().getWriter() != null)
							MainWindow.getInstance().getConsolePanel()
									.sendCommandToConsole(command, "");
					}
					break;

				case KeyEvent.VK_UP:

					keyEvent.consume();

					if (MainWindow.getInstance().getConsolePanel()
							.getProcessThread().getWriter() != null) {

						// If there are commands in the command record
						if (MainWindow.getInstance().getConsolePanel()
								.getCommandRecordCurrentIndex() > -1) {

							// If it is the first command
							if (MainWindow.getInstance().getConsolePanel()
									.getCommandRecordCurrentIndex() == 0)

								// Sets the last one as the current one
								MainWindow
										.getInstance()
										.getConsolePanel()
										.setCommandRecordCurrentIndex(
												MainWindow
														.getInstance()
														.getConsolePanel()
														.getCommandRecordMaximumIndex() - 1);
							else
								// Sets the previous one as the current one
								MainWindow
										.getInstance()
										.getConsolePanel()
										.setCommandRecordCurrentIndex(
												MainWindow
														.getInstance()
														.getConsolePanel()
														.getCommandRecordCurrentIndex() - 1);

							// Replaces the command by the previous one
							try {

								// Clears the zone after the prompt caret
								// position
								MainWindow
										.getInstance()
										.getConsolePanel()
										.getDefaultStyledDocument()
										.remove(MainWindow.getInstance()
												.getConsolePanel()
												.getPromptCaretPosition(),
												command.length());

								if (MainWindow.getInstance().getConsolePanel()
										.getCommandRecordCurrentIndex() > -1)
									// Puts the current command in the command
									// record
									MainWindow
											.getInstance()
											.getConsolePanel()
											.getDefaultStyledDocument()
											.insertString(
													MainWindow
															.getInstance()
															.getConsolePanel()
															.getPromptCaretPosition(),
													MainWindow
															.getInstance()
															.getConsolePanel()
															.getCommandRecord()
															.get(MainWindow
																	.getInstance()
																	.getConsolePanel()
																	.getCommandRecordCurrentIndex()),
													null);
							} catch (BadLocationException exception) {

								// Updates the log
								AcideLog.getLog().error(exception.getMessage());
								exception.printStackTrace();
							}
						}
					}
					break;

				case KeyEvent.VK_DOWN:

					if (MainWindow.getInstance().getConsolePanel()
							.getProcessThread().getWriter() != null) {

						// If there are commands in the command record
						if (MainWindow.getInstance().getConsolePanel()
								.getCommandRecordCurrentIndex() > -1) {

							// If it is the last command
							if (MainWindow.getInstance().getConsolePanel()
									.getCommandRecordCurrentIndex() >= MainWindow
									.getInstance().getConsolePanel()
									.getCommandRecordMaximumIndex() - 1)

								// Sets the first one as the current one
								MainWindow.getInstance().getConsolePanel()
										.setCommandRecordCurrentIndex(0);
							else

								// Sets the next one as the current one
								MainWindow
										.getInstance()
										.getConsolePanel()
										.setCommandRecordCurrentIndex(
												MainWindow
														.getInstance()
														.getConsolePanel()
														.getCommandRecordCurrentIndex() + 1);

							// Replaces the command by the previous one
							try {

								// Clears the zone after the prompt caret
								// position
								MainWindow
										.getInstance()
										.getConsolePanel()
										.getDefaultStyledDocument()
										.remove(MainWindow.getInstance()
												.getConsolePanel()
												.getPromptCaretPosition(),
												command.length());

								// Puts the current command in the command
								// record
								MainWindow
										.getInstance()
										.getConsolePanel()
										.getDefaultStyledDocument()
										.insertString(
												MainWindow
														.getInstance()
														.getConsolePanel()
														.getPromptCaretPosition(),
												MainWindow
														.getInstance()
														.getConsolePanel()
														.getCommandRecord()
														.get(MainWindow
																.getInstance()
																.getConsolePanel()
																.getCommandRecordCurrentIndex()),
												null);
							} catch (BadLocationException exception) {

								// Updates the log
								AcideLog.getLog().error(exception.getMessage());
								exception.printStackTrace();
							}
						}
					}
					break;

				case KeyEvent.VK_ESCAPE:

					if (MainWindow.getInstance().getConsolePanel()
							.getProcessThread().getWriter() != null) {

						// Removes the text selection
						try {
							MainWindow
									.getInstance()
									.getConsolePanel()
									.getDefaultStyledDocument()
									.remove(MainWindow.getInstance()
											.getConsolePanel()
											.getPromptCaretPosition(),
											command.length());
						} catch (BadLocationException exception) {

							// Updates the log
							AcideLog.getLog().error(exception.getMessage());
							exception.printStackTrace();
						}
					}
					break;

				case KeyEvent.VK_HOME:

					// Consumes the key
					keyEvent.consume();

					if (MainWindow.getInstance().getConsolePanel()
							.getProcessThread().getWriter() != null) {
						// Sets the caret after the prompt
						MainWindow
								.getInstance()
								.getConsolePanel()
								.getTextPane()
								.setCaretPosition(
										MainWindow.getInstance()
												.getConsolePanel()
												.getPromptCaretPosition());
					}
					break;

				case KeyEvent.VK_END:

					// Consumes the key
					keyEvent.consume();

					if (MainWindow.getInstance().getConsolePanel()
							.getProcessThread().getWriter() != null) {
						// Sets the caret at the end of the command
						MainWindow
								.getInstance()
								.getConsolePanel()
								.getTextPane()
								.setCaretPosition(
										MainWindow.getInstance()
												.getConsolePanel()
												.getPromptCaretPosition()
												+ command.length());
					}
					break;

				case KeyEvent.VK_C:

					// CTRL + C --> COPY
					if (keyEvent.isControlDown())
						MainWindow.getInstance().getConsolePanel()
								.getTextPane().copy();
					break;
				}
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyReleased(KeyEvent keyEvent) {

		// Gets the console panel
		AcideConsolePanel consolePanel = MainWindow.getInstance()
				.getConsolePanel();

		// If the console panel text pane is initialized
		if (consolePanel.getTextPane() != null)

			// If the caret is the not editable zone
			if (consolePanel.getTextPane().getCaretPosition() < consolePanel
					.getPromptCaretPosition())

				// Consumes the key
				keyEvent.consume();
	}
}
