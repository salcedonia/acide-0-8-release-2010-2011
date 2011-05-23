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
package acide.gui.consolePanel;

import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.consolePanel.listeners.AcideConsolePanelFocusListener;
import acide.gui.consolePanel.listeners.AcideConsolePanelKeyboardListener;
import acide.gui.consolePanel.listeners.AcideConsolePanelMouseListener;
import acide.gui.consolePanel.listeners.AcideConsolePanelMouseWheelListener;
import acide.gui.consolePanel.listeners.AcideConsolePanelPopupMenuListener;
import acide.gui.consolePanel.popup.AcideConsolePanelPopupMenu;
import acide.gui.fileEditor.fileEditorManager.utils.logic.AcideStyledDocument;
import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.io.IOException;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.JTextComponent;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.process.console.AcideConsoleProcess;
import acide.resources.AcideResourceManager;
import acide.resources.exception.MissedPropertyException;

/**
 * ACIDE - A Configurable IDE console panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideConsolePanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE console panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console panel text component.
	 */
	private JTextPane _textPane;
	/**
	 * ACIDE - A Configurable IDE console panel scroll pane.
	 */
	private JScrollPane _scrollPane;
	/**
	 * ACIDE - A Configurable IDE console panel text handler.
	 */
	private AcideStyledDocument _styledDocument;
	/**
	 * ACIDE - A Configurable IDE console panel process thread.
	 */
	private AcideConsoleProcess _consoleProcess;
	/**
	 * ACIDE - A Configurable IDE console panel popup menu.
	 */
	private AcideConsolePanelPopupMenu _popupMenu;
	/**
	 * ACIDE - A Configurable IDE console panel command to display.
	 */
	private String _command;
	/**
	 * ACIDE - A Configurable IDE console panel prompt caret position.
	 */
	private int _promptCaretPosition;
	/**
	 * ACIDE - A Configurable IDE console panel user commands record.
	 */
	private AcideConsolePanelCommandRecord _commandRecord;
	/**
	 * ACIDE - A Configurable IDE console panel lexicon configuration.
	 */
	private AcideLexiconConfiguration _lexiconConfiguration;
	/**
	 * ACIDE - A Configurable IDE console panel size.
	 */
	private int _size;

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel.
	 */
	public AcideConsolePanel(boolean editable) {

		super();

		// Creates the command record
		_commandRecord = new AcideConsolePanelCommandRecord();

		try {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s422"));

			// Builds the text pane
			_textPane = buildConsole();

			// Sets the editable property
			_textPane.setEditable(editable);

			// No command yet
			_command = "";

			// The prompt caret position is not set yet
			_promptCaretPosition = 0;

			// Sets the font by default to the text pane
			_textPane.setFont(new Font("Monospaced", Font.PLAIN, 12));

			// Sets the foreground color by default to the text pane
			_textPane.setForeground(Color.BLACK);

			// Sets the caret color by default to the text pane
			_textPane.setCaretColor(Color.BLACK);

			// Sets the background color by default to the text pane
			_textPane.setBackground(Color.WHITE);

			// This avoids the extra return carriages in the console
			_textPane.getInputMap()
					.put(KeyStroke.getKeyStroke("ENTER"), "none");

			// Sets the layout
			setLayout(new BorderLayout());

			// Creates the scroll panel with the text pane inside of it
			_scrollPane = new JScrollPane(_textPane);

			// Adds the scroll panel to the panel
			add(_scrollPane);

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s423"));

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s424"));
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Sets the listeners of the text pane
		setListeners();

		// Initializes the popup menu
		buildPopupMenu();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console text component.
	 * 
	 * @return the ACIDE - A Configurable IDE console text component.
	 * @see JTextComponent
	 */
	protected JTextPane buildConsole() {

		// Creates and loads the lexicon configuration from the lexicon assigner
		_lexiconConfiguration = new AcideLexiconConfiguration();

		// If the lexicon has to be applied to the console
		if (AcideWorkbenchConfiguration.getInstance()
				.getLexiconAssignerConfiguration().getApplyLexiconToConsole()) {

			// If there is no a lexicon configuration defined for the console
			if (AcideWorkbenchConfiguration.getInstance()
					.getLexiconAssignerConfiguration()
					.getConsoleLexiconConfiguration().matches(""))

				// Loads the default lexicon configuration by default
				_lexiconConfiguration
						.load(AcideLexiconConfiguration.DEFAULT_PATH
								+ AcideLexiconConfiguration.DEFAULT_NAME);
			else
				// Loads the defined lexicon configuration
				_lexiconConfiguration.load(AcideWorkbenchConfiguration
						.getInstance().getLexiconAssignerConfiguration()
						.getConsoleLexiconConfiguration());
		} else

			// Loads the default lexicon configuration by default
			_lexiconConfiguration.load(AcideLexiconConfiguration.DEFAULT_PATH
					+ AcideLexiconConfiguration.DEFAULT_NAME);

		// Creates the styled document
		_styledDocument = new AcideStyledDocument(_lexiconConfiguration);

		// Creates the text area
		JTextPane textArea = new JTextPane(_styledDocument) {

			/**
			 * Text area class serial version UID.
			 */
			private static final long serialVersionUID = 1L;

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.Component#setSize(java.awt.Dimension)
			 */
			@Override
			public void setSize(Dimension d) {
				if (d.width < getParent().getSize().width)
					d.width = getParent().getSize().width;

				super.setSize(d);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see javax.swing.JEditorPane#getScrollableTracksViewportWidth()
			 */
			@Override
			public boolean getScrollableTracksViewportWidth() {
				return false;
			}
		};
		return textArea;
	}

	/**
	 * Sets the listeners of the ACIDE - A Configurable IDE console panel text
	 * pane.
	 */
	private void setListeners() {

		// Sets the ACIDE - A Configurable IDE console panel keyboard listener
		_textPane.addKeyListener(new AcideConsolePanelKeyboardListener());

		// Sets the ACIDE - A Configurable IDE console panel focus listener
		_textPane.addFocusListener(new AcideConsolePanelFocusListener());

		// Sets the ACIDE - A Configurable IDE console panel mouse listener
		_textPane.addMouseListener(new AcideConsolePanelMouseListener());

		// Sets the ACIDE - A Configurable IDE console panel mouse wheel
		// listener
		_textPane
				.addMouseWheelListener(new AcideConsolePanelMouseWheelListener());

		// Sets the ACIDE - A Configurable IDE console panel popup menu listener
		_textPane.addMouseListener(new AcideConsolePanelPopupMenuListener());
	}

	/**
	 * Updates the ACIDE - A Configurable IDE console panel look and feel.
	 */
	public void setLookAndFeel() {

		Font newFont = null;
		try {

			// Creates the new font to display
			newFont = new Font(AcideResourceManager.getInstance().getProperty(
					"consolePanel.fontName"),
					Integer.parseInt(AcideResourceManager.getInstance()
							.getProperty("consolePanel.fontStyle")),
					Integer.parseInt(AcideResourceManager.getInstance()
							.getProperty("consolePanel.fontSize")));

			// Sets the font type
			_textPane.setFont(newFont);

			// Sets the foreground color
			setForegroundColor(new Color(Integer.parseInt(AcideResourceManager
					.getInstance().getProperty("consolePanel.foregroundColor"))));

			// Sets the background color
			setBackgroundColor(new Color(Integer.parseInt(AcideResourceManager
					.getInstance().getProperty("consolePanel.backgroundColor"))));

			// Sets the caret color
			_textPane.setCaretColor(new Color(Integer
					.parseInt(AcideResourceManager.getInstance().getProperty(
							"consolePanel.foregroundColor"))));

		} catch (NumberFormatException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		} catch (MissedPropertyException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console panel popup menu.
	 */
	public void buildPopupMenu() {
		_popupMenu = new AcideConsolePanelPopupMenu();
	}

	/**
	 * <p>
	 * Adds a text given as a parameter to the console text.
	 * </p>
	 * <p>
	 * Once the command has been sent to the console process, the reader in the
	 * console process sends the text to add to the console panel text as a
	 * response from the external shell.
	 * </p>
	 * 
	 * @param text
	 *            text to add.
	 */
	public void addText(String text) {

		// Erases the duplicated command
		String newText = eraseDuplicatedCommand(text);

		// Updates the text in the text pane
		_textPane.setText(_textPane.getText().concat(newText));

		// Updates the prompt caret position
		_promptCaretPosition = _textPane.getText().length();

		// Updates the caret position
		_textPane.setCaretPosition(_textPane.getText().length());
	}

	/**
	 * <p>
	 * Erases the duplicated command from the text to add to the console panel
	 * after sending a command to the console process.
	 * </p>
	 * <p>
	 * This phenomenon only seems to appear when the loaded shell is the
	 * operative system one, not with other different shells though.
	 * </p>
	 * 
	 * @param text
	 *            new text to add.
	 */
	public String eraseDuplicatedCommand(String text) {

		// Avoids index out of bounds exceptions
		if (_command.length() <= text.length()) {

			// If the first thing of the new text is the command
			if (text.substring(0, _command.length()).equals(_command))

				// Skips it
				return text.substring(_command.length(), text.length());
		}
		return text;
	}

	/**
	 * Executes the console exit command.
	 */
	public void executeExitCommand() {

		try {

			// Gets the exit command
			_command = AcideResourceManager.getInstance().getProperty(
					"consolePanel.exitCommand");

			// If the writer is initialized
			if (_consoleProcess.getWriter() != null) {

				// Sends the exit command to the writer
				_consoleProcess.getWriter().write(_command + '\n');

				// Flushes the writer
				_consoleProcess.getWriter().flush();
			}

			// Kills the process anyway
			killShellProcess();

		} catch (Exception exception) {

			// The stream is closed

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			// exception.printStackTrace();
		}
	}

	/**
	 * Forces to the operative system to kill the shell process when the
	 * specified exit command is incorrect.
	 */
	public void killShellProcess() {

		String shellPath;
		try {

			// Gets the shell path
			shellPath = AcideResourceManager.getInstance().getProperty(
					"consolePanel.shellPath");

			// Gets the name of the current process
			int lastIndexOfSlash = shellPath.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = shellPath.lastIndexOf("/");

			// Gets the shell name process
			String shellName = shellPath.substring(lastIndexOfSlash + 1);

			// Gets the operative system
			String operativeSystemName = System.getProperty("os.name");
			String command = "";

			// If it is WINDOWS
			if (operativeSystemName.toUpperCase().contains("WIN")) {
				command += "taskkill /im " + shellName + " /f";
			} else {
				command += "killall " + shellName;
			}

			Process killerProcess;
			try {

				String message = "";

				// Executes the killer process
				killerProcess = Runtime.getRuntime().exec(command);
				killerProcess.waitFor();

				if (killerProcess.exitValue() == 0)
					message += shellName + " succesfully killed";
				else
					message += "Unable to kill " + shellName + ". Exit code: "
							+ killerProcess.exitValue() + "n";

				// Updates the log
				AcideLog.getLog().info(message);

				// Destroy the cmd.exe process recently executed
				killerProcess.destroy();

			} catch (IOException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();

			} catch (InterruptedException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

		} catch (MissedPropertyException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Executes a command in the console given as a parameter.
	 * 
	 * @param command
	 *            command to execute.
	 * @param parameter
	 *            parameter to use.
	 */
	public void executeCommand(String command, String parameter) {

		if (_consoleProcess.getWriter() != null) {

			try {

				// If there are opened file editors
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getNumberOfFileEditorPanels() > 0) {

					// Replaces the active file variable for its real value
					command = command.replace("$activeFile$", AcideMainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath());

					// Replaces the active file path variable for its real value
					command = command
							.replace("$activeFilePath$", AcideMainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getFilePath());

					// Replaces the active files extension for its real value
					command = command.replace("$activeFileExt$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getFileExtension());

					// Replaces the active files name for its real value
					command = command.replace("$activeFileName$",
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getFileNameWithoutExtension());
				}

				// If it is the default project
				if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// Gets the main file editor panel
					AcideFileEditorPanel mainFileEditorPanel = AcideMainWindow
							.getInstance().getFileEditorManager()
							.getMainFileEditorPanel();

					// If exists
					if (mainFileEditorPanel != null) {

						// Replaces the $mainFile$ variable for its real value
						command = command.replace("$mainFile$",
								mainFileEditorPanel.getAbsolutePath());

						// Replaces the $mainFilePath$ variable for its real
						// value
						command = command.replace("$mainFilePath$",
								mainFileEditorPanel.getFilePath());

						// Replaces the $mainFileExt$ variable for its real
						// value
						command = command.replace("$mainFileExt$",
								mainFileEditorPanel.getFileExtension());

						// Replaces the $mainFileName$ variable for its real
						// value
						command = command.replace("$mainFileName$",
								mainFileEditorPanel
										.getFileNameWithoutExtension());
					}
				} else {

					// Not default project

					// Searches for the MAIN file into the ACIDE - A
					// Configurable IDE
					// project configuration
					int mainFileEditorPanelIndex = -1;
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getNumberOfFilesFromList(); index++) {
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(index).isMainFile())
							mainFileEditorPanelIndex = index;
					}

					// If exists
					if (mainFileEditorPanelIndex != -1) {

						// Replaces the $mainFile$ variable for its real value
						command = command.replace("$mainFile$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileEditorPanelIndex)
										.getAbsolutePath());

						// Replaces the $mainFilePath$ variable for its real
						// value
						command = command.replace("$mainFilePath$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileEditorPanelIndex)
										.getRelativePath());

						// Replaces the $mainFileExt$ variable for its real
						// value
						command = command.replace("$mainFileExt$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileEditorPanelIndex)
										.getFileExtension());

						// Replaces the $mainFileName$ variable for its real
						// value
						command = command.replace("$mainFileName$",
								AcideProjectConfiguration.getInstance()
										.getFileAt(mainFileEditorPanelIndex)
										.getFileName());
					}
				}

				// Erases the previous text after the prompt
				_textPane.setText(_textPane.getText().substring(0,
						_promptCaretPosition));

				// If the parameter is not ""
				if (!parameter.matches("")) {

					// If it is echo of the command
					if (Boolean.parseBoolean(AcideResourceManager.getInstance()
							.getProperty("consolePanel.isEchoCommand")))

						// Adds the command to the text pane text
						_textPane.setText(_textPane.getText().concat(
								command + " " + parameter));
				} else {

					// If it is echo of the command
					if (Boolean.parseBoolean(AcideResourceManager.getInstance()
							.getProperty("consolePanel.isEchoCommand")))

						// Adds the command to the text pane text
						_textPane.setText(_textPane.getText().concat(command));
				}

				// Sends the command to the console
				sendCommandToConsole(command, parameter);

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * Sends the command to the console writer so it can execute the command.
	 * 
	 * @param command
	 *            command to execute in the console writer.
	 * @param parameter
	 *            parameter to use.
	 */
	public void sendCommandToConsole(String command, String parameter) {

		try {

			if (_consoleProcess.getWriter() != null) {

				// If the parameter is not ""
				if (!parameter.matches("")) {

					// Updates the command to be executed
					_command = command + " " + parameter;

					// Sends the command to the output process
					_consoleProcess.getWriter().write(
							command + " " + parameter + '\n');

				} else {

					// Updates the command to be executed
					_command = command;

					// Sends the command to the output shell
					_consoleProcess.getWriter().write(command + '\n');
				}

				// Flushes the writer
				_consoleProcess.getWriter().flush();

				// Updates the command record
				updateCommandRecord();
			}
		} catch (IOException exception) {

			// The stream is closed

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			// exception.printStackTrace();
		}
	}

	/**
	 * <p>
	 * Updates the ACIDE - A Configurable IDE console command record with the
	 * new executed command.
	 * </p>
	 * <p>
	 * In case the command contains more than one line, the method splits it
	 * into different lines and stores them as separated commands into the
	 * console command record.
	 * </p>
	 */
	public void updateCommandRecord() {

		// The blank command does not count
		if (!_command.matches("\n")) {

			// Splits the command in lines
			String[] lines = _command.split("\n");

			for (int index = 0; index < lines.length; index++) {

				// If the command record contains the command
				if (_commandRecord.contains(lines[index])) {

					// Decreases the command record maximum index
					_commandRecord.setMaximumIndex(_commandRecord
							.getMaximumIndex() - 1);

					// Removes the command from the command record
					_commandRecord.remove(lines[index]);
				}

				// Adds the command to the command record
				_commandRecord.add(lines[index]);

				// Increases the command record maximum index
				_commandRecord
						.setMaximumIndex(_commandRecord.getMaximumIndex() + 1);

				// The current command record current index is the maximum
				// index
				_commandRecord
						.setCurrentIndex(_commandRecord.getMaximumIndex());
			}

			// The last command in execution was the last line of it
			_command = lines[lines.length - 1];
		}
	}

	/**
	 * Clears the console text buffer, leaving only the prompt ready for the
	 * next command execution.
	 */
	public void clearConsoleBuffer() {

		// Gets the text from the beginning to the prompt caret position
		String text = _textPane.getText().substring(0, _promptCaretPosition);

		// Splits the text in lines
		String[] lines = text.split("\n");

		// Puts only the last line
		_textPane.setText(lines[lines.length - 1]);

		// Updates the prompt caret position
		_promptCaretPosition = _textPane.getText().length();

		// Updates the caret position
		_textPane.setCaretPosition(_promptCaretPosition);
	}

	/**
	 * Resets the ACIDE - A Configurable IDE console panel.
	 */
	public void resetConsole() {

		// Kills the previous process
		executeExitCommand();

		// If the lexicon has to be applied to the console
		if (AcideWorkbenchConfiguration.getInstance()
				.getLexiconAssignerConfiguration().getApplyLexiconToConsole()) {

			// If there is no a lexicon configuration defined for the console
			if (AcideWorkbenchConfiguration.getInstance()
					.getLexiconAssignerConfiguration()
					.getConsoleLexiconConfiguration().matches(""))

				// Loads the default lexicon configuration by default
				_lexiconConfiguration
						.load(AcideLexiconConfiguration.DEFAULT_PATH
								+ AcideLexiconConfiguration.DEFAULT_NAME);
			else
				// Loads the defined lexicon configuration
				_lexiconConfiguration.load(AcideWorkbenchConfiguration
						.getInstance().getLexiconAssignerConfiguration()
						.getConsoleLexiconConfiguration());
		} else

			// Loads the default lexicon configuration by default
			_lexiconConfiguration.load(AcideLexiconConfiguration.DEFAULT_PATH
					+ AcideLexiconConfiguration.DEFAULT_NAME);

		// Applies the highlighting
		resetStyledDocument();

		// Sets the text pane as ""
		_textPane.setText("");

		// Creates a new console process
		_consoleProcess = new AcideConsoleProcess();

		// Starts the console process
		_consoleProcess.start();
	}

	/**
	 * <p>
	 * Zooms in or out the font size of the ACIDE - A Configurable IDE console
	 * panel text pane depending on a boolean variable given as a parameter.
	 * </p>
	 * <p>
	 * Also the increment is given as parameter as well.
	 * </p>
	 * 
	 * @param zoom
	 *            increment to apply.
	 * @param hasToIncrement
	 *            indicates if the zoom is in or out.
	 */
	public void zoomFont(int zoom, boolean hasToIncrement) {

		// Gets the current font
		Font currentFont = _textPane.getFont();

		Font newFont;

		// If it is zoom out
		if (hasToIncrement)
			newFont = new Font(currentFont.getFontName(),
					currentFont.getStyle(), currentFont.getSize() + zoom);
		else
			newFont = new Font(currentFont.getFontName(),
					currentFont.getStyle(), currentFont.getSize() - zoom);

		// Sets the new font
		_textPane.setFont(newFont);
	}

	/**
	 * Selects a text in the ACIDE - A Configurable IDE console panel text pane.
	 * 
	 * @param start
	 *            selection start.
	 * @param length
	 *            selection length.
	 */
	public void selectText(int start, int length) {

		// Sets the selection start
		_textPane.setSelectionStart(start);

		// Sets the selection end
		_textPane.setSelectionEnd(start + length);
	}

	/**
	 * Resets the styled document of the text pane in the ACIDE - A Configurable
	 * IDE console panel.
	 */
	public void resetStyledDocument() {

		// Initializes the document
		_styledDocument.init();
	}

	/**
	 * Shows the ACIDE - A Configurable IDE console panel.
	 */
	public void showConsolePanel() {

		// Sets the horizontal split panel divider location to the end
		AcideMainWindow.getInstance().getHorizontalSplitPane()
				.setDividerLocation(_size);

		// Hides the console panel
		AcideMainWindow.getInstance().getHorizontalSplitPane()
				.getBottomComponent().setVisible(true);
	}

	/**
	 * Disposes the ACIDE - A Configurable IDE console panel.
	 */
	public void disposeConsolePanel() {

		// Sets the ACIDE - A Configurable IDE console panel size
		_size = AcideMainWindow.getInstance().getHorizontalSplitPane()
				.getDividerLocation();

		// Sets the horizontal split panel divider location to 0
		AcideMainWindow.getInstance().getHorizontalSplitPane()
				.setDividerLocation(0);

		// Shows the console panel
		AcideMainWindow.getInstance().getHorizontalSplitPane()
				.getBottomComponent().setVisible(false);
	}

	/**
	 * Returns the prompt caret position.
	 * 
	 * @return the prompt caret position.
	 */
	public int getPromptCaretPosition() {
		return _promptCaretPosition;
	}

	/**
	 * Sets a new value to the font name.
	 * 
	 * @param fontName
	 *            new value to set.
	 */
	public void setFontName(Font font) {
		_textPane.setFont(font);
		repaint();
	}

	/**
	 * Sets a new value to the foreground color.
	 * 
	 * @param foregroundColor
	 *            new value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_textPane.setForeground(foregroundColor);
		_textPane.setCaretColor(foregroundColor);
		repaint();
	}

	/**
	 * Sets a new value to the background color.
	 * 
	 * @param backgroundColor
	 *            new value to set.
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_textPane.setBackground(backgroundColor);
		repaint();
	}

	/**
	 * Returns the default styled document.
	 * 
	 * @return the default styled document.
	 */
	public DefaultStyledDocument getDefaultStyledDocument() {
		return _styledDocument;
	}

	/**
	 * Sets a new value to the default styled document.
	 * 
	 * @param defaultStyledDocument
	 *            new value to set.
	 */
	public void setDefaultStyledDocument(
			AcideStyledDocument defaultStyledDocument) {
		_styledDocument = defaultStyledDocument;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console process thread.
	 * 
	 * @return the ACIDE - A Configurable IDE console process thread.
	 */
	public AcideConsoleProcess getProcessThread() {
		return _consoleProcess;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel command record.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel command record.
	 */
	public AcideConsolePanelCommandRecord getCommandRecord() {
		return _commandRecord;
	}

	/**
	 * Returns the console panel popup menu.
	 * 
	 * @return the console panel popup menu.
	 */
	public AcideConsolePanelPopupMenu getPopupMenu() {
		return _popupMenu;
	}

	/**
	 * Sets a new value to the command.
	 * 
	 * @param command
	 *            new value to set.
	 */
	public void setCommand(String command) {
		_command = command;
	}

	/**
	 * Sets a new value to the prompt caret position.
	 * 
	 * @param promptCaretPosition
	 *            new value to set.
	 */
	public void setPromptCaretPosition(int promptCaretPosition) {
		_promptCaretPosition = promptCaretPosition;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel text pane.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel text pane.
	 */
	public JTextComponent getTextPane() {
		return _textPane;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel lexicon
	 * configuration.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel lexicon
	 *         configuration.
	 */
	public AcideLexiconConfiguration getLexiconConfiguration() {
		return _lexiconConfiguration;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel size.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel size.
	 */
	public int getConsoleSize() {
		return _size;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel size.
	 * 
	 * @param size
	 *            new value to set.
	 */
	public void setConsoleSize(int size) {
		_size = size;
	}
}
