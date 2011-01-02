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
package gui.consolePanel;

import es.configuration.console.AcideConsoleConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import gui.consolePanel.listeners.AcideConsolePanelFocusListener;
import gui.consolePanel.listeners.AcideConsolePanelKeyboardListener;
import gui.consolePanel.listeners.AcideConsolePanelPopupMenuListener;
import gui.consolePanel.popup.AcideConsolePanelPopupMenu;
import gui.listeners.AcideKeyboardListenerForMenus;
import gui.mainWindow.MainWindow;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.io.IOException;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.JTextComponent;

import language.AcideLanguageManager;
import operations.console.ConsoleThread;
import operations.log.AcideLog;
import resources.AcideResourceManager;

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
	private JTextComponent _textPane;
	/**
	 * ACIDE - A Configurable IDE console panel scroll pane.
	 */
	private JScrollPane _scrollPane;
	/**
	 * ACIDE - A Configurable IDE console panel text handler.
	 */
	private DefaultStyledDocument _defaultStyledDocument;
	/**
	 * ACIDE - A Configurable IDE console panel process thread.
	 */
	private ConsoleThread _processThread;
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
	 * ACIDE - A Configurable IDE console panel selection size.
	 */
	private int _selectionSize;
	/**
	 * ACIDE - A Configurable IDE console panel historic current index.
	 */
	private int _commandRecordCurrentIndex;
	/**
	 * ACIDE - A Configurable IDE console panel historic maximum index value.
	 */
	private int _commandRecordMaximumIndex;
	/**
	 * ACIDE - A Configurable IDE console panel user commands record.
	 */
	private ArrayList<String> _commandRecord;

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel.
	 */
	public AcideConsolePanel(boolean editable) {

		super();

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

		try {

			// Updates the log
			AcideLog.getLog().info(labels.getString("s422"));

			_textPane = buildConsole();
			_textPane.setEditable(editable);
			_command = "";
			_promptCaretPosition = 0;
			_selectionSize = 0;

			// Listeners
			_textPane.addKeyListener(new AcideConsolePanelKeyboardListener());
			_textPane.addKeyListener(new AcideKeyboardListenerForMenus());
			_textPane.addFocusListener(new AcideConsolePanelFocusListener());
			_textPane.addCaretListener(new CaretListener() {
				/**
				 * Updates the caret position, setting it always after the
				 * prompt except when it is making the selection for copying the
				 * content.
				 * 
				 * @param caretEvent
				 *            caret event.
				 * @see CaretEvent
				 */
				public void caretUpdate(CaretEvent caretEvent) {

					if (_textPane.getSelectedText() == null) {
						if (_textPane.getCaretPosition() < _textPane.getText()
								.length() - _selectionSize) {
							_textPane.setCaretPosition(_textPane.getText()
									.length());
						}
					}
				}
			});

			_textPane.setFont(new Font("Monospaced", Font.PLAIN, 12));
			_textPane.setForeground(Color.BLACK);
			_textPane.setCaretColor(Color.BLACK);
			_textPane.setBackground(Color.WHITE);

			// Sets the layout
			setLayout(new BorderLayout());

			_scrollPane = new JScrollPane(_textPane);
			add(_scrollPane);

			// Updates the undo manager so it can handles CTRL-Z and CTRL-Y
			//AcideUndoRedoManager.getInstance().update(_defaultStyledDocument);

			// Updates the log
			AcideLog.getLog().info(labels.getString("s423"));

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(labels.getString("s424"));
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// COMMAND RECORD
		_commandRecord = new ArrayList<String>();
		_commandRecordCurrentIndex = 0;
		_commandRecordMaximumIndex = 0;

		// Text component listener
		_textPane.addMouseListener(new AcideConsolePanelPopupMenuListener());

		// INIT THE POPUP MENU
		buildPopupMenu();
	}

	/**
	 * Updates the output visualization.
	 */
	public void updateConsoleDisplayOptions() {

		int fontStyle = 0;

		// Parses from String to FontStyle
		if (AcideConsoleConfiguration.getInstance().getFontStyle()
				.matches("Font.PLAIN"))
			fontStyle = Font.PLAIN;
		if (AcideConsoleConfiguration.getInstance().getFontStyle()
				.matches("Font.BOLD"))
			fontStyle = Font.BOLD;
		if (AcideConsoleConfiguration.getInstance().getFontStyle()
				.matches("Font.ITALIC"))
			fontStyle = Font.ITALIC;
		if (AcideConsoleConfiguration.getInstance().getFontStyle()
				.matches("Font.BOLD+Font.ITALIC"))
			fontStyle = Font.BOLD + Font.ITALIC;

		// Sets the font type
		setFontName(new Font(AcideConsoleConfiguration.getInstance().getFontName(),
				fontStyle, AcideConsoleConfiguration.getInstance().getFontSize()));

		// Sets the foreground color
		setForegroundColor(AcideConsoleConfiguration.getInstance()
				.getForegroundColor());

		// Sets the background color
		setBackgroundColor(AcideConsoleConfiguration.getInstance()
				.getBackgroundColor());

		// Sets the caret color
		_textPane.setCaretColor(AcideConsoleConfiguration.getInstance()
				.getForegroundColor());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console panel popup menu.
	 */
	public void buildPopupMenu() {
		_popupMenu = new AcideConsolePanelPopupMenu();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console text component.
	 * 
	 * @return the ACIDE - A Configurable IDE console text component.
	 * @see JTextComponent
	 */
	protected JTextComponent buildConsole() {

		_defaultStyledDocument = new DefaultStyledDocument();
		JTextPane textArea = new JTextPane(_defaultStyledDocument) {

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
	 * Adds a text given as a parameter to the console text.
	 * 
	 * @param text
	 *            text to add.
	 */
	public void addText(String text) {

		try {
			if (AcideConsoleConfiguration.getInstance().getIsEchoCommand()
					&& (_command.length() <= text.length())) {
				if (text.substring(0, _command.length()).equals(_command)) {
					text = text.substring(_command.length(), text.length());
				}
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		String auxText = _textPane.getText();
		auxText = auxText + text;
		_textPane.setText(auxText);
		_promptCaretPosition = _textPane.getText().length();
		_textPane.setCaretPosition(_textPane.getText().length());
	}

	/**
	 * Executes the console exit command.
	 */
	public void executeExitCommand() {

		try {

			String exitCommand = AcideConsoleConfiguration.getInstance()
					.getExitCommand();
			_command = exitCommand;

			if (_processThread.getWriter() != null) {
				_processThread.getWriter().write(exitCommand + '\n');
				_processThread.getWriter().flush();
			}

			// Kill the process anyway
			killShellProcess();

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Forces to the operative system to kill the shell process when the
	 * specified exit command is incorrect.
	 */
	private void killShellProcess() {

		// Gets the name of the current process
		String shellPath = AcideConsoleConfiguration.getInstance().getShellPath();

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

		if (_processThread.getWriter() != null) {
			try {

				if (MainWindow.getInstance().getFileEditorManager()
						.getNumFileEditorPanels() > 0) {

					command = command.replace("$activeFile$", MainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath());
					command = command.replace("$activeFilePath$", MainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().getFilePath());
					command = command.replace("$activeFileExt$", MainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().getFileExtension());
					command = command.replace("$activeFileName$", MainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().getFileName());
				}

				// Default project
				if (AcideProjectConfiguration.getInstance()
						.isDefaultProject()) {

					// If it has a MAIN FILE
					if (MainWindow.getInstance().getFileEditorManager()
							.getMainEditor() != null) {

						command = command.replace("$mainFile$", MainWindow
								.getInstance().getFileEditorManager()
								.getMainEditor().getAbsolutePath());
						command = command.replace("$mainFilePath$", MainWindow
								.getInstance().getFileEditorManager()
								.getMainEditor().getFilePath());
						command = command.replace("$mainFileExt$", MainWindow
								.getInstance().getFileEditorManager()
								.getMainEditor().getFileExtension());
						command = command.replace("$mainFileName$", MainWindow
								.getInstance().getFileEditorManager()
								.getMainEditor().getFileName());
					}
				} else {

					// Not default project

					// Searches for the MAIN FILE
					int mainFileEditorPanelIndex = -1;
					for (int index = 0; index < AcideProjectConfiguration.getInstance().getNumFilesFromList(); index++) {
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(index).isMainFile())
							mainFileEditorPanelIndex = index;
					}

					// If exists
					if (mainFileEditorPanelIndex != -1) {
						command = command.replace("$mainFile$", AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileEditorPanelIndex).getAbsolutePath());
						command = command.replace("$mainFilePath$", AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileEditorPanelIndex)
								.getRelativePath());
						command = command.replace("$mainFileExt$", AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileEditorPanelIndex)
								.getFileExtension());
						command = command.replace("$mainFileName$", AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileEditorPanelIndex)
								.getFileName());
					}
				}

				// Executes the command
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
			_selectionSize = 0;
			_command = command;

			// Send the command to the output shell
			_processThread.getWriter().write(command + " " + parameter + "\n");	
			_processThread.getWriter().flush();

			// Updates the command record
			updateCommandRecord(command + " " + parameter);

		} catch (IOException exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Updates the ACIDE - A Configurable IDE console command record with the
	 * new executed command.
	 * 
	 * @param command
	 *            executed command.
	 */
	public void updateCommandRecord(String command) {

		// Sets the care at the end of the command
		_textPane.setCaretPosition(_promptCaretPosition);

		// The blank command does not count
		if (!command.matches("")) {

			// Updates the command record
			if (_commandRecord.contains(command)) {
				_commandRecordMaximumIndex--;
				_commandRecord.remove(command);
			}
			_commandRecord.add(command);
			_commandRecordMaximumIndex++;
			_commandRecordCurrentIndex = _commandRecordMaximumIndex;
		}
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
	 * Returns the console text pane content.
	 * 
	 * @return the console text pane content.
	 */
	public String getTextPaneContent() {
		return _textPane.getText();
	}

	/**
	 * Executes the ACIDE - A Configurable IDE console panel process thread.
	 */
	public void execute() {
		_processThread = new ConsoleThread();
		_processThread.start();
	}

	/**
	 * Resets the ACIDE - A Configurable IDE console panel.
	 */
	public void resetConsole() {
		_textPane.setText("");
		execute();
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
	 * Returns the selection size.
	 * 
	 * @return the selection size.
	 */
	public int getSelectionSize() {
		return _selectionSize;
	}

	/**
	 * Sets a new value to the selection size.
	 * 
	 * @param selectionSize
	 *            new value to set.
	 */
	public void setSelectionSize(int selectionSize) {
		_selectionSize = selectionSize;
	}

	/**
	 * Returns the default styled document.
	 * 
	 * @return the default styled document.
	 */
	public DefaultStyledDocument getDefaultStyledDocument() {
		return _defaultStyledDocument;
	}

	/**
	 * Sets a new value to the default styled document.
	 * 
	 * @param defaultStyledDocument
	 *            new value to set.
	 */
	public void setDefaultStyledDocument(
			DefaultStyledDocument defaultStyledDocument) {
		_defaultStyledDocument = defaultStyledDocument;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console process thread.
	 * 
	 * @return the ACIDE - A Configurable IDE console process thread.
	 */
	public ConsoleThread getProcessThread() {
		return _processThread;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel command record.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel command record.
	 */
	public ArrayList<String> getCommandRecord() {
		return _commandRecord;
	}

	/**
	 * Returns the command record current index.
	 * 
	 * @return the command record current index.
	 */
	public int getCommandRecordCurrentIndex() {
		return _commandRecordCurrentIndex;
	}

	/**
	 * Sets a new value to the command record current index.
	 * 
	 * @param commandRecordCurrentIndex
	 *            new value to set.
	 */
	public void setCommandRecordCurrentIndex(int commandRecordCurrentIndex) {
		_commandRecordCurrentIndex = commandRecordCurrentIndex;
	}

	/**
	 * Returns the command record maximum index.
	 * 
	 * @return the command record maximum index.
	 */
	public int getCommandRecordMaximumIndex() {
		return _commandRecordMaximumIndex;
	}

	/**
	 * Sets the command record maximum index.
	 * 
	 * @param commandRecordMaximumIndex
	 *            new value to set.
	 */
	public void setCommandRecordMaximumIndex(int commandRecordMaximumIndex) {
		_commandRecordMaximumIndex = commandRecordMaximumIndex;
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
	 * Clears the console buffer simulating the "cls" command in cmd in Windows
	 * OS.
	 */
	public void clearConsoleBuffer() {
		// TODO: Pending implementation
	}
}
