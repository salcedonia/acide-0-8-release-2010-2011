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
package es.configuration.console;

import java.awt.Color;
import java.awt.Font;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import language.AcideLanguageManager;

import operations.log.AcideLog;
import resources.AcideResourceManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import gui.mainWindow.MainWindow;

/**
 * ACIDE - A Configurable IDE console panel configuration.
 * 
 * @version 0.8
 */
public class AcideConsoleConfiguration {

	/**
	 * ACIDE - A Configurable IDE console panel configuration class instance.
	 */
	private static AcideConsoleConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE console panel configuration shell path.
	 */
	private String _shellPath;
	/**
	 * ACIDE - A Configurable IDE console panel configuration shell directory.
	 */
	private String _shellDirectory;
	/**
	 * Indicates if the command fired has to be displayed in the console.
	 */
	private boolean _isEchoCommand;
	/**
	 * ACIDE - A Configurable IDE console panel configuration console exit
	 * command.
	 */
	private String _exitCommand;
	/**
	 * ACIDE - A Configurable IDE console panel configuration font name.
	 */
	private String _fontName;
	/**
	 * ACIDE - A Configurable IDE console panel configuration font style.
	 */
	private int _fontStyle;
	/**
	 * ACIDE - A Configurable IDE console panel configuration font size.
	 */
	private int _fontSize;
	/**
	 * ACIDE - A Configurable IDE console panel configuration foreground color.
	 */
	private Color _foregroundColor;
	/**
	 * ACIDE - A Configurable IDE console panel configuration background color.
	 */
	private Color _backgroundColor;

	/**
	 * Returns the unique ACIDE - A Configurable IDE console panel configuration
	 * class instance.
	 * 
	 * @return the unique ACIDE - A Configurable IDE console panel configuration
	 *         class instance.
	 * @see AcideConsoleConfiguration
	 */
	public static AcideConsoleConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideConsoleConfiguration();
		return _instance;
	}

	/**
	 * Sets a new value to the is echo command flag.
	 * 
	 * @param isEchoCommand
	 *            new value to set.
	 */
	public void setEchoCommand(boolean isEchoCommand) {
		_isEchoCommand = isEchoCommand;
	}

	/**
	 * Sets a new value to console exit command.
	 * 
	 * @param exitCommand
	 *            new value to set.
	 */
	public void setExitCommand(String exitCommand) {
		_exitCommand = exitCommand;
	}

	/**
	 * Sets a new value to the shell directory.
	 * 
	 * @param shellDirectory
	 *            new value to set.
	 */
	public void setShellDirectory(String shellDirectory) {
		_shellDirectory = shellDirectory;
	}

	/**
	 * Returns the is echo command flag value.
	 * 
	 * @return the is echo command flag value.
	 */
	public boolean getIsEchoCommand() {
		return _isEchoCommand;
	}

	/**
	 * Returns the console exit command.
	 * 
	 * @return the console exit command.
	 */
	public String getExitCommand() {
		return _exitCommand;
	}

	/**
	 * Returns the shell directory.
	 * 
	 * @return the shell directory.
	 */
	public String getShellDirectory() {
		return _shellDirectory;
	}

	/**
	 * Sets a new value to the shell path.
	 * 
	 * @param shellPath
	 *            new value to set.
	 */
	public void setShellPath(String shellPath) {
		_shellPath = shellPath;
	}

	/**
	 * Returns the shell path.
	 * 
	 * @return the shell path.
	 */
	public String getShellPath() {
		return _shellPath;
	}

	/**
	 * Sets a new value to the font name.
	 * 
	 * @param fontName
	 *            new value to set.
	 */
	public void setFontName(String fontName) {
		_fontName = fontName;
	}

	/**
	 * Returns the font name.
	 * 
	 * @return the font name.
	 */
	public String getFontName() {
		return _fontName;
	}

	/**
	 * Sets a new value to the font size.
	 * 
	 * @param fontSize
	 *            new value to set.
	 */
	public void setFontSize(int fontSize) {
		_fontSize = fontSize;
	}

	/**
	 * Returns the font size.
	 * 
	 * @return the font size.
	 */
	public int getFontSize() {
		return _fontSize;
	}

	/**
	 * Returns the font style.
	 * 
	 * @return the font style.
	 */
	public int getFontStyle() {
		return _fontStyle;
	}

	/**
	 * Sets a new value to the font style.
	 * 
	 * @param fontStyle
	 *            new value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_fontStyle = fontStyle;
	}

	/**
	 * Sets a new value to the foreground color.
	 * 
	 * @param foregroundColor
	 *            new value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_foregroundColor = foregroundColor;
	}

	/**
	 * Sets a new value to the background color.
	 * 
	 * @param backgroundColor
	 *            new value to set.
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_backgroundColor = backgroundColor;
	}

	/**
	 * Returns the foreground color of the output.
	 * 
	 * @return the foreground color of the output.
	 */
	public Color getForegroundColor() {
		return _foregroundColor;
	}

	/**
	 * Returns the background color of the output.
	 * 
	 * @return the background color of the output.
	 */
	public Color getBackgroundColor() {
		return _backgroundColor;
	}

	/**
	 * Load the ACIDE - A Configurable IDE console panel configuration from an
	 * XML file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void load(String configurationFilePath) {

		// If the name is already set by the user
		if ((configurationFilePath != null)
				&& (!configurationFilePath.trim().equalsIgnoreCase(""))) {
			try {

				// Creates the XStream to handle XML files
				XStream xStream = new XStream(new DomDriver());

				// Creates the file input stream to read the file
				FileInputStream fileInputStream = new FileInputStream(
						configurationFilePath);

				// Gets the console configuration from the XML
				AcideConsoleConfiguration consoleConfiguration = (AcideConsoleConfiguration) xStream
						.fromXML(fileInputStream);

				// SHELL PATH
				String shellPath = consoleConfiguration._shellPath;

				// SHELL DIRECTORY
				String shellDirectory = consoleConfiguration._shellDirectory;

				// ECHO COMMAND
				Boolean echoCommand = consoleConfiguration._isEchoCommand;

				// EXIT COMMAND
				String exitCommand = consoleConfiguration._exitCommand;

				// FONT NAME
				String fontName = consoleConfiguration._fontName;

				// FONT STYLE
				Integer fontStyle = consoleConfiguration._fontStyle;

				// FONT SIZE
				Integer fontSize = consoleConfiguration._fontSize;

				// FOREGROUND COLOR
				Color foregroundColor = consoleConfiguration._foregroundColor;

				// BLACKGROUND COLOR
				Color backgroundColor = consoleConfiguration._backgroundColor;

				// Closes the file input stream
				fileInputStream.close();

				// SHELL PATH
				_shellPath = shellPath;

				// SHELL DIRECTORY
				_shellDirectory = shellDirectory;

				// IS ECHO COMMAND
				_isEchoCommand = echoCommand;

				// EXIT COMMAND
				_exitCommand = exitCommand;

				// FONT NAME
				_fontName = fontName;

				// FONT STYLE
				_fontStyle = fontStyle;

				// FONT SIZE
				_fontSize = fontSize;

				// FOREGROUND COLOR
				_foregroundColor = foregroundColor;

				// BACKGROUND COLOR
				_backgroundColor = backgroundColor;

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"consoleConfiguration", configurationFilePath);

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s990"));
				exception.printStackTrace();

				// Loads the default configuration
				_shellPath = null;
				_shellDirectory = null;
				_isEchoCommand = false;
				_exitCommand = null;
				_fontName = "Monospaced";
				_fontStyle = Font.PLAIN;
				_fontSize = 12;
				_foregroundColor = Color.BLACK;
				_backgroundColor = Color.WHITE;

				// Sets the console background
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.setBackground(_backgroundColor);

				// Sets the console foreground
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.setForeground(_foregroundColor);

				// Sets the console font style
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.setFont(new Font(_fontName, _fontStyle, _fontSize));

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"consoleConfiguration", configurationFilePath);
			}
		}
	}

	/**
	 * Saves the ACIDE - A Configurable IDE console panel configuration in a XML
	 * file and returns true if the operation was succeed or false in other
	 * case.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean save() {

		// NOTE: THE REST OF THE OPTIONS HAVE BEEN SAVED PREVIOUSLY
		// SO THERE IS NO NEED TO DO IT AGAIN

		// FONT NAME
		_fontName = MainWindow.getInstance().getConsolePanel().getTextPane()
				.getFont().getFontName();

		// FONT STYLE
		_fontStyle = MainWindow.getInstance().getConsolePanel().getTextPane()
				.getFont().getStyle();

		// FONT SIZE
		_fontSize = MainWindow.getInstance().getConsolePanel().getTextPane()
				.getFont().getSize();

		// FOREGROUND COLOR
		_foregroundColor = MainWindow.getInstance().getConsolePanel()
				.getTextPane().getForeground();

		// BACKGROUND COLOR
		_backgroundColor = MainWindow.getInstance().getConsolePanel()
				.getTextPane().getBackground();

		// Creates the XStream to handle XML files
		XStream xStream = new XStream();

		try {
			
			// Creates the file output stream to write on the file
			FileOutputStream fileOutputStream = new FileOutputStream(
					"./configuration/console/configuration.xml");
			
			// Parses to XML format
			xStream.toXML(this, fileOutputStream);
			
			// Closes the file output stream
			fileOutputStream.close();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
			return false;
		}

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty("consoleConfiguration",
				"./configuration/console/configuration.xml");

		return true;
	}
}
