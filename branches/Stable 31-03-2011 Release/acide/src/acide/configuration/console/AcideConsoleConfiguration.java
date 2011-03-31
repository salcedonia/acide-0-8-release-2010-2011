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
package acide.configuration.console;

import java.awt.Color;
import java.awt.Font;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.resources.AcideResourceManager;

import acide.gui.mainWindow.AcideMainWindow;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

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
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration is echo command flag.
	 * 
	 * @param isEchoCommand
	 *            new value to set.
	 */
	public void setEchoCommand(boolean isEchoCommand) {
		_isEchoCommand = isEchoCommand;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration exit command.
	 * 
	 * @param exitCommand
	 *            new value to set.
	 */
	public void setExitCommand(String exitCommand) {
		_exitCommand = exitCommand;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration shell directory.
	 * 
	 * @param shellDirectory
	 *            new value to set.
	 */
	public void setShellDirectory(String shellDirectory) {
		_shellDirectory = shellDirectory;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration is
	 * echo command flag value.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration is
	 *         echo command flag value.
	 */
	public boolean getIsEchoCommand() {
		return _isEchoCommand;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration exit
	 * command.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration exit
	 *         command.
	 */
	public String getExitCommand() {
		return _exitCommand;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration shell
	 * directory.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration shell
	 *         directory.
	 */
	public String getShellDirectory() {
		return _shellDirectory;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration shell path.
	 * 
	 * @param shellPath
	 *            new value to set
	 */
	public void setShellPath(String shellPath) {
		_shellPath = shellPath;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration shell
	 * path.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration shell
	 *         path
	 */
	public String getShellPath() {
		return _shellPath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration font name.
	 * 
	 * @param fontName
	 *            new value to set.
	 */
	public void setFontName(String fontName) {
		_fontName = fontName;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration font
	 * name.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration font
	 *         name.
	 */
	public String getFontName() {
		return _fontName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration font size.
	 * 
	 * @param fontSize
	 *            new value to set.
	 */
	public void setFontSize(int fontSize) {
		_fontSize = fontSize;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration font
	 * size.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration font
	 *         size.
	 */
	public int getFontSize() {
		return _fontSize;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration font
	 * style.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration font
	 *         style.
	 */
	public int getFontStyle() {
		return _fontStyle;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration font style.
	 * 
	 * @param fontStyle
	 *            new value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_fontStyle = fontStyle;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration foreground color.
	 * 
	 * @param foregroundColor
	 *            new value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_foregroundColor = foregroundColor;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration background color.
	 * 
	 * @param backgroundColor
	 *            new value to set.
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_backgroundColor = backgroundColor;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration
	 * foreground color.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration
	 *         foreground color.
	 */
	public Color getForegroundColor() {
		return _foregroundColor;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration
	 * background color.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration
	 *         background color.
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

				// Gets the shell path
				String shellPath = consoleConfiguration.getShellPath();

				// Gets the shell directory
				String shellDirectory = consoleConfiguration
						.getShellDirectory();

				// Gets the echo command
				Boolean echoCommand = consoleConfiguration.getIsEchoCommand();

				// Gets the exit command
				String exitCommand = consoleConfiguration.getExitCommand();

				// Gets the font name
				String fontName = consoleConfiguration.getFontName();

				// Gets the font style
				Integer fontStyle = consoleConfiguration.getFontStyle();

				// Gets the font size
				Integer fontSize = consoleConfiguration.getFontSize();

				// Gets the foreground color
				Color foregroundColor = consoleConfiguration
						.getForegroundColor();

				// Gets the background color
				Color backgroundColor = consoleConfiguration
						.getBackgroundColor();

				// Closes the file input stream
				fileInputStream.close();

				// Sets the shell path
				_shellPath = shellPath;

				// Sets the shell directory
				_shellDirectory = shellDirectory;

				// Sets the is echo command
				_isEchoCommand = echoCommand;

				// Sets the exit command
				_exitCommand = exitCommand;

				// Sets the font name
				_fontName = fontName;

				// Sets the font style
				_fontStyle = fontStyle;

				// Sets the font size
				_fontSize = fontSize;

				// Sets the foreground color
				_foregroundColor = foregroundColor;

				// Sets the background color
				_backgroundColor = backgroundColor;

				// Updates the ACIDE - A Configurable IDE console configuration
				AcideResourceManager.getInstance().setProperty(
						"consoleConfiguration", configurationFilePath);

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s990"));
				exception.printStackTrace();

				// The shell path is null
				_shellPath = null;

				// The shell directory is null
				_shellDirectory = null;

				// The is echo command is false
				_isEchoCommand = false;

				// The exit command is null
				_exitCommand = null;

				// The font name is monospaced
				_fontName = "Monospaced";

				// The font style is plain
				_fontStyle = Font.PLAIN;

				// The font size is 12
				_fontSize = 12;

				// The foreground color is black
				_foregroundColor = Color.BLACK;

				// The background color is white
				_backgroundColor = Color.WHITE;

				// Sets the console background
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.setBackground(_backgroundColor);

				// Sets the console foreground
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.setForeground(_foregroundColor);

				// Sets the console font style
				AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.setFont(new Font(_fontName, _fontStyle, _fontSize));

				// Updates the ACIDE - A Configurable IDE console configuration
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

		// NOTE: The rest of parameters have been saved previously

		// Sets the font name
		_fontName = AcideMainWindow.getInstance().getConsolePanel()
				.getTextPane().getFont().getFontName();

		// Sets the font style
		_fontStyle = AcideMainWindow.getInstance().getConsolePanel()
				.getTextPane().getFont().getStyle();

		// Sets the font size
		_fontSize = AcideMainWindow.getInstance().getConsolePanel()
				.getTextPane().getFont().getSize();

		// Sets the foreground color
		_foregroundColor = AcideMainWindow.getInstance().getConsolePanel()
				.getTextPane().getForeground();

		// Sets the background color
		_backgroundColor = AcideMainWindow.getInstance().getConsolePanel()
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

		// Updates the ACIDE - A Configurable IDE console configuration
		AcideResourceManager.getInstance().setProperty("consoleConfiguration",
				"./configuration/console/configuration.xml");

		return true;
	}
}
