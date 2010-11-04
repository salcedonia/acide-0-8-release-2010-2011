package es.configuration.output;

import java.awt.Color;
import java.awt.Font;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ResourceBundle;

import language.Language;

import operations.log.Log;
import properties.PropertiesManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import gui.MainWindow;
import gui.splashScreen.SplashScreen;

/**
 * Handles the Output configuration in the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class OutputConfiguration {

	/**
	 * Instance of the class.
	 */
	private static OutputConfiguration _instance;
	/**
	 * Shell path.
	 */
	private String _shellPath;
	/**
	 * Shell directory.
	 */
	private String _shellDirectory;
	/**
	 * Echo command to display in the output.
	 */
	private boolean _echoCommand;
	/**
	 * Exit command for the output.
	 */
	private String _exitCommand;
	/**
	 * Font name.
	 */
	private String _fontName;
	/**
	 * Font style.
	 */
	private String _fontStyle;
	/**
	 * Font size.
	 */
	private int _fontSize;
	/**
	 * Foreground color.
	 */
	private Color _foregroundColor;
	/**
	 * Background color.
	 */
	private Color _backgroundColor;

	/**
	 * Returns the output configuration.
	 * 
	 * @return The output configuration.
	 */
	public static OutputConfiguration getInstance() {

		if (_instance == null)
			_instance = new OutputConfiguration();
		return _instance;
	}

	/**
	 * Set a new value to the echo command.
	 * 
	 * @param echoCommand
	 *            New value to set.
	 */
	public void setEchoCommand(boolean echoCommand) {
		_echoCommand = echoCommand;
	}

	/**
	 * Set a new value to exit command.
	 * 
	 * @param exitCommand
	 *            New value to set.
	 */
	public void setExitCommand(String exitCommand) {
		_exitCommand = exitCommand;
	}

	/**
	 * Set a new value to the shell directory.
	 * 
	 * @param shellDirectory
	 *            New value to set.
	 */
	public void setShellDirectory(String shellDirectory) {
		_shellDirectory = shellDirectory;
	}

	/**
	 * Returns the echo command.
	 * 
	 * @return The echo command.
	 */
	public boolean getEchoCommand() {
		return _echoCommand;
	}

	/**
	 * Returns the exit command.
	 * 
	 * @return The exit command.
	 */
	public String getExitCommand() {
		return _exitCommand;
	}

	/**
	 * Returns the shell directory.
	 * 
	 * @return The shell directory.
	 */
	public String getShellDirectory() {
		return _shellDirectory;
	}

	/**
	 * Set a new value to the shell path.
	 * 
	 * @param shellPath
	 *            New value to set.
	 */
	public void setShellPath(String shellPath) {
		_shellPath = shellPath;
	}

	/**
	 * Returns the shell path.
	 * 
	 * @return The shell path.
	 */
	public String getShellPath() {
		return _shellPath;
	}

	/**
	 * Set a new value to the font name.
	 * 
	 * @param fontName
	 *            New value to set.
	 */
	public void setFontName(String fontName) {
		_fontName = fontName;
	}

	/**
	 * Returns the font name.
	 * 
	 * @return The font name.
	 */
	public String getFontName() {
		return _fontName;
	}

	/**
	 * Set a new value to the font size.
	 * 
	 * @param fontSize
	 *            New value to set.
	 */
	public void setFontSize(int fontSize) {
		_fontSize = fontSize;
	}

	/**
	 * Returns the font size.
	 * 
	 * @return The font size.
	 */
	public int getFontSize() {
		return _fontSize;
	}

	/**
	 * Returns the font style.
	 * 
	 * @return The font style.
	 */
	public String getFontStyle() {
		return _fontStyle;
	}

	/**
	 * Set a new value to the font style.
	 * 
	 * @param fontStyle
	 *            New value to set.
	 */
	public void setFontStyle(String fontStyle) {
		_fontStyle = fontStyle;
	}

	/**
	 * Set a new value to the foreground color.
	 * 
	 * @param foregroundColor
	 *            New value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_foregroundColor = foregroundColor;
	}

	/**
	 * Set a new value to the background color.
	 * 
	 * @param backgroundColor
	 *            New value to set.
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_backgroundColor = backgroundColor;
	}

	/**
	 * Returns the foreground color of the output.
	 * 
	 * @return The foreground color of the output.
	 */
	public Color getForegroundColor() {
		return _foregroundColor;
	}

	/**
	 * Returns the background color of the output.
	 * 
	 * @return The background color of the output.
	 */
	public Color getBackgroundColor() {
		return _backgroundColor;
	}

	/**
	 * Load the output configuration from an XML file.
	 * 
	 * @param path
	 *            File path.
	 */
	public void load(String path) {

		// IF THE NAME IS ALREADY SET BY THE USER
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {
			try {

				XStream x = new XStream(new DomDriver());
				FileInputStream f = new FileInputStream(path);

				OutputConfiguration outputConfiguration = (OutputConfiguration) x
						.fromXML(f);

				String shellPath = outputConfiguration._shellPath;
				String shellDirectory = outputConfiguration._shellDirectory;
				Boolean echoCommand = outputConfiguration._echoCommand;
				String exitCommand = outputConfiguration._exitCommand;
				String fontName = outputConfiguration._fontName;
				String fontStyleString = outputConfiguration._fontStyle;
				Integer fontSize = outputConfiguration._fontSize;
				Color foregroundColor = outputConfiguration._foregroundColor;
				Color backgroundColor = outputConfiguration._backgroundColor;

				f.close();

				_shellPath = shellPath;
				_shellDirectory = shellDirectory;
				_echoCommand = echoCommand;
				_exitCommand = exitCommand;
				_fontName = fontName;
				_fontStyle = fontStyleString;
				_fontSize = fontSize;
				_foregroundColor = foregroundColor;
				_backgroundColor = backgroundColor;
				
				int fontStyle = 0;
				
				if(_fontStyle.matches("Font.PLAIN"))
					fontStyle = Font.PLAIN;
				if(_fontStyle.matches("Font.BOLD"))
					fontStyle = Font.BOLD;
				if(_fontStyle.matches("Font.ITALIC"))
					fontStyle = Font.ITALIC;
				if(_fontStyle.matches("Font.BOLD+Font.ITALIC"))
					fontStyle = Font.BOLD+Font.ITALIC;
				
				// UPDATES THE OUTPUT
				MainWindow.getInstance().getOutput().getTextComponent().setBackground(_backgroundColor);
				MainWindow.getInstance().getOutput().getTextComponent().setForeground(_foregroundColor);
				MainWindow.getInstance().getOutput().getTextComponent().setFont(new Font(_fontName, fontStyle, _fontSize));
				
				PropertiesManager.setProperty("outputConfiguration", path);

			} catch (Exception e) {
				
				// GET THE LANGUAGE TO DISPLAY
				Language language = Language.getInstance();
				try {
					language.getLanguage(PropertiesManager.getProperty("language"));
				} catch (Exception ex) {
					ex.printStackTrace();
				}
				SplashScreen.setProgressBar(5);

				// GET THE LABELS
				ResourceBundle labels = language.getLabels();
				SplashScreen.setProgressBar(7);
				
				// UPDATE THE LOG
				Log.getLog().info(labels.getString("s990"));
				e.printStackTrace();
				
				// LOAD THE DEFAULT CONFIGURATION
				_shellPath = null;
				_shellDirectory = null;
				_echoCommand = false;
				_exitCommand = null;
				_fontName = "Monospaced";
				_fontStyle = "Font.PLAIN";
				_fontSize = 12;
				_foregroundColor = Color.BLACK;
				_backgroundColor = Color.WHITE;
				
				int fontStyle = 0;
				
				if(_fontStyle.matches("Font.PLAIN"))
					fontStyle = Font.PLAIN;
				if(_fontStyle.matches("Font.BOLD"))
					fontStyle = Font.BOLD;
				if(_fontStyle.matches("Font.ITALIC"))
					fontStyle = Font.ITALIC;
				if(_fontStyle.matches("Font.BOLD+Font.ITALIC"))
					fontStyle = Font.BOLD+Font.ITALIC;
				
				// UPDATES THE OUTPUT
				MainWindow.getInstance().getOutput().getTextComponent().setBackground(_backgroundColor);
				MainWindow.getInstance().getOutput().getTextComponent().setForeground(_foregroundColor);
				MainWindow.getInstance().getOutput().getTextComponent().setFont(new Font(_fontName, fontStyle, _fontSize));
				
				PropertiesManager.setProperty("outputConfiguration", path);
			}
		}
	}

	/**
	 * Save the lexical configuration for a programming language in a XML file
	 * and returns true if the operation was succeed or false in other case.
	 * 
	 * @param name
	 *            Name of the lexical configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            Indicates if the programming language is compiled or
	 *            interpreted.
	 * 
	 * @return True if the operation was succeed or false in other case.
	 */
	public boolean save() {
		
		// NOTE: THE REST OF THE OPTIONS HAVE BEEN SAVED PREVIOUSLY 
		// SO THERE IS NO NEED TO DO IT AGAIN
		
		// FONT NAME
		_fontName = MainWindow.getInstance().getOutput().getTextComponent().getFont().getFontName();
		
		// PARSE THE FONT STYLE TO STRING
		int fontStyle = MainWindow.getInstance().getOutput().getTextComponent().getFont().getStyle();
		
		switch(fontStyle){
		
		case Font.PLAIN: 		
			_fontStyle = "Font.PLAIN";
			break;
		case Font.BOLD: 
			_fontStyle = "Font.BOLD";
			break;
		case Font.ITALIC: 
			_fontStyle = "Font.ITALIC";
			break;
		case Font.BOLD+Font.ITALIC: 
			_fontStyle = "Font.BOLD+Font.ITALIC";
			break;
		}
		
		// FONT SIZE
		_fontSize = MainWindow.getInstance().getOutput().getTextComponent().getFont().getSize();
		
		// FOREGROUND COLOR
		_foregroundColor = MainWindow.getInstance().getOutput().getTextComponent().getForeground();
		
		// BACKGROUND COLOR
		_backgroundColor = MainWindow.getInstance().getOutput().getTextComponent().getBackground();

		XStream xStream = new XStream();
		try {
			FileOutputStream file = new FileOutputStream("./configuration/output/configuration.xml");
			xStream.toXML(this, file);
			file.close();
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		PropertiesManager.setProperty("outputConfiguration", "./configuration/output/configuration.xml");

		return true;
	}
}
