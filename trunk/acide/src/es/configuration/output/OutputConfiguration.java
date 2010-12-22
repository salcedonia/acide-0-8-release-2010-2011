package es.configuration.output;

import java.awt.Color;
import java.awt.Font;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ResourceBundle;

import language.AcideLanguage;

import operations.log.AcideLog;
import resources.ResourceManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import gui.mainWindow.MainWindow;


/************************************************************************																
 * Output panel configuration class of ACIDE - A Configurable IDE.											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8																														
 ***********************************************************************/
public class OutputConfiguration {

	/**
	 * Output configuration class instance.
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
	 * Returns the unique output configuration class instance.
	 * 
	 * @return the unique class instance.
	 * @see OutputConfiguration
	 */
	public static OutputConfiguration getInstance() {

		if (_instance == null)
			_instance = new OutputConfiguration();
		return _instance;
	}

	/**
	 * Sets a new value to the echo command.
	 * 
	 * @param echoCommand
	 *            new value to set.
	 */
	public void setEchoCommand(boolean echoCommand) {
		_echoCommand = echoCommand;
	}

	/**
	 * Sets a new value to exit command.
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
	 * Returns the echo command.
	 * 
	 * @return the echo command.
	 */
	public boolean getEchoCommand() {
		return _echoCommand;
	}

	/**
	 * Returns the exit command.
	 * 
	 * @return the exit command.
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
	public String getFontStyle() {
		return _fontStyle;
	}

	/**
	 * Sets a new value to the font style.
	 * 
	 * @param fontStyle
	 *            new value to set.
	 */
	public void setFontStyle(String fontStyle) {
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
	 * Load the output configuration from an XML file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void load(String configurationFilePath) {

		// If the name is already set by the user
		if ((configurationFilePath != null)
				&& (!configurationFilePath.trim().equalsIgnoreCase(""))) {
			try {

				XStream x = new XStream(new DomDriver());
				FileInputStream f = new FileInputStream(configurationFilePath);

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

				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty("outputConfiguration",
						configurationFilePath);

			} catch (Exception exception) {

				// Gets the language
				AcideLanguage language = AcideLanguage.getInstance();
				
				try {
					language.getLanguage(ResourceManager.getInstance().
							getProperty("language"));
				} catch (Exception exception2) {
					
					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				// Updates the log
				AcideLog.getLog().info(labels.getString("s990"));
				exception.printStackTrace();

				// Loads the default configuration
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

				if (_fontStyle.matches("Font.PLAIN"))
					fontStyle = Font.PLAIN;
				if (_fontStyle.matches("Font.BOLD"))
					fontStyle = Font.BOLD;
				if (_fontStyle.matches("Font.ITALIC"))
					fontStyle = Font.ITALIC;
				if (_fontStyle.matches("Font.BOLD+Font.ITALIC"))
					fontStyle = Font.BOLD + Font.ITALIC;

				// Updates the OUTPUT
				MainWindow.getInstance().getOutputPanel().getTextComponent()
						.setBackground(_backgroundColor);
				MainWindow.getInstance().getOutputPanel().getTextComponent()
						.setForeground(_foregroundColor);
				MainWindow.getInstance().getOutputPanel().getTextComponent()
						.setFont(new Font(_fontName, fontStyle, _fontSize));

				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty("outputConfiguration",
						configurationFilePath);
			}
		}
	}

	/**
	 * Saves the output panel configuration  in a XML file
	 * and returns true if the operation was succeed or false in other case.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean save() {

		// NOTE: THE REST OF THE OPTIONS HAVE BEEN SAVED PREVIOUSLY
		// SO THERE IS NO NEED TO DO IT AGAIN

		// FONT NAME
		_fontName = MainWindow.getInstance().getOutputPanel().getTextComponent()
				.getFont().getFontName();

		// Parses the font style to String
		int fontStyle = MainWindow.getInstance().getOutputPanel().getTextComponent()
				.getFont().getStyle();

		switch (fontStyle) {

		case Font.PLAIN:
			_fontStyle = "Font.PLAIN";
			break;
		case Font.BOLD:
			_fontStyle = "Font.BOLD";
			break;
		case Font.ITALIC:
			_fontStyle = "Font.ITALIC";
			break;
		case Font.BOLD + Font.ITALIC:
			_fontStyle = "Font.BOLD+Font.ITALIC";
			break;
		}

		// FONT SIZE
		_fontSize = MainWindow.getInstance().getOutputPanel().getTextComponent()
				.getFont().getSize();

		// FOREGROUND COLOR
		_foregroundColor = MainWindow.getInstance().getOutputPanel()
				.getTextComponent().getForeground();

		// BACKGROUND COLOR
		_backgroundColor = MainWindow.getInstance().getOutputPanel()
				.getTextComponent().getBackground();

		XStream xStream = new XStream();
		
		try {
			FileOutputStream file = new FileOutputStream(
					"./configuration/output/configuration.xml");
			xStream.toXML(this, file);
			file.close();
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
			return false;
		}
		
		// Updates the RESOURCE MANAGER
		ResourceManager.getInstance().setProperty("outputConfiguration",
				"./configuration/output/configuration.xml");

		return true;
	}
}
