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
package acide.configuration.fileEditor;

import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.Color;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file editor configuration.
 * 
 * @version 0.8
 */
public class AcideFileEditorConfiguration {

	/**
	 * ACIDE - A Configurable IDE file editor configuration unique class
	 * instance.
	 */
	private static AcideFileEditorConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE file editor configuration file list which
	 * contains the list of opened files in the configuration file.
	 */
	private AcideFileEditorPanelConfigurationList _fileEditorPanelConfigurationList;
	/**
	 * ACIDE - A Configurable IDE file editor configuration selected file editor
	 * panel selected index.
	 */
	private int _selectedFileEditorPanelIndex;
	/**
	 * ACIDE - A Configurable IDE file editor configuration font name.
	 */
	private String _fontName;
	/**
	 * ACIDE - A Configurable IDE file editor configuration font style.
	 */
	private int _fontStyle;
	/**
	 * ACIDE - A Configurable IDE file editor configuration font size.
	 */
	private int _fontSize;
	/**
	 * ACIDE - A Configurable IDE file editor configuration foreground color.
	 */
	private Color _foregroundColor;
	/**
	 * ACIDE - A Configurable IDE file editor configuration background color.
	 */
	private Color _backgroundColor;
	/**
	 * ACIDE - A Configurable IDE file editor configuration edition mode.
	 */
	private boolean _editionMode;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor configuration.
	 */
	public AcideFileEditorConfiguration() {

		// Creates the file editor panel configuration list
		_fileEditorPanelConfigurationList = new AcideFileEditorPanelConfigurationList();

		// The selected file editor panel index is -1 by default
		_selectedFileEditorPanelIndex = -1;

		// The edition mode is insert by default
		_editionMode = true;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration unique
	 * class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration unique
	 *         class instance.
	 */
	public static AcideFileEditorConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideFileEditorConfiguration();
		return _instance;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration file
	 * list.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration file
	 *         list.
	 */
	public AcideFileEditorPanelConfigurationList getFileList() {
		return _fileEditorPanelConfigurationList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration file list.
	 * 
	 * @param fileList
	 *            new value to set.
	 */
	public void setFileList(AcideFileEditorPanelConfigurationList fileList) {
		_fileEditorPanelConfigurationList = fileList;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager configuration
	 * selected file editor panel index.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager configuration
	 *         selected file editor panel index.
	 */
	public int getSelectedFileEditorPanelIndex() {
		return _selectedFileEditorPanelIndex;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor manager
	 * configuration selected file editor panel index.
	 * 
	 * @param selectedFileEditorPanelIndex
	 *            new value to set.
	 */
	public void setSelectedFileEditorPanelIndex(int selectedFileEditorPanelIndex) {
		_selectedFileEditorPanelIndex = selectedFileEditorPanelIndex;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration font name.
	 * 
	 * @param fontName
	 *            new value to set.
	 */
	public void setFontName(String fontName) {
		_fontName = fontName;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration font
	 * name.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration font
	 *         name.
	 */
	public String getFontName() {
		return _fontName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration font size.
	 * 
	 * @param fontSize
	 *            new value to set.
	 */
	public void setFontSize(int fontSize) {
		_fontSize = fontSize;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration font
	 * size.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration font
	 *         size.
	 */
	public int getFontSize() {
		return _fontSize;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration font
	 * style.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration font
	 *         style.
	 */
	public int getFontStyle() {
		return _fontStyle;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration font style.
	 * 
	 * @param fontStyle
	 *            new value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_fontStyle = fontStyle;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration foreground color.
	 * 
	 * @param foregroundColor
	 *            new value to set.
	 */
	public void setForegroundColor(Color foregroundColor) {
		_foregroundColor = foregroundColor;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration background color.
	 * 
	 * @param backgroundColor
	 *            new value to set.
	 */
	public void setBackgroundColor(Color backgroundColor) {
		_backgroundColor = backgroundColor;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration
	 * foreground color.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration
	 *         foreground color.
	 */
	public Color getForegroundColor() {
		return _foregroundColor;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration
	 * background color.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration
	 *         background color.
	 */
	public Color getBackgroundColor() {
		return _backgroundColor;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration edition
	 * mode.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration edition
	 *         mode.
	 */
	public boolean getEditionMode() {
		return _editionMode;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration edition mode.
	 * 
	 * @param editionMode
	 *            new value to set.
	 */
	public void setEditionMode(boolean editionMode) {
		_editionMode = editionMode;
	}

	/**
	 * Returns the number of files from the list.
	 * 
	 * @return the number of files from the list.
	 */
	public int getNumberOfFilesFromList() {
		return _fileEditorPanelConfigurationList.getSize();
	}

	/**
	 * Returns the file editor panel configuration at the position in the list
	 * given as a parameter.
	 * 
	 * @param index
	 *            position to get.
	 * 
	 * @return the file editor panel configuration at the position in the list
	 *         given as a parameter.
	 */
	public AcideFileEditorPanelConfiguration getFileAt(int index) {
		return _fileEditorPanelConfigurationList
				.getFileEditorPanelConfigurationAt(index);
	}

	/**
	 * Load the ACIDE - A Configurable IDE file editor manager configuration
	 * from an XML file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void load(String configurationFilePath) {

		// If the name is already set by the user
		if ((configurationFilePath != null)
				&& (!configurationFilePath.trim().equalsIgnoreCase(""))) {
			try {

				// Creates the XStream object to handle XML files
				XStream xStream = new XStream(new DomDriver());

				// Creates the file input stream
				FileInputStream fileInputStream = new FileInputStream(
						configurationFilePath);

				// Gets the file editor manager configuration
				AcideFileEditorConfiguration fileEditorManagerConfiguration = (AcideFileEditorConfiguration) xStream
						.fromXML(fileInputStream);

				// Gets the file editor panel configuration list
				AcideFileEditorPanelConfigurationList fileEditorPanelConfigurationList = fileEditorManagerConfiguration
						.getFileList();

				// Gets the selected file editor panel index
				int selectedFileEditorPanelIndex = fileEditorManagerConfiguration
						.getSelectedFileEditorPanelIndex();

				// Gets the font name
				String fontName = fileEditorManagerConfiguration.getFontName();

				// Gets the font style
				Integer fontStyle = fileEditorManagerConfiguration
						.getFontStyle();

				// Gets the font size
				Integer fontSize = fileEditorManagerConfiguration.getFontSize();

				// Gets the foreground color
				Color foregroundColor = fileEditorManagerConfiguration
						.getForegroundColor();

				// Gets the background color
				Color backgroundColor = fileEditorManagerConfiguration
						.getBackgroundColor();

				// Gets the edition mode
				Boolean editionMode = fileEditorManagerConfiguration
						.getEditionMode();

				// Closes the file input stream
				fileInputStream.close();

				// Sets the file editor panel configuration list
				_fileEditorPanelConfigurationList = fileEditorPanelConfigurationList;

				// Sets the selected file editor panel index
				_selectedFileEditorPanelIndex = selectedFileEditorPanelIndex;

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

				// Sets the edition mode
				_editionMode = editionMode;

				// Updates the ACIDE - A Configurable IDE file editor
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"fileEditorConfiguration", configurationFilePath);

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s990"));
				exception.printStackTrace();

				// Updates the ACIDE - A Configurable IDE file editor
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"fileEditorConfiguration", configurationFilePath);
			}
		}
	}

	/**
	 * Saves the ACIDE - A Configurable IDE file editor manager configuration in
	 * a XML file and returns true if the operation succeed or false in
	 * other case.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean save() {

		// Creates the file editor panel configuration list
		_fileEditorPanelConfigurationList = new AcideFileEditorPanelConfigurationList();

		// If there are opened file editors
		if (AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels() > 0) {

			// Analyzes all the opened file editor panels
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// Gets the selected file editor panel
				AcideFileEditorPanel fileEditorPanel = AcideMainWindow
						.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index);

				// Creates the file editor panel configuration for the current
				// file editor panel
				AcideFileEditorPanelConfiguration fileEditorPanelConfiguration = new AcideFileEditorPanelConfiguration();

				// Sets the path
				fileEditorPanelConfiguration.setPath(fileEditorPanel
						.getAbsolutePath());

				// Gets the caret position
				int caretPosition = fileEditorPanel
				.getActiveTextEditionArea().getCaretPosition();
				
				// If the caret is no valid
				if(caretPosition < 0 || caretPosition > fileEditorPanel
						.getActiveTextEditionArea().getText().length())
					
					// Puts it in the first position by default
					caretPosition = 0;
				
				// Sets the caret position
				fileEditorPanelConfiguration.setCaretPosition(caretPosition);

				// Sets the type
				String type = "Normal";
				if (fileEditorPanel.isCompilableFile())
					type = "Compilable";
				if (fileEditorPanel.isMainFile())
					type = "Main";
				fileEditorPanelConfiguration.setType(type);

				// Sets the split pane divider location
				fileEditorPanelConfiguration
						.setSplitPaneDividerLocation(fileEditorPanel
								.getHorizontalSplitPane().getDividerLocation());

				// Sets the active text edition area
				fileEditorPanelConfiguration
						.setActiveTextEditionArea(fileEditorPanel
								.getActiveTextEditionAreaIndex());

				// Sets the lexicon configuration
				fileEditorPanelConfiguration
						.setLexiconConfiguration(fileEditorPanel
								.getLexiconConfiguration().getPath());

				// Sets the previous grammar configuration
				fileEditorPanelConfiguration
						.setPreviousGrammarConfiguration(fileEditorPanel
								.getPreviousGrammarConfiguration().getPath());

				// Sets the current grammar configuration
				fileEditorPanelConfiguration
						.setCurrentGrammarConfiguration(fileEditorPanel
								.getCurrentGrammarConfiguration().getPath());

				// Inserts the file editor panel configuration into the list
				_fileEditorPanelConfigurationList
						.insertFileEditorPanelConfiguration(fileEditorPanelConfiguration);
			}

			// Sets the font name
			_fontName = AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getActiveTextEditionArea()
					.getFont().getFontName();

			// Sets the font style
			_fontStyle = AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getActiveTextEditionArea()
					.getFont().getStyle();

			// Sets the font size
			_fontSize = AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getActiveTextEditionArea()
					.getFont().getSize();

			// Sets the foreground color
			_foregroundColor = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getForeground();

			// Sets the background color
			_backgroundColor = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getBackground();

			// Sets the edition mode
			_editionMode = AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getEditionMode();
		}
		
		// Sets the selected file editor panel index
		_selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
		.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Creates the xStream object to handle XML files
		XStream xStream = new XStream();

		try {

			// Saves the file
			FileOutputStream fileOutputStream = new FileOutputStream(
					"./configuration/fileEditor/configuration.xml");

			// Saves the content into XML format
			xStream.toXML(this, fileOutputStream);

			// Closes the file output stream
			fileOutputStream.close();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
			return false;
		}

		// Updates the ACIDE - A Configurable IDE file editor configuration
		AcideResourceManager.getInstance().setProperty(
				"fileEditorConfiguration",
				"./configuration/fileEditor/configuration.xml");

		return true;
	}
}
