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
package acide.configuration.workbench.fileEditor;

import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.Color;
import java.awt.Font;

/**
 * ACIDE - A Configurable IDE file editor configuration.
 * 
 * @version 0.8
 */
public class AcideFileEditorConfiguration {

	/**
	 * ACIDE - A Configurable IDE file editor configuration file list which
	 * contains the list of opened files in the configuration file.
	 */
	private AcideFileEditorPanelConfigurationList _fileEditorPanelConfigurationList;
	/**
	 * ACIDE - A Configurable IDE file editor configuration selected file editor
	 * panel selected index.
	 */
	private String _selectedFileEditorPanelName;
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
	 * ACIDE - A Configurable IDE file editor configuration automatic indent.
	 */
	private boolean _automaticIndent;
	/**
	 * ACIDE - A Configurable IDE file editor configuration maximum lines to
	 * send to the console.
	 */
	private int _maximumLinesToConsole;

	/**
	 * ACIDE - A Configurable IDE file editor configuration line wrapping
	 * active.
	 */
	private boolean _lineWrapping;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor configuration.
	 */
	public AcideFileEditorConfiguration() {

		// Creates the file editor panel configuration list
		_fileEditorPanelConfigurationList = new AcideFileEditorPanelConfigurationList();

		// The selected file editor panel index is "" by default
		_selectedFileEditorPanelName = "";

		// The font name is monospaced by default
		_fontName = "Monospaced";

		// The font style by default is plain
		_fontStyle = Font.PLAIN;

		// The font size by default is 12
		_fontSize = 12;

		// The background by default is black
		_foregroundColor = Color.BLACK;

		// The background by default is white
		_backgroundColor = Color.WHITE;

		// The edition mode is insert by default
		_editionMode = false;

		// The automatic indent is activated
		_automaticIndent = true;

		// The line wrapping flag is true
		_lineWrapping = true;

		// Sets the maximum lines to send to the console
		_maximumLinesToConsole = 30;
	}

	/**
	 * Saves the ACIDE - A Configurable IDE file editor configuration .
	 */
	public void save() {

		// Creates the file editor panel configuration list
		_fileEditorPanelConfigurationList = new AcideFileEditorPanelConfigurationList();

		// If there are opened file editors
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

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
				int caretPosition = fileEditorPanel.getActiveTextEditionArea()
						.getCaretPosition();

				// If the caret is no valid
				if (caretPosition < 0
						|| caretPosition > fileEditorPanel
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

			// Sets the selected file editor panel name
			_selectedFileEditorPanelName = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getFileNameWithExtension();
		} else {

			// Sets the selected file editor panel name
			_selectedFileEditorPanelName = "";
		}
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
	 * selected file editor panel name.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager configuration
	 *         selected file editor panel name.
	 */
	public String getSelectedFileEditorPanelName() {
		return _selectedFileEditorPanelName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor manager
	 * configuration selected file editor panel name.
	 * 
	 * @param selectedFileEditorPanelName
	 *            new value to set.
	 */
	public void setSelectedFileEditorPanelName(
			String selectedFileEditorPanelName) {
		_selectedFileEditorPanelName = selectedFileEditorPanelName;
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
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration automatic indent.
	 * 
	 * @param automaticIndent
	 *            new value to set.
	 */
	public void setAutomaticIndent(boolean automaticIndent) {
		_automaticIndent = automaticIndent;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration
	 * automatic indent.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration
	 *         automatic indent.
	 */
	public boolean getAutomaticIndent() {
		return _automaticIndent;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration maximum
	 * lines to send to the console.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration maximum
	 *         lines to send to the console.
	 */
	public int getMaximumLinesToConsole() {
		return _maximumLinesToConsole;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor maximum
	 * lines to send to the console.
	 * 
	 * @param maximumLinesToConsole
	 *            new value to set.
	 */
	public void setMaximumLinesToConsole(int maximumLinesToConsole) {
		_maximumLinesToConsole = maximumLinesToConsole;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration line
	 * wrapping flag.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration line
	 *         wrapping flag.
	 */
	public boolean getLineWrapping() {
		return _lineWrapping;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration line wrapping flag.
	 * 
	 * @param lineWrappingActive
	 *            new value to set.
	 */
	public void setLineWrapping(boolean lineWrappingActive) {
		_lineWrapping = lineWrappingActive;
	}
}
