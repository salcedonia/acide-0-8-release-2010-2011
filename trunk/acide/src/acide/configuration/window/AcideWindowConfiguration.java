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
package acide.configuration.window;

import acide.gui.mainWindow.AcideMainWindow;

import java.io.FileInputStream;
import java.io.FileOutputStream;

import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * ACIDE - A Configurable IDE window configuration.
 * 
 * @version 0.8
 */
public class AcideWindowConfiguration {

	/**
	 * ACIDE - A Configurable IDE window panel configuration class instance.
	 */
	private static AcideWindowConfiguration _instance;
	/**
	 * Flag that indicates if the explorer panel is showed or not.
	 */
	private boolean _isExplorerPanelShowed;
	/**
	 * Flag that indicates if the console panel is showed or not.
	 */
	private boolean _isConsolePanelShowed;
	/**
	 * Window width.
	 */
	private int _width;
	/**
	 * Window height.
	 */
	private int _height;
	/**
	 * X window coordinate.
	 */
	private int _xCoordinate;
	/**
	 * Y window coordinate.
	 */
	private int _yCoordinate;
	/**
	 * Vertical split pane divider location.
	 */
	private int _verticalSplitPaneDividerLocation;
	/**
	 * Horizontal split pan divider location.
	 */
	private int _horizontalSplitPaneDividerLocation;

	/**
	 * Returns the unique ACIDE - A Configurable IDE console panel configuration
	 * class instance.
	 * 
	 * @return the unique ACIDE - A Configurable IDE console panel configuration
	 *         class instance.
	 * @see AcideWindowConfiguration
	 */
	public static AcideWindowConfiguration getInstance() {

		if (_instance == null)
			_instance = new AcideWindowConfiguration();
		return _instance;
	}

	/**
	 * Load the ACIDE - A Configurable IDE window panel configuration from an
	 * XML file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void load(String configurationFilePath) {

		try {

			// Creates the XSTREAM object
			XStream xStream = new XStream(new DomDriver());

			// Creates the input file
			FileInputStream file = new FileInputStream(configurationFilePath);

			// Gets the window configuration from the XML file
			AcideWindowConfiguration windowConfiguration = (AcideWindowConfiguration) xStream
					.fromXML(file);

			// IS EXPLORER PANEL SHOWED
			Boolean isExplorerPanelShowed = windowConfiguration
					.isExplorerPanelShowed();

			// IS CONSOLE PANEL SHOWED
			Boolean isConsolePanelShowed = windowConfiguration
					.isConsolePanelShowed();

			// WIDTH
			Integer windowWidth = windowConfiguration.getWindowWidth();

			// HEIGHT
			Integer windowHeight = windowConfiguration.getWindowHeight();

			// WINDOW X COORDINATE
			Integer xCoordinate = windowConfiguration.getXCoordinate();

			// WINDOW Y COORDINATE
			Integer yCoordinate = windowConfiguration.getYCoordinate();

			// VERTICAL SPLIT PANE DIVIDER LOCATION
			Integer verticalSplitPaneDividerLocation = windowConfiguration
					.getVerticalSplitPaneDividerLocation();

			// HORIZONTAL SPLIT PANE DIVIDER LOCATION
			Integer horizontalSplitPaneDividerLocation = windowConfiguration
					.getHorizontalSplitPanelDividerLocation();

			// Close the file
			file.close();

			// IS EXPLORER PANEL SHOWED
			_isExplorerPanelShowed = isExplorerPanelShowed;

			// IS CONSOLE PANEL SHOWED
			_isConsolePanelShowed = isConsolePanelShowed;

			// WIDTH
			_width = windowWidth;

			// HEIGHT
			_height = windowHeight;

			// WINDOW X COORDINATE
			_xCoordinate = xCoordinate;

			// WINDOW Y COORDINATE
			_yCoordinate = yCoordinate;

			// VERTICAL SPLIT PANE DIVIDER LOCATION
			_verticalSplitPaneDividerLocation = verticalSplitPaneDividerLocation;

			// HORIZONTAL SPLIT PANE DIVIDER LOCATION
			_horizontalSplitPaneDividerLocation = horizontalSplitPaneDividerLocation;

			// Updates the ACIDE - A Configurable IDE window configuration
			AcideResourceManager.getInstance().setProperty(
					"windowConfiguration", configurationFilePath);

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Saves the ACIDE - A Configurable IDE window panel configuration in a XML
	 * file and returns true if the operation was succeed or false in other
	 * case.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean save() {

		// IS EXPLORER PANEL SHOWED
		_isExplorerPanelShowed = AcideMainWindow.getInstance().getMenu()
				.getViewMenu().getShowExplorerPanelCheckBoxMenuItem()
				.isSelected();

		// IS CONSOLE PANEL SHOWED
		_isConsolePanelShowed = AcideMainWindow.getInstance().getMenu()
				.getViewMenu().getShowConsolePanelCheckBoxMenuItem()
				.isSelected();

		// WINDOW WIDTH
		_width = AcideMainWindow.getInstance().getWidth();

		// WINDOW HEIGHT
		_height = AcideMainWindow.getInstance().getHeight();

		// WINDOW X COORDINATE
		_xCoordinate = AcideMainWindow.getInstance().getX();

		// WINDOW Y COORDINATE
		_yCoordinate = AcideMainWindow.getInstance().getY();

		// VERTICAL SPLIT PANE DIVIDER LOCATION
		_verticalSplitPaneDividerLocation = AcideMainWindow.getInstance()
				.getVerticalSplitPane().getDividerLocation();

		// HORIZONTAL SPLIT PANE DIVIDER LOCATION
		_horizontalSplitPaneDividerLocation = AcideMainWindow.getInstance()
				.getHorizontalSplitPane().getDividerLocation();

		// Creates the XStream object
		XStream xStream = new XStream();

		try {

			// Creates the XML file
			FileOutputStream file = new FileOutputStream(
					"./configuration/window/configuration.xml");

			// Parse the AcideWindowConfiguration class to XML
			xStream.toXML(this, file);

			// Closes the file
			file.close();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
			return false;
		}

		// Updates the ACIDE - A Configurable IDE window configuration
		AcideResourceManager.getInstance().setProperty("windowConfiguration",
				"./configuration/window/configuration.xml");

		return true;
	}

	/**
	 * Returns the window height.
	 * 
	 * @return the window height.
	 */
	public int getWindowHeight() {
		return _height;
	}

	/**
	 * Sets a new value for the window height.
	 * 
	 * @param height
	 *            new value to set.
	 */
	public void setWindowHeight(int height) {
		_height = height;
	}

	/**
	 * Returns the window x coordinate.
	 * 
	 * @return the window x coordinate.
	 */
	public int getXCoordinate() {
		return _xCoordinate;
	}

	/**
	 * Sets a new value to the window x coordinate.
	 * 
	 * @param xCoordinate
	 *            new value to set.
	 */
	public void setXCoordinate(int xCoordinate) {
		_xCoordinate = xCoordinate;
	}

	/**
	 * Returns the window y coordinate.
	 * 
	 * @return the window y coordinate.
	 */
	public int getYCoordinate() {
		return _yCoordinate;
	}

	/**
	 * Sets a new value to the y coordinate of the window.
	 * 
	 * @param yCoordinate
	 *            new value to set.
	 */
	public void setYCoordinate(int yCoordinate) {
		_yCoordinate = yCoordinate;
	}

	/**
	 * Returns the is console panel showed flag.
	 * 
	 * @return the is console panel showed flag.
	 */
	public boolean isConsolePanelShowed() {
		return _isConsolePanelShowed;
	}

	/**
	 * Sets a new value to the is console panel showed flag.
	 * 
	 * @param isConsolePanelShowed
	 *            new value to set.
	 */
	public void setIsConsolePanelShowed(boolean isConsolePanelShowed) {
		_isConsolePanelShowed = isConsolePanelShowed;
	}

	/**
	 * Returns the explorer panel showed flag.
	 * 
	 * @return the explorer panel showed flag.
	 */
	public boolean isExplorerPanelShowed() {
		return _isExplorerPanelShowed;
	}

	/**
	 * Sets a new value to the is explorer panel showed flag.
	 * 
	 * @param isExplorerPanelShowed
	 *            new value to set.
	 */
	public void setIsExplorerPanelShowed(boolean isExplorerPanelShowed) {
		_isExplorerPanelShowed = isExplorerPanelShowed;
	}

	/**
	 * Returns the window width.
	 * 
	 * @return the window width.
	 */
	public int getWindowWidth() {
		return _width;
	}

	/**
	 * Sets a new value for the window width.
	 * 
	 * @param width
	 *            new value to set.
	 */
	public void setWindowWidth(int width) {
		_width = width;
	}

	/**
	 * Returns the horizontal split pane divider location.
	 * 
	 * @return the horizontal split pane divider location.
	 */
	public int getHorizontalSplitPanelDividerLocation() {
		return _horizontalSplitPaneDividerLocation;
	}

	/**
	 * Sets a new value for the horizontal split pane divider location.
	 * 
	 * @param horizontalSplitPaneDividerLocation
	 *            new value to set.
	 */
	public void setHorizontalSplitPaneDividerLocation(
			int horizontalSplitPaneDividerLocation) {
		_horizontalSplitPaneDividerLocation = horizontalSplitPaneDividerLocation;
	}

	/**
	 * Returns the vertical split pane divider location.
	 * 
	 * @return the vertical split pane divider location.
	 */
	public int getVerticalSplitPaneDividerLocation() {
		return _verticalSplitPaneDividerLocation;
	}

	/**
	 * Sets a new value for the splitPaneVerticalDividerLocation.
	 * 
	 * @param verticalSplitPaneDividerLocation
	 *            new value to set.
	 */
	public void setVerticalSplitPaneDividerLocation(
			int verticalSplitPaneDividerLocation) {
		_verticalSplitPaneDividerLocation = verticalSplitPaneDividerLocation;
	}
}
