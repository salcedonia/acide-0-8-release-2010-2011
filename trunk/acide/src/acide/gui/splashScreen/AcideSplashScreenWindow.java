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
package acide.gui.splashScreen;

import java.awt.*;

import javax.swing.*;

/**
 * ACIDE - A Configurable IDE splash screen window.
 * 
 * @version 0.8
 * @see JWindow
 */
public class AcideSplashScreenWindow extends JWindow {

	/**
	 * ACIDE - A Configurable IDE splash screen window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE splash screen window images resource path.
	 */
	private static final ImageIcon IMAGE = new ImageIcon(
			"./resources/images/splashScreen.png");
	/**
	 * ACIDE - A Configurable IDE splash screen window unique class instance.
	 */
	private static AcideSplashScreenWindow _instance;
	/**
	 * ACIDE - A Configurable IDE splash screen window label to show the image.
	 */
	private static JLabel _image;
	/**
	 * ACIDE - A Configurable IDE splash screen window main panel.
	 */
	private static JPanel _mainPanel;

	/**
	 * Returns the ACIDE - A Configurable IDE splash screen window unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE splash screen window unique class
	 *         instance.
	 */
	public static AcideSplashScreenWindow getInstance() {
		if (_instance == null)
			_instance = new AcideSplashScreenWindow();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE splash screen window unique
	 * class instance.
	 */
	public AcideSplashScreenWindow() {

		// Builds the components
		buildComponents();

		// Adds the components
		addComponents();
		
		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE splash screen window configuration.
	 */
	private void setWindowConfiguration() {

		// Applies the layout
		pack();

		// Centers the window
		setLocationRelativeTo(null);
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE splash screen
	 * window.
	 */
	private void addComponents() {

		// Sets the layout
		_mainPanel.setLayout(new BorderLayout());

		// Adds the image to the panel
		_mainPanel.add(_image, BorderLayout.CENTER);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE splash screen window components.
	 */
	private void buildComponents() {

		// Gets the content pane
		_mainPanel = (JPanel) getContentPane();

		// Sets the window border
		_mainPanel.setBorder(BorderFactory.createLineBorder(new Color(30, 70,
				115), 2));

		// Creates the image label
		_image = new JLabel(IMAGE);
	}

	/**
	 * Shows the ACIDE - A Configurable IDE splash screen window.
	 */
	public void showSplashScreenWindow() {

		// Displays it
		setVisible(true);
	}

	/**
	 * Closes the ACIDE - A Configurable IDE splash screen window.
	 */
	public void closeSplashScreenWindow() {

		// Closes it
		dispose();
	}
}
