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
	 * ACIDE - A Configurable IDE splash screen window unique class instance.
	 */
	private static AcideSplashScreenWindow _instance;
	/**
	 * ACIDE - A Configurable IDE splash screen window image file.
	 */
	private static final String IMAGE = "./resources/images/splashScreen.png";
	/**
	 * ACIDE - A Configurable IDE splash screen window main panel.
	 */
	private static JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE splash screen window message panel.
	 */
	private static JPanel _messagePanel;
	/**
	 * ACIDE - A Configurable IDE splash screen window label to show the image.
	 */
	private static JLabel _image;
	/**
	 * ACIDE - A Configurable IDE splash screen window progress bar.
	 */
	private static JProgressBar _progressBar;
	/**
	 * ACIDE - A Configurable IDE splash screen window message label.
	 */
	private static JLabel _messageLabel;
	
	/**
	 * Returns the ACIDE - A Configurable IDE splash screen window unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE splash screen window unique class instance.
	 */
	public static AcideSplashScreenWindow getInstance(){
		if(_instance == null)
			_instance = new AcideSplashScreenWindow();
		return _instance;
	}
	
	/**
	 * Creates a new ACIDE - A Configurable IDE splash screen window unique class instance.
	 */
	public AcideSplashScreenWindow(){
		
		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());
		_mainPanel.setBorder(BorderFactory.createEmptyBorder());
		
		// MESSAGE PANEL
		_messagePanel = new JPanel(new GridBagLayout());
		_messagePanel.setBackground(new Color(70, 110, 175));
		
		// MESSAGE LABEL
		_messageLabel = new JLabel(" ");
		_messageLabel.setFont(new Font("Arial", Font.BOLD, 12));
		_messageLabel.setForeground(Color.WHITE);
		
		// CONTENT PANEL
		JPanel content = (JPanel) getContentPane();
		content.setLayout(new BorderLayout());
			
		// IMAGE
		_image = new JLabel(new ImageIcon(IMAGE));
		
		// PROGRESS BAR
		_progressBar = new JProgressBar();
		_progressBar.setStringPainted(true);
		_progressBar.setForeground(new Color(200, 0, 0));
		_progressBar.setBackground(Color.WHITE);
		_progressBar.setFont(new Font("Arial", Font.PLAIN, 11));
		_progressBar.setBorder(BorderFactory.createMatteBorder(1, 0, 1, 0, Color.BLACK));
		
		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.anchor = GridBagConstraints.NORTH;
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_image, constraints);	
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.insets = new Insets(230, 0, 0, 0);
		_mainPanel.add(_progressBar, constraints);
		content.add(_mainPanel, BorderLayout.CENTER);
		
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.WEST;
		_messagePanel.add(_messageLabel, constraints);
		_messagePanel.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.BLACK));
		content.add(_messagePanel, BorderLayout.SOUTH);
		content.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
		
		// Centers the window
		int width = getPreferredSize().width;
		int height = getPreferredSize().height;
		Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
		int x = (screen.width - width) / 2;
		int y = (screen.height - height) / 2;
		setBounds(x, y, width, height);
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

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE splash screen window
	 * progress bar and the message label given as parameters.
	 * 
	 * @param percent
	 *            new value for the progress bar.
	 * @param message
	 *            new message to display.           
	 */
	public void setProgressBar(final int percent, final String message) {
		
		SwingUtilities.invokeLater(new Runnable() {
			
			/*
			 * (non-Javadoc)
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				_progressBar.setValue(percent);
				_messageLabel.setText(message);
			}
		});	
	}
}
