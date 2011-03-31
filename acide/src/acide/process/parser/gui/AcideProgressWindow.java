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
package acide.process.parser.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE progress window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideProgressWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE progress window serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE progress window unique class instance.
	 */
	private static AcideProgressWindow _instance;
	/**
	 * ACIDE - A Configurable IDE progress window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE progress window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE progress window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE progress window text pane.
	 */
	private JTextPane _textPane;
	/**
	 * ACIDE - A Configurable IDE progress window close button.
	 */
	private JButton _closeButton;

	/**
	 * Returns the ACIDE - A Configurable IDE progress window unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE progress window unique class
	 *         instance.
	 */
	public static AcideProgressWindow getInstance() {

		if (_instance == null)
			_instance = new AcideProgressWindow();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE progress window.
	 */
	public AcideProgressWindow() {

		super();

		// Builds the window components
		buildComponents();

		// Sets the window components listeners
		setListeners();
		
		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE progress window components.
	 */
	private void buildComponents() {
		
		// Creates the main panel
		_mainPanel = new JPanel(new BorderLayout());

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the text pane
		_textPane = new JTextPane();

		// Sets its foreground color
		_textPane.setForeground(Color.WHITE);

		// Sets its background color
		_textPane.setBackground(Color.BLACK);

		// Sets the intial text
		_textPane.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s1063"));
		
		// Sets the font
		_textPane.setFont(new Font("Monospaced", Font.PLAIN, 12));
		
		// It is not editable
		_textPane.setEditable(false);

		// Creates the scroll pane which contains the text pane
		JScrollPane scrollPane = new JScrollPane(_textPane);

		// Sets its preferred size
		scrollPane.setPreferredSize(new Dimension(700, 425));

		// Adds the scroll pane to the main panel
		_mainPanel.add(scrollPane, BorderLayout.CENTER);

		// Creates the close button
		_closeButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s1048"));

		// It is disabled at the beginning
		_closeButton.setEnabled(false);
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE progress window.
	 */
	private void addComponents() {
		
		// Sets the layout
		setLayout(new BorderLayout());
		
		// Adds the close button to the button panel
		_buttonPanel.add(_closeButton);

		// Adds the main panel to the window
		add(_mainPanel, BorderLayout.CENTER);
		
		// Adds the button panel to the window
		add(_buttonPanel, BorderLayout.SOUTH);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE progress window configuration.
	 */
	private void setWindowConfiguration() {
		
		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s1047"));

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// The window is not resizable
		setResizable(false);

		// Packs its components
		pack();

		// Centers the window
		setLocationRelativeTo(null);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE progress window components listeners.
	 */
	private void setListeners() {

		// Sets the close button action listener
		_closeButton.addActionListener(new CloseButtonAction());
	}

	/**
	 * Shows the ACIDE - A Configurable IDE progress window.
	 */
	public void showWindow() {
		
		// Sets the window visible
		setVisible(true);
	}

	/**
	 * Closes the ACIDE - A Configurable IDE progress window.
	 */
	public void closeWindow() {

		// Closes the configuration window
		dispose();
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE progress window text
	 * pane.
	 * 
	 * @param text
	 *            new value to set.
	 */
	public void setText(final String text) {

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				_textPane.setText(_textPane.getText() + "\n" + text);
			}
		});
	}

	/**
	 * Enables the ACIDE - A Configurable IDE progress window close button.
	 * 
	 * This method will be invoked at the time in which the process currently in
	 * execution has been completed.
	 */
	public void enableCloseButton() {

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				_closeButton.setEnabled(true);
			}
		});
	}

	/**
	 * ACIDE - A Configurable IDE progress window close button action listener.
	 * 
	 * @version 0.8
	 * @see JFrame
	 */
	class CloseButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			closeWindow();
		}
	}
}
