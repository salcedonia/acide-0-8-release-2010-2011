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
package gui.menuBar.helpMenu.gui;

import gui.listeners.AcideWindowListener;
import gui.mainWindow.MainWindow;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE about us window.											
 *					
 * @version 0.8	
 * @see JFrame																													
 */
public class AcideAboutUsWindow extends JFrame{

	/**
	 * ACIDE - A Configurable IDE about us window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE about us window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon("./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE about us window image file.
	 */
	private static final ImageIcon IMAGE = new ImageIcon("./resources/images/aboutUs.png");
	/**
	 * ACIDE - A Configurable IDE about us window panel which contains the info about the developers of the application.
	 */
	private JPanel _developersPanel;
	/**
	 * ACIDE - A Configurable IDE about us window info panel.
	 */
	private JPanel _infoPanel;
	/**
	 * ACIDE - A Configurable IDE about us window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE about us window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE about us window image label.
	 */
	private JLabel _image;
	/**
	 * ACIDE - A Configurable IDE about us window accept button.
	 */
	private JButton _acceptButton;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE about us window.
	 */
	public AcideAboutUsWindow() {

		super();
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// DISABLE THE MAIN WINDOW
		final MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(false);
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s619"));

		// FRAME
		setTitle(labels.getString("s622"));
		setIconImage(ICON.getImage());
		setLayout(new BorderLayout());

		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());

		// IMAGE
		_image = new JLabel(IMAGE);
		add(_image, BorderLayout.NORTH);

		// INFO PANEL
		_infoPanel = new JPanel(new GridBagLayout());
		_infoPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s630"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		
		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		
		// DEVELOPERS PANEL
		_developersPanel = new JPanel();
		_developersPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s625"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));		
		_developersPanel.setLayout(new GridBagLayout());

		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s177"));
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s621"));
				
				// CLOSE THE WINDOW
				dispose();
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
				mainWindow.setEnabled(true);
			}
		});

		// Listeners
		_developersPanel.addKeyListener(new AboutUsWindowKeyboardListener());
		_infoPanel.addKeyListener(new AboutUsWindowKeyboardListener());
		_acceptButton.addKeyListener(new AboutUsWindowKeyboardListener());
		addWindowListener(new AcideWindowListener());

		// ADD THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.weightx = 500;
		
		// INFO PANEL
		constraints.gridx = 0;
		constraints.gridy = 0;
		_infoPanel.add(new JLabel(labels.getString("s631")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_infoPanel.add(new JLabel(labels.getString("s632")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		_infoPanel.add(new JLabel(labels.getString("s633")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 3;
		_infoPanel.add(new JLabel(labels.getString("s634")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 4;
		_infoPanel.add(new JLabel(labels.getString("s635")), constraints);
		
		// DEVELOPERS PANEL
		constraints.gridx = 0;
		constraints.gridy = 0;
		_developersPanel.add(new JLabel(labels.getString("s626")), constraints);
		constraints.insets = new Insets(5, 25, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_developersPanel.add(new JLabel(labels.getString("s627")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		_developersPanel.add(new JLabel(labels.getString("s628")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 3;
		_developersPanel.add(new JLabel(labels.getString("s629")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 4;
		_developersPanel.add(new JLabel(labels.getString("s966")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 5;
		_developersPanel.add(new JLabel(labels.getString("s967")), constraints);

		// MAIN PANEL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_infoPanel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_developersPanel, constraints);
		add(_mainPanel, BorderLayout.CENTER);
		
		// BUTTON PANEL
		_buttonPanel.add(_acceptButton);
		add(_buttonPanel, BorderLayout.SOUTH);
		
		setResizable(false);
		pack();
		
		// Centers the dimension
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		setVisible(true);
	}

	/**																
	 * ACIDE - A Configurable IDE about us window keyboard listener.											
	 *					
	 * @version 0.8	
	 * @see KeyAdapter																													
	 */
	class AboutUsWindowKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {

			if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {
				
				MainWindow.getInstance().setEnabled(true);
				dispose();
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}