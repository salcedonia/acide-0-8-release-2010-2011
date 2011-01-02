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
package gui.menuBar.configurationMenu.lexiconMenu.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import language.AcideLanguageManager;
import operations.log.AcideLog;

/**
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideColorWindow extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * 
	 */
	private JPanel _mainPanel;
	/**
	 * 
	 */
	private final JColorChooser _colorChooser;
	/**
	 * 
	 */
	private JButton _acceptButton;
	
	/**
	 * 
	 */
	public AcideColorWindow(){
		
		super();
		
		// Gets the labels
		final ResourceBundle _labels = AcideLanguageManager.getInstance().getLabels();
		
		// Updates the log
		AcideLog.getLog().info(_labels.getString("s407"));

		// Sets the layout
		setLayout(new BorderLayout());

		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());

		// COLOR CHOOSER
		_colorChooser = new JColorChooser(Color.BLUE);
		_colorChooser.getSelectionModel().addChangeListener(
				new ChangeListener() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see
					 * javax.swing.event.ChangeListener#stateChanged(javax.swing
					 * .event.ChangeEvent)
					 */
					@Override
					public void stateChanged(ChangeEvent changeEvent) {
						
						// Gets the color from the color chooser
						//Color newColor = _colorChooser.getColor();
						
						// Updates the preview text field with the reserved word text field text
						//_previewTextField.setText(_reservedWordTextField
								//.getText());
						
						// Updates the preview text field foreground with the new color 
						//_previewTextField.setForeground(newColor);
					}
				});
		
		// Sets a border to the color chooser
		_colorChooser.setBorder(BorderFactory.createTitledBorder(null,
				_labels.getString("s409"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// ACCEPT BUTTON
		_acceptButton = new JButton(_labels.getString("s410"));
		_acceptButton.setToolTipText(_labels.getString("s411"));
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
				
				// Closes the window
				dispose();
			}
		});

		// ADD THE COMPONENTS TO THE FRAME WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 5;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_colorChooser, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 3;
		constraints.gridy = 1;
		constraints.insets = new Insets(10, 5, 5, 5);
		_mainPanel.add(_acceptButton, constraints);
		add(_mainPanel, BorderLayout.CENTER);
		
		// FRAME
		setTitle(_labels.getString("s408"));
		setIconImage(ICON.getImage());
		setResizable(false);
		setVisible(true);
		setLocation(250, 100);
		pack();

		// Updates the log
		AcideLog.getLog().info(_labels.getString("s412"));
	}
}
