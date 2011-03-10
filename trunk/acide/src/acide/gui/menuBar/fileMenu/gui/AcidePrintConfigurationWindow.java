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
package acide.gui.menuBar.fileMenu.gui;

import acide.gui.listeners.AcideWindowListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.fileMenu.utils.AcidePrinterManager;

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
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE print configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcidePrintConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE print configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE print configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE print configuration window print button.
	 */
	private JButton _printButton;
	/**
	 * ACIDE - A Configurable IDE print configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE print configuration window configure page
	 * button.
	 */
	private JButton _configurePageButton;
	/**
	 * ACIDE - A Configurable IDE print configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE print configuration window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE print configuration window page number label.
	 */
	private JLabel _pageNumberLabel;
	/**
	 * ACIDE - A Configurable IDE print configuration window page number check
	 * box.
	 */
	private JCheckBox _pageNumberCheckBox;
	/**
	 * ACIDE - A Configurable IDE print configuration window date label.
	 */
	private JLabel _dateLabel;
	/**
	 * ACIDE - A Configurable IDE print configuration window date check box.
	 */
	private JCheckBox _dateCheckBox;
	/**
	 * ACIDE - A Configurable IDE print configuration window printer manager.
	 */
	private AcidePrinterManager _printerManager;

	/**
	 * ACIDE - A Configurable IDE print configuration window unique class
	 * instance.
	 */
	private static AcidePrintConfigurationWindow _instance;

	/**
	 * Returns the ACIDE - A Configurable IDE print configuration window unique
	 * class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE print configuration window unique
	 *         class instance.
	 */
	public static AcidePrintConfigurationWindow getInstance() {
		if (_instance == null)
			_instance = new AcidePrintConfigurationWindow();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE print configuration window.
	 */
	public AcidePrintConfigurationWindow() {

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Sets the layout
		setLayout(new GridBagLayout());

		// PRINTER MANAGER
		_printerManager = new AcidePrinterManager(AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getActiveTextEditionArea(), false, false);

		// PAGE NUMBER CHECK BOX
		_pageNumberCheckBox = new JCheckBox();
		_pageNumberCheckBox.setEnabled(false);

		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s965"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// PAGE NUMBER
		_pageNumberLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s962"));

		// DATE LABEL
		_dateLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s963"));

		// DATE CHECK BOX
		_dateCheckBox = new JCheckBox();
		_dateCheckBox.setEnabled(false);

		// PRINT BUTTON
		_printButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s624"));
		_printButton.setHorizontalAlignment(JButton.CENTER);
		_printButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s624"));
		_printButton.setEnabled(false);

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s919"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s919"));

		// CONFIGURE PAGE BUTTON
		_configurePageButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s961"));
		_configurePageButton.setHorizontalAlignment(JButton.CENTER);
		_configurePageButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s961"));

		// Sets the listeners of the window components.
		setListeners();

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(10, 10, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;

		// GENERAL PANEL
		_mainPanel.add(_configurePageButton, constraints);
		constraints.gridy = 1;
		_mainPanel.add(_pageNumberLabel, constraints);
		constraints.gridx = 1;
		_mainPanel.add(_pageNumberCheckBox, constraints);
		constraints.gridy = 2;
		constraints.gridx = 0;
		_mainPanel.add(_dateLabel, constraints);
		constraints.gridx = 1;
		_mainPanel.add(_dateCheckBox, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_mainPanel, constraints);

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		_buttonPanel.add(_printButton);
		_buttonPanel.add(_cancelButton);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s964"));
		setIconImage(ICON.getImage());
		setLocationRelativeTo(null);
		pack();
		setVisible(true);
		setResizable(false);

		// Get the size of the screen
		Dimension dimension = Toolkit.getDefaultToolkit().getScreenSize();

		// Determine the new location of the window
		int w = getSize().width;
		int h = getSize().height;
		int x = (dimension.width - w) / 2;
		int y = (dimension.height - h) / 2;

		// Move the window
		setLocation(x, y);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// PAGE NUMBER CHECK BOX
		_pageNumberCheckBox.addActionListener(new PageNumberCheckBoxAction());

		// DATE CHECK BOX
		_dateCheckBox.addActionListener(new DateCheckBoxAction());

		// CONFIGURE PAGE BUTTON
		_configurePageButton.addActionListener(new ConfigurePageButtonAction());
		_configurePageButton
				.addKeyListener(new PrintConfigurationWindowKeyboardListener());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());
		_cancelButton
				.addKeyListener(new PrintConfigurationWindowKeyboardListener());

		// PRINT BUTTON
		_printButton.addActionListener(new PrintButtonAction());
		_printButton
				.addKeyListener(new PrintConfigurationWindowKeyboardListener());

		// WINDOW
		addWindowListener(new AcideWindowListener());
	}

	/**
	 * Return the ACIDE - A Configurable IDE print configuration window
	 * configure page button.
	 * 
	 * @return the ACIDE - A Configurable IDE print configuration window
	 *         configure page button.
	 */
	public JButton getConfigurePageButton() {
		return _configurePageButton;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE print configuration
	 * window configure page button.
	 * 
	 * @param configurePageButton
	 *            new value to set.
	 */
	public void setConfigurePageButton(JButton configurePageButton) {
		_configurePageButton = configurePageButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE print configuration window print
	 * button.
	 * 
	 * @return the ACIDE - A Configurable IDE print configuration window print
	 *         button.
	 */
	public JButton getPrintButton() {
		return _printButton;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE print configuration
	 * window print button.
	 * 
	 * @param printButton
	 *            new value to set.
	 */
	public void setPrintButton(JButton printButton) {
		_printButton = printButton;
	}

	/**
	 * ACIDE - A Configurable IDE print configuration window page number check
	 * box action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class PageNumberCheckBoxAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the printer manager
			_printerManager.setShowPage(_pageNumberCheckBox.isSelected());
		}
	}

	/**
	 * ACIDE - A Configurable IDE print configuration window date check box
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class DateCheckBoxAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the printer manager
			_printerManager.setDate(_dateCheckBox.isSelected());
		}
	}

	/**
	 * ACIDE - A Configurable IDE print configuration window configure page
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ConfigurePageButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Configures the page
			_printerManager.configurePage();

			if (_printerManager.getPageFormat() != null) {

				// Enables the print button
				_printButton.setEnabled(true);

				// Enables the date check box
				_dateCheckBox.setEnabled(true);

				// Enables the page number check box
				_pageNumberCheckBox.setEnabled(true);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE print configuration window print button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class PrintButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Prints
			_printerManager.print();

			// Closes the window
			dispose();

			// Enables the main window
			AcideMainWindow.getInstance().setEnabled(true);
		}
	}

	/**
	 * ACIDE - A Configurable IDE print configuration window cancel button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the main window
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE print configuration window keyboard listener.
	 * 
	 * @version 0.8
	 * @see KeyAdapter
	 */
	class PrintConfigurationWindowKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {

			if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {
				
				// Enables the main window
				AcideMainWindow.getInstance().setEnabled(true);
				
				// Closes the window
				dispose();
				
				// Brings the main window to the front
				AcideMainWindow.getInstance().setAlwaysOnTop(true);
				
				// But only this time
				AcideMainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}
