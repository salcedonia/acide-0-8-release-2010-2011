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

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.border.TitledBorder;

import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.fileMenu.utils.AcidePrinterManager;
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
	 * ACIDE - A Configurable IDE print configuration window unique class
	 * instance.
	 */
	private static AcidePrintConfigurationWindow _instance;
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

		super();

		// Builds the window components
		buildComponents();

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE print configuration window
	 * components.
	 */
	private void buildComponents() {

		// Creates the page number check box
		_pageNumberCheckBox = new JCheckBox();

		// Disables the page number check box
		_pageNumberCheckBox.setEnabled(false);

		// Creates the main panel
		_mainPanel = new JPanel(new GridBagLayout());

		// Sets the main panel border
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s965"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the page number label
		_pageNumberLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s962"));

		// Creates the date label
		_dateLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s963"));

		// Creates the date check box
		_dateCheckBox = new JCheckBox();

		// Disables the date cjack box
		_dateCheckBox.setEnabled(false);

		// Creates the print button
		_printButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s624"));

		// Sets the print button horizontal alignment as center
		_printButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the print button tool tip text
		_printButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s624"));

		// Disables the print button
		_printButton.setEnabled(false);

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s919"));

		// Sets the cancel button horizontal alignment as center
		_cancelButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the cancel button tool tip text
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s919"));

		// Creates the configure page button
		_configurePageButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s961"));

		// Sets the configure page button horizontal alignment as center
		_configurePageButton.setHorizontalAlignment(JButton.CENTER);

		// Sets the configure page button tool tip text
		_configurePageButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s961"));

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Adds the print button to the button panel
		_buttonPanel.add(_printButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE print configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s964"));

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// The window is not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Displays the window
		setVisible(true);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE print configuration
	 * window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(10, 10, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the configure page button to the main panel
		_mainPanel.add(_configurePageButton, constraints);

		constraints.gridy = 1;

		// Adds the page number label to the main panel
		_mainPanel.add(_pageNumberLabel, constraints);

		constraints.gridx = 1;

		// Adds the page number check box to the main panel
		_mainPanel.add(_pageNumberCheckBox, constraints);

		constraints.gridy = 2;
		constraints.gridx = 0;

		// Adds the date label to the main panel
		_mainPanel.add(_dateLabel, constraints);

		constraints.gridx = 1;

		// Adds the date check box to the main panel
		_mainPanel.add(_dateCheckBox, constraints);

		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the main panel to the window
		add(_mainPanel, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Sets the listeners of the ACIDE - A Configurable IDE print configuration
	 * window components.
	 */
	private void setListeners() {

		// Sets the page number check box action listener
		_pageNumberCheckBox.addActionListener(new PageNumberCheckBoxAction());

		// Sets the date check box action listener
		_dateCheckBox.addActionListener(new DateCheckBoxAction());

		// Sets the configure page button action listener
		_configurePageButton.addActionListener(new ConfigurePageButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// Sets the print button action listener
		_printButton.addActionListener(new PrintButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
		
		// Puts the escape key in the input map of the window
		getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, false), "EscapeKey");

		// Puts the escape key in the action map of the window
		getRootPane().getActionMap().put("EscapeKey", new EscapeKeyAction());
	}

	/**
	 * Closes the ACIDE - A Configurable IDE print configuration window.
	 */
	public void closeWindow() {
		
		// Enables the main window
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But only this time
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
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
			AcidePrinterManager.getInstance().setShowPage(
					_pageNumberCheckBox.isSelected());
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
			AcidePrinterManager.getInstance().setDate(
					_dateCheckBox.isSelected());
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
			AcidePrinterManager.getInstance().configurePage();

			if (AcidePrinterManager.getInstance().getPageFormat() != null) {

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
			AcidePrinterManager.getInstance().print();

			// Closes the window
			closeWindow();
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

			// Closes the window
			closeWindow();
		}
	}
	
	/**
	 * ACIDE - A Configurable IDE print configuration window escape key action.
	 * 
	 * @version 0.8
	 * @see AbstractAction
	 */
	class EscapeKeyAction extends AbstractAction {

		/**
		 * Escape key action serial version UID.
		 */
		private static final long serialVersionUID = 1L;

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			closeWindow();
		}
	}
}
