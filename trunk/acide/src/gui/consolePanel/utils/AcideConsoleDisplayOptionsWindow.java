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
package gui.consolePanel.utils;

import es.configuration.console.AcideConsoleConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import gui.listeners.AcideWindowListener;
import gui.mainWindow.MainWindow;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.ResourceBundle;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import language.AcideLanguageManager;

import operations.log.AcideLog;

/**
 * ACIDE - A Configurable IDE console display options window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideConsoleDisplayOptionsWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE console display options window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console display options window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE console display options menu item image icon.
	 */
	private final static ImageIcon COLOR_PALETTE_IMAGE = new ImageIcon(
			"./resources/icons/buttons/colorPalette.png");
	/**
	 * Current font size of the console in the main window.
	 */
	private int INITIAL_SIZE = MainWindow.getInstance().getConsolePanel()
			.getTextPane().getFont().getSize();
	/**
	 * Current font style of the output in the main window.
	 */
	private int INITIAL_STYLE = MainWindow.getInstance().getConsolePanel()
			.getTextPane().getFont().getStyle();
	/**
	 * Current font name of the output in the main window.
	 */
	private String INITIAL_FONTNAME = MainWindow.getInstance()
			.getConsolePanel().getTextPane().getFont().getFamily();
	/**
	 * Current foreground color of the output in the main window.
	 */
	private Color INITIAL_FOREGROUND = MainWindow.getInstance()
			.getConsolePanel().getTextPane().getForeground();
	/**
	 * Current background color of the output in the main window.
	 */
	private Color INITIAL_BACKGROUND = MainWindow.getInstance()
			.getConsolePanel().getTextPane().getBackground();
	/**
	 * Where the sample text is displayed.
	 */
	private PreviewPanel _displayArea;
	/**
	 * ACIDE - A Configurable IDE console display options window preview panel
	 * which contains the display area.
	 */
	private JPanel _previewPanel;
	/**
	 * ACIDE - A Configurable IDE console display options window font size
	 * slider.
	 */
	private JSlider _sizeSlider;
	/**
	 * ACIDE - A Configurable IDE console display options window font combo box.
	 */
	private JComboBox _fontComboBox;
	/**
	 * ACIDE - A Configurable IDE console display options window controls panel.
	 */
	private JPanel _controlsPanel;
	/**
	 * ACIDE - A Configurable IDE console display options window color buttons
	 * panel.
	 */
	private JPanel _colorButtonsPanel;
	/**
	 * ACIDE - A Configurable IDE console display options window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE console display options window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE console display options window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE console display options window font label.
	 */
	private JLabel _fontLabel;
	/**
	 * ACIDE - A Configurable IDE console display options window background
	 * label.
	 */
	private JLabel _backgroundLabel;
	/**
	 * ACIDE - A Configurable IDE console display options window foreground
	 * label.
	 */
	private JLabel _foregroundLabel;
	/**
	 * ACIDE - A Configurable IDE console display options window size label.
	 */
	private JLabel _sizeLabel;
	/**
	 * ACIDE - A Configurable IDE console display options window font type
	 * label.
	 */
	private JLabel _fontStyleLabel;
	/**
	 * ACIDE - A Configurable IDE console display options window font style
	 * combo box.
	 */
	private JComboBox _fontStyleComboBox;
	/**
	 * ACIDE - A Configurable IDE console display options window foreground
	 * button.
	 */
	private JButton _foregroundButton;
	/**
	 * ACIDE - A Configurable IDE console display options window background
	 * button.
	 */
	private JButton _backgroundButton;

	/**
	 * Creates a new ACIDE - A Configurable IDE console display options window.
	 */
	public AcideConsoleDisplayOptionsWindow() {

		// Gets the labels
		final ResourceBundle labels = AcideLanguageManager.getInstance()
				.getLabels();

		// Disables the main window
		MainWindow.getInstance().setEnabled(false);

		// Sets the layout
		setLayout(new BorderLayout());

		// CONTROLS PANEL
		_controlsPanel = new JPanel(new GridBagLayout());
		_controlsPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s1010")));

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// COLOR BUTTON PANEL
		_colorButtonsPanel = new JPanel(new GridBagLayout());

		// FONT LABEL
		_fontLabel = new JLabel(labels.getString("s981"));

		// Creates the font combo box
		getFontComboBox();

		// FONT STYLE LABEL
		_fontStyleLabel = new JLabel(labels.getString("s983"), JLabel.CENTER);

		// Create the font style combo box
		getFontStyleComboBox(labels);

		// SIZE LABEL
		_sizeLabel = new JLabel(labels.getString("s982"));

		// Creates the size slide
		createSizeSlider();

		// FOREGROUND LABEL
		_foregroundLabel = new JLabel(labels.getString("s984"));

		// FOREGROUND BUTTON
		_foregroundButton = new JButton(COLOR_PALETTE_IMAGE);
		_foregroundButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Ask for the color to the user
				Color foregroundColor = JColorChooser.showDialog(null,
						labels.getString("s992"), INITIAL_FOREGROUND);

				// If the user has selected any
				if (foregroundColor != null)

					// Updates the display area foreground color
					_displayArea.setForegroundColor(foregroundColor);
			}
		});

		// BACKGROUND LABEL
		_backgroundLabel = new JLabel(labels.getString("s985"));

		// BACKGROUND BUTTON
		_backgroundButton = new JButton(COLOR_PALETTE_IMAGE);
		_backgroundButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Ask for the color to the user
				Color backgroundColor = JColorChooser.showDialog(null,
						labels.getString("s991"), INITIAL_BACKGROUND);

				// If the user has selected any
				if (backgroundColor != null)

					// Updates the display area background color
					_displayArea.setBackgroundColor(backgroundColor);
			}
		});

		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s445"));
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
				AcideLog.getLog().info("989");

				// Sets the new font style
				MainWindow
						.getInstance()
						.getConsolePanel()
						.getTextPane()
						.setFont(
								new Font(_displayArea.getFontName(),
										_displayArea.getFontStyle(),
										_displayArea.getFontSize()));

				// Sets the new background color
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.setBackground(_displayArea.getBackground());

				// Sets the new foreground color
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.setForeground(_displayArea.getForeground());

				// Sets the new caret color
				MainWindow.getInstance().getConsolePanel().getTextPane()
						.setCaretColor(_displayArea.getForeground());

				// Not default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())

					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

				// Saves the output configuration
				AcideConsoleConfiguration.getInstance().save();

				// Set the main window enabled again
				MainWindow.getInstance().setEnabled(true);

				// Closes the window
				dispose();
			}
		});

		ActionListener actionListener = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Enables the main window again
				MainWindow.getInstance().setEnabled(true);

				// Closes the window
				dispose();
			}
		};

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s446"));
		_cancelButton.addActionListener(new ActionListener() {
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
				AcideLog.getLog().info("988");

				// Enables the main window again
				MainWindow.getInstance().setEnabled(true);

				// Closes window
				dispose();
			}
		});
		// When the user press the escape button, executes the cancel option
		_cancelButton.registerKeyboardAction(actionListener, "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_controlsPanel.add(_fontLabel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_controlsPanel.add(_fontComboBox, constraints);
		constraints.gridx = 1;
		constraints.gridy = 0;
		_controlsPanel.add(_sizeLabel, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;
		_controlsPanel.add(_sizeSlider, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_controlsPanel.add(_fontStyleLabel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 3;
		_controlsPanel.add(_fontStyleComboBox, constraints);
		constraints.gridwidth = 1;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_colorButtonsPanel.add(_foregroundLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 0;
		_colorButtonsPanel.add(_foregroundButton, constraints);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_colorButtonsPanel.add(_backgroundLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 1;
		_colorButtonsPanel.add(_backgroundButton, constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 4;
		_controlsPanel.add(_colorButtonsPanel, constraints);

		// CONTROLS PANEL
		add(_controlsPanel, BorderLayout.NORTH);

		// PREVIEW PANEL
		_previewPanel = new JPanel();
		_previewPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s1011")));

		// Creates a panel where display the fonts
		_displayArea = new PreviewPanel(INITIAL_FONTNAME, INITIAL_STYLE,
				INITIAL_SIZE, INITIAL_FOREGROUND, INITIAL_BACKGROUND);

		_previewPanel.add(_displayArea);

		add(_previewPanel, BorderLayout.CENTER);
		add(_buttonPanel, BorderLayout.SOUTH);

		// FRAME
		addWindowListener(new AcideWindowListener());
		setTitle(labels.getString("s977"));
		setIconImage(ICON.getImage());
		setResizable(false);

		pack();

		setVisible(true);
		setLocationRelativeTo(null);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s978"));
	}

	/**
	 * Creates the font style combo box.
	 * 
	 * @param labels
	 *            labels to display.
	 */
	public void getFontStyleComboBox(final ResourceBundle labels) {

		// FONT STYLE COMBO BOX
		_fontStyleComboBox = new JComboBox();
		_fontStyleComboBox.addItem(labels.getString("s413"));
		_fontStyleComboBox.addItem(labels.getString("s414"));
		_fontStyleComboBox.addItem(labels.getString("s415"));
		_fontStyleComboBox.addItem(labels.getString("s416"));
		_fontStyleComboBox.setEnabled(true);
		_fontStyleComboBox.setToolTipText(labels.getString("s400"));
		_fontStyleComboBox.setSelectedItem(INITIAL_STYLE);
		_fontStyleComboBox.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				String selectedItem = (String) _fontStyleComboBox
						.getSelectedItem();

				if (selectedItem.equals(labels.getString("s413")))
					_displayArea.setFontStyle(Font.PLAIN);
				else if (selectedItem.equals(labels.getString("s414")))
					_displayArea.setFontStyle(Font.ITALIC);
				else if (selectedItem.equals(labels.getString("s415")))
					_displayArea.setFontStyle(Font.BOLD);
				else if (selectedItem.equals(labels.getString("s416")))
					_displayArea.setFontStyle(Font.BOLD + Font.ITALIC);
			}
		});

		// Sets the selected item
		SwingUtilities.invokeLater(new Runnable() {
			
			/*
			 * (non-Javadoc)
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				switch (INITIAL_STYLE) {
				case Font.PLAIN:
					_fontStyleComboBox.setSelectedItem(labels.getString("s413"));
					break;
				case Font.ITALIC:
					_fontStyleComboBox.setSelectedItem(labels.getString("s414"));
					break;
				case Font.BOLD:
					_fontStyleComboBox.setSelectedItem(labels.getString("s415"));
					break;
				case Font.BOLD + Font.ITALIC:
					_fontStyleComboBox.setSelectedItem(labels.getString("s416"));
					break;
				}
			}
		});	
	}

	/**
	 * Creates the size slider.
	 */
	public void createSizeSlider() {

		_sizeSlider = new JSlider(JSlider.HORIZONTAL, 5, 60, INITIAL_SIZE);
		_sizeSlider.setMajorTickSpacing(10);
		_sizeSlider.setMinorTickSpacing(1);
		_sizeSlider.setPaintTicks(true);
		_sizeSlider.setPaintLabels(true);
		_sizeSlider.addChangeListener(new ChangeListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * javax.swing.event.ChangeListener#stateChanged(javax.swing.event
			 * .ChangeEvent)
			 */
			@Override
			public void stateChanged(ChangeEvent changeEvent) {
				_displayArea.setFontSize(_sizeSlider.getValue());
			}
		});
	}

	/**
	 * Creates the font combo box.
	 */
	public void getFontComboBox() {

		// Gets all the font families
		String[] fontNames = GraphicsEnvironment.getLocalGraphicsEnvironment()
				.getAvailableFontFamilyNames();

		// Makes the vector with all fonts that can display the basic chars
		Vector<String> visFonts = new Vector<String>(fontNames.length);

		for (String fontName : fontNames) {

			Font f = new Font(fontName, Font.PLAIN, 12);

			// Display only fonts that have the alphabetic characters
			if (f.canDisplay('a'))
				visFonts.add(fontName);
		}

		// FONT COMBO BOX
		_fontComboBox = new JComboBox(visFonts);
		_fontComboBox.setSelectedItem(INITIAL_FONTNAME);
		_fontComboBox.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				_displayArea.setFontName((String) _fontComboBox
						.getSelectedItem());
			}
		});
	}
}