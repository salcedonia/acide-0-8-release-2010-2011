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
package gui.fileEditor.fileEditorPanel.utils;

import es.configuration.fileEditor.AcideFileEditorConfiguration;
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
import utils.PreviewPanel;

/**
 * ACIDE - A Configurable IDE file editor display options window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideFileEditorDisplayOptionsWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE file editor display options window class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE file editor display options window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE file editor display options window menu item
	 * image icon.
	 */
	private final static ImageIcon COLOR_PALETTE_IMAGE = new ImageIcon(
			"./resources/icons/buttons/colorPalette.png");
	/**
	 * Current font size of the file editor in the main window.
	 */
	private int INITIAL_SIZE = MainWindow.getInstance().getFileEditorManager()
			.getSelectedFileEditorPanel().getActiveTextEditionArea().getFont()
			.getSize();
	/**
	 * Current font style of the output in the main window.
	 */
	private int INITIAL_STYLE = MainWindow.getInstance().getFileEditorManager()
			.getSelectedFileEditorPanel().getActiveTextEditionArea().getFont()
			.getStyle();
	/**
	 * Current font name of the output in the main window.
	 */
	private String INITIAL_FONTNAME = MainWindow.getInstance()
			.getFileEditorManager().getSelectedFileEditorPanel()
			.getActiveTextEditionArea().getFont().getFamily();
	/**
	 * Current foreground color of the output in the main window.
	 */
	private Color INITIAL_FOREGROUND_COLOR = MainWindow.getInstance()
			.getFileEditorManager().getSelectedFileEditorPanel()
			.getActiveTextEditionArea().getForeground();
	/**
	 * Current background color of the output in the main window.
	 */
	private Color INITIAL_BACKGROUND_COLOR = MainWindow.getInstance()
			.getFileEditorManager().getSelectedFileEditorPanel()
			.getActiveTextEditionArea().getBackground();
	/**
	 * Where the sample text is displayed.
	 */
	private PreviewPanel _displayArea;
	/**
	 * ACIDE - A Configurable IDE file editor display options window preview
	 * panel which contains the display area.
	 */
	private JPanel _previewPanel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window font size
	 * slider.
	 */
	private JSlider _sizeSlider;
	/**
	 * ACIDE - A Configurable IDE file editor display options window font combo
	 * box.
	 */
	private JComboBox _fontComboBox;
	/**
	 * ACIDE - A Configurable IDE file editor display options window controls
	 * panel.
	 */
	private JPanel _controlsPanel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window color
	 * buttons panel.
	 */
	private JPanel _colorButtonsPanel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window button
	 * panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window accept
	 * button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE file editor display options window cancel
	 * button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE file editor display options window font label.
	 */
	private JLabel _fontLabel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window background
	 * color label.
	 */
	private JLabel _backgroundColorLabel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window foreground
	 * color label.
	 */
	private JLabel _foregroundColorLabel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window size label.
	 */
	private JLabel _sizeLabel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window font type
	 * label.
	 */
	private JLabel _fontStyleLabel;
	/**
	 * ACIDE - A Configurable IDE file editor display options window font style
	 * combo box.
	 */
	private JComboBox _fontStyleComboBox;
	/**
	 * ACIDE - A Configurable IDE file editor display options window foreground
	 * color button.
	 */
	private JButton _foregroundColorButton;
	/**
	 * ACIDE - A Configurable IDE file editor display options window background
	 * color button.
	 */
	private JButton _backgroundColorButton;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor display options
	 * window.
	 */
	public AcideFileEditorDisplayOptionsWindow() {

		// Disables the main window
		MainWindow.getInstance().setEnabled(false);

		// Sets the layout
		setLayout(new BorderLayout());

		// CONTROLS PANEL
		_controlsPanel = new JPanel(new GridBagLayout());
		_controlsPanel.setBorder(BorderFactory
				.createTitledBorder(AcideLanguageManager.getInstance()
						.getLabels().getString("s1010")));

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// COLOR BUTTON PANEL
		_colorButtonsPanel = new JPanel(new GridBagLayout());

		// FONT LABEL
		_fontLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s981"));

		// Creates the font combo box
		getFontComboBox();

		// FONT STYLE LABEL
		_fontStyleLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s983"), JLabel.CENTER);

		// Create the font style combo box
		getFontStyleComboBox();

		// SIZE LABEL
		_sizeLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s982"));

		// Creates the size slide
		createSizeSlider();

		// FOREGROUND COLOR LABEL
		_foregroundColorLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s984"));

		// FOREGROUND COLOR BUTTON
		_foregroundColorButton = new JButton(COLOR_PALETTE_IMAGE);

		// BACKGROUND COLOR LABEL
		_backgroundColorLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s985"));

		// BACKGROUND COLOR BUTTON
		_backgroundColorButton = new JButton(COLOR_PALETTE_IMAGE);

		// ACCEPT BUTTON
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s445"));
		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s446"));

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);

		// Sets the listeners of the window components
		setListeners();

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
		_colorButtonsPanel.add(_foregroundColorLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 0;
		_colorButtonsPanel.add(_foregroundColorButton, constraints);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_colorButtonsPanel.add(_backgroundColorLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 1;
		_colorButtonsPanel.add(_backgroundColorButton, constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 4;
		_controlsPanel.add(_colorButtonsPanel, constraints);

		// CONTROLS PANEL
		add(_controlsPanel, BorderLayout.NORTH);

		// PREVIEW PANEL
		_previewPanel = new JPanel();
		_previewPanel.setBorder(BorderFactory
				.createTitledBorder(AcideLanguageManager.getInstance()
						.getLabels().getString("s1011")));

		// Creates a panel where display the fonts
		_displayArea = new PreviewPanel(INITIAL_FONTNAME, INITIAL_STYLE,
				INITIAL_SIZE, INITIAL_FOREGROUND_COLOR,
				INITIAL_BACKGROUND_COLOR);
		_previewPanel.add(_displayArea);
		add(_previewPanel, BorderLayout.CENTER);
		add(_buttonPanel, BorderLayout.SOUTH);

		// FRAME
		addWindowListener(new AcideWindowListener());
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s1041"));
		setIconImage(ICON.getImage());
		setResizable(false);
		pack();
		setVisible(true);
		setLocationRelativeTo(null);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1042"));
	}

	/**
	 * Sets the listeners of the window components.
	 */
	public void setListeners() {

		// FOREGROUND COLOR BUTTON
		_foregroundColorButton
				.addActionListener(new ForegroundColorButtonAction());

		// BACKGROUND COLOR BUTTON
		_backgroundColorButton
				.addActionListener(new BackgroundColorButtonAction());

		// ACCEPT BUTTON
		_acceptButton.addActionListener(new AcceptButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());

		// When the user press the escape button, executes the cancel option
		_cancelButton.registerKeyboardAction(new EscapeKeyAction(),
				"EscapeKey", KeyStroke.getKeyStroke(
						java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// FONT STYLE COMBO BOX
		_fontStyleComboBox.addActionListener(new FontStyleComboBoxAction());

		// FONT COMBO BOX
		_fontComboBox.addActionListener(new FontComboBoxAction());

		// SIZE SLIDER
		_sizeSlider.addChangeListener(new SizeSliderListener());
	}

	/**
	 * Creates the font style combo box.
	 */
	public void getFontStyleComboBox() {

		// FONT STYLE COMBO BOX
		_fontStyleComboBox = new JComboBox();
		_fontStyleComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s413"));
		_fontStyleComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s414"));
		_fontStyleComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s415"));
		_fontStyleComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s416"));
		_fontStyleComboBox.setEnabled(true);
		_fontStyleComboBox.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s400"));
		_fontStyleComboBox.setSelectedItem(INITIAL_STYLE);

		// Sets the selected item
		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				switch (INITIAL_STYLE) {
				case Font.PLAIN:
					_fontStyleComboBox.setSelectedItem(AcideLanguageManager
							.getInstance().getLabels().getString("s413"));
					break;
				case Font.ITALIC:
					_fontStyleComboBox.setSelectedItem(AcideLanguageManager
							.getInstance().getLabels().getString("s414"));
					break;
				case Font.BOLD:
					_fontStyleComboBox.setSelectedItem(AcideLanguageManager
							.getInstance().getLabels().getString("s415"));
					break;
				case Font.BOLD + Font.ITALIC:
					_fontStyleComboBox.setSelectedItem(AcideLanguageManager
							.getInstance().getLabels().getString("s416"));
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
	}

	/**
	 * Creates the font combo box.
	 */
	public void getFontComboBox() {

		// Gets all the font families
		String[] fontNames = GraphicsEnvironment.getLocalGraphicsEnvironment()
				.getAvailableFontFamilyNames();

		// Makes the vector with all fonts that can display the basic chars
		Vector<String> availableFonts = new Vector<String>(fontNames.length);

		for (String fontName : fontNames) {

			Font font = new Font(fontName, Font.PLAIN, 12);

			// Display only fonts that have the alphabetic characters
			if (font.canDisplay('a'))
				availableFonts.add(fontName);
		}

		// FONT COMBO BOX
		_fontComboBox = new JComboBox(availableFonts);
		_fontComboBox.setSelectedItem(INITIAL_FONTNAME);
	}

	/**
	 * ACIDE - A Configurable IDE file editor display options window foreground
	 * color button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ForegroundColorButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Ask for the color to the user
			Color foregroundColor = JColorChooser.showDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s992"), INITIAL_FOREGROUND_COLOR);

			// If the user has selected any
			if (foregroundColor != null)

				// Updates the display area foreground color
				_displayArea.setForegroundColor(foregroundColor);
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor display options window background
	 * color button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class BackgroundColorButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Ask for the color to the user
			Color backgroundColor = JColorChooser.showDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s991"), INITIAL_BACKGROUND_COLOR);

			// If the user has selected any
			if (backgroundColor != null)

				// Updates the display area background color
				_displayArea.setBackgroundColor(backgroundColor);
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor display options window accept
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info("1043");

			// Apply the changes to the opened file editor panels
			for (int index = 0; index < MainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// Sets the new font style
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(index)
						.setDisplayOptions(
								new Font(_displayArea.getFontName(),
										_displayArea.getFontStyle(),
										_displayArea.getFontSize()),
								_displayArea.getBackground(),
								_displayArea.getForeground());
				
				// Gets the caret position of the file editor panel
				/*int caretPosition = MainWindow.getInstance().getFileEditorManager()
				.getFileEditorPanelAt(index).getActiveTextEditionArea().getCaretPosition();
				
				// Resets the file editor text edition area
				MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).resetStyledDocument(caretPosition);*/
			}

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject())

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);

			// Saves the file editor configuration
			AcideFileEditorConfiguration.getInstance().save();

			// Set the main window enabled again
			MainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor display options window cancel
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info("1044");

			// Enables the main window again
			MainWindow.getInstance().setEnabled(true);

			// Closes window
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor display options window escape key
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class EscapeKeyAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Enables the main window again
			MainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor display options window font style
	 * combo box action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class FontStyleComboBoxAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			String selectedItem = (String) _fontStyleComboBox.getSelectedItem();

			if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s413")))
				_displayArea.setFontStyle(Font.PLAIN);
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s414")))
				_displayArea.setFontStyle(Font.ITALIC);
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s415")))
				_displayArea.setFontStyle(Font.BOLD);
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s416")))
				_displayArea.setFontStyle(Font.BOLD + Font.ITALIC);
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor display options window size slider
	 * change listener.
	 * 
	 * @version 0.8
	 * @see ChangeListener
	 */
	class SizeSliderListener implements ChangeListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event
		 * .ChangeEvent)
		 */
		@Override
		public void stateChanged(ChangeEvent changeEvent) {
			_displayArea.setFontSize(_sizeSlider.getValue());
		}
	}

	/**
	 * ACIDE - A Configurable IDE file editor display options window font combo
	 * box action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class FontComboBoxAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			_displayArea.setFontName((String) _fontComboBox.getSelectedItem());
		}
	}
}
