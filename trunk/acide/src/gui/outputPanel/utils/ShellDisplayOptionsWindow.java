package gui.outputPanel.utils;

import es.configuration.output.OutputConfiguration;
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
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import javax.swing.KeyStroke;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import language.AcideLanguage;

import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Configuration window for the display options of the output of 
 * ACIDE - A Configurable IDE.
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan Jos� Ortiz S�nchez										
 *         </ul>														
 *         <ul>															
 *         Delf�n Rup�rez Ca�as											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Mart�n L�zaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo G�mez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8		
 * @see JFrame																												
 ***********************************************************************/
public class ShellDisplayOptionsWindow extends JFrame {
	
	/**
	 * Shell display options window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Shell display options window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon("./resources/images/icon.png");
	/**
	 * Shell display options menu item image icon.
	 */
	private final static ImageIcon COLOR_PALETTE_IMAGE = new ImageIcon("./resources/icons/buttons/colorPalette.png");
	/**
	 * Current font size of the output in the main window of the application.
	 */
	private int INITIAL_SIZE = MainWindow.getInstance()
			.getOutputPanel().getTextComponent().getFont().getSize();
	/**
	 * Current font style of the output in the main window of the application.
	 */
	private int INITIAL_STYLE = MainWindow.getInstance()
			.getOutputPanel().getTextComponent().getFont().getStyle();
	/**
	 * Current font name of the output in the main window of the application.
	 */
	private String INITIAL_FONTNAME = MainWindow.getInstance()
			.getOutputPanel().getTextComponent().getFont().getFamily();
	/**
	 * Current foreground color of the output in the main window of the
	 * application.
	 */
	private Color INITIAL_FOREGROUND = MainWindow.getInstance()
			.getOutputPanel().getTextComponent().getForeground();
	/**
	 * Current background color of the output in the main window of the
	 * application.
	 */
	private Color INITIAL_BACKGROUND = MainWindow.getInstance()
			.getOutputPanel().getTextComponent().getBackground();
	/**
	 * Where the sample text is displayed.
	 */
	private PreviewPanel _displayArea;
	/**
	 * Preview panel which contains the display area.
	 */
	private JPanel _previewPanel;
	/**
	 * Sets the font size.
	 */
	private JSlider _sizeSlider;
	/**
	 * For selecting one of the installed fonts.
	 */
	private JComboBox _fontComboBox;
	/**
	 * Shell display options window controls panel.
	 */
	private JPanel _controlsPanel;
	/**
	 * Shell display options window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Shell display options window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * Shell display options window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Shell display options window style plain Radio Button.
	 */
	private JRadioButton _stylePlainRadioButton;
	/**
	 * Shell display options window style italic radio button.
	 */
	private JRadioButton _styleItalicRadioButton;
	/**
	 * Shell display options window style bold radio button.
	 */
	private JRadioButton _styleBoldRadioButton;
	/**
	 * Shell display options window style italic + bold radio button.
	 */
	private JRadioButton _styleItalicBoldRadioButton;
	/**
	 * Shell display options window style button group.
	 */
	private ButtonGroup _styleGroup;
	/**
	 * Shell display options window foreground button.
	 */
	private JButton _foregroundButton;
	/**
	 * Shell display options window background button.
	 */
	private JButton _backgroundButton;

	/**
	 * Creates a new shell display options window.
	 */
	public ShellDisplayOptionsWindow() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		// Disables the main window
		MainWindow.getInstance().setEnabled(false);

		// Sets the layout
		setLayout(new BorderLayout());
		
		// CONTROLS PANEL
		_controlsPanel = new JPanel(new GridBagLayout());
		_controlsPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s1010")));
		
		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		
		// Creates the font combo box
		createFontComboBox();

		// Creates the style button group
		createStyleButtonGroup();

		// Creates the size slide
		createSizeSlider();

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
				Color foregroundColor = JColorChooser.showDialog(null, labels
						.getString("s992"), INITIAL_FOREGROUND);

				// If the user has selected any
				if (foregroundColor != null)
					
					// Updates the display area foreground color
					_displayArea.setForegroundColor(foregroundColor);
			}
		});

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
				Color backgroundColor = JColorChooser.showDialog(null, labels
						.getString("s991"), INITIAL_BACKGROUND);

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
				MainWindow.getInstance().getOutputPanel().getTextComponent()
						.setFont(
								new Font(_displayArea.getFontName(),
										_displayArea.getFontStyle(),
										_displayArea.getFontSize()));
				
				// Sets the new background color
				MainWindow.getInstance().getOutputPanel().getTextComponent()
						.setBackground(_displayArea.getBackground());
				
				// Sets the new foreground color
				MainWindow.getInstance().getOutputPanel().getTextComponent()
						.setForeground(_displayArea.getForeground());
				
				// Sets the new caret color
				MainWindow.getInstance().getOutputPanel().getTextComponent()
				.setCaretColor(_displayArea.getForeground());
		
				// Not default project
				if(!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
					
					// The project has been modified
					MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
				
				// Saves the output configuration
				OutputConfiguration.getInstance().save();
					
				// Set the main window enabled again
				MainWindow.getInstance().setEnabled(true);

				// Closes the window
				dispose();
				
				// Set the main window visible again
				MainWindow.getInstance().setVisible(true);
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
				
				// Set the main window visible again
				MainWindow.getInstance().setVisible(true);
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
				
				// Set the main window visible again
				MainWindow.getInstance().setVisible(true);
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
		_controlsPanel.add(new JLabel(labels.getString("s981")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_controlsPanel.add(_fontComboBox, constraints);
		constraints.gridx = 1;
		constraints.gridy = 0;
		_controlsPanel.add(new JLabel(labels.getString("s982")), constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;
		_controlsPanel.add(_sizeSlider, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		_controlsPanel.add(new JLabel(labels.getString("s983")), constraints);
		constraints.insets = new Insets(0, 10, 0, 0);
		constraints.gridx = 0;
		constraints.gridy = 3;
		_controlsPanel.add(_stylePlainRadioButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 4;
		_controlsPanel.add(_styleItalicRadioButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 5;
		_controlsPanel.add(_styleBoldRadioButton, constraints);
		constraints.insets = new Insets(0, 10, 10, 0);
		constraints.gridx = 0;
		constraints.gridy = 6;
		_controlsPanel.add(_styleItalicBoldRadioButton, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.insets = new Insets(5, 25, 5, 5);
		constraints.gridx = 1;
		constraints.gridheight = 2;
		constraints.gridy = 3;
		_controlsPanel.add(new JLabel(labels.getString("s984")), constraints);
		constraints.insets = new Insets(5,5,5,15);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 3;
		_controlsPanel.add(_foregroundButton, constraints);
		constraints.insets = new Insets(5,25,5,5);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 4;
		_controlsPanel.add(new JLabel(labels.getString("s985")), constraints);
		constraints.insets = new Insets(5,5,5,15);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 4;
		_controlsPanel.add(_backgroundButton, constraints);
		
		// CONTROLS PANEL
		add(_controlsPanel, BorderLayout.NORTH);
		
		// PREVIEW PANEL
		_previewPanel = new JPanel();
		_previewPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s1011")));
		
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
	 * Creates the style button group.
	 */
	public void createStyleButtonGroup() {
		
		if (INITIAL_STYLE == Font.PLAIN)
			_stylePlainRadioButton = new JRadioButton("Font.PLAIN", true);
		else
			_stylePlainRadioButton = new JRadioButton("Font.PLAIN", false);

		if (INITIAL_STYLE == Font.ITALIC)
			_styleItalicRadioButton = new JRadioButton("Font.ITALIC", true);
		else
			_styleItalicRadioButton = new JRadioButton("Font.ITALIC", false);

		if (INITIAL_STYLE == Font.BOLD)
			_styleBoldRadioButton = new JRadioButton("Font.BOLD", true);
		else
			_styleBoldRadioButton = new JRadioButton("Font.BOLD", false);

		if (INITIAL_STYLE == Font.BOLD+Font.ITALIC)
			_styleItalicBoldRadioButton = new JRadioButton(
					"Font.BOLD+Font.ITALIC", true);
		else
			_styleItalicBoldRadioButton = new JRadioButton(
					"Font.BOLD+Font.ITALIC", false);

		// Adds the buttons to the button group
		_styleGroup = new ButtonGroup();
		_styleGroup.add(_stylePlainRadioButton);
		_styleGroup.add(_styleItalicRadioButton);
		_styleGroup.add(_styleBoldRadioButton);
		_styleGroup.add(_styleItalicBoldRadioButton);

		// Listeners
		_stylePlainRadioButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				_displayArea.setFontStyle(Font.PLAIN);
			}
		});
		_styleItalicRadioButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				_displayArea.setFontStyle(Font.ITALIC);
			}
		});
		_styleBoldRadioButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent actionEvent) {
				_displayArea.setFontStyle(Font.BOLD);
			}
		});
		_styleItalicBoldRadioButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				_displayArea.setFontStyle(Font.ITALIC + Font.BOLD);
			}
		});
	}

	/**
	 * Creates the font combo box.
	 */
	public void createFontComboBox() {
		
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