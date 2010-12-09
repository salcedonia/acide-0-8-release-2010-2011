package gui.outputPanel.utils;

import es.configuration.output.OutputConfiguration;
import gui.listeners.AcideWindowListener;
import gui.mainWindow.MainWindow;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.ResourceBundle;
import java.util.Vector;

import javax.swing.BoxLayout;
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
import javax.swing.border.EmptyBorder;
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
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
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
	 * Current font size of the output in the main window of the application.
	 */
	private int INITIAL_SIZE = MainWindow.getInstance()
			.getOutput().getTextComponent().getFont().getSize();
	/**
	 * Current font style of the output in the main window of the application.
	 */
	private int INITIAL_STYLE = MainWindow.getInstance()
			.getOutput().getTextComponent().getFont().getStyle();
	/**
	 * Current font name of the output in the main window of the application.
	 */
	private String INITIAL_FONTNAME = MainWindow.getInstance()
			.getOutput().getTextComponent().getFont().getFamily();
	/**
	 * Current foreground color of the output in the main window of the
	 * application.
	 */
	private Color INITIAL_FOREGROUND = MainWindow.getInstance()
			.getOutput().getTextComponent().getForeground();
	/**
	 * Current background color of the output in the main window of the
	 * application.
	 */
	private Color INITIAL_BACKGROUND = MainWindow.getInstance()
			.getOutput().getTextComponent().getBackground();
	/**
	 * Where the sample text is displayed.
	 */
	private PreviewPanel _displayArea;
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
	 * Shell display options window content panel.
	 */
	private JPanel _mainPanel;
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

		MainWindow.getInstance().setEnabled(false);

		// Creates a panel where display the fonts
		_displayArea = new PreviewPanel(INITIAL_FONTNAME, INITIAL_STYLE,
				INITIAL_SIZE, INITIAL_FOREGROUND, INITIAL_BACKGROUND);

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

		// RADIO BUTTONS FOR THE STYLE
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

		// SIZE SLIDER
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

		// FOREGROUND BUTTON
		_foregroundButton = new JButton(labels.getString("s920"));
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

				Color foregroundColor = JColorChooser.showDialog(null, labels
						.getString("s992"), INITIAL_FOREGROUND);

				if (foregroundColor != null)
					_displayArea.setForegroundColor(foregroundColor);
			}
		});

		// BACKGROUND BUTTON
		_backgroundButton = new JButton(labels.getString("s920"));
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

				Color backgroundColor = JColorChooser.showDialog(null, labels
						.getString("s991"), INITIAL_BACKGROUND);

				if (backgroundColor != null)
					_displayArea.setBackgroundColor(backgroundColor);
			}
		});

		// CONTROLS PANEL
		_controlsPanel = new JPanel();
		_controlsPanel
				.setLayout(new BoxLayout(_controlsPanel, BoxLayout.Y_AXIS));

		// ADD COMPONENTS TO THE CONTROL PANEL
		addToBox(_controlsPanel, Component.LEFT_ALIGNMENT, new JLabel(labels
				.getString("s981")), _fontComboBox, new JLabel(labels
				.getString("s982")), _sizeSlider, new JLabel(labels
				.getString("s983")), _stylePlainRadioButton,
				_styleItalicRadioButton, _styleBoldRadioButton,
				_styleItalicBoldRadioButton, new JLabel(labels
						.getString("s984")), _foregroundButton, new JLabel(
						labels.getString("s985")), _backgroundButton);

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

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

				// Apply the changes to the output in the main window
				MainWindow.getInstance().getOutput().getTextComponent()
						.setFont(
								new Font(_displayArea.getFontName(),
										_displayArea.getFontStyle(),
										_displayArea.getFontSize()));
				MainWindow.getInstance().getOutput().getTextComponent()
						.setBackground(_displayArea.getBackground());
				MainWindow.getInstance().getOutput().getTextComponent()
						.setForeground(_displayArea.getForeground());
				MainWindow.getInstance().getOutput().getTextComponent()
				.setCaretColor(_displayArea.getForeground());
		
				// Not default project
				if(!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
					MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
				
				// Saves the output configuration
				OutputConfiguration.getInstance().save();
					
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
				
				// Closes the window
				dispose();
				
				// Enables the main window again
				MainWindow.getInstance().setEnabled(true);
			}
		};

		_acceptButton.registerKeyboardAction(actionListener, "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

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

				// Closes window
				dispose();
				
				// Enables the main window again
				MainWindow.getInstance().setEnabled(true);
			}
		});
		_buttonPanel.add(_cancelButton);

		// PUT DISPLAY + CONTROLS IN THE CONTENT PANE
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new BorderLayout(5, 5));
		_mainPanel.add(_displayArea, BorderLayout.CENTER);
		_mainPanel.add(_controlsPanel, BorderLayout.WEST);
		_mainPanel.add(_buttonPanel, BorderLayout.SOUTH);
		_mainPanel.setBorder(new EmptyBorder(12, 12, 12, 12));

		// FRAME
		addWindowListener(new AcideWindowListener());
		setContentPane(_mainPanel);
		setTitle(labels.getString("s977"));
		AcideLog.getLog().info(labels.getString("s978"));
		setIconImage(ICON.getImage());
		setVisible(true);
		setResizable(false);
		pack();
	}

	/**
	 * Utility method to add elements to a BoxLayout container.
	 * 
	 * @param container
	 *            Container.
	 * @param align
	 *            Align.
	 * @param components
	 *            Components to add.
	 */
	private void addToBox(Container container, float align, JComponent... components) {
		for (JComponent comp : components) {
			comp.setAlignmentX(align);
			container.add(comp);
		}
	}
}