package gui.output;

import gui.MainWindow;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
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

import language.Language;

import operations.log.Log;
import properties.PropertiesManager;

/**
 * GUI for the visualization options of the output of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class OutputVisualizationGUI extends JFrame {
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Icon for the main window.
	 */
	private static final String ICON = "./resources/images/icon.png";
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
	 * Controls panel.
	 */
	private JPanel _controlsPanel;
	/**
	 * Content panel.
	 */
	private JPanel _contentPanel;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Accept button.
	 */
	private JButton _acceptButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Style plain Radio Button.
	 */
	private JRadioButton _stylePlainRadioButton;
	/**
	 * Style italic radio button.
	 */
	private JRadioButton _styleItalicRadioButton;
	/**
	 * Style bold radio button.
	 */
	private JRadioButton _styleBoldRadioButton;
	/**
	 * Style italic + bold radio button.
	 */
	private JRadioButton _styleItalicBoldRadioButton;
	/**
	 * Style button group.
	 */
	private ButtonGroup _styleGroup;
	/**
	 * Foreground button.
	 */
	private JButton _foregroundButton;
	/**
	 * Background button.
	 */
	private JButton _backgroundButton;

	/**
	 * Constructor of the class.
	 */
	public OutputVisualizationGUI() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS TO DISPLAY
		final ResourceBundle labels = language.getLabels();

		MainWindow.getInstance().setEnabled(false);

		// CREATE A PANEL WHERE DISPLAY THE FONTS
		_displayArea = new PreviewPanel(INITIAL_FONTNAME, INITIAL_STYLE,
				INITIAL_SIZE, INITIAL_FOREGROUND, INITIAL_BACKGROUND);

		// GET ALL THE FONT FAMILIES
		String[] fontNames = GraphicsEnvironment.getLocalGraphicsEnvironment()
				.getAvailableFontFamilyNames();

		// MAKE THE VECTOR WITH ALL FONTS THAT CAN DISPLAY BASIC CHARS
		Vector<String> visFonts = new Vector<String>(fontNames.length);

		for (String fontName : fontNames) {

			Font f = new Font(fontName, Font.PLAIN, 12);

			// DISPLAY ONLY FONTS THAT HAVE THE ALPHABETIC CHARACTERS
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
			public void actionPerformed(ActionEvent e) {
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

		// ADD THE BUTTONS TO A BUTTON GROUP
		_styleGroup = new ButtonGroup();
		_styleGroup.add(_stylePlainRadioButton);
		_styleGroup.add(_styleItalicRadioButton);
		_styleGroup.add(_styleBoldRadioButton);
		_styleGroup.add(_styleItalicBoldRadioButton);

		// LISTENERS
		_stylePlainRadioButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
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
			public void actionPerformed(ActionEvent e) {
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
			public void actionPerformed(ActionEvent e) {
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
			public void actionPerformed(ActionEvent e) {
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
			public void stateChanged(ChangeEvent arg0) {
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
			public void actionPerformed(ActionEvent e) {

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
			public void actionPerformed(ActionEvent e) {

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
		_buttonPanel = new JPanel();

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
			public void actionPerformed(ActionEvent e) {

				// UPDATES THE LOG
				Log.getLog().info("989");

				// APPLY THE CHANGES TO THE OUTPUT IN THE MAIN WINDOW
				MainWindow.getInstance().getOutput().getTextComponent()
						.setFont(
								new Font(_displayArea.getFontName(),
										_displayArea.getFontStyle(),
										_displayArea.getFontSize()));
				MainWindow.getInstance().getOutput().getTextComponent()
						.setBackground(_displayArea.getBackground());
				MainWindow.getInstance().getOutput().getTextComponent()
						.setForeground(_displayArea.getForeground());

				// NOT DEFAULT PROJECT
				if(!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
					MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
					
				// SET THE MAIN WINDOW ENABLED AGAIN
				MainWindow.getInstance().setEnabled(true);

				// CLOSES THE WINDOW
				dispose();
				
				// ENABLES THE MAIN WINDOW AGAIN
				MainWindow.getInstance().setEnabled(true);
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
			public void actionPerformed(ActionEvent e) {
				
				// CLOSES THE WINDOW
				dispose();
				
				// ENABLES THE MAIN WINDOW AGAIN
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
			public void actionPerformed(ActionEvent e) {

				// UPDATES THE LOG
				Log.getLog().info("988");

				// CLOSES THE WINDOW
				dispose();
				
				// ENABLES THE MAIN WINDOW AGAIN
				MainWindow.getInstance().setEnabled(true);
			}
		});
		_buttonPanel.add(_cancelButton);

		// PUT DISPLAY + CONTROLS IN THE CONTENT PANE
		_contentPanel = new JPanel();
		_contentPanel.setLayout(new BorderLayout(5, 5));
		_contentPanel.add(_displayArea, BorderLayout.CENTER);
		_contentPanel.add(_controlsPanel, BorderLayout.WEST);
		_contentPanel.add(_buttonPanel, BorderLayout.SOUTH);
		_contentPanel.setBorder(new EmptyBorder(12, 12, 12, 12));

		// SET THE WINDOW CHARACTERISTICS
		setContentPane(_contentPanel);
		setTitle(labels.getString("s977"));
		Log.getLog().info(labels.getString("s978"));
		setIconImage(new ImageIcon(ICON).getImage());
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);
	}

	/**
	 * Utility method to add elements to a BoxLayout container.
	 * 
	 * @param cont
	 *            Container.
	 * @param align
	 *            Align.
	 * @param comps
	 *            Components to add.
	 */
	private void addToBox(Container cont, float align, JComponent... comps) {
		for (JComponent comp : comps) {
			comp.setAlignmentX(align);
			cont.add(comp);
		}
	}
}