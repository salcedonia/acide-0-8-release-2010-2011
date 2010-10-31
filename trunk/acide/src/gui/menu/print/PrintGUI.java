package gui.menu.print;

import gui.MainWindow;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import language.Language;
import operations.listeners.AcideWindowListener;
import properties.PropertiesManager;

/**
 * Print GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class PrintGUI {

	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Frame of the window.
	 */
	private JFrame _frame;
	/**
	 * Print button.
	 */
	private JButton _printButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Configure page button.
	 */
	private JButton _configurePageButton;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * General panel.
	 */
	private JPanel _mainPanel;
	/**
	 * Page number label.
	 */
	private JLabel _pageNumberLabel;
	/**
	 * Page number check box.
	 */
	private JCheckBox _pageNumberCheckBox;
	/**
	 * Date label.
	 */
	private JLabel _dateLabel;
	/**
	 * Date check box.
	 */
	private JCheckBox _dateCheckBox;
	/**
	 * Printer manager.
	 */
	private PrinterManager _printerManager;
	/**
	 * Labels in the selected language.
	 */
	private ResourceBundle _labels;
	/**
	 * Instance of the class.
	 */
	private static PrintGUI _instance;
	
	/**
	 * Return the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static PrintGUI getInstance() {
		if (_instance == null)
			_instance = new PrintGUI();
		return _instance;
	}

	/**
	 * Constructor of the class.
	 */
	public PrintGUI() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE MAIN WINDOW
		MainWindow mainWindow = MainWindow.getInstance();
		_printerManager = new PrinterManager(mainWindow.getEditorBuilder().getSelectedEditor()
				.getEditor(), false, false);
		
		_pageNumberCheckBox = new JCheckBox();
		_pageNumberCheckBox.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				if (_pageNumberCheckBox.isSelected())
					_printerManager.setShowPage(true);
				else
					_printerManager.setShowPage(false);
			}

		});

		// GET THE LABELS
		_labels = language.getLabels();
		
		// FRAME
		_frame = new JFrame();
		_frame.setTitle(_labels.getString("s964"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s965")));
		_mainPanel.setLayout(new GridBagLayout());
		
		// PAGE NUMBER
		_pageNumberLabel = new JLabel(_labels.getString("s962"));
		
		// DATE 
		_dateLabel = new JLabel(_labels.getString("s963"));
		_dateCheckBox = new JCheckBox();
		_dateCheckBox.setEnabled(false);
		_pageNumberCheckBox.setEnabled(false);
		_dateCheckBox.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				if (_dateCheckBox.isSelected())
					_printerManager.setDate(true);
				else
					_printerManager.setDate(false);
			}
		});

		// PRINT BUTTON
		_printButton = new JButton(_labels.getString("s624"));
		_printButton.setHorizontalAlignment(JButton.CENTER);
		_printButton.setToolTipText(_labels.getString("s624"));
		_printButton.setEnabled(false);
		_printButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				_printerManager.print();
				_frame.dispose();
				mainWindow.setEnabled(true);
			}
		});
		
		// CANCEL BUTTON
		_cancelButton = new JButton(_labels.getString("s919"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(_labels.getString("s919"));
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
			}
		});

		// CONFIGURE PAGE BUTTON
		_configurePageButton = new JButton(_labels.getString("s961"));
		_configurePageButton.setHorizontalAlignment(JButton.CENTER);
		_configurePageButton.setToolTipText(_labels.getString("s961"));
		_configurePageButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				_printerManager.configurePage();
				
				if (_printerManager.getPageFormat() != null) {
					_printButton.setEnabled(true);
					_dateCheckBox.setEnabled(true);
					_pageNumberCheckBox.setEnabled(true);
				}
			}
		});
		
		// ADD THE COMPONENTS TO THE WINDOW WITH THE LAYOUT
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
		_frame.add(_mainPanel, constraints);

		// BUTTON PANEL
		_buttonPanel.add(_printButton, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_cancelButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_buttonPanel, constraints);
		
		_frame.setLocationRelativeTo(null);
		_frame.pack();
		_frame.setVisible(true);
		_frame.setResizable(false);
		
		// LISTENERS
		_configurePageButton.addKeyListener(new PrintGUIKeyboardListener());
		_printButton.addKeyListener(new PrintGUIKeyboardListener());
		_cancelButton.addKeyListener(new PrintGUIKeyboardListener());
		
		mainWindow.setEnabled(false);
		AcideWindowListener window = new AcideWindowListener();
		_frame.addWindowListener(window);
	}

	/**
	 * Listener for the keyboard events of the class PrintGUI components.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class PrintGUIKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
			}
		}
	}

	/**
	 * Return the configure page button.
	 * 
	 * @return The configure page button.
	 */
	public JButton getConfigurePageButton() {
		return _configurePageButton;
	}

	/**
	 * Set a new value to the configure page button.
	 * 
	 * @param configurePageButton new value to set.
	 */
	public void setConfigurePageButton(JButton configurePageButton) {
		_configurePageButton = configurePageButton;
	}

	/**
	 * Return the print button.
	 * 
	 * @return The print button.
	 */
	public JButton getPrintButton() {
		return _printButton;
	}

	/**
	 * Set a new value to the print button.
	 * 
	 * @param printButton New value to set.
	 */
	public void setPrintButton(JButton printButton) {
		_printButton = printButton;
	}

	/**
	 * Returns the frame of the window.
	 * 
	 * @return The frame of the window.
	 */
	public JFrame getFrame() {
		return _frame;
	}

	/**
	 * Set a new value to the frame of the window.
	 * 
	 * @param frame New value to set.
	 */
	public void setFrame(JFrame frame) {
		_frame = frame;
	}
}
