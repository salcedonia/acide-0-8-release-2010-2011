package gui.menu.help;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import gui.MainWindow;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import language.Language;
import operations.listeners.AcideWindowListener;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * About us Window of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class AboutUs {

	/**
	 * Icon for the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Image file for the about us window.
	 */
	private static final String IMAGE = "./resources/images/aboutUs.png";
	/**
	 * Panel which contains the info about the developers of the application.
	 */
	private JPanel _developersPanel;
	/**
	 * Panel which contains the info of the application.
	 */
	private JPanel _infoPanel;
	/**
	 * Main panel of the window.
	 */
	private JPanel _mainPanel;
	/**
	 * Image of the about us window.
	 */
	private JLabel _image;
	/**
	 * Frame of the window.
	 */
	private JFrame _frame;
	/**
	 * Accept button.
	 */
	private JButton _acceptButton;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	
	/**
	 * Constructor of the class.
	 */
	public AboutUs() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();

		// DISABLE THE MAIN WINDOW
		final MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(false);
		_logger.info(labels.getString("s619"));

		// FRAME
		_frame = new JFrame();
		_frame.setTitle(labels.getString("s622"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new BorderLayout());

		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());

		// IMAGE
		_image = new JLabel(new ImageIcon(IMAGE));
		_frame.add(_image, BorderLayout.NORTH);

		// INFO PANEL
		_infoPanel = new JPanel();
		_infoPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s630"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_infoPanel.setLayout(new GridBagLayout());
		
		// DEVELOPERS PANEL
		_developersPanel = new JPanel();
		_developersPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s625"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));		
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
			public void actionPerformed(ActionEvent arg0) {
				_logger.info(labels.getString("s621"));
				_frame.dispose();
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
				mainWindow.setEnabled(true);
			}
		});

		// LISTENERS
		_developersPanel.addKeyListener(new AboutUsWindowKeyboardListener());
		_infoPanel.addKeyListener(new AboutUsWindowKeyboardListener());
		_acceptButton.addKeyListener(new AboutUsWindowKeyboardListener());
		_frame.addWindowListener(new AcideWindowListener());

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

		// ADD THE PANELS
		constraints.insets = new Insets(10, 50, 10, 50);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_infoPanel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_developersPanel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.fill = GridBagConstraints.NONE;
		_mainPanel.add(_acceptButton, constraints);

		// ADD THE MAIN PANEL
		_frame.add(_mainPanel, BorderLayout.CENTER);
		_frame.setResizable(false);
		_frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = _frame.getSize();
		_frame.setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		_frame.setVisible(true);
	}

	/**
	 * Keyboard listener for the about us window of the application.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class AboutUsWindowKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * 
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
}