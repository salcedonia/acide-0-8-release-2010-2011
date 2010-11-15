package gui.menu.help.gui;

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

import gui.mainWindow.MainWindow;

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

import properties.PropertiesManager;

/************************************************************************																
 * About us window of ACIDE - A Configurable IDE											
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
public class AboutUsWindow extends JFrame{

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Icon for the window
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Image file for the about us window
	 */
	private static final String IMAGE = "./resources/images/aboutUs.png";
	/**
	 * Panel which contains the info about the developers of the application
	 */
	private JPanel _developersPanel;
	/**
	 * Panel which contains the info of the application
	 */
	private JPanel _infoPanel;
	/**
	 * Main panel of the window
	 */
	private JPanel _mainPanel;
	/**
	 * Image of the about us window
	 */
	private JLabel _image;
	/**
	 * Accept button
	 */
	private JButton _acceptButton;
	
	/**
	 * Class constructor
	 */
	public AboutUsWindow() {

		super();
		
		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// DISABLE THE MAIN WINDOW
		final MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(false);
		
		// Updates the log
		Log.getLog().info(labels.getString("s619"));

		// FRAME
		setTitle(labels.getString("s622"));
		setIconImage(new ImageIcon(ICON).getImage());
		setLayout(new BorderLayout());

		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());

		// IMAGE
		_image = new JLabel(new ImageIcon(IMAGE));
		add(_image, BorderLayout.NORTH);

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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				// Updates the log
				Log.getLog().info(labels.getString("s621"));
				
				// CLOSE THE WINDOW
				dispose();
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
				mainWindow.setEnabled(true);
			}
		});

		// Listeners
		_developersPanel.addKeyListener(new AboutUsWindowKeyboardListener());
		_infoPanel.addKeyListener(new AboutUsWindowKeyboardListener());
		_acceptButton.addKeyListener(new AboutUsWindowKeyboardListener());
		addWindowListener(new AcideWindowListener());

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
		add(_mainPanel, BorderLayout.CENTER);
		setResizable(false);
		pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		setVisible(true);
	}

	/************************************************************************																
	 * About us window keyboard listener											
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
	 * @see KeyAdapter																													
	 ***********************************************************************/
	class AboutUsWindowKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {

			if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {
				
				MainWindow.getInstance().setEnabled(true);
				dispose();
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}