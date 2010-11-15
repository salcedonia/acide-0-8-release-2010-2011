package gui.menu.file.gui;

import gui.mainWindow.MainWindow;
import gui.menu.file.utils.PrinterManager;

import java.awt.Color;
import java.awt.Font;
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
import javax.swing.border.TitledBorder;

import language.Language;
import operations.listeners.AcideWindowListener;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Print configuration window of ACIDE - A Configurable IDE											
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
public class PrintConfigurationWindow extends JFrame{

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the window icon
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Print button
	 */
	private JButton _printButton;
	/**
	 * Cancel button
	 */
	private JButton _cancelButton;
	/**
	 * Configure page button
	 */
	private JButton _configurePageButton;
	/**
	 * Button panel
	 */
	private JPanel _buttonPanel;
	/**
	 * Main panel
	 */
	private JPanel _mainPanel;
	/**
	 * Page number label
	 */
	private JLabel _pageNumberLabel;
	/**
	 * Page number check box
	 */
	private JCheckBox _pageNumberCheckBox;
	/**
	 * Date label
	 */
	private JLabel _dateLabel;
	/**
	 * Date check box
	 */
	private JCheckBox _dateCheckBox;
	/**
	 * Printer manager
	 */
	private PrinterManager _printerManager;
	/**
	 * Labels in the selected language.
	 */
	private ResourceBundle _labels;
	/**
	 * Class instance
	 */
	private static PrintConfigurationWindow _instance;
	
	/**
	 * Returns the unique class instance
	 * 
	 * @return the unique class instance
	 */
	public static PrintConfigurationWindow getInstance() {
		if (_instance == null)
			_instance = new PrintConfigurationWindow();
		return _instance;
	}

	/**
	 * Class constructor
	 */
	public PrintConfigurationWindow() {
		
		// Gets the language
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// PRINTER MANAGER
		_printerManager = new PrinterManager(MainWindow.getInstance().getEditorManager().getSelectedEditor()
				.getEditor(), false, false);
		
		_pageNumberCheckBox = new JCheckBox();
		_pageNumberCheckBox.addActionListener(new ActionListener(){
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent){			
				if (_pageNumberCheckBox.isSelected())
					_printerManager.setShowPage(true);
				else
					_printerManager.setShowPage(false);
			}
		});

		// Gets the labels
		_labels = language.getLabels();
		
		// FRAME
		setTitle(_labels.getString("s964"));
		setIconImage(new ImageIcon(ICON).getImage());
		setLayout(new GridBagLayout());
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null, _labels
				.getString("s965"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				_printerManager.print();
				dispose();
				MainWindow.getInstance().setEnabled(true);
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
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().setEnabled(true);
				dispose();
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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
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
		add(_mainPanel, constraints);

		// BUTTON PANEL
		_buttonPanel.add(_printButton, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_cancelButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_buttonPanel, constraints);
		
		setLocationRelativeTo(null);
		pack();
		setVisible(true);
		setResizable(false);
		
		// Listeners
		_configurePageButton.addKeyListener(new PrintConfigurationWindowKeyboardListener());
		_printButton.addKeyListener(new PrintConfigurationWindowKeyboardListener());
		_cancelButton.addKeyListener(new PrintConfigurationWindowKeyboardListener());
		
		setEnabled(false);
		addWindowListener(new AcideWindowListener());
	}

	/**
	 * Return the configure page button
	 * 
	 * @return the configure page button
	 */
	public JButton getConfigurePageButton() {
		return _configurePageButton;
	}

	/**
	 * Sets a new value to the configure page button
	 * 
	 * @param configurePageButton new value to set
	 */
	public void setConfigurePageButton(JButton configurePageButton) {
		_configurePageButton = configurePageButton;
	}

	/**
	 * Returns the print button
	 * 
	 * @return the print button
	 */
	public JButton getPrintButton() {
		return _printButton;
	}

	/**
	 * Sets a new value to the print button
	 * 
	 * @param printButton new value to set
	 */
	public void setPrintButton(JButton printButton) {
		_printButton = printButton;
	}
	
	/************************************************************************																
	 * Print configuration window keyboard listener											
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
	class PrintConfigurationWindowKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
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
