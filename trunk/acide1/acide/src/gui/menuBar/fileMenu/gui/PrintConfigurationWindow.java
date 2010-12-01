package gui.menuBar.fileMenu.gui;

import gui.mainWindow.MainWindow;
import gui.menuBar.fileMenu.utils.PrinterManager;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
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

import language.AcideLanguage;
import operations.listeners.AcideWindowListener;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Print configuration window of ACIDE - A Configurable IDE.											
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
	 * Print configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Print configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon("./resources/images/icon.png");
	/**
	 * Print configuration window print button.
	 */
	private JButton _printButton;
	/**
	 * Print configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Print configuration window configure page button.
	 */
	private JButton _configurePageButton;
	/**
	 * Print configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Print configuration window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * Print configuration window page number label.
	 */
	private JLabel _pageNumberLabel;
	/**
	 * Print configuration window page number check box.
	 */
	private JCheckBox _pageNumberCheckBox;
	/**
	 * Print configuration window date label.
	 */
	private JLabel _dateLabel;
	/**
	 * Print configuration window date check box.
	 */
	private JCheckBox _dateCheckBox;
	/**
	 * Print configuration window printer manager.
	 */
	private PrinterManager _printerManager;
	/**
	 * Labels in the selected language.
	 */
	private ResourceBundle _labels;
	/**
	 * Print configuration window unique class instance.
	 */
	private static PrintConfigurationWindow _instance;
	
	/**
	 * Returns the print configuration window unique class instance.
	 * 
	 * @return the print configuration window unique class instance.
	 */
	public static PrintConfigurationWindow getInstance() {
		if (_instance == null)
			_instance = new PrintConfigurationWindow();
		return _instance;
	}

	/**
	 * Creates a new print configuration window.
	 */
	public PrintConfigurationWindow() {
		
		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// PRINTER MANAGER
		_printerManager = new PrinterManager(MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
				.getActiveTextEditionArea(), false, false);
		
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
		
		// Sets the layout
		setLayout(new GridBagLayout());
		
		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null, _labels
				.getString("s965"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
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
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		_buttonPanel.add(_printButton);
		_buttonPanel.add(_cancelButton);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_buttonPanel, constraints);
		
		// FRAME
		setTitle(_labels.getString("s964"));
		setIconImage(ICON.getImage());
		setLocationRelativeTo(null);
		pack();
		setVisible(true);
		setResizable(false);
		
		// Centers the window
		
		// Get the size of the screen
	    Dimension dimension = Toolkit.getDefaultToolkit().getScreenSize();
	    
	    // Determine the new location of the window
	    int w = getSize().width;
	    int h = getSize().height;
	    int x = (dimension.width-w)/2;
	    int y = (dimension.height-h)/2;
	    
	    // Move the window
	    setLocation(x, y);
	    
		// Listeners
		_configurePageButton.addKeyListener(new PrintConfigurationWindowKeyboardListener());
		_printButton.addKeyListener(new PrintConfigurationWindowKeyboardListener());
		_cancelButton.addKeyListener(new PrintConfigurationWindowKeyboardListener());
		addWindowListener(new AcideWindowListener());
		
		// Disables the main window
		MainWindow.getInstance().setEnabled(false);	
	}

	/**
	 * Return the configure page button.
	 * 
	 * @return the configure page button.
	 */
	public JButton getConfigurePageButton() {
		return _configurePageButton;
	}

	/**
	 * Sets a new value to the configure page button.
	 * 
	 * @param configurePageButton new value to set.
	 */
	public void setConfigurePageButton(JButton configurePageButton) {
		_configurePageButton = configurePageButton;
	}

	/**
	 * Returns the print button.
	 * 
	 * @return the print button.
	 */
	public JButton getPrintButton() {
		return _printButton;
	}

	/**
	 * Sets a new value to the print button.
	 * 
	 * @param printButton new value to set.
	 */
	public void setPrintButton(JButton printButton) {
		_printButton = printButton;
	}
	
	/************************************************************************																
	 * Print configuration window keyboard listener.										
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
