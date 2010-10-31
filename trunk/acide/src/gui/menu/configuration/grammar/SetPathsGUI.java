package gui.menu.configuration.grammar;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import language.Language;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import es.text.TextFile;

/**
 * Set paths GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class SetPathsGUI {

	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Frame of the window.
	 */
	private JFrame _frame;
	/**
	 * Java panel.
	 */
	private JPanel _javaPanel;
	/**
	 * Jar panel.
	 */
	private JPanel _jarPanel;
	/**
	 * Javac panel.
	 */
	private JPanel _javacPanel;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Java text field.
	 */
	private final JTextField _javaTextField;
	/**
	 * Java check box.
	 */
	private final JCheckBox _javaCheckBox;
	/**
	 * Java browse button.
	 */
	private final JButton _javaBrowseButton;
	/**
	 * Javac text field.
	 */
	private final JTextField _javacTextField;
	/**
	 * Javac check box.
	 */
	private final JCheckBox _javacCheckBox;
	/**
	 * Javc browse button.
	 */
	private final JButton _javacBrowseButton;
	/**
	 * Jar text field.
	 */
	private final JTextField _jarTextField;
	/**
	 * Jar check box.
	 */
	private final JCheckBox _jarCheckBox;
	/**
	 * Jar browse button.
	 */
	private final JButton _jarBrowseButton;
	/**
	 * Accept button.
	 */
	private JButton _acceptButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Java path string.
	 */
	private final String _javaPath;
	/**
	 * Javac path string.
	 */
	private final String _javacPath;
	/**
	 * Jar path string.
	 */
	private final String _jarPath;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public SetPathsGUI() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();
		
		_logger.info(labels.getString("s914"));
		
		// FRAME
		_frame = new JFrame();
		_frame.setTitle(labels.getString("s913"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());
		
		// JAVA PANEL
		_javaPanel = new JPanel();
		_javaPanel.setLayout(new GridBagLayout());
		_javaPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s915")));
	
		// JAVAC PANEL
		_javacPanel = new JPanel();
		_javacPanel.setLayout(new GridBagLayout());
		_javacPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s916")));
		
		// JAR PANEL
		_jarPanel = new JPanel();
		_jarPanel.setLayout(new GridBagLayout());
		_jarPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s917")));
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridLayout());
		
		// JAVA
		_javaTextField = new JTextField();
		_javaCheckBox = new JCheckBox();
		_javaBrowseButton = new JButton(labels.getString("s920"));
		
		// JAVAC
		_javacTextField = new JTextField();
		_javacCheckBox = new JCheckBox();
		_javacBrowseButton = new JButton(labels.getString("s921"));
		
		// JAR
		_jarTextField = new JTextField();
		_jarCheckBox = new JCheckBox();
		_jarBrowseButton = new JButton(labels.getString("s922"));
		
		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s918"));
		_cancelButton = new JButton(labels.getString("s919"));
		
		String javaPath = null;
		String javacPath = null;
		String jarPath = null;
		
		try {
			javaPath = PropertiesManager.getProperty("javaPath");
			javacPath = PropertiesManager.getProperty("javacPath");
			jarPath = PropertiesManager.getProperty("jarPath");
		}
		catch (Exception e) {
			_logger.error(e.getMessage());
		}
		
		_javaPath = javaPath;
		_javacPath = javacPath;
		_jarPath = jarPath;
		
		// LISTENERS
		setListeners();
		
		// ADD THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		
		// JAVA PANEL
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_javaPanel.add(_javaCheckBox, constraints);	
		constraints.gridx = 1;
		constraints.ipadx = 400;
		_javaPanel.add(_javaTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 2;
		_javaPanel.add(_javaBrowseButton, constraints);
		constraints.gridx = 0;
		
		// JAVAC PANEL
		_javacPanel.add(_javacCheckBox, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 400;
		_javacPanel.add(_javacTextField, constraints);
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_javacPanel.add(_javacBrowseButton, constraints);
		constraints.gridx = 0;
		
		// JAR PANEL
		_jarPanel.add(_jarCheckBox, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 400;
		_jarPanel.add(_jarTextField, constraints);
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_jarPanel.add(_jarBrowseButton, constraints);
		
		// BUTTON PANEL
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		
		// FRAME
		constraints.gridx = 0;
		_frame.add(_javaPanel, constraints);
		constraints.gridy = 1;
		_frame.add(_javacPanel, constraints);
		constraints.gridy = 2;
		_frame.add(_jarPanel, constraints);
		constraints.gridy = 3;
		constraints.fill = GridBagConstraints.NONE;
		_frame.add(_buttonPanel, constraints);
		_frame.pack();
		
		if (javaPath.equals("null")) {
			_javaCheckBox.setSelected(false);
			_javaTextField.setText("");
			_javaTextField.setEnabled(false);
			_javaBrowseButton.setEnabled(false);
		} else {
			_javaCheckBox.setSelected(true);
			_javaTextField.setText(javaPath);
			_javaTextField.setEnabled(true);
			_javaBrowseButton.setEnabled(true);
		}
		
		if (javacPath.equals("null")) {
			_javacCheckBox.setSelected(false);
			_javacTextField.setText("");
			_javacTextField.setEnabled(false);
			_javacBrowseButton.setEnabled(false);
		} else {
			_javacCheckBox.setSelected(true);
			_javacTextField.setText(javacPath);
			_javacTextField.setEnabled(true);
			_javacBrowseButton.setEnabled(true);
		}
		if (jarPath.equals("null")) {
			_jarCheckBox.setSelected(false);
			_jarTextField.setText("");
			_jarTextField.setEnabled(false);
			_jarBrowseButton.setEnabled(false);
		} else {
			_jarCheckBox.setSelected(true);
			_jarTextField.setText(jarPath);
			_jarTextField.setEnabled(true);
			_jarBrowseButton.setEnabled(true);
		}
		
		_frame.setLocationRelativeTo(null);
		_frame.setResizable(false);
		_frame.setVisible(true);
		_logger.info(labels.getString("s923"));
	}

	/**
	 * Set the listeners.
	 */
	public void setListeners() {
		
		// JAVA CHECK BOX
		_javaCheckBox.addActionListener(new JavaCheckBoxListener());
		
		// JAVAC CHECK BOX
		_javacCheckBox.addActionListener(new JavacCheckBoxListener());
		
		// JAR CHECK BOX
		_jarCheckBox.addActionListener(new JarCheckBoxListener());
		
		// JAVA BROWSE BUTTON
		_javaBrowseButton.addActionListener(new JavaBrowseButtonListener());
		
		// JAVAC BROWSE BUTTON
		_javacBrowseButton.addActionListener(new JavacBrowseButtonListener());
		
		// JAR BROWSE BUTTON
		_jarBrowseButton.addActionListener(new JarBrowseButtonListener());
		
		// OK BUTTON
		_acceptButton.addActionListener(new AcceptButtonListener());
		
		// BUTTON CANCEL
		_cancelButton.addActionListener(new CancelButtonListener());		
		ActionListener escPressed = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
			}
		};
		_cancelButton.registerKeyboardAction(escPressed, "EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);
	}
	
	/**
	 * Cancel button listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class CancelButtonListener implements ActionListener{

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			// GET THE LANGUAGE
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			
			// GET THE LABELS
			final ResourceBundle labels = language.getLabels();
			
			_frame.dispose();
			_logger.info(labels.getString("s924"));
		}
	}
	
	/**
	 * Accept button listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class AcceptButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			// GET THE LANGUAGE
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			
			// GET THE LABELS
			final ResourceBundle labels = language.getLabels();
			
			String java = _javaTextField.getText();
			String javac = _javacTextField.getText();
			String jar = _jarTextField.getText();
			
			if (java.equals(""))
				PropertiesManager.setProperty("javaPath", "null");
			else
				PropertiesManager.setProperty("javaPath", java);
			if (javac.equals(""))
				PropertiesManager.setProperty("javacPath", "null");
			else
				PropertiesManager.setProperty("javacPath", javac);
			if (jar.equals(""))
				PropertiesManager.setProperty("jarPath", "null");
			else
				PropertiesManager.setProperty("jarPath", jar);
			_logger.info(labels.getString("s925"));
			_frame.dispose();
		}
	}
	
	/**
	 * Jar browse button listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class JarBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			TextFile f = new TextFile();
			String path = f.read();
			_jarTextField.setText(path);
		}
	}
	
	/**
	 * Javac browse button listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class JavacBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			TextFile f = new TextFile();
			String path = f.read();
			_javacTextField.setText(path);
		}
	}
	
	/**
	 * Java browse button listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class JavaBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			TextFile f = new TextFile();
			String path = f.read();
			_javaTextField.setText(path);
		}
	}
	
	/**
	 * Jar check box listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class JarCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			if (_jarCheckBox.isSelected()) {
				if (_jarPath.equals("null"))
					_jarTextField.setText("");
				else
					_jarTextField.setText(_jarPath);
				_jarTextField.setEnabled(true);
				_jarBrowseButton.setEnabled(true);
			} else {
				_jarTextField.setText("");
				_jarTextField.setEnabled(false);
				_jarBrowseButton.setEnabled(false);
			}
		}
	}
	
	/**
	 * Javac check box listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class JavacCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			if (_javacCheckBox.isSelected()) {
				if (_javacPath.equals("null"))
					_javacTextField.setText("");
				else
					_javacTextField.setText(_javacPath);
				_javacTextField.setEnabled(true);
				_javacBrowseButton.setEnabled(true);
			} else {
				_javacTextField.setText("");
				_javacTextField.setEnabled(false);
				_javacBrowseButton.setEnabled(false);
			}
		}
	}
	
	/**
	 * Java check box listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class JavaCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
						
			if (_javaCheckBox.isSelected()) {
				if (_javaPath.equals("null"))
					_javaTextField.setText("");
				else
					_javaTextField.setText(_javaPath);
				_javaTextField.setEnabled(true);
				_javaBrowseButton.setEnabled(true);
			} else {
				_javaTextField.setText("");
				_javaTextField.setEnabled(false);
				_javaBrowseButton.setEnabled(false);
			}
		}
	}
}

