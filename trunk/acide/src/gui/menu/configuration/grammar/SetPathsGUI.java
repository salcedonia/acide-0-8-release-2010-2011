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
 * 
 */
public class SetPathsGUI {

	/**
	 * 
	 */
	private JFrame _frame;
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private JPanel _javaPanel;
	/**
	 * 
	 */
	private JPanel _jarPanel;
	/**
	 * 
	 */
	private JPanel _javacPanel;
	/**
	 * 
	 */
	private JPanel _buttonPanel;
	/**
	 * 
	 */
	private final JTextField _tfJava;
	/**
	 * 
	 */
	private final JCheckBox _chbJava;
	/**
	 * 
	 */
	private final JButton _btnJavaBrowse;
	/**
	 * 
	 */
	private final JTextField _tfJavac;
	/**
	 * 
	 */
	private final JCheckBox _chbJavac;
	/**
	 * 
	 */
	private final JButton _btnJavacBrowse;
	/**
	 * 
	 */
	private final JTextField _tfJar;
	/**
	 * 
	 */
	private final JCheckBox _chbJar;
	/**
	 * 
	 */
	private final JButton _btnJarBrowse;
	/**
	 * 
	 */
	private JButton _btnOk;
	/**
	 * 
	 */
	private JButton _btnCancel;
	/**
	 * 
	 */
	private final String _javaPath;
	/**
	 * 
	 */
	private final String _javacPath;
	/**
	 * 
	 */
	private final String _jarPath;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public SetPathsGUI() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
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
		_tfJava = new JTextField();
		_chbJava = new JCheckBox();
		_btnJavaBrowse = new JButton(labels.getString("s920"));
		
		// JAVAC
		_tfJavac = new JTextField();
		_chbJavac = new JCheckBox();
		_btnJavacBrowse = new JButton(labels.getString("s921"));
		
		// JAR
		_tfJar = new JTextField();
		_chbJar = new JCheckBox();
		_btnJarBrowse = new JButton(labels.getString("s922"));
		
		// BUTTON
		_btnOk = new JButton(labels.getString("s918"));
		_btnCancel = new JButton(labels.getString("s919"));
		
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
		_javaPanel.add(_chbJava, constraints);	
		constraints.gridx = 1;
		constraints.ipadx = 400;
		_javaPanel.add(_tfJava, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 2;
		_javaPanel.add(_btnJavaBrowse, constraints);
		constraints.gridx = 0;
		
		// JAVAC PANEL
		_javacPanel.add(_chbJavac, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 400;
		_javacPanel.add(_tfJavac, constraints);
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_javacPanel.add(_btnJavacBrowse, constraints);
		constraints.gridx = 0;
		
		// JAR PANEL
		_jarPanel.add(_chbJar, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 400;
		_jarPanel.add(_tfJar, constraints);
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_jarPanel.add(_btnJarBrowse, constraints);
		
		// BUTTON PANEL
		_buttonPanel.add(_btnOk);
		_buttonPanel.add(_btnCancel);
		
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
			_chbJava.setSelected(false);
			_tfJava.setText("");
			_tfJava.setEnabled(false);
			_btnJavaBrowse.setEnabled(false);
		} else {
			_chbJava.setSelected(true);
			_tfJava.setText(javaPath);
			_tfJava.setEnabled(true);
			_btnJavaBrowse.setEnabled(true);
		}
		
		if (javacPath.equals("null")) {
			_chbJavac.setSelected(false);
			_tfJavac.setText("");
			_tfJavac.setEnabled(false);
			_btnJavacBrowse.setEnabled(false);
		} else {
			_chbJavac.setSelected(true);
			_tfJavac.setText(javacPath);
			_tfJavac.setEnabled(true);
			_btnJavacBrowse.setEnabled(true);
		}
		if (jarPath.equals("null")) {
			_chbJar.setSelected(false);
			_tfJar.setText("");
			_tfJar.setEnabled(false);
			_btnJarBrowse.setEnabled(false);
		} else {
			_chbJar.setSelected(true);
			_tfJar.setText(jarPath);
			_tfJar.setEnabled(true);
			_btnJarBrowse.setEnabled(true);
		}
		
		_frame.setLocationRelativeTo(null);
		_frame.setResizable(false);
		_frame.setVisible(true);
		_logger.info(labels.getString("s923"));
	}

	/**
	 * 
	 */
	public void setListeners() {
		
		// JAVA CHECK BOX
		_chbJava.addActionListener(new JavaCheckBoxListener());
		
		// JAVAC CHECK BOX
		_chbJavac.addActionListener(new JavacCheckBoxListener());
		
		// JAR CHECK BOX
		_chbJar.addActionListener(new JarCheckBoxListener());
		
		// JAVA BROWSE BUTTON
		_btnJavaBrowse.addActionListener(new JavaBrowseButtonListener());
		
		// JAVAC BROWSE BUTTON
		_btnJavacBrowse.addActionListener(new JavacBrowseButtonListener());
		
		// JAR BROWSE BUTTON
		_btnJarBrowse.addActionListener(new JarBrowseButtonListener());
		
		// OK BUTTON
		_btnOk.addActionListener(new OkButtonListener());
		
		// BUTTON CANCEL
		_btnCancel.addActionListener(new CancelButtonListener());		
		ActionListener escPressed = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
			}
		};
		_btnCancel.registerKeyboardAction(escPressed, "EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);
	}
	
	/**
	 * 
	 */
	class CancelButtonListener implements ActionListener{

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			final ResourceBundle labels = language.getLabels();
			
			_frame.dispose();
			_logger.info(labels.getString("s924"));
		}
	}
	
	class OkButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			final ResourceBundle labels = language.getLabels();
			
			String java = _tfJava.getText();
			String javac = _tfJavac.getText();
			String jar = _tfJar.getText();
			
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
	 * 
	 */
	class JarBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			TextFile f = new TextFile();
			String path = f.read();
			_tfJar.setText(path);
		}
	}
	
	/**
	 * 
	 */
	class JavacBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			TextFile f = new TextFile();
			String path = f.read();
			_tfJavac.setText(path);
		}
	}
	
	/**
	 * 
	 */
	class JavaBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			TextFile f = new TextFile();
			String path = f.read();
			_tfJava.setText(path);
		}
	}
	
	/**
	 * 
	 */
	class JarCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			if (_chbJar.isSelected()) {
				if (_jarPath.equals("null"))
					_tfJar.setText("");
				else
					_tfJar.setText(_jarPath);
				_tfJar.setEnabled(true);
				_btnJarBrowse.setEnabled(true);
			} else {
				_tfJar.setText("");
				_tfJar.setEnabled(false);
				_btnJarBrowse.setEnabled(false);
			}
		}
	}
	
	/**
	 * 
	 */
	class JavacCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			if (_chbJavac.isSelected()) {
				if (_javacPath.equals("null"))
					_tfJavac.setText("");
				else
					_tfJavac.setText(_javacPath);
				_tfJavac.setEnabled(true);
				_btnJavacBrowse.setEnabled(true);
			} else {
				_tfJavac.setText("");
				_tfJavac.setEnabled(false);
				_btnJavacBrowse.setEnabled(false);
			}
		}
	}
	
	/**
	 * 
	 */
	class JavaCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
						
			if (_chbJava.isSelected()) {
				if (_javaPath.equals("null"))
					_tfJava.setText("");
				else
					_tfJava.setText(_javaPath);
				_tfJava.setEnabled(true);
				_btnJavaBrowse.setEnabled(true);
			} else {
				_tfJava.setText("");
				_tfJava.setEnabled(false);
				_btnJavaBrowse.setEnabled(false);
			}
		}
	}
}

