/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package gui.menuBar.configurationMenu.grammarMenu.gui;

import java.awt.Color;
import java.awt.Font;
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
import javax.swing.border.TitledBorder;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;


import es.text.TextFile;

/**																
 * Paths configuration window of ACIDE - A Configurable IDE.
 *					
 * @version 0.8	
 * @see JFrame																													
 */
public class PathsConfigurationWindow extends JFrame{

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the window icon.
	 */
	private static final String ICON = "./resources/images/icon.png";
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
	 * Class constructor.
	 */
	public PathsConfigurationWindow() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s914"));
		
		// FRAME
		setTitle(labels.getString("s913"));
		setIconImage(new ImageIcon(ICON).getImage());
		setLayout(new GridBagLayout());
		
		// JAVA PANEL
		_javaPanel = new JPanel();
		_javaPanel.setLayout(new GridBagLayout());
		_javaPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s915"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
	
		// JAVAC PANEL
		_javacPanel = new JPanel();
		_javacPanel.setLayout(new GridBagLayout());
		_javacPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s916"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		
		// JAR PANEL
		_jarPanel = new JPanel();
		_jarPanel.setLayout(new GridBagLayout());
		_jarPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s917"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		
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
			javaPath = AcideResourceManager.getInstance().getProperty("javaPath");
			javacPath = AcideResourceManager.getInstance().getProperty("javacPath");
			jarPath = AcideResourceManager.getInstance().getProperty("jarPath");
		}
		catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
		
		_javaPath = javaPath;
		_javacPath = javacPath;
		_jarPath = jarPath;
		
		// Listeners
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
		add(_javaPanel, constraints);
		constraints.gridy = 1;
		add(_javacPanel, constraints);
		constraints.gridy = 2;
		add(_jarPanel, constraints);
		constraints.gridy = 3;
		constraints.fill = GridBagConstraints.NONE;
		add(_buttonPanel, constraints);
		pack();
		
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
		
		setLocationRelativeTo(null);
		setResizable(false);
		setVisible(true);
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s923"));
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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				dispose();
			}
		};
		_cancelButton.registerKeyboardAction(escPressed, "EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);
	}
	
	/************************************************************************																
	 * Cancel button listener.									
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
	 * @see ActionListener																													
	 ***********************************************************************/
	class CancelButtonListener implements ActionListener{

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			
			// Gets the language
			AcideLanguageManager language = AcideLanguageManager.getInstance();
			
			try {
				language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
			} catch (Exception exception) {
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
			
			// Gets the labels
			final ResourceBundle labels = language.getLabels();
			
			dispose();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s924"));
		}
	}
	
	/************************************************************************																
	 * Accept button listener.
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
	 * @see ActionListener																													
	 ***********************************************************************/
	class AcceptButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			
			// Gets the language
			AcideLanguageManager language = AcideLanguageManager.getInstance();
			
			try {
				language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
			} catch (Exception exception) {
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
			
			// Gets the labels
			final ResourceBundle labels = language.getLabels();
			
			String java = _javaTextField.getText();
			String javac = _javacTextField.getText();
			String jar = _jarTextField.getText();
			
			// Updates the RESOURCE MANAGER
			if (java.equals(""))
				AcideResourceManager.getInstance().setProperty("javaPath", "null");
			else
				AcideResourceManager.getInstance().setProperty("javaPath", java);
			if (javac.equals(""))
				AcideResourceManager.getInstance().setProperty("javacPath", "null");
			else
				AcideResourceManager.getInstance().setProperty("javacPath", javac);
			if (jar.equals(""))
				AcideResourceManager.getInstance().setProperty("jarPath", "null");
			else
				AcideResourceManager.getInstance().setProperty("jarPath", jar);
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s925"));
			
			dispose();
		}
	}
	
	/************************************************************************																
	 * Jar browse button listener.									
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
	 * @see ActionListener																													
	 ***********************************************************************/
	class JarBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			TextFile f = new TextFile();
			String path = f.read();
			_jarTextField.setText(path);
		}
	}
	
	/************************************************************************																
	 * Javac browse button listener.
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
	 * @see ActionListener																													
	 ***********************************************************************/
	class JavacBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			TextFile f = new TextFile();
			String path = f.read();
			_javacTextField.setText(path);
		}
	}
	
	/************************************************************************																
	 * Java browse button listener.
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
	 * @see ActionListener																													
	 ***********************************************************************/
	class JavaBrowseButtonListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			TextFile f = new TextFile();
			String path = f.read();
			_javaTextField.setText(path);
		}
	}
	
	/************************************************************************																
	 * Jar check box listener.
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
	 * @see ActionListener																													
	 ***********************************************************************/
	class JarCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			
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
	
	/************************************************************************																
	 * Javac check box listener.
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
	 * @see ActionListener																													
	 ***********************************************************************/
	class JavacCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
			
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
	
	/************************************************************************																
	 * Java check box listener.									
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
	 * @see ActionListener																													
	 ***********************************************************************/
	class JavaCheckBoxListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {
						
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