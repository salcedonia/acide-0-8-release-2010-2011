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
package acide.gui.menuBar.configurationMenu.grammarMenu.gui;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.border.TitledBorder;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE paths configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcidePathsConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE paths configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE paths configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE paths configuration window java panel.
	 */
	private JPanel _javaPanel;
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar panel.
	 */
	private JPanel _jarPanel;
	/**
	 * ACIDE - A Configurable IDE paths configuration window javac panel.
	 */
	private JPanel _javacPanel;
	/**
	 * ACIDE - A Configurable IDE paths configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE paths configuration window java text field.
	 */
	private JTextField _javaTextField;
	/**
	 * ACIDE - A Configurable IDE paths configuration window java check box.
	 */
	private JCheckBox _javaCheckBox;
	/**
	 * ACIDE - A Configurable IDE paths configuration window java browse button.
	 */
	private JButton _javaBrowseButton;
	/**
	 * ACIDE - A Configurable IDE paths configuration window javac text field.
	 */
	private JTextField _javacTextField;
	/**
	 * ACIDE - A Configurable IDE paths configuration window javac check box.
	 */
	private JCheckBox _javacCheckBox;
	/**
	 * ACIDE - A Configurable IDE paths configuration window javc browse button.
	 */
	private JButton _javacBrowseButton;
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar text field.
	 */
	private JTextField _jarTextField;
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar check box.
	 */
	private JCheckBox _jarCheckBox;
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar browse button.
	 */
	private JButton _jarBrowseButton;
	/**
	 * ACIDE - A Configurable IDE paths configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE paths configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE paths configuration window java path string.
	 */
	private String _javaPath = "null";
	/**
	 * ACIDE - A Configurable IDE paths configuration window javac path string.
	 */
	private String _javacPath = "null";
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar path string.
	 */
	private String _jarPath = "null";

	/**
	 * Creates a new ACIDE - A Configurable IDE paths configuration window.
	 */
	public AcidePathsConfigurationWindow() {

		super();

		try {
			// Gets the ACIDE - A Configurable IDE java path
			_javaPath = AcideResourceManager.getInstance().getProperty(
					"javaPath");

			// Gets the ACIDE - A Configurable IDE javac path
			_javacPath = AcideResourceManager.getInstance().getProperty(
					"javacPath");

			// Gets the ACIDE - A Configurable IDE jar path
			_jarPath = AcideResourceManager.getInstance()
					.getProperty("jarPath");
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}

		// Builds the window components
		buildComponents();

		// Sets the window components listeners
		setListeners();

		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s914"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE paths configuration window
	 * components.
	 */
	private void buildComponents() {

		// Creates the java panel
		_javaPanel = new JPanel(new GridBagLayout());

		// Sets the java panel border
		_javaPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s915"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));

		// Creates the javac panel
		_javacPanel = new JPanel(new GridBagLayout());

		// Sets the javac panel border
		_javacPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s916"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));

		// Creates the jar panel
		_jarPanel = new JPanel(new GridBagLayout());

		// Sets the jar panel border
		_jarPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s917"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the java text field
		_javaTextField = new JTextField();

		// Creates the java check box
		_javaCheckBox = new JCheckBox();

		// Creates the java browse button
		_javaBrowseButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s920"));

		// Creates the javac text field
		_javacTextField = new JTextField();

		// Creates the javac check box
		_javacCheckBox = new JCheckBox();

		// Creates the javac browse button
		_javacBrowseButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s921"));

		// Creates the jar text field
		_jarTextField = new JTextField();

		// Creates the jar check box
		_jarCheckBox = new JCheckBox();

		// Creates the jar browse button
		_jarBrowseButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s922"));

		// Creates the accept button
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s918"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s919"));

		if (_javaPath.equals("null")) {
			_javaCheckBox.setSelected(false);
			_javaTextField.setText("");
			_javaTextField.setEnabled(false);
			_javaBrowseButton.setEnabled(false);
		} else {
			_javaCheckBox.setSelected(true);
			_javaTextField.setText(_javaPath);
			_javaTextField.setEnabled(true);
			_javaBrowseButton.setEnabled(true);
		}

		if (_javacPath.equals("null")) {
			_javacCheckBox.setSelected(false);
			_javacTextField.setText("");
			_javacTextField.setEnabled(false);
			_javacBrowseButton.setEnabled(false);
		} else {
			_javacCheckBox.setSelected(true);
			_javacTextField.setText(_javacPath);
			_javacTextField.setEnabled(true);
			_javacBrowseButton.setEnabled(true);
		}
		if (_jarPath.equals("null")) {
			_jarCheckBox.setSelected(false);
			_jarTextField.setText("");
			_jarTextField.setEnabled(false);
			_jarBrowseButton.setEnabled(false);
		} else {
			_jarCheckBox.setSelected(true);
			_jarTextField.setText(_jarPath);
			_jarTextField.setEnabled(true);
			_jarBrowseButton.setEnabled(true);
		}
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE paths configuration
	 * window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);

		// Adds the java check box to the java panel
		_javaPanel.add(_javaCheckBox, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 400;

		// Adds the java text field to the java panel
		_javaPanel.add(_javaTextField, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 2;

		// Adds the java browse button to the java panel
		_javaPanel.add(_javaBrowseButton, constraints);

		constraints.gridx = 0;

		// Adds the javac check box to the javac panel
		_javacPanel.add(_javacCheckBox, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 400;

		// Adds the javac text field to the javac panel
		_javacPanel.add(_javacTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;

		// Adds the javac browse button to the javac panel
		_javacPanel.add(_javacBrowseButton, constraints);

		constraints.gridx = 0;

		// Adds the jar check box to the jar panel
		_jarPanel.add(_jarCheckBox, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 400;

		// Adds the jar text field to the jar panel
		_jarPanel.add(_jarTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;

		// Adds the jar browse button to the jar panel
		_jarPanel.add(_jarBrowseButton, constraints);

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);

		constraints.gridx = 0;

		// Adds the java panel to the window
		add(_javaPanel, constraints);

		constraints.gridy = 1;

		// Adds the javac panel to the window
		add(_javacPanel, constraints);

		constraints.gridy = 2;

		// Adds the jar panel to the window
		add(_jarPanel, constraints);

		constraints.gridy = 3;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE paths configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s913"));

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// The window is not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Displays the window
		setVisible(true);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s923"));
	}

	/**
	 * Set the listeners for the different window components.
	 */
	public void setListeners() {

		// Sets the java check box action listener
		_javaCheckBox.addActionListener(new JavaCheckBoxListener());

		// Sets the javac check box action listener
		_javacCheckBox.addActionListener(new JavacCheckBoxListener());

		// Sets the jar check box action listener
		_jarCheckBox.addActionListener(new JarCheckBoxListener());

		// Sets the java browse button action listener
		_javaBrowseButton.addActionListener(new JavaBrowseButtonListener());

		// Sets the javac browse button action listener
		_javacBrowseButton.addActionListener(new JavacBrowseButtonListener());

		// Sets the jar browse button action listener
		_jarBrowseButton.addActionListener(new JarBrowseButtonListener());

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonListener());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonListener());

		// When the escape key is pressed the cancel button will execute its
		// action listener
		_cancelButton.registerKeyboardAction(new EscapeKeyAction(),
				"EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
	}

	/**
	 * Closes the ACIDE - A Configurable IDE paths configuration window.
	 */
	private void closeWindow() {

		// Set the main window enabled again
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window cancel button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			closeWindow();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s924"));
		}
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window accept button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the text from the java text field
			String java = _javaTextField.getText();
			
			// Gets the text from the javac text field
			String javac = _javacTextField.getText();
			
			// Gets the text from the jar text field
			String jar = _jarTextField.getText();

			if (java.equals(""))

				// Updates the ACIDE - A Configurable IDE java path
				AcideResourceManager.getInstance().setProperty("javaPath",
						"null");
			else

				// Updates the ACIDE - A Configurable IDE java path
				AcideResourceManager.getInstance()
						.setProperty("javaPath", java);
			
			if (javac.equals(""))

				// Updates the ACIDE - A Configurable IDE javac path
				AcideResourceManager.getInstance().setProperty("javacPath",
						"null");
			else
				// Updates the ACIDE - A Configurable IDE javac path
				AcideResourceManager.getInstance().setProperty("javacPath",
						javac);
			
			if (jar.equals(""))

				// Updates the ACIDE - A Configurable IDE jar path
				AcideResourceManager.getInstance().setProperty("jarPath",
						"null");
			else
				// Updates the ACIDE - A Configurable IDE jar path
				AcideResourceManager.getInstance().setProperty("jarPath", jar);

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s925"));

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window jar browse button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class JarBrowseButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			File selectedFile = null;
			String absolutePath = null;
			String lastPath = null;

			// Creates the file chooser
			JFileChooser _fileChooser = new JFileChooser();

			try {

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedFileDirectory");

				// Sets the current directory to the default path
				_fileChooser.setCurrentDirectory(new File(lastPath));

				// Disables the multiple selection of files
				_fileChooser.setMultiSelectionEnabled(false);

				// Sets only files
				_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Ask for the file to the user
				int returnValue = _fileChooser.showOpenDialog(null);

				// If it is ok
				if (returnValue == JFileChooser.APPROVE_OPTION) {

					// Gets the selected file
					selectedFile = _fileChooser.getSelectedFile();

					// Gets the absolute path
					absolutePath = selectedFile.getAbsolutePath();

					if (absolutePath != null) {

						// Updates the jar text field with the absolute path
						_jarTextField.setText(absolutePath);

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s300")
										+ absolutePath);

						// Updates the ACIDE - A Configurable IDE last opened
						// file
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedFileDirectory", absolutePath);
					}
				} else if (returnValue == JFileChooser.CANCEL_OPTION) {

					// Cancels the selection
					_fileChooser.cancelSelection();

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s302"));
				}

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window javac browse button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class JavacBrowseButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			File selectedFile = null;
			String absolutePath = null;
			String lastPath = null;

			// Creates the file chooser
			JFileChooser _fileChooser = new JFileChooser();

			try {

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedFileDirectory");

				// Sets the current directory to the default path
				_fileChooser.setCurrentDirectory(new File(lastPath));

				// Disables the multiple selection of files
				_fileChooser.setMultiSelectionEnabled(false);

				// Sets only files
				_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Ask for the file to the user
				int returnValue = _fileChooser.showOpenDialog(null);

				// If it is ok
				if (returnValue == JFileChooser.APPROVE_OPTION) {

					// Gets the selected file
					selectedFile = _fileChooser.getSelectedFile();

					// Gets the absolute path
					absolutePath = selectedFile.getAbsolutePath();

					if (absolutePath != null) {

						// Updates the javac text field with te absolute path
						_javacTextField.setText(absolutePath);

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s300")
										+ absolutePath);

						// Updates the ACIDE - A Configurable IDE last opened
						// file
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedFileDirectory", absolutePath);
					}
				} else if (returnValue == JFileChooser.CANCEL_OPTION) {

					// Cancels the selection
					_fileChooser.cancelSelection();

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s302"));
				}

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window java browse button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class JavaBrowseButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			File selectedFile = null;
			String absolutePath = null;
			String lastPath = null;

			// Creates the file chooser
			JFileChooser _fileChooser = new JFileChooser();

			try {

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastPath = AcideResourceManager.getInstance().getProperty(
						"lastOpenedFileDirectory");

				// Sets the current directory to the default path
				_fileChooser.setCurrentDirectory(new File(lastPath));

				// Disables the multiple selection of files
				_fileChooser.setMultiSelectionEnabled(false);

				// Sets only files
				_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Ask for the file to the user
				int returnValue = _fileChooser.showOpenDialog(null);

				// If it is ok
				if (returnValue == JFileChooser.APPROVE_OPTION) {

					// Gets the selected file
					selectedFile = _fileChooser.getSelectedFile();

					// Gets the absolute path
					absolutePath = selectedFile.getAbsolutePath();

					if (absolutePath != null) {

						// Updates the java text field with the the absolute
						// path
						_javaTextField.setText(absolutePath);

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s300")
										+ absolutePath);

						// Updates the ACIDE - A Configurable IDE last opened
						// file
						// directory
						AcideResourceManager.getInstance().setProperty(
								"lastOpenedFileDirectory", absolutePath);
					}
				} else if (returnValue == JFileChooser.CANCEL_OPTION) {

					// Cancels the selection
					_fileChooser.cancelSelection();

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s302"));
				}

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window jar check box
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class JarCheckBoxListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			if (_jarCheckBox.isSelected()) {
				
				// If there is no jar path defined
				if (_jarPath.equals("null"))
					
					// Sets the jar text field as empty
					_jarTextField.setText("");
				else
					
					// Updates the jar text field with the jar path
					_jarTextField.setText(_jarPath);
				
				// Enables the jar text field
				_jarTextField.setEnabled(true);
				
				// Enables the jar browse button
				_jarBrowseButton.setEnabled(true);
			} else {
				
				// Sets the jar text field as empty
				_jarTextField.setText("");
				
				// Disables the jar text field 
				_jarTextField.setEnabled(false);
				
				// Disables the jar browse button
				_jarBrowseButton.setEnabled(false);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window javac check box
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class JavacCheckBoxListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			if (_javacCheckBox.isSelected()) {
				
				// If there is no javac path defined
				if (_javacPath.equals("null"))
					
					// Sets the javac text field as empty
					_javacTextField.setText("");
				else
					
					// Updates the javac text field with the javac path
					_javacTextField.setText(_javacPath);
				
				// Enables the javac text field
				_javacTextField.setEnabled(true);
				
				// Enables the javac browse button
				_javacBrowseButton.setEnabled(true);
			} else {
				
				// Sets the javac text field as empty
				_javacTextField.setText("");
				
				// Disables the javac text field
				_javacTextField.setEnabled(false);
				
				// Disables the javac browse button
				_javacBrowseButton.setEnabled(false);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window java check box
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class JavaCheckBoxListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			if (_javaCheckBox.isSelected()) {
				
				// If there is no java path defined
				if (_javaPath.equals("null"))
					
					// Sets the java text field as empty
					_javaTextField.setText("");
				else
					
					// Updates the java text field with the java path
					_javaTextField.setText(_javaPath);
				
				// Enables the java text field
				_javaTextField.setEnabled(true);
				
				// Enables the java browse button
				_javaBrowseButton.setEnabled(true);
			} else {
				
				// Sets the java text field as empty
				_javaTextField.setText("");
				
				// Disables the java text field
				_javaTextField.setEnabled(false);
				
				// Disables the java browse button
				_javaBrowseButton.setEnabled(false);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE paths configuration window escape key action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class EscapeKeyAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			closeWindow();
		}
	}
}