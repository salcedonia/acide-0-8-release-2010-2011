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

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

import acide.files.AcideFileManager;
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
	private final JTextField _javaTextField;
	/**
	 * ACIDE - A Configurable IDE paths configuration window java check box.
	 */
	private final JCheckBox _javaCheckBox;
	/**
	 * ACIDE - A Configurable IDE paths configuration window java browse button.
	 */
	private final JButton _javaBrowseButton;
	/**
	 * ACIDE - A Configurable IDE paths configuration window javac text field.
	 */
	private final JTextField _javacTextField;
	/**
	 * ACIDE - A Configurable IDE paths configuration window javac check box.
	 */
	private final JCheckBox _javacCheckBox;
	/**
	 * ACIDE - A Configurable IDE paths configuration window javc browse button.
	 */
	private final JButton _javacBrowseButton;
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar text field.
	 */
	private final JTextField _jarTextField;
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar check box.
	 */
	private final JCheckBox _jarCheckBox;
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar browse button.
	 */
	private final JButton _jarBrowseButton;
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
	private final String _javaPath;
	/**
	 * ACIDE - A Configurable IDE paths configuration window javac path string.
	 */
	private final String _javacPath;
	/**
	 * ACIDE - A Configurable IDE paths configuration window jar path string.
	 */
	private final String _jarPath;

	/**
	 * Creates a new ACIDE - A Configurable IDE paths configuration window.
	 */
	public AcidePathsConfigurationWindow() {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s914"));

		// Sets the layout
		setLayout(new GridBagLayout());

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);
		
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

		// Gets the paths
		String javaPath = null;
		String javacPath = null;
		String jarPath = null;

		try {
			// Gets the ACIDE - A Configurable IDE java path
			javaPath = AcideResourceManager.getInstance().getProperty(
					"javaPath");

			// Gets the ACIDE - A Configurable IDE javac path
			javacPath = AcideResourceManager.getInstance().getProperty(
					"javacPath");

			// Gets the ACIDE - A Configurable IDE jar path
			jarPath = AcideResourceManager.getInstance().getProperty("jarPath");
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}

		_javaPath = javaPath;
		_javacPath = javacPath;
		_jarPath = jarPath;

		// Listeners
		setListeners();

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
		
		// When the escape key is pressed the cancel button will execute its action listener
		_cancelButton.registerKeyboardAction(new EscapeKeyAction(),
				"EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);
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
	
			// Set the main window enabled again
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
			
			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);
			
			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);

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

			String java = _javaTextField.getText();
			String javac = _javacTextField.getText();
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
			
			// Set the main window enabled again
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
			
			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);
			
			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);
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

			// Ask the path to the user
			String path = AcideFileManager.getInstance().askAbsolutePath();
			_jarTextField.setText(path);
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

			// Asks the path to the user
			String path = AcideFileManager.getInstance().askAbsolutePath();
			_javacTextField.setText(path);
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

			// Asks the path to the user
			String path = AcideFileManager.getInstance().askAbsolutePath();
			_javaTextField.setText(path);
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
			
			// Set the main window enabled again
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();
			
			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);
			
			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);
		}
	}
}