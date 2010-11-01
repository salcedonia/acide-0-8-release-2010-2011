package gui.menu.configuration.output;

import es.text.TextFile;
import gui.MainWindow;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import java.util.StringTokenizer;

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import language.Language;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * Output GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class OutputGUI {

	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Frame of the window.
	 */
	private JFrame _frame;
	/**
	 * Main panel of the window.
	 */
	private JPanel _mainPanel;
	/**
	 * Echo command label.
	 */
	private JLabel _echoCommandLabel;
	/**
	 * Exec label.
	 */
	private JLabel _execLabel;
	/**
	 * Output command text field.
	 */
	private JLabel _outputCommandLabel;
	/**
	 * Exec path label.
	 */
	private final JLabel _execPathLabel;
	/**
	 * Exce text field.
	 */
	private final JTextField _execTextField;
	/**
	 * Output command text field.
	 */
	private final JTextField _outputCommandTextField;
	/**
	 * Exec path text field.
	 */
	private final JTextField _execPathTextField;
	/**
	 * Manual path label.
	 */
	private JLabel _manualPathLabel;
	/**
	 * Manual path label.
	 */
	private final JCheckBox _manualPathCheckBox;
	/**
	 * Echo command check box.
	 */
	private final JCheckBox _echoCommandCheckBox;
	/**
	 * Apply button.
	 */
	private JButton _applyButton;
	/**
	 * Examine button.
	 */
	private JButton _examineButton;
	/**
	 * Examine 2 button. 
	 */
	private final JButton _examine2Button;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public OutputGUI() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();

		_logger.info(labels.getString("s331"));
		
		// FRAME 
		_frame = new JFrame();
		_frame.setSize(new Dimension(500, 250));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setTitle(labels.getString("s334"));

		// PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());
		
		// EXEC PATH
		_execPathLabel = new JLabel(labels.getString("s337"),
				JLabel.LEFT);
		_execPathTextField = new JTextField();
		_execPathLabel.setEnabled(false);
		_execPathTextField.setEnabled(false);
		
		// MANUAL PATH
		_manualPathLabel = new JLabel(labels.getString("s350"),
				JLabel.LEFT);
		_manualPathCheckBox = new JCheckBox();
		
		// EXEC
		_execLabel = new JLabel(labels.getString("s338"), JLabel.LEFT);
		_execTextField = new JTextField();
		
		// OUTPUT COMMAND
		_outputCommandLabel = new JLabel(labels.getString("s339"), JLabel.LEFT);
		_outputCommandTextField = new JTextField();
		
		// ECHO COMMAND
		_echoCommandLabel = new JLabel(labels.getString("s340"), JLabel.LEFT);
		_echoCommandCheckBox = new JCheckBox();
		try {
			_execPathTextField.setText(PropertiesManager.getProperty("execPath"));
			_execTextField.setText(PropertiesManager.getProperty("exec"));
			_outputCommandTextField.setText(PropertiesManager
					.getProperty("exitCommand"));
			_echoCommandCheckBox.setSelected(Boolean.parseBoolean(PropertiesManager
					.getProperty("echoCommand")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// APPLY BUTTON
		_applyButton = new JButton(labels.getString("s335"));
		_applyButton.setVerticalTextPosition(AbstractButton.CENTER);
		_applyButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_applyButton.setMnemonic(KeyEvent.VK_A);
		_applyButton.setToolTipText(labels.getString("s336"));
		
		// EXAMINE BUTTON
		_examineButton = new JButton(labels.getString("s142"));
		_examineButton.setToolTipText(labels.getString("s301"));
		
		// EXAMINE2 BUTTON
		_examine2Button = new JButton(labels.getString("s142"));
		_examine2Button.setToolTipText(labels.getString("s301"));
		
		// LISTENERS
		_examineButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				TextFile file = new TextFile();
				String path = file.read();
				_execTextField.setText(path);
			}
		});
		
		_examine2Button.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				String path = "";
				JFileChooser fc = new JFileChooser();
				fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				int option = fc.showOpenDialog(null);

				if (option == JFileChooser.APPROVE_OPTION)
					path = fc.getSelectedFile().getAbsolutePath();

				_execPathTextField.setText(path);
			}
		});
		_examine2Button.setEnabled(false);
		
		_manualPathCheckBox.addItemListener(new ItemListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
			 */
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					_execPathLabel.setEnabled(true);
					_execPathTextField.setEnabled(true);
					_examine2Button.setEnabled(true);
				} else {
					_execPathLabel.setEnabled(false);
					_execPathTextField.setEnabled(false);
					_examine2Button.setEnabled(false);
				}
			}
		});
		
		_applyButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				try {
					
					MainWindow.getInstance().getOutput().executeExitCommand();
					
					// UPDATES THE EXEC PATH
					if (_execPathTextField.isEnabled()) {
						PropertiesManager.setProperty("execPath",
								_execPathTextField.getText());
						MainWindow.getInstance().getProjectConfiguration()
								.setShellDirectory(_execPathTextField.getText());
					} else {
						
						String calculatedPath = "";
						String execTextField = _execTextField.getText();
						StringTokenizer stringTokenizer = new StringTokenizer(execTextField, "\\");
						
						int limit = stringTokenizer.countTokens();
						for (int i = 0; i < limit - 1; i++) {
							calculatedPath = calculatedPath + stringTokenizer.nextToken()
									+ "\\";
						}
						
						PropertiesManager
								.setProperty("execPath", calculatedPath);
						MainWindow.getInstance().getProjectConfiguration()
								.setShellDirectory(calculatedPath);
					}
					
					// UPDATES THE PROPERTIES 
					PropertiesManager.setProperty("exec", _execTextField.getText());
					PropertiesManager.setProperty("echoCommand",
							Boolean.toString(_echoCommandCheckBox.isSelected()));
					PropertiesManager.setProperty("exitCommand",
							_outputCommandTextField.getText());
					
					// UPDATE THE PROJECT CONFIGURATION
					MainWindow.getInstance().getOutput().resetOutput();
					MainWindow.getInstance().getProjectConfiguration()
							.setShellPath(_execTextField.getText());
					MainWindow
							.getInstance()
							.getProjectConfiguration()
							.setEchoCommand(
									Boolean.toString(_echoCommandCheckBox
											.isSelected()));
					MainWindow.getInstance().getProjectConfiguration()
							.setExitCommand(_outputCommandTextField.getText());

					// SAVE THE DEFAULT CONFIGURATION
					String project = null;
					try {
						project = PropertiesManager
								.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if (!(project.equals("./configuration/default.acidePrj") && MainWindow
							.getInstance().getProjectConfiguration().getName()
							.equals(""))) {
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);
					}

					if (MainWindow.getInstance().getProjectGUI() != null)
						MainWindow.getInstance().getProjectGUI().setAreShellPathsDefined(true);

				} catch (Exception e1) {
					e1.printStackTrace();
				}
				_frame.dispose();
			}
		});

		ActionListener actionListener = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
			}
		};

		_applyButton.registerKeyboardAction(actionListener, "EscapeKey", KeyStroke.getKeyStroke(
				java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// SET THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.HORIZONTAL;
		
		// EXEC
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_execLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_execTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examineButton, constraints);

		constraints.ipadx = 0;
		constraints.insets = new Insets(10, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_manualPathLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_manualPathCheckBox, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;
		_mainPanel.add(_execPathLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_execPathTextField, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examine2Button, constraints);

		constraints.gridx = 0;
		constraints.gridy = 3;
		_mainPanel.add(_outputCommandLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_outputCommandTextField, constraints);

		constraints.gridx = 0;
		constraints.gridy = 4;
		_mainPanel.add(_echoCommandLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_echoCommandCheckBox, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 5;
		_mainPanel.add(_applyButton, constraints);

		_frame.add(_mainPanel);
		_frame.setVisible(true);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setLocationRelativeTo(null);
	}
}
