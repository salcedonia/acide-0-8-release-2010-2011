package gui.menu.configuration.output;

import es.configuration.output.OutputConfiguration;
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

import properties.PropertiesManager;

/**
 * Output GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class OutputGUI extends JFrame{

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Main panel of the window.
	 */
	private JPanel _mainPanel;
	/**
	 * Echo command label.
	 */
	private JLabel _echoCommandLabel;
	/**
	 * Shell path label.
	 */
	private JLabel _shellPathLabel;
	/**
	 * Exit command text field.
	 */
	private JLabel _exitCommandLabel;
	/**
	 * Shell directory label.
	 */
	private final JLabel _shellDirectoryLabel;
	/**
	 * Shell path text field.
	 */
	private final JTextField _shellPathTextField;
	/**
	 * Exit command text field.
	 */
	private final JTextField _exitCommandTextField;
	/**
	 * Shell directory text field.
	 */
	private final JTextField _shellDirectoryTextField;
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
	 * Constructor of the class.
	 */
	public OutputGUI() {

		super();
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
		ResourceBundle labels = language.getLabels();

		// UPDATES THE LOG
		Log.getLog().info(labels.getString("s331"));

		// PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());

		// SHELL DIRECTORY
		_shellDirectoryLabel = new JLabel(labels.getString("s337"), JLabel.LEFT);
		_shellDirectoryTextField = new JTextField();
		_shellDirectoryLabel.setEnabled(false);
		_shellDirectoryTextField.setEnabled(false);

		// MANUAL PATH
		_manualPathLabel = new JLabel(labels.getString("s350"), JLabel.LEFT);
		_manualPathCheckBox = new JCheckBox();

		// SHELL PATH
		_shellPathLabel = new JLabel(labels.getString("s338"), JLabel.LEFT);
		_shellPathTextField = new JTextField();

		// EXIT COMMAND
		_exitCommandLabel = new JLabel(labels.getString("s339"), JLabel.LEFT);
		_exitCommandTextField = new JTextField();
		_exitCommandTextField.setColumns(10);

		// ECHO COMMAND
		_echoCommandLabel = new JLabel(labels.getString("s340"), JLabel.LEFT);
		_echoCommandCheckBox = new JCheckBox();

		// SET THE VALUES INTO THE TEXT FIELDS
		try {

			// SHELL PATH
			if (OutputConfiguration.getInstance().getShellPath().matches("null"))
				_shellPathTextField.setText("");
			else
				_shellPathTextField.setText(OutputConfiguration.getInstance().getShellPath());

			// SHELL DIRECTORY
			if (OutputConfiguration.getInstance().getShellDirectory().matches("null"))
				_shellDirectoryTextField.setText("");
			else
				_shellDirectoryTextField.setText(OutputConfiguration.getInstance().getShellDirectory());

			// EXIT COMMAND
			if (OutputConfiguration.getInstance().getExitCommand().matches("null"))
				_exitCommandTextField.setText("null");
			else
				_exitCommandTextField.setText(OutputConfiguration.getInstance().getExitCommand());

			// ECHO COMMAND
			_echoCommandCheckBox.setSelected(OutputConfiguration.getInstance().getEchoCommand());

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
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				TextFile file = new TextFile();
				String path = file.read();
				_shellPathTextField.setText(path);
			}
		});

		_examine2Button.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				String path = "";

				JFileChooser fc = new JFileChooser();
				fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

				int option = fc.showOpenDialog(null);

				if (option == JFileChooser.APPROVE_OPTION)
					path = fc.getSelectedFile().getAbsolutePath();

				_shellDirectoryTextField.setText(path);
			}
		});
		_examine2Button.setEnabled(false);

		_manualPathCheckBox.addItemListener(new ItemListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent
			 * )
			 */
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					_shellDirectoryLabel.setEnabled(true);
					_shellDirectoryTextField.setEnabled(true);
					_examine2Button.setEnabled(true);
				} else {
					_shellDirectoryLabel.setEnabled(false);
					_shellDirectoryTextField.setEnabled(false);
					_examine2Button.setEnabled(false);
				}
			}
		});

		_applyButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				try {

					// EXIT THE OUTPUT
					MainWindow.getInstance().getOutput().executeExitCommand();

					// SHELL DIRECTORY
					if (_shellDirectoryTextField.isEnabled()) {

						OutputConfiguration.getInstance().setShellDirectory(
										_shellDirectoryTextField.getText());
					} else {

						String calculatedPath = "";
						String execTextField = _shellPathTextField.getText();
						StringTokenizer stringTokenizer = new StringTokenizer(
								execTextField, "\\");

						int limit = stringTokenizer.countTokens();
						for (int i = 0; i < limit - 1; i++) {
							calculatedPath = calculatedPath
									+ stringTokenizer.nextToken() + "\\";
						}

						OutputConfiguration.getInstance().setShellDirectory(calculatedPath);
					}

					// UPDATE THE PROJECT CONFIGURATION
					MainWindow.getInstance().getOutput().resetOutput();

					// SHELL PATH
					OutputConfiguration.getInstance().setShellPath(_shellPathTextField.getText());

					// ECHO COMMAND
					OutputConfiguration.getInstance().setEchoCommand(_echoCommandCheckBox.isSelected());
					
					// EXIT COMMAND
					OutputConfiguration.getInstance().setExitCommand(_exitCommandTextField.getText());

					// UPDATES THE PROPERTIES
					PropertiesManager.setProperty("outputConfiguration", "./configuration/output/configuration.xml");
					OutputConfiguration.getInstance().save();
					
					// NOT DEFAULT PROJECT
					if (!MainWindow.getInstance().getProjectConfiguration()
							.isDefaultProject())

						// THE PROJECT HAS BEEN MODIFIED
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);

					if (MainWindow.getInstance().getProjectGUI() != null)
						MainWindow.getInstance().getProjectGUI()
								.setAreShellPathsDefined(true);

				} catch (Exception e1) {
					e1.printStackTrace();
				}
				dispose();
			}
		});

		ActionListener actionListener = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		};

		_applyButton.registerKeyboardAction(actionListener, "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

		// SET THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.NONE;

		// SHELL
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_shellPathLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.ipadx = 150;
		constraints.gridy = 0;
		_mainPanel.add(_shellPathTextField, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.gridy = 0;
		_mainPanel.add(_examineButton, constraints);
		
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_manualPathLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 1;
		_mainPanel.add(_manualPathCheckBox, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_mainPanel.add(_shellDirectoryLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.ipadx = 150;
		constraints.gridx = 1;
		constraints.gridy = 2;
		_mainPanel.add(_shellDirectoryTextField, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.gridy = 2;
		_mainPanel.add(_examine2Button, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;
		_mainPanel.add(_exitCommandLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 3;
		_mainPanel.add(_exitCommandTextField, constraints);

		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;
		_mainPanel.add(_echoCommandLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.gridy = 4;
		_mainPanel.add(_echoCommandCheckBox, constraints);

		constraints.insets = new Insets(15, 15, 15, 15);
		constraints.gridwidth = 3;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 5;
		_mainPanel.add(_applyButton, constraints);
		add(_mainPanel);
		
		// FRAME
		setSize(new Dimension(500, 250));
		setIconImage(new ImageIcon(ICON).getImage());
		setTitle(labels.getString("s334"));
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);
	}
}
