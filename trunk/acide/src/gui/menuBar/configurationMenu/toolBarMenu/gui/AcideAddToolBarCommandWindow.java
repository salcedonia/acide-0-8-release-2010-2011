package gui.menuBar.configurationMenu.toolBarMenu.gui;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import es.configuration.toolBar.consoleComandToolBar.ConsoleCommand;
import es.text.AcideFileManager;
import gui.toolBarPanel.consoleCommandToolBar.utils.AcideParameterType;

/**
 * ACIDE - A Configurable IDE add tool bar command window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideAddToolBarCommandWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE add tool bar command window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE add tool bar command window name label.
	 */
	private JLabel _nameLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window name text field.
	 */
	private final JTextField _nameTextField;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window action label.
	 */
	private JLabel _actionLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window action text field.
	 */
	private final JTextField _actionTextField;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window help label.
	 */
	private JLabel _helpLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window hint text label.
	 */
	private JLabel _hintTextLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window hint text text
	 * field.
	 */
	private final JTextField _hintTextTextField;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window image label.
	 */
	private JLabel _iconLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window image text field.
	 */
	private final JTextField _iconTextField;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window extra parameter
	 * label.
	 */
	private JLabel _extraParameterLabel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window extra parameter
	 * combo box.
	 */
	private JComboBox _extraParameterComboBox;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window main panel.
	 */
	private JPanel _commandPanel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window examine button.
	 */
	private JButton _examineButton;
	/**
	 * ACIDE - A Configurable IDE add tool bar command window tool bar
	 * configuration window.
	 */
	private AcideToolBarConfigurationWindow _toolBarConfigurationWindow;

	/**
	 * Creates a new ACIDE - A Configurable IDE add tool bar command window.
	 * 
	 * @param acideToolBarConfigurationWindow
	 */
	public AcideAddToolBarCommandWindow(
			AcideToolBarConfigurationWindow acideToolBarConfigurationWindow) {

		super();

		// Stores the tool bar configuration window instance
		_toolBarConfigurationWindow = acideToolBarConfigurationWindow;

		// Sets the layout
		setLayout(new GridBagLayout());

		// NAME
		_nameLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s133"), JLabel.LEFT);
		_nameTextField = new JTextField();

		// ACTION
		_actionLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s134"), JLabel.LEFT);
		_actionTextField = new JTextField();

		// ITALIC LABEL
		_helpLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s146"), JLabel.CENTER);
		_helpLabel.setFont(new Font(_nameLabel.getFont().getFontName(),
				Font.ITALIC, _nameLabel.getFont().getSize()));

		// HINT TEXT
		_hintTextLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s135"), JLabel.LEFT);
		_hintTextTextField = new JTextField();

		// ICON
		_iconLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s136"), JLabel.LEFT);
		_iconTextField = new JTextField();

		// EXTRA PARAMETER LABEL
		_extraParameterLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s139"), JLabel.CENTER);

		// EXTRA PARAMETER COMBO BOX
		String[] data = {
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1005"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1006"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1007"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1008") };
		_extraParameterComboBox = new JComboBox(data);

		// EXAMINE BUTTON
		_examineButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s142"));
		_examineButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s143"));
		
		// ACCEPT BUTTON
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s155"));
		
		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s163"));
		
		// COMMAND PANEL
		_commandPanel = new JPanel();
		_commandPanel.setLayout(new GridBagLayout());

		// Sets the listeners to the window components
		setListeners();

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_commandPanel.add(_nameLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 200;
		constraints.ipady = 5;
		_commandPanel.add(_nameTextField, constraints);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_commandPanel.add(_actionLabel, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_actionTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.ipady = 0;
		constraints.gridwidth = 3;
		_commandPanel.add(_helpLabel, constraints);
		constraints.gridy = 3;
		constraints.gridwidth = 1;
		_commandPanel.add(_hintTextLabel, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_hintTextTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 4;
		constraints.ipady = 0;
		_commandPanel.add(_iconLabel, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_iconTextField, constraints);
		constraints.gridx = 2;
		constraints.ipady = 0;
		_commandPanel.add(_examineButton, constraints);
		constraints.gridx = 0;
		constraints.gridwidth = 3;
		constraints.gridy = 5;
		_commandPanel.add(_extraParameterLabel, constraints);
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 6;
		_commandPanel.add(_extraParameterComboBox, constraints);

		// Adds the panels to the frame with the layout
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		add(_commandPanel, constraints);

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		constraints.gridy = 1;
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s138"));
		setIconImage(ICON.getImage());
		setResizable(false);
		pack();

		// Centers the location
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);

		// Shows the window
		setVisible(true);
	}

	/**
	 * Sets the listeners to the window components.
	 */
	private void setListeners() {

		// EXAMINE BUTTON
		_examineButton.addActionListener(new ExamineButtonAction());

		// ACCEPT BUTTON
		_acceptButton.addActionListener(new AcceptButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());
	}

	/**
	 * Closes the ACIDE - A Configurable IDE add tool bar command window.
	 */
	private void closeWindow() {
		dispose();
	}

	/**
	 * ACIDE - A Configurable IDE add tool bar command window examine button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExamineButtonAction implements ActionListener {

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
			_iconTextField.setText(path);
		}
	}

	/**
	 * ACIDE - A Configurable IDE add tool bar command window accept button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			String name = _nameTextField.getText();
			String command = _actionTextField.getText();
			String helpText = _hintTextTextField.getText();
			String image = _iconTextField.getText();
			String extraParameterString = (String) _extraParameterComboBox
					.getSelectedItem();

			// No empty name and command are accepted
			if (!name.matches("") && !command.matches("")) {

				// Creates the new row
				ConsoleCommand editableToolBarCommand;

				// Sets the image
				if (image.equals(""))
					editableToolBarCommand = new ConsoleCommand(name, command,
							helpText,
							AcideParameterType
									.fromStringToEnum(extraParameterString));
				else
					editableToolBarCommand = new ConsoleCommand(name, command,
							helpText, image,
							AcideParameterType
									.fromStringToEnum(extraParameterString));

				// Adds the command
				_toolBarConfigurationWindow.addCommand(editableToolBarCommand);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s167"));

			} else

			// Name text field is empty
			if (name.matches(""))

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s997"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s995"), JOptionPane.ERROR_MESSAGE);

			// Action text field is empty
			else if (command.matches(""))
				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s998"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s995"), JOptionPane.ERROR_MESSAGE);

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE add tool bar command window cancel button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {

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
			dispose();
		}
	}
}
