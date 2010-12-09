package gui.menuBar.configurationMenu.toolBarMenu.gui;

import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.toolBarMenu.utils.ToolBarTableModel;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import language.AcideLanguage;
import operations.log.AcideLog;

import resources.ResourceManager;

import es.configuration.toolBar.shellComandToolBar.ShellCommand;
import es.configuration.toolBar.shellComandToolBar.ShellCommandList;
import es.text.TextFile;

/************************************************************************
 * Tool bar configuration window of ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
public class ToolBarConfigurationWindow extends JFrame {

	/**
	 * Tool bar configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Tool bar configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * Command panel.
	 */
	private JPanel _commandPanel;
	/**
	 * List panel.
	 */
	private JPanel _listPanel;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Icon buttons panel.
	 */
	private JPanel _iconButtonsPanel;
	/**
	 * List buttons panel.
	 */
	private JPanel _listButtonsPanel;
	/**
	 * Name label.
	 */
	private JLabel _nameLabel;
	/**
	 * Table of tool bar commands.
	 */
	private JTable _table;
	/**
	 * Table scroll panel.
	 */
	private JScrollPane _tableScrollPane;
	/**
	 * Save button.
	 */
	private JButton _saveButton;
	/**
	 * Load button.
	 */
	private JButton _loadButton;
	/**
	 * Accept button.
	 */
	private JButton _acceptButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Name text field.
	 */
	private final JTextField _nameTextField;
	/**
	 * Command label.
	 */
	private JLabel _commandLabel;
	/**
	 * Command text field.
	 */
	private final JTextField _commandTextField;
	/**
	 * Italic label.
	 */
	private JLabel _italicLabel;
	/**
	 * Help text label.
	 */
	private JLabel _helpTextLabel;
	/**
	 * Help text text field.
	 */
	private final JTextField _helpTextTextField;
	/**
	 * Image label.
	 */
	private JLabel _imageLabel;
	/**
	 * Image text field.
	 */
	private final JTextField _imageTextField;
	/**
	 * Note label.
	 */
	private JLabel _noteLabel;
	/**
	 * Add button.
	 */
	private JButton _addButton;
	/**
	 * Examine button.
	 */
	private JButton _examineButton;
	/**
	 * Quit button.
	 */
	private JButton _quitButton;
	/**
	 * Modify button.
	 */
	private JButton _modifyButton;
	/**
	 * Tool bar command list.
	 */
	private Vector<ShellCommand> _commandList;
	/**
	 * Tool bar command matrix table.
	 */
	private String[][] _commandMatrixTable;
	/**
	 * Row show.
	 */
	private int _rowShown;
	/**
	 * Tool bar table model.
	 */
	private ToolBarTableModel _model;
	/**
	 * Table columns.
	 */
	private String[] _tableColumns;
	/**
	 * Flag that indicates if the changes are saved.
	 */
	private static boolean _areChangesSaved;
	/**
	 * Flag that indicates if there are changes in the window. The changes are
	 * applied only when the user selects the add, modify or quit button.
	 */
	private static boolean _areThereChanges;

	/**
	 * Creates a new tool bar configuration window.
	 * 
	 * @param isModified
	 *            indicates if the tool bar configuration window has to be used
	 *            for modifying the tool bar configuration or not
	 */
	public ToolBarConfigurationWindow(boolean isModified) {

		super();

		_areChangesSaved = true;
		_areThereChanges = false;

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s132"));

		// TABLE COLUMNS
		_tableColumns = new String[4];
		_tableColumns[0] = labels.getString("s260");
		_tableColumns[1] = labels.getString("s261");
		_tableColumns[2] = labels.getString("s262");
		_tableColumns[3] = labels.getString("s263");

		// FRAME
		setIconImage(ICON.getImage());
		setLayout(new GridBagLayout());

		// COMMAND PANEL
		_commandPanel = new JPanel();
		_commandPanel.setLayout(new GridBagLayout());

		// LIST PANEL
		_listPanel = new JPanel();
		_listPanel.setLayout(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// ICON BUTTON PANEL
		_iconButtonsPanel = new JPanel();
		_iconButtonsPanel.setLayout(new GridBagLayout());

		// LIST BUTTON PANEL
		_listButtonsPanel = new JPanel();
		_listButtonsPanel.setLayout(new GridBagLayout());

		// NAME
		_nameLabel = new JLabel(labels.getString("s133"), JLabel.LEFT);
		_nameTextField = new JTextField();

		// COMMAND
		_commandLabel = new JLabel(labels.getString("s134"), JLabel.LEFT);
		_commandTextField = new JTextField();

		// ITALIC LABEL
		_italicLabel = new JLabel(labels.getString("s146"), JLabel.CENTER);
		_italicLabel.setFont(new Font(_nameLabel.getFont().getFontName(),
				Font.ITALIC, _nameLabel.getFont().getSize()));

		// HELP TEXT
		_helpTextLabel = new JLabel(labels.getString("s135"), JLabel.LEFT);
		_helpTextTextField = new JTextField();

		// IMAGE
		_imageLabel = new JLabel(labels.getString("s136"), JLabel.LEFT);
		_imageTextField = new JTextField();
		_noteLabel = new JLabel(labels.getString("s139"), JLabel.CENTER);

		// ADD BUTTON
		_addButton = new JButton(labels.getString("s137"));
		_addButton.setToolTipText(labels.getString("s138"));

		// EXAMINE BUTTON
		_examineButton = new JButton(labels.getString("s142"));
		_examineButton.setToolTipText(labels.getString("s143"));

		// QUIT BUTTON
		_quitButton = new JButton(labels.getString("s148"));
		_quitButton.setToolTipText(labels.getString("s149"));

		// MODIFY BUTTON
		_modifyButton = new JButton(labels.getString("s257"));
		_modifyButton.setToolTipText(labels.getString("s258"));

		// ADD THE COMPONENTS TO THE WINDOW WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();

		// COMMAND PANEL
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
		_commandPanel.add(_commandLabel, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_commandTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.ipady = 0;
		constraints.gridwidth = 3;
		_commandPanel.add(_italicLabel, constraints);
		constraints.gridy = 3;
		constraints.gridwidth = 1;
		_commandPanel.add(_helpTextLabel, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_helpTextTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 4;
		constraints.ipady = 0;
		_commandPanel.add(_imageLabel, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_imageTextField, constraints);
		constraints.gridx = 2;
		constraints.ipady = 0;
		_commandPanel.add(_examineButton, constraints);
		constraints.gridy = 5;
		constraints.gridwidth = 3;
		constraints.gridx = 0;
		_commandPanel.add(_noteLabel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.insets = new Insets(5, 5, 5, 5);

		// ICON BUTTONS PANEL
		_iconButtonsPanel.add(_addButton, constraints);
		constraints.gridx = 1;
		_iconButtonsPanel.add(_modifyButton, constraints);
		constraints.gridx = 2;
		_iconButtonsPanel.add(_quitButton, constraints);

		_commandList = new Vector<ShellCommand>();
		_commandMatrixTable = new String[_commandList.size()][4];
		_model = new ToolBarTableModel();

		setToolBarTableMatrix();

		_model.setValues(_tableColumns, _commandMatrixTable);
		_table = new JTable(_model);
		_table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		_table.setPreferredScrollableViewportSize(new Dimension(300, 100));
		_tableScrollPane = new JScrollPane(_table);

		// SAVE BUTTON
		_saveButton = new JButton(labels.getString("s150"));
		_saveButton.setToolTipText(labels.getString("s151"));

		// LOAD BUTTON
		_loadButton = new JButton(labels.getString("s152"));
		_loadButton.setToolTipText(labels.getString("s153"));

		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.ipadx = 150;
		constraints.ipady = 40;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridy = 1;
		_listPanel.add(_tableScrollPane, constraints);

		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s154"));
		_acceptButton.setToolTipText(labels.getString("s155"));

		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s162"));
		_cancelButton.setToolTipText(labels.getString("s163"));

		// Listeners
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// If there have been changes ask for saving the changes
				if (_areThereChanges) {
					
					// Asks the user if wants to save the changes
					int choosenOption = JOptionPane.showConfirmDialog(null,
							labels.getString("s996"), labels.getString("s995"),
							JOptionPane.YES_NO_CANCEL_OPTION);

					switch (choosenOption) {

					// YES
					case JOptionPane.YES_OPTION:

						// Modifies the current tool bar
						_modifyButton.doClick();

						// Sets the list
						ShellCommandList.setList(_commandList);
						String newName = "./configuration/toolbar/lastModified.TBcfg";
						ShellCommandList.saveList(newName);

						try {

							// Sets the previous tool bar configuration
							String previous = ResourceManager.getInstance()
									.getProperty("currentToolBarConfiguration");

							if (!previous.endsWith("lastModified.TBcfg"))

								// Updates the RESOURCE MANAGER
								ResourceManager.getInstance().setProperty(
										"previousToolBarConfiguration",
										previous);

							// Updates the RESOURCE MANAGER
							ResourceManager.getInstance().setProperty(
									"currentToolBarConfiguration", newName);

							// Builds the tool bar
							MainWindow.getInstance().buildToolBar();
							_areChangesSaved = false;
							MainWindow.getInstance().validate();
							MainWindow.getInstance().repaint();

							// Closes the window
							dispose();
							MainWindow.getInstance().setEnabled(true);
							MainWindow.getInstance().setAlwaysOnTop(true);
							MainWindow.getInstance().setAlwaysOnTop(false);

							// Enables the save tool bar menu option
							MainWindow.getInstance().getMenu()
									.getConfiguration().getToolBar()
									.getSaveToolBar().setEnabled(true);

							// Updates the tool bar
							AcideLog.getLog().info(labels.getString("s170"));

						} catch (Exception exception) {

							// Error message
							JOptionPane.showMessageDialog(null,
									exception.getMessage(),
									labels.getString("s909"),
									JOptionPane.ERROR_MESSAGE);

							// Updates the log
							AcideLog.getLog().error(exception.getMessage());
						}

						break;

					// NO
					case JOptionPane.NO_OPTION:
						_cancelButton.doClick();
						break;
					}
				}
			}
		});

		// CANCEL BUTTON
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Updates the log
				AcideLog.getLog().info(labels.getString("s164"));

				// Closes the window
				dispose();

				// Returns to the main window
				MainWindow.getInstance().setEnabled(true);
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
			}
		});

		// QUIT BUTTON
		_quitButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Checks if there is a selected row in the table
				int selectedRow = _table.getSelectedRow();

				// If there is a selected row
				if (selectedRow != -1) {

					// Removes the command
					_commandList.remove(selectedRow);

					// Updates the tool bar table matrix
					setToolBarTableMatrix();

					// There are changes
					_areThereChanges = true;
				} else {

					// Error message
					JOptionPane.showMessageDialog(null,
							labels.getString("s156"), labels.getString("s157"),
							JOptionPane.ERROR_MESSAGE);
				}

				// Updates the table model
				_model.setValues(_tableColumns, _commandMatrixTable);
				_model.fireTableDataChanged();

				// Updates the log
				AcideLog.getLog().info(labels.getString("s168"));
			}
		});

		// ADD BUTTON
		_addButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				String name = _nameTextField.getText();
				String command = _commandTextField.getText();
				String helpText = _helpTextTextField.getText();
				String image = _imageTextField.getText();

				// No empty name and command are accepted
				if (!name.matches("") && !command.matches("")) {

					ShellCommand editableToolBarCommand;

					// Sets the image
					if (image.equals(""))
						editableToolBarCommand = new ShellCommand(name,
								command, helpText);
					else
						editableToolBarCommand = new ShellCommand(name,
								command, helpText, true, image);

					// Adds the command to the list
					_commandList.add(editableToolBarCommand);

					// Adds the command
					addCommand(editableToolBarCommand);

					// Updates the model
					_model.setValues(_tableColumns, _commandMatrixTable);
					_model.fireTableDataChanged();

					// There are changes
					_areThereChanges = true;

					// Updates the log
					AcideLog.getLog().info(labels.getString("s167"));
				} else

				// Error messages
				if (name.matches(""))
					JOptionPane.showMessageDialog(null,
							labels.getString("s997"), labels.getString("s995"),
							JOptionPane.ERROR_MESSAGE);
				else if (command.matches(""))
					JOptionPane.showMessageDialog(null,
							labels.getString("s998"), labels.getString("s995"),
							JOptionPane.ERROR_MESSAGE);
			}
		});

		// EXAMINE BUTTON
		_examineButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				TextFile file = new TextFile();
				String path = file.read();
				_imageTextField.setText(path);
			}
		});

		// TABLE
		_table.getSelectionModel().addListSelectionListener(
				new ListSelectionListener() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see
					 * javax.swing.event.ListSelectionListener#valueChanged(
					 * javax.swing.event.ListSelectionEvent)
					 */
					@Override
					public void valueChanged(
							ListSelectionEvent listSelectionEvent) {

						ListSelectionModel listSelectionModel = (ListSelectionModel) listSelectionEvent
								.getSource();

						// There are selected rows
						if (!listSelectionModel.isSelectionEmpty()) {

							_rowShown = listSelectionModel
									.getMinSelectionIndex();

							_nameTextField
									.setText(_commandMatrixTable[_rowShown][0]);
							_commandTextField
									.setText(_commandMatrixTable[_rowShown][1]);
							_helpTextTextField
									.setText(_commandMatrixTable[_rowShown][2]);
							_imageTextField
									.setText(_commandMatrixTable[_rowShown][3]);
						}
					}
				});

		// MODIFY BUTTON
		_modifyButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				String name = _nameTextField.getText();
				String command = _commandTextField.getText();
				String helpText = _helpTextTextField.getText();
				String image = _imageTextField.getText();
				ShellCommand editableToolBarCommand;

				if (image.equals("")) {
					editableToolBarCommand = new ShellCommand(name, command,
							helpText);
					_commandList.set(_rowShown, editableToolBarCommand);
				} else {
					editableToolBarCommand = new ShellCommand(name, command,
							helpText, true, image);
					_commandList.set(_rowShown, editableToolBarCommand);
				}

				modifyToolBarMatrixTable(editableToolBarCommand);
				_model.setValues(_tableColumns, _commandMatrixTable);
				_model.fireTableDataChanged();

				// There have been changes
				_areThereChanges = true;

				// Updates the log
				AcideLog.getLog().info(labels.getString("s259"));
			}
		});

		ActionListener escapePressed = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Closes the window
				dispose();
				MainWindow.getInstance().setEnabled(true);
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
			}
		};
		_cancelButton.registerKeyboardAction(escapePressed, "EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Loads the command list
		if (isModified) {
			try {

				String current = ResourceManager.getInstance().getProperty(
						"currentToolBarConfiguration");
				ShellCommandList.loadAuxList(current);
				_commandList = ShellCommandList.getAuxList();
				setToolBarTableMatrix();
				_model.setValues(_tableColumns, _commandMatrixTable);
				_model.fireTableDataChanged();

			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						labels.getString("s269"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
		}

		// Sets the title
		if (isModified) {

			// Gets the name of the current tool bar configuration
			String path = null;
			try {
				path = ResourceManager.getInstance().getProperty(
						"currentToolBarConfiguration");
				int index = path.lastIndexOf("\\");
				if (index == -1)
					index = path.lastIndexOf("/");
				path = path.substring(index + 1, path.length() - 6);
			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						labels.getString("s295"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
			setTitle(labels.getString("s147") + " - " + path);
		} else
			setTitle(labels.getString("s910"));

		// Adds the panels to the frame with the layout
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		add(_commandPanel, constraints);
		constraints.gridy = 1;
		add(_iconButtonsPanel, constraints);
		constraints.gridy = 2;
		add(_listPanel, constraints);
		constraints.gridy = 3;

		// BUTTON PANEL
		_buttonPanel.add(_acceptButton, constraints);
		_buttonPanel.add(_cancelButton, constraints);
		add(_buttonPanel, constraints);
		setResizable(false);
		pack();

		// Centers the location
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);

		setVisible(true);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s207"));
	}

	/**
	 * Sets the data for the tool bar command table matrix.
	 */
	private void setToolBarTableMatrix() {

		String[][] data = new String[_commandList.size()][4];
		ShellCommand command;
		for (int j = 0; j < _commandList.size(); j++) {
			command = _commandList.get(j);
			data[j][0] = command.getName();
			data[j][1] = command.getAction();
			data[j][2] = command.getHintText();
			if (command.getHasIcon())
				data[j][3] = command.getIcon();
			else
				data[j][3] = "";
		}
		_commandMatrixTable = data;
	}

	/**
	 * Adds a string command tool bar to the matrix.
	 * 
	 * @param modifiableCommand
	 *            new modifiable command to add.
	 */
	private void addCommand(ShellCommand modifiableCommand) {

		String[][] aux = new String[_commandList.size()][4];

		for (int j = 0; j < _commandMatrixTable.length; j++) {
			aux[j] = _commandMatrixTable[j];
		}

		aux[_commandMatrixTable.length][0] = modifiableCommand.getName();
		aux[_commandMatrixTable.length][1] = modifiableCommand.getAction();
		aux[_commandMatrixTable.length][2] = modifiableCommand.getHintText();
		if (modifiableCommand.getHasIcon())
			aux[_commandMatrixTable.length][3] = modifiableCommand.getIcon();
		else
			aux[_commandMatrixTable.length][3] = "";
		_commandMatrixTable = aux;
	}

	/**
	 * Modifies the tool bar matrix table at with the new data from the
	 * modifiable command given as a parameter.
	 * 
	 * @param modifiableCommand
	 *            modifiable command which contains the data to modify the
	 *            matrix table.
	 */
	private void modifyToolBarMatrixTable(ShellCommand modifiableCommand) {

		_commandMatrixTable[_rowShown][0] = modifiableCommand.getName();
		_commandMatrixTable[_rowShown][1] = modifiableCommand.getAction();
		_commandMatrixTable[_rowShown][2] = modifiableCommand.getHintText();
		if (modifiableCommand.getHasIcon())
			_commandMatrixTable[_rowShown][3] = modifiableCommand.getIcon();
		else
			_commandMatrixTable[_rowShown][3] = "";
	}

	/**
	 * Returns the are change saved flag.
	 * 
	 * @return the are change saved flag.
	 */
	public static boolean areChangesSaved() {
		return _areChangesSaved;
	}

	/**
	 * Sets a new value to the are change saved flag.
	 * 
	 * @param areChangesSaved
	 *            new value to set.
	 */
	public static void setAreChangesSaved(boolean areChangesSaved) {
		_areChangesSaved = areChangesSaved;
	}
}
