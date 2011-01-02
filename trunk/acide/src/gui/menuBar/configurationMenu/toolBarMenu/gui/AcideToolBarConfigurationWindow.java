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
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.toolBar.consoleComandToolBar.ConsoleCommand;
import es.configuration.toolBar.consoleComandToolBar.ConsoleCommandList;
import es.text.TextFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.toolBarMenu.utils.ToolBarTableModel;
import gui.toolBarPanel.consoleCommandToolBar.utils.AcideParameterType;

/**
 * ACIDE - A Configurable IDE tool bar configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideToolBarConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window command panel.
	 */
	private JPanel _commandPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window list panel.
	 */
	private JPanel _listPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window icon buttons
	 * panel.
	 */
	private JPanel _iconButtonsPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window list buttons
	 * panel.
	 */
	private JPanel _listButtonsPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window name label.
	 */
	private JLabel _nameLabel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table of tool
	 * bar commands.
	 */
	private JTable _table;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table scroll
	 * panel.
	 */
	private JScrollPane _tableScrollPane;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window modify button.
	 */
	private JButton _modifyButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window save button.
	 */
	private JButton _saveButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window load button.
	 */
	private JButton _loadButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window name text field.
	 */
	private final JTextField _nameTextField;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window command label.
	 */
	private JLabel _actionLabel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window command text
	 * field.
	 */
	private final JTextField _actionTextField;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window italic label.
	 */
	private JLabel _italicLabel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window hint text label.
	 */
	private JLabel _hintTextLabel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window hint text text
	 * field.
	 */
	private final JTextField _hintTextTextField;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window image label.
	 */
	private JLabel _iconLabel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window image text
	 * field.
	 */
	private final JTextField _iconTextField;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window extra parameter
	 * label.
	 */
	private JLabel _extraParameterLabel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window extra parameter
	 * list.
	 */
	private JList _extraParameterList;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window add button.
	 */
	private JButton _addButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window examine button.
	 */
	private JButton _examineButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window quit button.
	 */
	private JButton _quitButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window tool bar command
	 * list.
	 */
	private Vector<ConsoleCommand> _commandList;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window tool bar command
	 * matrix table.
	 */
	private String[][] _commandMatrixTable;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window row show.
	 */
	private int _rowShown;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window tool bar table
	 * model.
	 */
	private ToolBarTableModel _tableModel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table columns.
	 */
	private String[] _tableColumns;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window flag that
	 * indicates if the changes are saved.
	 */
	private static boolean _areChangesSaved;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window flag that
	 * indicates if there are changes in the window. The changes are applied
	 * only when the user selects the add, modify or quit button.
	 */
	private static boolean _areThereChanges;

	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar configuration window.
	 * 
	 * @param isModified
	 *            indicates if the tool bar configuration window has to be used
	 *            for modifying the tool bar configuration or not
	 */
	public AcideToolBarConfigurationWindow(boolean isModified) {

		super();

		_areChangesSaved = true;
		_areThereChanges = false;

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
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
		_tableColumns = new String[5];
		_tableColumns[0] = labels.getString("s260");
		_tableColumns[1] = labels.getString("s261");
		_tableColumns[2] = labels.getString("s262");
		_tableColumns[3] = labels.getString("s263");
		_tableColumns[4] = labels.getString("s1003");

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

		// ACTION
		_actionLabel = new JLabel(labels.getString("s134"), JLabel.LEFT);
		_actionTextField = new JTextField();

		// ITALIC LABEL
		_italicLabel = new JLabel(labels.getString("s146"), JLabel.CENTER);
		_italicLabel.setFont(new Font(_nameLabel.getFont().getFontName(),
				Font.ITALIC, _nameLabel.getFont().getSize()));

		// HINT TEXT
		_hintTextLabel = new JLabel(labels.getString("s135"), JLabel.LEFT);
		_hintTextTextField = new JTextField();

		// ICON
		_iconLabel = new JLabel(labels.getString("s136"), JLabel.LEFT);
		_iconTextField = new JTextField();

		// EXTRA PARAMETER LABEL
		_extraParameterLabel = new JLabel(labels.getString("s139"),
				JLabel.CENTER);

		// EXTRA PARAMETER LIST
		String[] data = { labels.getString("s1005"), labels.getString("s1006"),
				labels.getString("s1007"), labels.getString("s1008") };
		_extraParameterList = new JList(data);
		_extraParameterList
				.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		_extraParameterList.setLayoutOrientation(JList.VERTICAL);
		_extraParameterList.setVisibleRowCount(-1);
		JScrollPane listScrollPane = new JScrollPane(_extraParameterList);
		listScrollPane.setPreferredSize(new Dimension(70, 75));

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
		_commandPanel.add(_actionLabel, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_actionTextField, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.ipady = 0;
		constraints.gridwidth = 3;
		_commandPanel.add(_italicLabel, constraints);
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
		_commandPanel.add(listScrollPane, constraints);

		// ICON BUTTONS PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_iconButtonsPanel.add(_addButton, constraints);
		constraints.gridx = 1;
		_iconButtonsPanel.add(_modifyButton, constraints);
		constraints.gridx = 2;
		_iconButtonsPanel.add(_quitButton, constraints);

		_commandList = new Vector<ConsoleCommand>();
		_commandMatrixTable = new String[_commandList.size()][5];
		_tableModel = new ToolBarTableModel();

		setToolBarTableMatrix();

		_tableModel.setValues(_tableColumns, _commandMatrixTable);
		_table = new JTable(_tableModel);
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

						// Sets the list
						ConsoleCommandList.setFinalList(_commandList);
						String newName = "./configuration/toolbar/lastModified.TBcfg";
						ConsoleCommandList.saveFinalList(newName);

						try {

							// Sets the previous tool bar configuration
							String previous = AcideResourceManager.getInstance()
									.getProperty("currentToolBarConfiguration");

							if (!previous.endsWith("lastModified.TBcfg"))

								// Updates the RESOURCE MANAGER
								AcideResourceManager.getInstance().setProperty(
										"previousToolBarConfiguration",
										previous);

							// Updates the RESOURCE MANAGER
							AcideResourceManager.getInstance().setProperty(
									"currentToolBarConfiguration", newName);

							// Builds the tool bar
							MainWindow.getInstance().buildToolBarPanel();
							_areChangesSaved = false;
							MainWindow.getInstance().validate();
							MainWindow.getInstance().repaint();

							// Closes the tool bar configuration window
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
				} else {

					// Closes the tool bar configuration window
					dispose();

					// Enables the main window
					MainWindow.getInstance().setEnabled(true);

					// Shows the main window
					MainWindow.getInstance().setAlwaysOnTop(true);
					MainWindow.getInstance().setAlwaysOnTop(false);
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

				// Closes the tool bar configuration window
				dispose();

				// Enables the main window
				MainWindow.getInstance().setEnabled(true);

				// Shows the main window
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
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

				// Gets the language
				AcideLanguageManager language = AcideLanguageManager
						.getInstance();

				try {
					language.getLanguage(AcideResourceManager.getInstance()
							.getProperty("language"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				final ResourceBundle labels = language.getLabels();

				String name = _nameTextField.getText();
				String action = _actionTextField.getText();
				String hintText = _hintTextTextField.getText();
				String icon = _iconTextField.getText();
				String extraParameterString = (String) _extraParameterList
						.getSelectedValue();

				// Creates the new shell command to update
				ConsoleCommand shellCommand = new ConsoleCommand(name, action,
						hintText, !icon.matches(""), icon, AcideParameterType
								.fromStringToEnum(extraParameterString));

				// Updates the shell command list
				_commandList.set(_rowShown, shellCommand);

				// There are changes
				_areThereChanges = true;

				// Updates the table in the configuration window
				modifyToolBarMatrixTable(shellCommand);
				_tableModel.setValues(_tableColumns, _commandMatrixTable);
				_tableModel.fireTableDataChanged();

				// Updates the log
				AcideLog.getLog().info(labels.getString("s259"));
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
				_rowShown = _table.getSelectedRow();

				// If there is a selected row
				if (_rowShown != -1) {

					// Removes the command
					_commandList.remove(_rowShown);

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
				_tableModel.setValues(_tableColumns, _commandMatrixTable);
				_tableModel.fireTableDataChanged();

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
				String command = _actionTextField.getText();
				String helpText = _hintTextTextField.getText();
				String image = _iconTextField.getText();
				String extraParameterString = (String) _extraParameterList
						.getSelectedValue();

				// No empty name and command are accepted
				if (!name.matches("") && !command.matches("")) {

					ConsoleCommand editableToolBarCommand;

					// Sets the image
					if (image.equals(""))
						editableToolBarCommand = new ConsoleCommand(name,
								command, helpText, AcideParameterType
										.fromStringToEnum(extraParameterString));
					else
						editableToolBarCommand = new ConsoleCommand(name,
								command, helpText, true, image,
								AcideParameterType
										.fromStringToEnum(extraParameterString));

					// Adds the command to the list
					_commandList.add(editableToolBarCommand);

					// Adds the command
					addCommand(editableToolBarCommand);

					// Updates the model
					_tableModel.setValues(_tableColumns, _commandMatrixTable);
					_tableModel.fireTableDataChanged();

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
				_iconTextField.setText(path);
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

						// Gets the list selection model
						ListSelectionModel listSelectionModel = (ListSelectionModel) listSelectionEvent
								.getSource();

						// There are selected rows
						if (!listSelectionModel.isSelectionEmpty()) {

							// Gets the row selected
							_rowShown = listSelectionModel
									.getMinSelectionIndex();

							// NAME
							_nameTextField
									.setText(_commandMatrixTable[_rowShown][0]);

							// ACTION
							_actionTextField
									.setText(_commandMatrixTable[_rowShown][1]);

							// HINT TEXT
							_hintTextTextField
									.setText(_commandMatrixTable[_rowShown][2]);

							// ICON
							_iconTextField
									.setText(_commandMatrixTable[_rowShown][3]);

							// EXTRA PARAMETER
							if (_commandMatrixTable[_rowShown][4]
									.matches(labels.getString("s1005")))
								_extraParameterList.setSelectedIndex(0);
							else if (_commandMatrixTable[_rowShown][4]
									.matches(labels.getString("s1006")))
								_extraParameterList.setSelectedIndex(1);
							else if (_commandMatrixTable[_rowShown][4]
									.matches(labels.getString("s1007")))
								_extraParameterList.setSelectedIndex(2);
							else if (_commandMatrixTable[_rowShown][4]
									.matches(labels.getString("s1008")))
								_extraParameterList.setSelectedIndex(3);
						}
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
				_acceptButton.doClick();
			}
		};
		_cancelButton.registerKeyboardAction(escapePressed, "EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Loads the command list
		if (isModified) {

			try {

				// Gets the current tool bar configuration
				String currentToolBarConfiguration = AcideResourceManager
						.getInstance().getProperty(
								"currentToolBarConfiguration");

				// Loads the shell command auxiliar list
				ConsoleCommandList
						.loadTemporalList(currentToolBarConfiguration);
				_commandList = ConsoleCommandList.getTemporalList();

				// Updates the tool bar table matrix
				setToolBarTableMatrix();

				// Updates the table model data
				_tableModel.setValues(_tableColumns, _commandMatrixTable);

				// Refresh the table model
				_tableModel.fireTableDataChanged();

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
				path = AcideResourceManager.getInstance().getProperty(
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

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		String[][] data = new String[_commandList.size()][5];
		ConsoleCommand command;
		for (int j = 0; j < _commandList.size(); j++) {
			command = _commandList.get(j);

			// NAME
			data[j][0] = command.getName();

			// ACTION
			data[j][1] = command.getAction();

			// HINT TEXT
			data[j][2] = command.getHintText();

			// ICON
			if (command.getHasIcon())
				data[j][3] = command.getIcon();
			else
				data[j][3] = "";

			// EXTRA PARAMETER
			switch (command.getParameterType()) {
			case NONE:
				data[j][4] = labels.getString("s1005");
				break;
			case TEXT:
				data[j][4] = labels.getString("s1006");
				break;
			case FILE:
				data[j][4] = labels.getString("s1007");
				break;
			case DIRECTORY:
				data[j][4] = labels.getString("s1008");
				break;
			}
		}
		_commandMatrixTable = data;
	}

	/**
	 * Adds a string command tool bar to the matrix.
	 * 
	 * @param shellCommand
	 *            new shell command to add.
	 */
	private void addCommand(ConsoleCommand shellCommand) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		String[][] aux = new String[_commandList.size()][5];

		for (int j = 0; j < _commandMatrixTable.length; j++) {
			aux[j] = _commandMatrixTable[j];
		}

		// NAME
		aux[_commandMatrixTable.length][0] = shellCommand.getName();

		// ACTION
		aux[_commandMatrixTable.length][1] = shellCommand.getAction();

		// HINT TEXT
		aux[_commandMatrixTable.length][2] = shellCommand.getHintText();

		// ICON
		if (shellCommand.getHasIcon())
			aux[_commandMatrixTable.length][3] = shellCommand.getIcon();
		else
			aux[_commandMatrixTable.length][3] = "";

		// EXTRA PARAMETER
		switch (shellCommand.getParameterType()) {
		case NONE:
			aux[_commandMatrixTable.length][4] = labels.getString("s1005");
			break;
		case TEXT:
			aux[_commandMatrixTable.length][4] = labels.getString("s1006");
			break;
		case FILE:
			aux[_commandMatrixTable.length][4] = labels.getString("s1007");
			break;
		case DIRECTORY:
			aux[_commandMatrixTable.length][4] = labels.getString("s1008");
			break;
		}

		// Updates the current matrix table
		_commandMatrixTable = aux;
	}

	/**
	 * Modifies the tool bar matrix table at with the new data from the shell
	 * command given as a parameter.
	 * 
	 * @param shellCommand
	 *            shell command which contains the data to modify the matrix
	 *            table.
	 */
	private void modifyToolBarMatrixTable(ConsoleCommand shellCommand) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// NAME
		_commandMatrixTable[_rowShown][0] = shellCommand.getName();

		// ACTION
		_commandMatrixTable[_rowShown][1] = shellCommand.getAction();

		// HINT TEXT
		_commandMatrixTable[_rowShown][2] = shellCommand.getHintText();

		// ICON
		if (shellCommand.getHasIcon())
			_commandMatrixTable[_rowShown][3] = shellCommand.getIcon();
		else
			_commandMatrixTable[_rowShown][3] = "";

		// EXTRA PARAMETER
		switch (shellCommand.getParameterType()) {
		case NONE:
			_commandMatrixTable[_rowShown][4] = labels.getString("s1005");
			break;
		case TEXT:
			_commandMatrixTable[_rowShown][4] = labels.getString("s1006");
			break;
		case FILE:
			_commandMatrixTable[_rowShown][4] = labels.getString("s1007");
			break;
		case DIRECTORY:
			_commandMatrixTable[_rowShown][4] = labels.getString("s1008");
			break;
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar configuration window are change saved flag.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar configuration window are change saved flag.
	 */
	public static boolean areChangesSaved() {
		return _areChangesSaved;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar configuration window are change saved flag.
	 * 
	 * @param areChangesSaved
	 *            new value to set.
	 */
	public static void setAreChangesSaved(boolean areChangesSaved) {
		_areChangesSaved = areChangesSaved;
	}
}
