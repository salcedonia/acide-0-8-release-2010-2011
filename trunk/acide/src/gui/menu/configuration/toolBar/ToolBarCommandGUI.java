package gui.menu.configuration.toolBar;

import gui.MainWindow;
import gui.toolBarButton.ToolBarButtonTableModel;
import gui.toolBarButton.ToolBarCommand;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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

import language.Language;
import operations.configuration.EditableToolBarCommand;
import operations.configuration.EditableToolBarCommandList;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import es.text.TextFile;

/**
 * Tool bar command GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ToolBarCommandGUI {

	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Frame of the window.
	 */
	private JFrame _frame;
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
	private Vector<EditableToolBarCommand> _toolBarCommandList;
	/**
	 * Matrix table tool bar command.
	 */
	private String[][] _toolBarCommandMatrixTable;
	/**
	 * Row show.
	 */
	private int _rowShown;
	/**
	 * Tool bar button table model.
	 */
	private ToolBarButtonTableModel _model;
	/**
	 * Labels to display in the selected language.
	 */
	private ResourceBundle _labels = Language.getInstance().getLabels();
	/**
	 * Table columns.
	 */
	private String[] _tableColumns = { _labels.getString("s260"),
			_labels.getString("s261"), _labels.getString("s262"),
			_labels.getString("s263") };
	/**
	 * Flag that indicates if the changes are saved.
	 */
	private static boolean _areChangesSaved;
	/**
	 * Log of the class.
	 */
	private static Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 * 
	 * @param modify
	 */
	public ToolBarCommandGUI(boolean modify) {

		_areChangesSaved = true;

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();

		_logger.info(labels.getString("s132"));

		// FRAME
		_frame = new JFrame();
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());

		if (modify) {

			String path = null;
			try {
				path = PropertiesManager
						.getProperty("currentToolBarConfiguration");
				int index = path.lastIndexOf("\\");
				if (index == -1)
					index = path.lastIndexOf("/");
				path = path.substring(index + 1, path.length() - 6);
			} catch (Exception e2) {
				JOptionPane.showMessageDialog(null, e2.getMessage(),
						labels.getString("s295"), JOptionPane.ERROR_MESSAGE);
			}
			_frame.setTitle(labels.getString("s147") + " - " + path);
		} else
			_frame.setTitle(labels.getString("s910"));

		// COMMAND PANEL
		_commandPanel = new JPanel();
		_commandPanel.setLayout(new GridBagLayout());

		// LIST PANEL
		_listPanel = new JPanel();
		_listPanel.setLayout(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());

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

		_italicLabel = new JLabel(labels.getString("s146"),
				JLabel.CENTER);
		_italicLabel.setFont(new Font(_nameLabel.getFont()
				.getFontName(), Font.ITALIC, _nameLabel.getFont().getSize()));

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

		_toolBarCommandList = new Vector<EditableToolBarCommand>();
		_toolBarCommandMatrixTable = new String[_toolBarCommandList.size()][4];
		_model = new ToolBarButtonTableModel();

		setToolBarCommandsTableMatrix();

		_model.setValues(_tableColumns, _toolBarCommandMatrixTable);
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

		// LISTENERS
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				EditableToolBarCommandList.setList(_toolBarCommandList);
				String newName = "./configuration/toolbar/lastModified.BHcfg";
				EditableToolBarCommandList.saveList(newName);

				try {
					String previous = PropertiesManager
							.getProperty("currentToolBarConfiguration");
					if (!previous.endsWith("lastModified.BHcfg"))
						PropertiesManager.setProperty(
								"previousToolBarConfiguration", previous);
					PropertiesManager.setProperty(
							"currentToolBarConfiguration", newName);
					ToolBarCommand.buildToolBar();
					ToolBarCommand.buildEditableToolBar();
					_areChangesSaved = false;
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.validate();
					mainWindow.repaint();
					_frame.dispose();
					mainWindow.setEnabled(true);
					mainWindow.setAlwaysOnTop(true);
					mainWindow.setAlwaysOnTop(false);
					mainWindow.getMenu().getConfiguration().getToolBar()
							.getSaveToolBar().setEnabled(true);
					_logger.info(labels.getString("s170"));
				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null, e1.getMessage(),
							labels.getString("s909"), JOptionPane.ERROR_MESSAGE);
					_logger.error(e1.getMessage());
				}
			}
		});
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_logger.info(labels.getString("s164"));
				_frame.dispose();
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
			}
		});
		_quitButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				int selectedRow = _table.getSelectedRow();
				if (selectedRow == -1) {
					JOptionPane.showMessageDialog(null,
							labels.getString("s156"), labels.getString("s157"),
							JOptionPane.ERROR_MESSAGE);
				} else {
					_toolBarCommandList.remove(selectedRow);
					setToolBarCommandsTableMatrix();
				}
				_model.setValues(_tableColumns, _toolBarCommandMatrixTable);
				_model.fireTableDataChanged();
				_logger.info(labels.getString("s168"));
			}
		});
		_addButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				String name = _nameTextField.getText();
				String command = _commandTextField.getText();
				String helpText = _helpTextTextField.getText();
				String image = _imageTextField.getText();
				EditableToolBarCommand editableToolBarCommand;

				if (image.equals("")) {
					editableToolBarCommand = new EditableToolBarCommand(name,
							command, helpText);
					_toolBarCommandList.add(editableToolBarCommand);
				} else {
					editableToolBarCommand = new EditableToolBarCommand(name,
							command, helpText, true, image);
					_toolBarCommandList.add(editableToolBarCommand);
				}
				addStringToolBarCommand(editableToolBarCommand);
				_model.setValues(_tableColumns, _toolBarCommandMatrixTable);
				_model.fireTableDataChanged();
				_logger.info(labels.getString("s167"));
			}
		});
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
				_imageTextField.setText(path);
			}
		});

		_table.getSelectionModel().addListSelectionListener(
				new ListSelectionListener() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see
					 * javax.swing.event.ListSelectionListener#valueChanged(
					 * javax.swing.event.ListSelectionEvent)
					 */
					public void valueChanged(ListSelectionEvent e) {
						ListSelectionModel lsm = (ListSelectionModel) e
								.getSource();
						// There are rows selected
						if (!lsm.isSelectionEmpty()) {
							_rowShown = lsm.getMinSelectionIndex();
							// rowShown is selected
							_nameTextField.setText(_toolBarCommandMatrixTable[_rowShown][0]);
							_commandTextField
									.setText(_toolBarCommandMatrixTable[_rowShown][1]);
							_helpTextTextField
									.setText(_toolBarCommandMatrixTable[_rowShown][2]);
							_imageTextField.setText(_toolBarCommandMatrixTable[_rowShown][3]);
						}
					}
				});

		_modifyButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				String name = _nameTextField.getText();
				String command = _commandTextField.getText();
				String helpText = _helpTextTextField.getText();
				String image = _imageTextField.getText();
				EditableToolBarCommand editableToolBarCommand;

				if (image.equals("")) {
					editableToolBarCommand = new EditableToolBarCommand(name,
							command, helpText);
					_toolBarCommandList.set(_rowShown, editableToolBarCommand);
				} else {
					editableToolBarCommand = new EditableToolBarCommand(name,
							command, helpText, true, image);
					_toolBarCommandList.set(_rowShown, editableToolBarCommand);
				}
				modifyStringToolBarCommand(editableToolBarCommand);
				_model.setValues(_tableColumns, _toolBarCommandMatrixTable);
				_model.fireTableDataChanged();
				_logger.info(labels.getString("s259"));
			}
		});
		
		ActionListener escPressed = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
			}
		};
		_cancelButton.registerKeyboardAction(escPressed, "EscapeKey", KeyStroke
				.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		if (modify) {
			try {
				String current = PropertiesManager
						.getProperty("currentToolBarConfiguration");
				EditableToolBarCommandList.loadAuxList(current);
				_toolBarCommandList = EditableToolBarCommandList.getAuxList();
				setToolBarCommandsTableMatrix();
				_model.setValues(_tableColumns, _toolBarCommandMatrixTable);
				_model.fireTableDataChanged();
			} catch (Exception e2) {
				JOptionPane.showMessageDialog(null, e2.getMessage(),
						labels.getString("s269"), JOptionPane.ERROR_MESSAGE);
			}
		}
		
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_buttonPanel.add(_acceptButton, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_cancelButton, constraints);

		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		_frame.add(_commandPanel, constraints);
		constraints.gridy = 1;
		_frame.add(_iconButtonsPanel, constraints);
		constraints.gridy = 2;
		_frame.add(_listPanel, constraints);
		constraints.gridy = 3;
		_frame.add(_buttonPanel, constraints);
		_frame.setResizable(false);
		_frame.pack();

		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = _frame.getSize();
		_frame.setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		_frame.setVisible(true);
		_logger.info(labels.getString("s207"));
	}

	/**
	 * Set the data for the tool bar command table matrix.
	 */
	private void setToolBarCommandsTableMatrix() {

		String[][] data = new String[_toolBarCommandList.size()][4];
		EditableToolBarCommand icon;
		for (int j = 0; j < _toolBarCommandList.size(); j++) {
			icon = _toolBarCommandList.get(j);
			data[j][0] = icon.getName();
			data[j][1] = icon.getCommand();
			data[j][2] = icon.getHelpText();
			if (icon.getHasIcon())
				data[j][3] = icon.getIcon();
			else
				data[j][3] = "";
		}
		_toolBarCommandMatrixTable = data;
	}

	/**
	 * Add a string tool bar command to the matrix.
	 * 
	 * @param editableToolBarCommand New editable tool bar command to add.
	 */
	private void addStringToolBarCommand(EditableToolBarCommand editableToolBarCommand) {

		String[][] aux = new String[_toolBarCommandList.size()][4];
		for (int j = 0; j < _toolBarCommandMatrixTable.length; j++) {
			aux[j] = _toolBarCommandMatrixTable[j];
		}
		aux[_toolBarCommandMatrixTable.length][0] = editableToolBarCommand.getName();
		aux[_toolBarCommandMatrixTable.length][1] = editableToolBarCommand.getCommand();
		aux[_toolBarCommandMatrixTable.length][2] = editableToolBarCommand.getHelpText();
		if (editableToolBarCommand.getHasIcon())
			aux[_toolBarCommandMatrixTable.length][3] = editableToolBarCommand.getIcon();
		else
			aux[_toolBarCommandMatrixTable.length][3] = "";
		_toolBarCommandMatrixTable = aux;
	}

	/**
	 * Modify the tool bar command matrix table.
	 * 
	 * @param editableToolBarCommand Tool bar command to use to modify the matrix table.
	 */
	private void modifyStringToolBarCommand(
			EditableToolBarCommand editableToolBarCommand) {

		_toolBarCommandMatrixTable[_rowShown][0] = editableToolBarCommand
				.getName();
		_toolBarCommandMatrixTable[_rowShown][1] = editableToolBarCommand
				.getCommand();
		_toolBarCommandMatrixTable[_rowShown][2] = editableToolBarCommand
				.getHelpText();
		if (editableToolBarCommand.getHasIcon())
			_toolBarCommandMatrixTable[_rowShown][3] = editableToolBarCommand
					.getIcon();
		else
			_toolBarCommandMatrixTable[_rowShown][3] = "";
	}

	/**
	 * Returns the are change saved flag.
	 * 
	 * @return The are change saved flag.
	 */
	public static boolean areChangesSaved() {
		return _areChangesSaved;
	}

	/**
	 * Set a new value to the are change saved flag.
	 * 
	 * @param areChangesSaved New value to set.
	 */
	public static void setAreChangesSaved(boolean areChangesSaved) {
		_areChangesSaved = areChangesSaved;
	}
}
