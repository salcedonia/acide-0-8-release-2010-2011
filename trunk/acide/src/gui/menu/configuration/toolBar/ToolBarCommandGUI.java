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
 * 
 */
public class ToolBarCommandGUI {

	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private JFrame _frame;
	/**
	 * 
	 */
	private JPanel _commandPanel;
	/**
	 * 
	 */
	private JPanel _listPanel;
	/**
	 * 
	 */
	private JPanel _buttonPanel;
	/**
	 * 
	 */
	private JPanel _iconButtonsPanel;
	/**
	 * 
	 */
	private JPanel _listButtonsPanel;
	/**
	 * 
	 */
	private JLabel _lblName;
	/**
	 * 
	 */
	private Vector<EditableToolBarCommand> _toolBarCommandList;
	/**
	 * 
	 */
	private String[][] _matrixTableToolBarCommand;
	/**
	 * 
	 */
	private int _rowShown;
	/**
	 * 
	 */
	private JTable _table;
	/**
	 * 
	 */
	private JScrollPane _tableScrollPane;
	/**
	 * 
	 */
	private JButton _btnSave;
	/**
	 * 
	 */
	private JButton _btnLoad;
	/**
	 * 
	 */
	private JButton _btnAccept;
	/**
	 * 
	 */
	private JButton _btnCancel;
	/**
	 * 
	 */
	private ToolBarButtonTableModel _model;
	/**
	 * 
	 */
	private ResourceBundle _labels = Language.getInstance().getLabels();
	/**
	 * 
	 */
	private String[] _tableColumns = { _labels.getString("s260"),
			_labels.getString("s261"), _labels.getString("s262"),
			_labels.getString("s263") };

	/**
	 * 
	 */
	private final JTextField _tfName;
	/**
	 * 
	 */
	private JLabel _lblCommand;
	/**
	 * 
	 */
	private final JTextField _tfCommand;
	/**
	 * 
	 */
	private JLabel _lblcomandoItalicLabel;
	/**
	 * 
	 */
	private JLabel _lblHelpText;
	/**
	 * 
	 */
	private final JTextField _tfHelpText;
	/**
	 * 
	 */
	private JLabel _lblImage;
	/**
	 * 
	 */
	private final JTextField _tfImage;
	/**
	 * 
	 */
	private JLabel _lblNote;
	/**
	 * 
	 */
	private JButton _btnAdd;
	/**
	 * 
	 */
	private JButton _btnExamine;
	/**
	 * 
	 */
	private JButton _btnQuit;
	/**
	 * 
	 */
	private JButton _btnModify;
	/**
	 * 
	 */
	private static boolean _areChangesSaved;
	/**
	 * 
	 */
	private static Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 * 
	 * @param modify
	 */
	public ToolBarCommandGUI(boolean modify) {

		_areChangesSaved = true;

		Language language = Language.getInstance();

		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}

		final ResourceBundle labels = language.getLabels();

		_logger.info(labels.getString("s132"));

		// FRAME
		_frame = new JFrame();
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());

		if (modify) {

			String s = null;
			try {
				s = PropertiesManager
						.getProperty("currentToolBarConfiguration");
				int index = s.lastIndexOf("\\");
				if (index == -1)
					index = s.lastIndexOf("/");
				s = s.substring(index + 1, s.length() - 6);
			} catch (Exception e2) {
				JOptionPane.showMessageDialog(null, e2.getMessage(),
						labels.getString("s295"), JOptionPane.ERROR_MESSAGE);
			}
			_frame.setTitle(labels.getString("s147") + " - " + s);
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
		_lblName = new JLabel(labels.getString("s133"), JLabel.LEFT);
		_tfName = new JTextField();

		// COMMAND
		_lblCommand = new JLabel(labels.getString("s134"), JLabel.LEFT);
		_tfCommand = new JTextField();

		_lblcomandoItalicLabel = new JLabel(labels.getString("s146"),
				JLabel.CENTER);
		_lblcomandoItalicLabel.setFont(new Font(_lblName.getFont()
				.getFontName(), Font.ITALIC, _lblName.getFont().getSize()));

		// HELP TEXT
		_lblHelpText = new JLabel(labels.getString("s135"), JLabel.LEFT);
		_tfHelpText = new JTextField();

		// IMAGE
		_lblImage = new JLabel(labels.getString("s136"), JLabel.LEFT);
		_tfImage = new JTextField();
		_lblNote = new JLabel(labels.getString("s139"), JLabel.CENTER);

		// ADD BUTTON
		_btnAdd = new JButton(labels.getString("s137"));
		_btnAdd.setToolTipText(labels.getString("s138"));

		// EXAMINE BUTTON
		_btnExamine = new JButton(labels.getString("s142"));
		_btnExamine.setToolTipText(labels.getString("s143"));

		// QUIT BUTTON
		_btnQuit = new JButton(labels.getString("s148"));
		_btnQuit.setToolTipText(labels.getString("s149"));

		// MODIFY BUTTON
		_btnModify = new JButton(labels.getString("s257"));
		_btnModify.setToolTipText(labels.getString("s258"));

		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_commandPanel.add(_lblName, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 200;
		constraints.ipady = 5;
		_commandPanel.add(_tfName, constraints);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_commandPanel.add(_lblCommand, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_tfCommand, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.ipady = 0;
		constraints.gridwidth = 3;
		_commandPanel.add(_lblcomandoItalicLabel, constraints);
		constraints.gridy = 3;
		constraints.gridwidth = 1;
		_commandPanel.add(_lblHelpText, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_tfHelpText, constraints);
		constraints.gridx = 0;
		constraints.gridy = 4;
		constraints.ipady = 0;
		_commandPanel.add(_lblImage, constraints);
		constraints.gridx = 1;
		constraints.ipady = 5;
		_commandPanel.add(_tfImage, constraints);
		constraints.gridx = 2;
		constraints.ipady = 0;
		_commandPanel.add(_btnExamine, constraints);
		constraints.gridy = 5;
		constraints.gridwidth = 3;
		constraints.gridx = 0;
		_commandPanel.add(_lblNote, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_iconButtonsPanel.add(_btnAdd, constraints);
		constraints.gridx = 1;
		_iconButtonsPanel.add(_btnModify, constraints);
		constraints.gridx = 2;
		_iconButtonsPanel.add(_btnQuit, constraints);

		_toolBarCommandList = new Vector<EditableToolBarCommand>();
		_matrixTableToolBarCommand = new String[_toolBarCommandList.size()][4];
		_model = new ToolBarButtonTableModel();

		getToolBarCommandsForTable();

		_model.setValues(_tableColumns, _matrixTableToolBarCommand);
		_table = new JTable(_model);
		_table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		_table.setPreferredScrollableViewportSize(new Dimension(300, 100));
		_tableScrollPane = new JScrollPane(_table);

		// SAVE BUTTON
		_btnSave = new JButton(labels.getString("s150"));
		_btnSave.setToolTipText(labels.getString("s151"));

		// LOAD BUTTON
		_btnLoad = new JButton(labels.getString("s152"));
		_btnLoad.setToolTipText(labels.getString("s153"));

		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.ipadx = 150;
		constraints.ipady = 40;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridy = 1;
		_listPanel.add(_tableScrollPane, constraints);

		// ACCEPT BUTTON
		_btnAccept = new JButton(labels.getString("s154"));
		_btnAccept.setToolTipText(labels.getString("s155"));

		// CANCEL BUTTON
		_btnCancel = new JButton(labels.getString("s162"));
		_btnCancel.setToolTipText(labels.getString("s163"));

		// LISTENERS
		_btnAccept.addActionListener(new ActionListener() {
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
		_btnCancel.addActionListener(new ActionListener() {
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
		_btnQuit.addActionListener(new ActionListener() {
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
					getToolBarCommandsForTable();
				}
				_model.setValues(_tableColumns, _matrixTableToolBarCommand);
				_model.fireTableDataChanged();
				_logger.info(labels.getString("s168"));
			}
		});
		_btnAdd.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				String name = _tfName.getText();
				String command = _tfCommand.getText();
				String helpText = _tfHelpText.getText();
				String image = _tfImage.getText();
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
				_model.setValues(_tableColumns, _matrixTableToolBarCommand);
				_model.fireTableDataChanged();
				_logger.info(labels.getString("s167"));
			}
		});
		_btnExamine.addActionListener(new ActionListener() {
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
				_tfImage.setText(path);
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
							_tfName.setText(_matrixTableToolBarCommand[_rowShown][0]);
							_tfCommand
									.setText(_matrixTableToolBarCommand[_rowShown][1]);
							_tfHelpText
									.setText(_matrixTableToolBarCommand[_rowShown][2]);
							_tfImage.setText(_matrixTableToolBarCommand[_rowShown][3]);
						}
					}
				});
		_btnModify.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				String name = _tfName.getText();
				String command = _tfCommand.getText();
				String helpText = _tfHelpText.getText();
				String image = _tfImage.getText();
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
				_model.setValues(_tableColumns, _matrixTableToolBarCommand);
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
		_btnCancel.registerKeyboardAction(escPressed, "EscapeKey", KeyStroke
				.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		if (modify) {
			try {
				String current = PropertiesManager
						.getProperty("currentToolBarConfiguration");
				EditableToolBarCommandList.loadAuxList(current);
				_toolBarCommandList = EditableToolBarCommandList.getAuxList();
				getToolBarCommandsForTable();
				_model.setValues(_tableColumns, _matrixTableToolBarCommand);
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
		_buttonPanel.add(_btnAccept, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_btnCancel, constraints);

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
	 * 
	 */
	private void getToolBarCommandsForTable() {

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
		_matrixTableToolBarCommand = data;
	}

	/**
	 * 
	 * @param i
	 */
	private void addStringToolBarCommand(EditableToolBarCommand i) {

		String[][] aux = new String[_toolBarCommandList.size()][4];
		for (int j = 0; j < _matrixTableToolBarCommand.length; j++) {
			aux[j] = _matrixTableToolBarCommand[j];
		}
		aux[_matrixTableToolBarCommand.length][0] = i.getName();
		aux[_matrixTableToolBarCommand.length][1] = i.getCommand();
		aux[_matrixTableToolBarCommand.length][2] = i.getHelpText();
		if (i.getHasIcon())
			aux[_matrixTableToolBarCommand.length][3] = i.getIcon();
		else
			aux[_matrixTableToolBarCommand.length][3] = "";
		_matrixTableToolBarCommand = aux;
	}

	/**
	 * 
	 * @param editableToolBarCommand
	 */
	private void modifyStringToolBarCommand(
			EditableToolBarCommand editableToolBarCommand) {

		_matrixTableToolBarCommand[_rowShown][0] = editableToolBarCommand
				.getName();
		_matrixTableToolBarCommand[_rowShown][1] = editableToolBarCommand
				.getCommand();
		_matrixTableToolBarCommand[_rowShown][2] = editableToolBarCommand
				.getHelpText();
		if (editableToolBarCommand.getHasIcon())
			_matrixTableToolBarCommand[_rowShown][3] = editableToolBarCommand
					.getIcon();
		else
			_matrixTableToolBarCommand[_rowShown][3] = "";
	}

	/**
	 * 
	 * @return
	 */
	public static boolean areChangesSaved() {
		return _areChangesSaved;
	}

	/**
	 * 
	 * @param areChangesSaved
	 */
	public static void setAreChangesSaved(boolean areChangesSaved) {
		_areChangesSaved = areChangesSaved;
	}
}
