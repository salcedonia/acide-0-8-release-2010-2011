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

import es.configuration.toolBar.consoleComandToolBar.ConsoleCommand;
import es.configuration.toolBar.consoleComandToolBar.ConsoleCommandList;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.toolBarMenu.utils.AcideComboBoxTableCellEditor;
import gui.menuBar.configurationMenu.toolBarMenu.utils.AcideComboBoxTableCellRenderer;
import gui.menuBar.configurationMenu.toolBarMenu.utils.AcideToolBarConfigurationWindowTableModel;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

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
	 * ACIDE - A Configurable IDE tool bar configuration window table panel.
	 */
	private JPanel _tablePanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window icon buttons
	 * panel.
	 */
	private JPanel _tableButtonsPanel;
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
	 * ACIDE - A Configurable IDE tool bar configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window add button.
	 */
	private JButton _addButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window quit button.
	 */
	private JButton _quitButton;
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
	 * @param forModifying
	 */
	public AcideToolBarConfigurationWindow(boolean forModifying) {

		super();

		// The changes are saved
		_areChangesSaved = true;

		// There are no changes by the moment
		_areThereChanges = false;

		// Gets the labels
		final ResourceBundle labels = AcideLanguageManager.getInstance()
				.getLabels();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s132"));

		// Sets the layout
		setLayout(new GridBagLayout());

		// TABLE PANEL
		_tablePanel = new JPanel(new BorderLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// TABLE BUTTON PANEL
		_tableButtonsPanel = new JPanel(new GridBagLayout());

		// ADD BUTTON
		_addButton = new JButton(labels.getString("s137"));
		_addButton.setToolTipText(labels.getString("s138"));
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

				showAcideAddToolBarCommandWindow();
			}
		});

		// QUIT BUTTON
		_quitButton = new JButton(labels.getString("s148"));
		_quitButton.setToolTipText(labels.getString("s149"));
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

				// If there is a selected row
				if (_table.getSelectedRow() != -1) {

					// Removes the selected row from the model
					((AcideToolBarConfigurationWindowTableModel) _table
							.getModel()).removeItem(_table.getSelectedRow());

					// Updates the table model
					((AcideToolBarConfigurationWindowTableModel) _table
							.getModel()).fireTableRowsDeleted(0,
							((AcideToolBarConfigurationWindowTableModel) _table
									.getModel()).getRowCount());

					// There are changes
					_areThereChanges = true;
				} else {

					// Error message
					JOptionPane.showMessageDialog(null,
							labels.getString("s156"), labels.getString("s157"),
							JOptionPane.ERROR_MESSAGE);
				}

				// Updates the log
				AcideLog.getLog().info(labels.getString("s168"));
			}
		});

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		// TABLE BUTTONS PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_tableButtonsPanel.add(_addButton, constraints);
		constraints.gridx = 1;
		_tableButtonsPanel.add(_quitButton, constraints);

		// Creates the table with the model
		_table = new JTable(new AcideToolBarConfigurationWindowTableModel(this));

		// Sets the single selection in the table
		_table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		// The columns width are not equal
		_table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

		// POPUP MENU for the 4th column
		_table.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent
			 * )
			 */
			@Override
			public void mousePressed(MouseEvent mouseEvent) {
				maybeShowPopup(mouseEvent);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent
			 * )
			 */
			@Override
			public void mouseReleased(MouseEvent mouseEvent) {
				maybeShowPopup(mouseEvent);
			}

			/**
			 * Shows the popup menu only for the 4th column in the table.
			 * 
			 * @param mouseEvent
			 *            mouse event
			 */
			private void maybeShowPopup(MouseEvent mouseEvent) {

				if (mouseEvent.isPopupTrigger() && _table.isEnabled()) {

					// Gets the point
					Point p = new Point(mouseEvent.getX(), mouseEvent.getY());
					int col = _table.columnAtPoint(p);
					int row = _table.rowAtPoint(p);

					// Translate table index to model index
					int modelColumn = _table.getColumn(
							_table.getColumnName(col)).getModelIndex();

					if (row >= 0 && row < _table.getRowCount()) {

						// Creates popup menu
						JPopupMenu contextMenu = createContextMenu(row,
								modelColumn);

						// And show it
						if (contextMenu != null
								&& contextMenu.getComponentCount() > 0) {

							// Only if this is the 4th column
							if (modelColumn == 3)
								contextMenu.show(_table, p.x, p.y);
						}
					}
				}
			}
		});

		// These are the combo box values
		String[] values = new String[] { labels.getString("s1005"),
				labels.getString("s1006"), labels.getString("s1007"),
				labels.getString("s1008") };

		// Set the combo box editor on the 4th visible column
		TableColumn extraParameterColumn = _table.getColumnModel().getColumn(4);
		extraParameterColumn.setCellEditor(new AcideComboBoxTableCellEditor(
				values));

		// If the cell should appear like a combobox in its
		// non-editing state, also set the combobox renderer
		extraParameterColumn
				.setCellRenderer(new AcideComboBoxTableCellRenderer(values));

		_tableScrollPane = new JScrollPane(_table);
		_tableScrollPane.setPreferredSize(new Dimension(650, 250));
		_tablePanel.add(_tableScrollPane, BorderLayout.CENTER);
		
		// Sets the table columns width
		setTableColumnsWidth();

		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s154"));
		_acceptButton.setToolTipText(labels.getString("s155"));
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
						ConsoleCommandList
								.setFinalList(((AcideToolBarConfigurationWindowTableModel) _table
										.getModel()).getItems());
						String newName = "./configuration/toolbar/lastModified.TBcfg";
						ConsoleCommandList.saveFinalList(newName);

						try {

							// Sets the previous tool bar configuration
							String previous = AcideResourceManager
									.getInstance().getProperty(
											"currentToolBarConfiguration");

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
							closeWindow();

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
					closeWindow();
				}
			}
		});

		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s162"));
		_cancelButton.setToolTipText(labels.getString("s163"));
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

				// Closes the tool bar configuration window
				closeWindow();
			}
		});

		// When the user presses the escape key the accept button action is done
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

		// Adds the panels to the frame with the layout
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		add(_tableButtonsPanel, constraints);
		constraints.gridy = 1;
		add(_tablePanel, constraints);

		// BUTTON PANEL
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		constraints.gridy = 2;
		add(_buttonPanel, constraints);

		// FRAME
		setIconImage(ICON.getImage());
		setResizable(false);

		// If the window is used for modifying the tool bar configuration
		if (forModifying) {

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

			// Sets the window title for modifying
			setTitle(labels.getString("s147") + " - " + path);

			try {

				// Gets the current tool bar configuration
				String currentToolBarConfiguration = AcideResourceManager
						.getInstance().getProperty(
								"currentToolBarConfiguration");

				// Loads the shell command temporal list
				ConsoleCommandList
						.loadTemporalList(currentToolBarConfiguration);

				// Updates the model with the data
				((AcideToolBarConfigurationWindowTableModel) _table.getModel())
						.setItems(ConsoleCommandList.getTemporalList());

			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						labels.getString("s269"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
		} else {

			// Sets the window title by default
			setTitle(labels.getString("s910"));

			// Creates the model with empty data
			((AcideToolBarConfigurationWindowTableModel) _table.getModel())
					.setItems(new ArrayList<ConsoleCommand>());
		}

		// Shows the window
		setVisible(true);
		pack();

		// Centers the location
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s207"));
	}

	/**
	 * Sets the width of each one of the table columns.
	 */
	public void setTableColumnsWidth() {
		
		// Gets the scroll pane viewport
		JViewport scroll = (JViewport) _table.getParent();
		
		// Sets the scroll pane background color
		scroll.setBackground(Color.WHITE);
		
		// Gets the scroll pane viewport width
		int width = scroll.getPreferredSize().width;
		
		int columnWidth = 0;
		TableColumnModel columnModel = _table.getColumnModel();
		TableColumn tableColumn;
		
		for (int index = 0; index < _table.getColumnCount(); index++) {
			
			tableColumn = columnModel.getColumn(index);
			
			// One different width per each different column
			switch (index) {
			case 0:
				columnWidth = (20 * width) / 100;
				break;
			case 1:
				columnWidth = (25 * width) / 100;
				break;
			case 2:
				columnWidth = (39 * width) / 100;
				break;
			case 3:
				columnWidth = (35 * width) / 100;
				break;
			case 4:
				columnWidth = (25 * width) / 100;
				break;
			}
			
			// Sets the table column preferred size
			tableColumn.setPreferredWidth(columnWidth);
		}
	}

	/**
	 * Shows the ACIDE - A Configurable IDE add tool bar command window.
	 */
	private void showAcideAddToolBarCommandWindow() {
		// Shows the add tool bar command window
		new AcideAddToolBarCommandWindow(this);
	}

	/**
	 * Create the context menu for the cell in the table.
	 * 
	 * @param rowIndex
	 *            row index.
	 * @param columnIndex
	 *            column index.
	 * 
	 * @return the context menu for the cell in the table.
	 */
	private JPopupMenu createContextMenu(final int rowIndex,
			final int columnIndex) {

		// Creates the context menu
		JPopupMenu contextMenu = new JPopupMenu();

		// Creates the load image menu item
		JMenuItem loadImageMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s1018"));
		loadImageMenuItem.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				JFileChooser _fileChooser = new JFileChooser(new File("."));

				// Shows the open file dialog
				if (_fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {

					File file = _fileChooser.getSelectedFile();
					_table.getModel().setValueAt(file.getAbsolutePath(),
							rowIndex, columnIndex);
				}
			}
		});
		contextMenu.add(loadImageMenuItem);

		return contextMenu;
	}

	/**
	 * Closes the ACIDE - A Configurable IDE tool bar configuration window.
	 */
	public void closeWindow() {
		// Hides the window
		setVisible(false);
		MainWindow.getInstance().setEnabled(true);
		MainWindow.getInstance().setAlwaysOnTop(true);
		MainWindow.getInstance().setAlwaysOnTop(false);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar configuration window are
	 * change saved flag.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar configuration window are
	 *         change saved flag.
	 */
	public static boolean areChangesSaved() {
		return _areChangesSaved;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar configuration
	 * window are change saved flag.
	 * 
	 * @param areChangesSaved
	 *            new value to set.
	 */
	public static void setAreChangesSaved(boolean areChangesSaved) {
		_areChangesSaved = areChangesSaved;
	}

	/**
	 * Returns the table.
	 * 
	 * @return the table.
	 */
	public JTable getTable() {
		return _table;
	}

	/**
	 * Add a new console command to the table.
	 * 
	 * @param consoleCommand
	 *            new command to be added.
	 */
	public void addCommand(ConsoleCommand consoleCommand) {

		// Removes the selected row from the model
		((AcideToolBarConfigurationWindowTableModel) _table.getModel())
				.addItem(consoleCommand);

		// Updates the table model
		((AcideToolBarConfigurationWindowTableModel) _table.getModel())
				.fireTableRowsInserted(0,
						((AcideToolBarConfigurationWindowTableModel) _table
								.getModel()).getRowCount());

		// There are changes
		_areThereChanges = true;
	}

	/**
	 * Sets a new value to the are there changes flag.
	 * 
	 * @param areThereChange
	 *            new value to set.
	 */
	public void setAreThereChanges(boolean areThereChange) {
		_areThereChanges = areThereChange;
	}
}
