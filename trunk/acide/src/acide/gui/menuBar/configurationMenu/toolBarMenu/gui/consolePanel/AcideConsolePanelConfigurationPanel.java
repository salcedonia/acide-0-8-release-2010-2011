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
package acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import acide.configuration.toolBar.consolePanelToolBar.AcideConsolePanelToolBarButtonConf;
import acide.files.AcideFileManager;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel.utils.AcideComboBoxTableCellEditor;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel.utils.AcideComboBoxTableCellRenderer;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel.utils.AcideConsolePanelConfigurationPanelTableModel;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE -A Configurable IDE tool bar configuration window console panel
 * configuration panel.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideConsolePanelConfigurationPanel extends JPanel {

	/**
	 * ACIDE -A Configurable IDE tool bar configuration window console panel
	 * configuration panel serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table panel.
	 */
	private JPanel _tablePanel;
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
	 * ACIDE - A Configurable IDE tool bar configuration window add button.
	 */
	private JButton _addButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window quit button.
	 */
	private JButton _quitButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window flag that
	 * indicates if there are changes in the window. The changes are applied
	 * only when the user selects the add, modify or quit button.
	 */
	private static boolean _areThereChanges;

	/**
	 * Creates a new ACIDE -A Configurable IDE tool bar configuration window
	 * console panel configuration panel.
	 */
	public AcideConsolePanelConfigurationPanel() {

		super();

		// There are no changes by the moment
		_areThereChanges = false;

		// Builds the window components
		buildComponents();

		// Adds the components to the window
		addComponents();

		// Sets the action listeners of the window componens
		setListeners();
	}

	/**
	 * Sets the listeners for the ACIDE - A Configurable IDE console panel
	 * configuration panel components.
	 */
	private void setListeners() {

		// Sets the add button action listener
		_addButton.addActionListener(new AddButtonAction());

		// Sets the quit button action listener
		_quitButton.addActionListener(new QuitButtonAction());

		// Sets the table mouse listener
		_table.addMouseListener(new TableMouseListener());
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE console panel
	 * configuration panel.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.insets = new Insets(5, 5, 5, 5);

		// Adds the add button to the table buttons panel
		_tableButtonsPanel.add(_addButton, constraints);

		constraints.gridx = 1;

		// Adds the quit button to the table buttons panel
		_tableButtonsPanel.add(_quitButton, constraints);

		// Creates the table
		buildTable();

		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;

		// Adds the table buttons panel to the window
		add(_tableButtonsPanel, constraints);

		constraints.gridy = 1;

		// Adds the table panel to the window
		add(_tablePanel, constraints);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE console panel configuration panel
	 * components.
	 */
	private void buildComponents() {

		// Creates the table panel
		_tablePanel = new JPanel(new BorderLayout());

		// Creates the table buttons panel
		_tableButtonsPanel = new JPanel(new GridBagLayout());

		// Creates the add button
		_addButton = new JButton(AcideLanguageManager.getInstance().getLabels()
				.getString("s137"));

		// Sets the add button tool tip text
		_addButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s138"));

		// Creates the quit button
		_quitButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s148"));

		// Sets the quit button tool tip text
		_quitButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s149"));
	}

	/**
	 * Creates and configure the table and its model.
	 */
	public void buildTable() {

		// Creates the table with the model
		_table = new JTable(new AcideConsolePanelConfigurationPanelTableModel(
				this));

		// Sets the single selection in the table
		_table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		// The columns width are not equal
		_table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);

		// These are the combo box values
		String[] values = new String[] {
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1005"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1006"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1007"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1008") };

		// Set the combo box editor on the 4th visible column
		TableColumn extraParameterColumn = _table.getColumnModel().getColumn(4);
		extraParameterColumn.setCellEditor(new AcideComboBoxTableCellEditor(
				values));

		// If the cell should appear like a combobox in its
		// non-editing state, also set the combobox renderer
		extraParameterColumn
				.setCellRenderer(new AcideComboBoxTableCellRenderer(values));

		// Creates the table scroll pane with the table
		_tableScrollPane = new JScrollPane(_table);

		// Sets its preferred size
		_tableScrollPane.setPreferredSize(new Dimension(750, 250));

		// Adds the table scroll panel to the window
		_tablePanel.add(_tableScrollPane, BorderLayout.CENTER);

		// Sets the table columns width
		setTableColumnsWidth();
	}

	/**
	 * Sets the width of each one of the table columns.
	 */
	public void setTableColumnsWidth() {

		// Gets the scroll pane viewport
		JViewport scroll = (JViewport) _table.getParent();

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
			case 5:
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
		new AcideAddConsolePanelToolBarButtonWindow(this);
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

				// Removes the filter
				AcideFileManager
						.getInstance()
						.getFileChooser()
						.removeChoosableFileFilter(
								AcideFileManager.getInstance().getFileChooser()
										.getFileFilter());

				// Ask the path to the user
				String absolutePath = AcideFileManager.getInstance()
						.askForOpenFile(true);

				if (absolutePath != null)

					// Updates the table model with the absolute path
					_table.getModel().setValueAt(absolutePath, rowIndex,
							columnIndex);
			}
		});
		contextMenu.add(loadImageMenuItem);

		return contextMenu;
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
	 * Returns the are there changes flag.
	 * 
	 * @return the are there changes flag.
	 */
	public boolean areThereChanges() {
		return _areThereChanges;
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

	/**
	 * Add a new console command to the table.
	 * 
	 * @param consoleCommand
	 *            new command to be added.
	 */
	public void addCommand(AcideConsolePanelToolBarButtonConf consoleCommand) {

		// Adds the selected row to the model
		((AcideConsolePanelConfigurationPanelTableModel) _table.getModel())
				.addItem(consoleCommand);

		// Updates the table model
		((AcideConsolePanelConfigurationPanelTableModel) _table.getModel())
				.fireTableRowsInserted(0,
						((AcideConsolePanelConfigurationPanelTableModel) _table
								.getModel()).getRowCount());

		// Selects the new row in the table
		_table.setRowSelectionInterval(_table.getRowCount() - 1,
				_table.getRowCount() - 1);

		// Scrolls automatically to see the new row, without doing it
		// automatically
		_table.scrollRectToVisible(_table.getCellRect(_table.getRowCount() - 1,
				0, true));

		// There are changes
		_areThereChanges = true;
	}

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window console panel
	 * configuration panel add button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AddButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Shows the add tool bar command window
			showAcideAddToolBarCommandWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window console panel
	 * configuration panel quit button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class QuitButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If there is a selected row
			if (_table.getSelectedRow() != -1) {

				// Gets the selected row
				int selectedRow = _table.getSelectedRow();

				// Gets the last row index
				int lastRow = _table.getRowCount() - 1;

				// Removes the selected row from the model
				((AcideConsolePanelConfigurationPanelTableModel) _table
						.getModel()).removeItem(_table.getSelectedRow());

				// Updates the table model
				((AcideConsolePanelConfigurationPanelTableModel) _table
						.getModel()).fireTableRowsDeleted(0,
						((AcideConsolePanelConfigurationPanelTableModel) _table
								.getModel()).getRowCount());

				// If there is more than on element
				if (lastRow > 0) {

					// If the selected row is not the last row in the table
					if (selectedRow < lastRow)
						// Selects the following row in the table
						_table.setRowSelectionInterval(selectedRow, selectedRow);
					else
						// Selects the last row in the table
						_table.setRowSelectionInterval(lastRow - 1, lastRow - 1);
				}

				// There are changes
				_areThereChanges = true;
			} else {

				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s156"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s157"), JOptionPane.ERROR_MESSAGE);
			}

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s168"));
		}
	}

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window console panel
	 * configuration panel table mouse listener.
	 * 
	 * @version 0.8
	 * @see MouseAdapter
	 */
	class TableMouseListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			maybeShowPopup(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent )
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
				int column = _table.columnAtPoint(p);
				int row = _table.rowAtPoint(p);

				// Translate table index to model index
				int modelColumn = _table
						.getColumn(_table.getColumnName(column))
						.getModelIndex();

				if (row >= 0 && row < _table.getRowCount()) {

					// Creates popup menu
					JPopupMenu contextMenu = createContextMenu(row, modelColumn);

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
	}
}
