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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.delimiters;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.delimiters.utils.AcideDelimitersPanelTableModel;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE delimiter panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideDelimitersPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE delimiter panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE delimiter panel delimiter label.
	 */
	private JLabel _delimiterLabel;
	/**
	 * ACIDE - A Configurable IDE delimiter panel delimiter text field.
	 */
	private JTextField _delimiterTextField;
	/**
	 * ACIDE - A Configurable IDE delimiter panel add button.
	 */
	private JButton _addButton;
	/**
	 * ACIDE - A Configurable IDE delimiter panel delete button.
	 */
	private JButton _deleteButton;
	/**
	 * ACIDE - A Configurable IDE delimiter panel delimiter list table.
	 */
	private JTable _table;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window are there changes
	 * flag.
	 */
	private boolean _areThereChanges;

	/**
	 * Creates a new ACIDE - A Configurable IDE delimiter panel.
	 */
	public AcideDelimitersPanel() {

		super();

		// There are no changes yet
		_areThereChanges = false;

		// Builds the panel components
		buildComponents();

		// Sets the listeners of the panel components
		setListeners();

		// Adds the components to the panel
		addComponents();
	}

	/**
	 * Builds the panel components.
	 */
	private void buildComponents() {

		// Creates the delimiter label
		_delimiterLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s432"), JLabel.CENTER);

		// Creates the delimiter text field
		_delimiterTextField = new JTextField();

		// Creates the add button
		_addButton = new JButton(AcideLanguageManager.getInstance().getLabels()
				.getString("s385"));

		// Sets the add button vertical text position as center
		_addButton.setVerticalTextPosition(AbstractButton.CENTER);

		// Sets the add button horizontal text position as leading
		_addButton.setHorizontalTextPosition(AbstractButton.LEADING);
		// addDivider.setToolTipText(labels.getString("s386"));

		// Creates the delete button
		_deleteButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s387"));

		// Sets the delete button vertical text position as center
		_deleteButton.setVerticalTextPosition(AbstractButton.CENTER);

		// Sets the delete button horizontal text position as leading
		_deleteButton.setHorizontalTextPosition(AbstractButton.LEADING);
		// deleteDivider.setToolTipText(labels.getString("s388"));
	}

	/**
	 * Adds the components to the panel with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the panel with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);

		// Adds the delimiter label to the panel
		add(_delimiterLabel, constraints);

		constraints.gridx = 1;

		// Adds the delimiter text field to the panel
		add(_delimiterTextField, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the add button to the panel
		add(_addButton, constraints);

		constraints.gridx = 1;

		// Adds the delete button to the panel
		add(_deleteButton, constraints);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// Sets the add button action listener
		_addButton.addActionListener(new AddButtonAction());

		// Sets the delete button action listener
		_deleteButton.addActionListener(new DeleteButtonAction());
	}

	/**
	 * Returns the build delimiter list table.
	 * 
	 * @return the build delimiter list table.
	 */
	public JScrollPane buildTable() {

		// Creates the table
		_table = new JTable(new AcideDelimitersPanelTableModel(this));

		// Updates the table model with the data in the delimiters manager
		((AcideDelimitersPanelTableModel) _table.getModel())
				.setItems(AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getDelimitersManager().getList());

		// Sets the color cell renderer
		//_table.setDefaultRenderer(Color.class, new AcideColorCellRenderer(true));

		// Sets the cell color editor
		//_table.setDefaultEditor(Color.class, new AcideCellColorEditor());

		// Sets the preferred scrollable viewport size
		_table.setPreferredScrollableViewportSize(new Dimension(280, 200));

		// Creates the scroll pane
		JScrollPane scrollPane = new JScrollPane(_table);
		return scrollPane;
	}

	/**
	 * Returns the table.
	 * 
	 * @return the table.
	 */
	public JTable getTable(){
		return _table;
	}
	
	/**
	 * Returns the are there changes flag.
	 * 
	 * @return the are there changes flag.
	 */
	public boolean getAreThereChanges() {
		return _areThereChanges;
	}

	/**
	 * Sets a new value to the are there changes flag.
	 * 
	 * @param areThereChanges
	 *            new value to set.
	 */
	public void setAreThereChanges(boolean areThereChanges) {
		_areThereChanges = areThereChanges;
	}

	/**
	 * ACIDE - A Configurable IDE delimiter panel escape add button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AddButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Adds the item to the table
			((AcideDelimitersPanelTableModel) _table.getModel())
					.addItem(_delimiterTextField.getText());

			// Updates the table model
			((AcideDelimitersPanelTableModel) _table.getModel())
					.fireTableRowsInserted(0,
							((AcideDelimitersPanelTableModel) _table
									.getModel()).getRowCount());
			
			// Clears the delimiter text field
			_delimiterTextField.setText("");

			// There are changes in the panel
			_areThereChanges = true;
		}
	}

	/**
	 * ACIDE - A Configurable IDE delimiter panel delete button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class DeleteButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
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
				((AcideDelimitersPanelTableModel) _table
						.getModel()).removeItem(_table.getSelectedRow());

				// Updates the table model
				((AcideDelimitersPanelTableModel) _table
						.getModel()).fireTableRowsDeleted(0,
						((AcideDelimitersPanelTableModel) _table
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
		}
	}
}
