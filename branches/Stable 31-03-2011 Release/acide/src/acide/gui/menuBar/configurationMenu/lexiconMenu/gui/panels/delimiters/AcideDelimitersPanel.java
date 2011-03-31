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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.delimiters;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import acide.configuration.lexicon.delimiters.AcideLexiconDelimitersManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.utils.AcideCellColorEditor;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.utils.AcideColorCellRenderer;
import acide.gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableModel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableSorter;
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
	private JTable _delimiterListTable;
	/**
	 * ACIDE - A Configurable IDE delimiter panel delimiter list table columns.
	 */
	private String[] _delimiterListTableColumns;
	/**
	 * ACIDE - A Configurable IDE delimiter panel sorter delimiter list table.
	 */
	private LexiconTableSorter _delimiterListTableSorter;
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
		
		// Creates the delimiter list table columns
		_delimiterListTableColumns = new String[1];
		_delimiterListTableColumns[0] = AcideLanguageManager.getInstance()
				.getLabels().getString("s440");

		// Sets the border
		setBorder(BorderFactory.createTitledBorder(null, AcideLanguageManager
				.getInstance().getLabels().getString("s429"),
				TitledBorder.LEADING, TitledBorder.DEFAULT_POSITION));

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
	public JScrollPane buildDelimiterListTable() {

		// Gets the delimiters manager
		AcideLexiconDelimitersManager delimitersManager = AcideMainWindow
				.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getDelimitersManager();

		// Builds the data to display in the table.
		Object[][] data = new Object[delimitersManager.getSize()][1];
		int aux = 0;
		for (int i = 0; i < delimitersManager.getSize(); i++) {
			data[aux][0] = delimitersManager.getDelimiterAt(i);
			aux++;
		}

		// Builds the lexicon table model
		LexiconTableModel lexiconTableModel = new LexiconTableModel();
		lexiconTableModel.setValues(_delimiterListTableColumns, data);

		// Builds the table sorter
		_delimiterListTableSorter = new LexiconTableSorter(lexiconTableModel);
		_delimiterListTable = new JTable(_delimiterListTableSorter);
		_delimiterListTableSorter
				.addTableHeaderMouseListeners(_delimiterListTable);
		_delimiterListTable.setDefaultRenderer(Color.class,
				new AcideColorCellRenderer(true));
		_delimiterListTable.setDefaultEditor(Color.class,
				new AcideCellColorEditor());
		_delimiterListTable.setPreferredScrollableViewportSize(new Dimension(
				280, 200));
		JScrollPane scrollPane = new JScrollPane(_delimiterListTable);
		return scrollPane;
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

			// Gets the delimiters manager
			AcideLexiconDelimitersManager delimitersManager = AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getLexiconConfiguration()
					.getDelimitersManager();

			delimitersManager.insertDelimiter(_delimiterTextField.getText());
			int num = delimitersManager.getSize();

			Object[][] data = new Object[num][1];
			int aux = 0;

			for (int i = 0; i < delimitersManager.getSize(); i++) {
				data[aux][0] = delimitersManager.getDelimiterAt(i);
				aux++;
			}

			LexiconTableModel myModel = new LexiconTableModel();
			myModel.setValues(_delimiterListTableColumns, data);
			_delimiterListTableSorter.setModel(myModel);
			int column = _delimiterListTableSorter.getColumn();
			if (column != -1)
				_delimiterListTableSorter.sortByColumn(column);
			_delimiterListTableSorter.fireTableDataChanged();
			_delimiterTextField.setText("");
			
			// There are changes in the panel
			_areThereChanges = true;
		}
	}

	/**
	 * ACIDE - A Configurable IDE delimiter panel escape delete button action
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

			int row = _delimiterListTable.getSelectedRow();
			int col = 0;
			for (int i = 0; i < _delimiterListTable.getColumnCount(); i++) {
				col = i;
			}
			if (row != -1) {

				String value = (String) _delimiterListTable
						.getValueAt(row, col);

				// Gets the delimiters manager
				AcideLexiconDelimitersManager delimitersManager = AcideMainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getDelimitersManager();
				delimitersManager.deleteDelimiter(value);
				int num = delimitersManager.getSize();

				Object[][] data = new Object[num][1];
				int aux = 0;
				for (int i = 0; i < delimitersManager.getSize(); i++) {
					data[aux][0] = delimitersManager.getDelimiterAt(i);
					aux++;
				}
				LexiconTableModel myModel = new LexiconTableModel();
				myModel.setValues(_delimiterListTableColumns, data);
				_delimiterListTableSorter.setModel(myModel);
				int column = _delimiterListTableSorter.getColumn();
				if (column != -1)
					_delimiterListTableSorter.sortByColumn(column);
				_delimiterListTableSorter.fireTableDataChanged();
				
				// There are changes in the panel
				_areThereChanges = true;
			}
		}
	}
}
