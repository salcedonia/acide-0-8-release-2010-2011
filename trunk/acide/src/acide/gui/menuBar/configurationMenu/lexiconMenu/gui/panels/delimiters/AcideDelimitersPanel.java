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

import acide.configuration.lexicon.delimiters.DelimiterList;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.utils.AcideCellColorEditor;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.utils.AcideColorCellRenderer;
import acide.gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableModel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableSorter;

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


import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE delimiter panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideDelimitersPanel extends JPanel{

	/**
	 * ACIDE - A Configurable IDE delimiter panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE delimiter panel delimiter label.
	 */
	private final JLabel _delimiterLabel;
	/**
	 * ACIDE - A Configurable IDE delimiter panel delimiter text field.
	 */
	private final JTextField _delimiterTextField;
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
	 * Creates a new ACIDE - A Configurable IDE delimiter panel.
	 */
	public AcideDelimitersPanel(){
	
		_delimiterListTableColumns = new String[1];
		_delimiterListTableColumns[0] = AcideLanguageManager.getInstance().getLabels().getString("s440");
	
		// Sets the layout
		setLayout(new GridBagLayout());
		
		// Sets the border
		setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels().getString("s429"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		
		// DELIMITER LABEL
		_delimiterLabel = new JLabel(AcideLanguageManager.getInstance().getLabels().getString("s432"), JLabel.CENTER);
		
		// DELIMITER TEXT FIELD
		_delimiterTextField = new JTextField();
		
		// ADD DIVIDER BUTTON
		_addButton = new JButton(AcideLanguageManager.getInstance().getLabels().getString("s385"));
		_addButton.setVerticalTextPosition(AbstractButton.CENTER);
		_addButton.setHorizontalTextPosition(AbstractButton.LEADING);
		// addDivider.setToolTipText(labels.getString("s386"));
		
		// DELETE DIVIDER BUTTON
		_deleteButton = new JButton(AcideLanguageManager.getInstance().getLabels().getString("s387"));
		_deleteButton.setVerticalTextPosition(AbstractButton.CENTER);
		_deleteButton
				.setHorizontalTextPosition(AbstractButton.LEADING);
		// deleteDivider.setToolTipText(labels.getString("s388"));
		
		// Sets the listeners of the window components
		setListeners();
		
		// Adds the components to the panel with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		add(_delimiterLabel, constraints);
		constraints.gridx = 1;
		add(_delimiterTextField, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_addButton, constraints);
		constraints.gridx = 1;
		add(_deleteButton, constraints);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {
		
		// ADD BUTTON
		_addButton.addActionListener(new AddButtonAction());
		
		// DELETE BUTTON
		_deleteButton.addActionListener(new DeleteButtonAction());
	}
	
	/**
	 * Returns the build delimiter list table.
	 * 
	 * @return the build delimiter list table.
	 */
	public JScrollPane buildDelimiterListTable() {

		// Gets the delimiter list
		DelimiterList delimiterList = DelimiterList.getInstance();

		// Builds the data to display in the table.
		Object[][] data = new Object[delimiterList.getSize()][1];
		int aux = 0;
		for (int i = 0; i < delimiterList.getSize(); i++) {
			data[aux][0] = delimiterList.getDelimiterAt(i);
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
		_delimiterListTable.setDefaultRenderer(Color.class, new AcideColorCellRenderer(
				true));
		_delimiterListTable.setDefaultEditor(Color.class, new AcideCellColorEditor());
		_delimiterListTable.setPreferredScrollableViewportSize(new Dimension(
				280, 200));
		JScrollPane scrollPane = new JScrollPane(_delimiterListTable);
		return scrollPane;
	}
	
	/**
	 * ACIDE - A Configurable IDE delimiter panel escape add button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AddButtonAction implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			DelimiterList dividerList = DelimiterList.getInstance();
			dividerList.insertDelimiter(_delimiterTextField.getText());
			int num = dividerList.getSize();

			Object[][] data = new Object[num][1];
			int aux = 0;

			for (int i = 0; i < dividerList.getSize(); i++) {
				data[aux][0] = dividerList.getDelimiterAt(i);
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
		}
	}
	
	/**
	 * ACIDE - A Configurable IDE delimiter panel escape delete button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class DeleteButtonAction implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
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

				String value = (String) _delimiterListTable.getValueAt(row,
						col);
				DelimiterList dl = DelimiterList.getInstance();
				dl.deleteDelimiter(value);
				int num = dl.getSize();

				Object[][] data = new Object[num][1];
				int aux = 0;
				for (int i = 0; i < dl.getSize(); i++) {
					data[aux][0] = dl.getDelimiterAt(i);
					aux++;
				}
				LexiconTableModel myModel = new LexiconTableModel();
				myModel.setValues(_delimiterListTableColumns, data);
				_delimiterListTableSorter.setModel(myModel);
				int column = _delimiterListTableSorter.getColumn();
				if (column != -1)
					_delimiterListTableSorter.sortByColumn(column);
				_delimiterListTableSorter.fireTableDataChanged();
			}
		}
	}
}
