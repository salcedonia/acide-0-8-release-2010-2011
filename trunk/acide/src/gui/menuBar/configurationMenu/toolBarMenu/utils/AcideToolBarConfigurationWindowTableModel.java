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
package gui.menuBar.configurationMenu.toolBarMenu.utils;

import java.util.ArrayList;

import es.configuration.toolBar.consoleComandToolBar.ConsoleCommand;
import gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;
import gui.toolBarPanel.consoleCommandToolBar.utils.AcideParameterType;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;

import language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE tool bar configuration window table model.
 * 
 * @version 0.8
 * @see AbstractTableModel
 */
public class AcideToolBarConfigurationWindowTableModel extends
		DefaultTableModel {

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table model
	 * class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table model column name constant.
	 */
	public static final int COLUMN_NAME = 0;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table model column name constant.
	 */
	public static final int COLUMN_ACTION = 1;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table model column action constant.
	 */
	public static final int COLUMN_HINT_TEXT = 2;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table model column hint text constant.
	 */
	public static final int COLUMN_ICON = 3;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table model column icon constant.
	 */
	public static final int COLUMN_PARAMETER_TYPE = 4;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window table model column parameter type constant.
	 */
	private ArrayList<ConsoleCommand> _items;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window instance.
	 */
	private AcideToolBarConfigurationWindow _toolBarConfigurationWindow;

	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar configuration window table model.
	 * 
	 * @param acideToolBarConfigurationWindow ACIDE - A Configurable IDE tool bar configuration window instance.
	 */
	public AcideToolBarConfigurationWindowTableModel(AcideToolBarConfigurationWindow acideToolBarConfigurationWindow){
	
		_toolBarConfigurationWindow = acideToolBarConfigurationWindow;
		_items = new ArrayList<ConsoleCommand>();
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return ConsoleCommand.NUMBER_OF_PARAMETERS;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		if (_items != null)
			return _items.size();
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int columnIndex) {

		switch (columnIndex) {
		case COLUMN_NAME:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s260");
		case COLUMN_ACTION:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s261");
		case COLUMN_HINT_TEXT:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s262");
		case COLUMN_ICON:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s263");
		case COLUMN_PARAMETER_TYPE:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s1003");

		default:
			return "Column " + columnIndex;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@Override
	public Object getValueAt(int rowIndex, int columnIndex) {

		ConsoleCommand item = (ConsoleCommand) _items.get(rowIndex);

		switch (columnIndex) {
		case COLUMN_NAME:
			return item.getName();
		case COLUMN_ACTION:
			return item.getAction();
		case COLUMN_HINT_TEXT:
			return item.getHintText();
		case COLUMN_ICON:
			return item.getIcon();
		case COLUMN_PARAMETER_TYPE:
			return item.getParameterType().toString();
		default:
			return null;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
	 */
	@Override
	public Class<?> getColumnClass(int column) {
		return getValueAt(0, column).getClass();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
	 */
	@Override
	public boolean isCellEditable(int row, int col) {

		// All the cells are editable
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object,
	 * int, int)
	 */
	@Override
	public void setValueAt(Object value, int rowIndex, int columnIndex) {

		// There are changes on the table in the tool bar configuration window
		_toolBarConfigurationWindow.setAreThereChanges(true);
		
		ConsoleCommand consoleCommand = (ConsoleCommand) _items.get(rowIndex);

		switch (columnIndex) {
		case COLUMN_NAME:
			consoleCommand.setName(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_ACTION:
			consoleCommand.setAction(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_HINT_TEXT:
			consoleCommand.setHintText(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_ICON:
			consoleCommand.setIcon(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_PARAMETER_TYPE:
			consoleCommand.setParameterType(AcideParameterType
					.fromStringToEnum(value.toString()));
			fireTableCellUpdated(rowIndex, columnIndex);

		default:
			return;
		}
	}

	/**
	 * Returns the item list.
	 * 
	 * @return the item list.
	 */
	public ArrayList<ConsoleCommand> getItems() {
		return _items;
	}

	/**
	 * Sets a new value to the item list.
	 * 
	 * @param items new value to set.
	 */
	public void setItems(ArrayList<ConsoleCommand> items) {

		_items.clear();

		for (ConsoleCommand consoleCommand : items)
			_items.add(consoleCommand);
		
		// Updates the model
		fireTableDataChanged();
	}

	/**
	 * Adds a new item (row) to the model.
	 * 
	 * @param consoleCommand new item to be added.
	 */
	public void addItem(ConsoleCommand consoleCommand) {
		_items.add(consoleCommand);
	}

	/**
	 * Removes the item from the item list at the position given as a parameter.
	 * 
	 * @param rowIndex position to be removed.
	 */
	public void removeItem(int rowIndex) {
		_items.remove(rowIndex);
	}
}
