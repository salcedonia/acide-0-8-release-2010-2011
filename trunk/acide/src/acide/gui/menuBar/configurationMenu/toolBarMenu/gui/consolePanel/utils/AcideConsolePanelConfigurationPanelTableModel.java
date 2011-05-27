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
package acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel.utils;

import java.util.ArrayList;

import acide.configuration.toolBar.consolePanelToolBar.AcideConsolePanelToolBarButtonConf;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel.AcideConsolePanelConfigurationPanel;
import acide.gui.toolBarPanel.consolePanelToolBar.utils.AcideParameterType;

import javax.swing.table.DefaultTableModel;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE console panel configuration panel table model.
 * 
 * @version 0.8
 * @see DefaultTableModel
 */
public class AcideConsolePanelConfigurationPanelTableModel extends
		DefaultTableModel {

	/**
	 * ACIDE - A Configurable IDE console panel configuration panel table model
	 * class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE console panel configuration panel table model
	 * column name constant.
	 */
	public static final int COLUMN_NAME = 0;
	/**
	 * ACIDE - A Configurable IDE console panel configuration panel table model
	 * column name constant.
	 */
	public static final int COLUMN_ACTION = 1;
	/**
	 * ACIDE - A Configurable IDE console panel configuration panel table model
	 * column action constant.
	 */
	public static final int COLUMN_HINT_TEXT = 2;
	/**
	 * ACIDE - A Configurable IDE console panel configuration panel table model
	 * column hint text constant.
	 */
	public static final int COLUMN_ICON = 3;
	/**
	 * ACIDE - A Configurable IDE console panel configuration panel table model
	 * column parameter type constant.
	 */
	public static final int COLUMN_PARAMETER_TYPE = 4;
	/**
	 * ACIDE - A Configurable IDE console panel configuration panel table model
	 * column is executed in system shell constant.
	 */
	public static final int COLUMN_IS_EXECUTED_IN_SYSTEM_SHELL = 5;
	/**
	 * ACIDE - A Configurable IDE console panel configuration panel table model
	 * item list.
	 */
	private ArrayList<AcideConsolePanelToolBarButtonConf> _items;
	/**
	 * ACIDE - A Configurable IDE console panel configuration panel instance.
	 */
	private AcideConsolePanelConfigurationPanel _consolePanelConfigurationPanel;

	/**
	 * Creates a new ACIDE - A Configurable IDE console panel configuration
	 * panel table model.
	 * 
	 * @param consolePanelConfigurationPanel
	 *            <p>
	 *            ACIDE - A Configurable IDE console panel configuration panel
	 *            instance.
	 *            </p>
	 *            <p>
	 *            It is needed to tell it that there have been modifications on
	 *            it. Otherwise, when a changed is performed in the model, the
	 *            configuration window will not be able to notice it.
	 *            </p>
	 */
	public AcideConsolePanelConfigurationPanelTableModel(
			AcideConsolePanelConfigurationPanel consolePanelConfigurationPanel) {

		// Stores the console configuration panel instance
		_consolePanelConfigurationPanel = consolePanelConfigurationPanel;

		// Creates the item list
		_items = new ArrayList<AcideConsolePanelToolBarButtonConf>();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return AcideConsolePanelToolBarButtonConf.NUMBER_OF_PARAMETERS;
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
		case COLUMN_IS_EXECUTED_IN_SYSTEM_SHELL:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s1070");
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

		AcideConsolePanelToolBarButtonConf item = (AcideConsolePanelToolBarButtonConf) _items
				.get(rowIndex);

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
		case COLUMN_IS_EXECUTED_IN_SYSTEM_SHELL:
			return item.isExecutedInSystemShell();
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

		// There are changes on the table in the console panel configuration
		// panel
		_consolePanelConfigurationPanel.setAreThereChanges(true);

		// Gets the console command from the table row
		AcideConsolePanelToolBarButtonConf consoleCommand = (AcideConsolePanelToolBarButtonConf) _items
				.get(rowIndex);

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
			break;
		case COLUMN_IS_EXECUTED_IN_SYSTEM_SHELL:
			consoleCommand.setIsExecutedInSystemShell((Boolean) value);
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		default:
			return;
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration panel
	 * table model item list.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration panel
	 *         table model item list.
	 */
	public ArrayList<AcideConsolePanelToolBarButtonConf> getItems() {
		return _items;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE console panel
	 * configuration panel table model item list.
	 * 
	 * @param items
	 *            new value to set.
	 */
	public void setItems(ArrayList<AcideConsolePanelToolBarButtonConf> items) {

		// Clears the item list
		_items.clear();

		// Adds all the items to it
		for (AcideConsolePanelToolBarButtonConf consoleCommand : items)
			_items.add(consoleCommand);

		// Updates the model
		fireTableDataChanged();
	}

	/**
	 * Adds a new item (row) to the model.
	 * 
	 * @param consoleCommand
	 *            new item to be added.
	 */
	public void addItem(AcideConsolePanelToolBarButtonConf consoleCommand) {
		_items.add(consoleCommand);
	}

	/**
	 * Removes the item from the item list at the position given as a parameter.
	 * 
	 * @param rowIndex
	 *            position to be removed.
	 */
	public void removeItem(int rowIndex) {
		_items.remove(rowIndex);
	}
}
