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
package acide.gui.menuBar.configurationMenu.toolBarMenu.gui.externalApps.utils;

import java.util.ArrayList;

import javax.swing.table.DefaultTableModel;

import acide.configuration.toolBar.externalAppsToolBar.AcideExternalAppsToolBarButtonConf;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.externalApps.AcideExternalAppsConfigurationPanel;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE external applications configuration panel table
 * model.
 * 
 * @version 0.8
 * @see DefaultTableModel
 */
public class AcideExternalAppsConfigurationPanelTableModel extends
		DefaultTableModel {

	/**
	 * ACIDE - A Configurable IDE external applications configuration panel
	 * table model class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE external applications configuration panel
	 * table model column name constant.
	 */
	public static final int COLUMN_NAME = 0;
	/**
	 * ACIDE - A Configurable IDE external applications configuration panel
	 * table model column path constant.
	 */
	public static final int COLUMN_PATH = 1;
	/**
	 * ACIDE - A Configurable IDE external applications configuration panel
	 * table model column hint text constant.
	 */
	public static final int COLUMN_HINT_TEXT = 2;
	/**
	 * ACIDE - A Configurable IDE external applications configuration panel
	 * table model column icon constant.
	 */
	public static final int COLUMN_ICON = 3;
	/**
	 * ACIDE - A Configurable IDE external applications configuration panel
	 * table model item list.
	 */
	private ArrayList<AcideExternalAppsToolBarButtonConf> _items;
	/**
	 * ACIDE - A Configurable IDE external applications configuration panel
	 * instance.
	 */
	private AcideExternalAppsConfigurationPanel _externalApplicationsConfigurationPanel;

	/**
	 * Creates a new ACIDE - A Configurable IDE external applications
	 * configuration panel table model.
	 * 
	 * @param externalApplicationsConfigurationPanel
	 *            <p>
	 *            ACIDE - A Configurable IDE external applications configuration
	 *            panel instance.
	 *            </p>
	 *            <p>
	 *            It is needed to tell it that there have been modifications on
	 *            it. Otherwise, when a changed is performed in the model, the
	 *            configuration window will not be able to notice it.
	 *            </p>
	 */
	public AcideExternalAppsConfigurationPanelTableModel(
			AcideExternalAppsConfigurationPanel externalApplicationsConfigurationPanel) {

		// Stores the external applications configuration panel instance
		_externalApplicationsConfigurationPanel = externalApplicationsConfigurationPanel;

		// Creates the item list
		_items = new ArrayList<AcideExternalAppsToolBarButtonConf>();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return AcideExternalAppsToolBarButtonConf.NUMBER_OF_PARAMETERS;
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
		case COLUMN_PATH:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s337");
		case COLUMN_HINT_TEXT:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s262");
		case COLUMN_ICON:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s263");
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

		AcideExternalAppsToolBarButtonConf item = (AcideExternalAppsToolBarButtonConf) _items
				.get(rowIndex);

		switch (columnIndex) {
		case COLUMN_NAME:
			return item.getName();
		case COLUMN_PATH:
			return item.getPath();
		case COLUMN_HINT_TEXT:
			return item.getHintText();
		case COLUMN_ICON:
			return item.getIcon();
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

		// There are changes on the table in the external applications
		// configuration panel
		_externalApplicationsConfigurationPanel.setAreThereChanges(true);

		// Gets the console command from the table row
		AcideExternalAppsToolBarButtonConf buttonConfiguration = (AcideExternalAppsToolBarButtonConf) _items
				.get(rowIndex);

		switch (columnIndex) {
		case COLUMN_NAME:
			buttonConfiguration.setName(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_PATH:
			buttonConfiguration.setPath(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_HINT_TEXT:
			buttonConfiguration.setHintText(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_ICON:
			buttonConfiguration.setIcon(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		default:
			return;
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications
	 * configuration panel table model item list.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications
	 *         configuration panel table model item list.
	 */
	public ArrayList<AcideExternalAppsToolBarButtonConf> getItems() {
		return _items;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE external applications
	 * configuration panel table model item list.
	 * 
	 * @param items
	 *            new value to set.
	 */
	public void setItems(ArrayList<AcideExternalAppsToolBarButtonConf> items) {

		// Clears the item list
		_items.clear();

		// Adds all the items to it
		for (AcideExternalAppsToolBarButtonConf consoleCommand : items)
			_items.add(consoleCommand);

		// Updates the model
		fireTableDataChanged();
	}

	/**
	 * Adds a new item (row) to the model.
	 * 
	 * @param item
	 *            new item to be added.
	 */
	public void addItem(AcideExternalAppsToolBarButtonConf item) {
		_items.add(item);
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
