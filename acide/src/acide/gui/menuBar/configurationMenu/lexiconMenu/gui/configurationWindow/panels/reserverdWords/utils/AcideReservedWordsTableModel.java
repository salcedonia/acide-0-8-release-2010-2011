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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.reserverdWords.utils;

import java.awt.Color;

import acide.configuration.lexicon.tokens.AcideLexiconTokenGroup;
import acide.configuration.lexicon.tokens.AcideLexiconTokenManager;
import acide.configuration.utils.ObjectList;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.reserverdWords.AcideReservedWordsPanel;

import javax.swing.table.DefaultTableModel;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE reserved words table model.
 * 
 * @version 0.8
 * @see DefaultTableModel
 */
public class AcideReservedWordsTableModel extends DefaultTableModel {

	/**
	 * ACIDE - A Configurable IDE reserved words table model serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE reserved words table model column token
	 * constant.
	 */
	public static final int COLUMN_TOKEN = 0;
	/**
	 * ACIDE - A Configurable IDE reserved words table model column font style
	 * constant.
	 */
	public static final int COLUMN_FONT_STYLE = 1;
	/**
	 * ACIDE - A Configurable IDE reserved words table model column case
	 * sensitive constant.
	 */
	public static final int COLUMN_CASE_SENSITIVE = 2;
	/**
	 * ACIDE - A Configurable IDE reserved words table model column color
	 * constant.
	 */
	public static final int COLUMN_COLOR = 3;
	/**
	 * ACIDE - A Configurable IDE reserved words table model token manager.
	 */
	private AcideLexiconTokenManager _tokenManager;
	/**
	 * ACIDE - A Configurable IDE reserved words panel instance.
	 */
	private AcideReservedWordsPanel _reservedWordsPanel;

	/**
	 * ACIDE - A Configurable IDE reserved words table model.
	 * 
	 * @param reservedWordsPanel
	 *            ACIDE - A Configurable IDE reserved words panel instance.
	 */
	public AcideReservedWordsTableModel(
			AcideReservedWordsPanel reservedWordsPanel) {

		// Creates the token manager
		_tokenManager = new AcideLexiconTokenManager();

		// Stores the reserved words panel
		_reservedWordsPanel = reservedWordsPanel;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return 4;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		
		// Calculates the total number of rows in the table
		
		int totalSize = 0;
		for (int index = 0; index < _tokenManager.getSize(); index++) {
			totalSize += _tokenManager
							.getTokenGroupAt(index).getSize();
		}
		return totalSize;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int columnIndex) {

		switch (columnIndex) {
		case COLUMN_TOKEN:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s260");
		case COLUMN_FONT_STYLE:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s261");
		case COLUMN_CASE_SENSITIVE:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s262");
		case COLUMN_COLOR:
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

		// Gets the item
		AcideLexiconTokenGroup item = (AcideLexiconTokenGroup) _tokenManager
				.getTokenGroupAt(rowIndex);

		switch (columnIndex) {
		case COLUMN_TOKEN:

			// Returns the name
			return item.getName();
		case COLUMN_FONT_STYLE:

			// Returns the font style
			return item.getFontStyle();
		case COLUMN_CASE_SENSITIVE:

			// Returns the case sensitive flag
			return item.isCaseSensitive();
		case COLUMN_COLOR:

			// Returns the color
			return item.getColor();
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

		// There are changes in the reserved word panel
		_reservedWordsPanel.setAreThereChanges(true);

		// Gets the item
		AcideLexiconTokenGroup item = (AcideLexiconTokenGroup) _tokenManager
				.getTokenGroupAt(rowIndex);

		switch (columnIndex) {
		case COLUMN_TOKEN:

			//
			item.insertToken(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_FONT_STYLE:
			item.setFontStyle((Integer) value);
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_CASE_SENSITIVE:
			item.setCaseSensitive((Boolean) value);
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_COLOR:
			item.setColor((Color) value);
			fireTableCellUpdated(rowIndex, columnIndex);
			break;

		default:
			return;
		}
	}

	/**
	 * Returns the item list.
	 * 
	 * @return the item list.
	 */
	public ObjectList getItems() {
		return _tokenManager.getList();
	}

	/**
	 * Sets a new value to the item list.
	 * 
	 * @param items
	 *            new value to set.
	 */
	public void setItems(ObjectList items) {

		// Clears the list
		_tokenManager.getList().clear();

		for (int index = 0; index < items.size(); index++)
			_tokenManager.getList().insert(index, (String) items.getObjectAt(index));

		// Updates the model
		fireTableDataChanged();
	}

	/**
	 * Adds a new item (row) to the model.
	 * 
	 * @param tokenType
	 *            new item to be added.
	 * @param token
	 *            token name to be added.
	 */
	public void addItem(AcideLexiconTokenGroup tokenType, String token) {
		
		// Inserts a new token type
		_tokenManager.insertToken(tokenType, token);
	}

	/**
	 * Removes the item from the item list at the position given as a parameter.
	 * 
	 * @param rowIndex
	 *            position to be removed.
	 */
	public void removeItem(int rowIndex) {
		
		// Removes the token type from the object list at the index
		_tokenManager.removeTokenGroupAt(rowIndex);
	}
}
