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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.reserverdWords.utils;

import java.awt.Color;
import java.util.ArrayList;

import acide.configuration.lexicon.tokens.AcideLexiconTokenType;
import acide.configuration.toolBar.consoleComandToolBar.AcideConsoleCommand;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.reserverdWords.AcideReservedWordsPanel;

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
	 * ACIDE - A Configurable IDE reserved words table model column name
	 * constant.
	 */
	public static final int COLUMN_NAME = 0;
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

		_reservedWordsPanel = reservedWordsPanel;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return AcideConsoleCommand.NUMBER_OF_PARAMETERS;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getTokenTypeManager() != null)
			return AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getLexiconConfiguration()
					.getTokenTypeManager().getSize();
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

		AcideLexiconTokenType item = (AcideLexiconTokenType) AcideMainWindow
				.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getTokenTypeManager().getTokenType(rowIndex);

		switch (columnIndex) {
		case COLUMN_NAME:
			return item.getName();
		case COLUMN_FONT_STYLE:
			// return item.getAction();
		case COLUMN_CASE_SENSITIVE:
			return item.isCaseSensitive();
		case COLUMN_COLOR:
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

		// There are changes on the table in the reserved words panel
		_reservedWordsPanel.setAreThereChanges(true);

		AcideLexiconTokenType tokenType = (AcideLexiconTokenType) AcideMainWindow
				.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getTokenTypeManager().getTokenType(rowIndex);

		switch (columnIndex) {
		case COLUMN_NAME:
			tokenType.setToken(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_FONT_STYLE:
			// tokenType.setAction(value.toString());
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_CASE_SENSITIVE:
			tokenType.setCaseSensitive((Boolean) value);
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_COLOR:
			tokenType.setColor((Color) value);
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
	public ArrayList<AcideConsoleCommand> getItems() {
		return null;
	}

	/**
	 * Sets a new value to the item list.
	 * 
	 * @param items
	 *            new value to set.
	 */
	public void setItems(ArrayList<AcideConsoleCommand> items) {

		/*
		 * TokenTypeList.getInstance().reset();
		 * 
		 * for (TokenTypeList consoleCommand : items)
		 * TokenTypeList.getInstance().add(consoleCommand);
		 */
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
	public void addItem(AcideLexiconTokenType tokenType, String token) {
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getTokenTypeManager().insertTokenType(tokenType, token);
	}

	/**
	 * Removes the item from the item list at the position given as a parameter.
	 * 
	 * @param rowIndex
	 *            position to be removed.
	 */
	public void removeItem(int rowIndex) {
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getTokenTypeManager().removeTokenType(rowIndex);
	}
}
