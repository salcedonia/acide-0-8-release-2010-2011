package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.delimiters.utils;

import java.util.ArrayList;

import javax.swing.table.DefaultTableModel;

import acide.utils.ObjectList;

import acide.language.AcideLanguageManager;

import acide.configuration.toolBar.consoleComandToolBar.AcideConsoleCommand;

/**
 * ACIDE - A Configurable IDE delimiters panel table model.
 * 
 * @version 0.8
 * @see DefaultTableModel
 */
public class AcideDelimitersPanelTableModel extends DefaultTableModel {

	/**
	 * ACIDE - A Configurable IDE delimiters panel table model class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * ACIDE - A Configurable IDE delimiters panel table model column symbol
	 * constant.
	 */
	public static final int COLUMN_SYMBOL = 0;

	/**
	 * ACIDE - A Configurable IDE delimiters panel table model item list.
	 */
	private ArrayList<String> _items;

	/**
	 * Creates a new ACIDE - A Configurable IDE delimiters panel table model.
	 */
	public AcideDelimitersPanelTableModel() {
		_items = new ArrayList<String>();
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
		case COLUMN_SYMBOL:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s440");
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

		switch (columnIndex) {
		case COLUMN_SYMBOL:
			return _items.get(rowIndex);
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

		switch (columnIndex) {
		case COLUMN_SYMBOL:
			_items.set(rowIndex, value.toString());
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
	public ArrayList<String> getItems() {
		return _items;
	}

	/**
	 * Sets a new value to the item list.
	 * 
	 * @param items
	 *            new value to set.
	 */
	public void setItems(ObjectList items) {

		_items.clear();

		for (int index = 0; index < items.size(); index++)
			_items.add((String)items.getObjectAt(index));

		// Updates the model
		fireTableDataChanged();
	}

	/**
	 * Adds a new item (row) to the model.
	 * 
	 * @param symbol
	 *            new item to be added.
	 */
	public void addItem(String symbol) {
		_items.add(symbol);
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
