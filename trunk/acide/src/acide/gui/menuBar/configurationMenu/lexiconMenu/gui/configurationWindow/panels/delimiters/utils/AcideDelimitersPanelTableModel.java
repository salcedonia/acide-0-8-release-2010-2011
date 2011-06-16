package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.delimiters.utils;

import javax.swing.table.DefaultTableModel;

import acide.language.AcideLanguageManager;

import acide.configuration.utils.ObjectList;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.configurationWindow.panels.delimiters.AcideDelimitersPanel;

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
	private ObjectList _items;
	/**
	 * ACIDE - A Configurable IDE delimiters panel instance.
	 */
	private AcideDelimitersPanel _delimitersPanel;

	/**
	 * Creates a new ACIDE - A Configurable IDE delimiters panel table model.
	 * 
	 * @param delimitersPanel
	 *            ACIDE - A Configurable IDE delimiters panel instance for
	 *            notify it from the changes in the model.
	 */
	public AcideDelimitersPanelTableModel(
			AcideDelimitersPanel delimitersPanel) {
		
		// Creates the object list
		_items = new ObjectList();
		
		// Stores the delimiters panel instance
		_delimitersPanel = delimitersPanel;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return 1;
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
			return (String) _items.getObjectAt(rowIndex);
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
		return String.class;
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

		// There are changes in the delimiters panel
		_delimitersPanel.setAreThereChanges(true);
		
		switch (columnIndex) {
		case COLUMN_SYMBOL:
			_items.setValueAt(rowIndex, value.toString());
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
		return _items;
	}

	/**
	 * Sets a new value to the item list.
	 * 
	 * @param items
	 *            new value to set.
	 */
	public void setItems(ObjectList items) {

		// Clears the item list
		_items.clear();

		for (int index = 0; index < items.size(); index++)
			_items.insert(index, (String) items.getObjectAt(index));

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
		_items.insert(symbol);
	}

	/**
	 * Removes the item from the item list at the position given as a parameter.
	 * 
	 * @param rowIndex
	 *            position to be removed.
	 */
	public void removeItem(int rowIndex) {
		_items.removeAt(rowIndex);
	}
}
