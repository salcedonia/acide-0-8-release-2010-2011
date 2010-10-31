package gui.toolBarButton;

import javax.swing.table.AbstractTableModel;

/**
 * Table model for the editable toolBar of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ToolBarButtonTableModel extends AbstractTableModel {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Column names.
	 */
	private String[] _columnNames;
	/**
	 * Data of the table.
	 */
	private Object[][] _data;

	/**
	 * Returns the column count.
	 * 
	 * @return the column count.
	 */
	public int getColumnCount() {
		return _columnNames.length;
	}

	/**
	 * Returns the row count.
	 * 
	 * @return The row count.
	 */
	public int getRowCount() {
		return _data.length;
	}

	/**
	 * Returns the column at the position col given as a parameter.
	 * 
	 * @param col Column to select.
	 * 
	 * @return The column at the position col given as a parameter.
	 */
	public String getColumnName(int col) {
		return _columnNames[col];
	}

	/**
	 * Returns the value at the row and column given as parameters.
	 * 
	 * @param row Row of the table.
	 * @param col Column of the table.
	 * 
	 * @return The value.
	 */
	public Object getValueAt(int row, int col) {
		return _data[row][col];
	}

	/**
	 * Returns the column class of a column given as a parameter.
	 * 
	 * @param c Column to select.
	 * 
	 * @return The column class of a column given as a parameter.
	 */
	public Class<?> getColumnClass(int c) {
		return getValueAt(0, c).getClass();
	}

//	/*
//	 * Don't need to implement this method unless your table's editable.
//	 */
//	public boolean isCellEditable(int row, int col) {
//		// Note that the data/cell address is constant,
//		// no matter where the cell appears onscreen.
//		if (col < 2) {
//			return false;
//		}
//		else {
//			return true;
//		}
//	}
//
//	/*
//	 * Don't need to implement this method unless your table's data can change.
//	 */
//	public void setValueAt(Object value, int row, int col) {
//		// debugging code not shown...
//		// ugly class cast code for Integers not shown...
//		data[row][col] = value;
//		// debugging code not shown...
//	}
	
	/**
	 * Set the values in a column list given as a parameter.
	 * 
	 * @param c Column array.
	 * @param d Data to set.
	 */
	public void setValues(String[] c, Object[][] d){
    	_columnNames = c;
    	_data = d;
    }
}
