package gui.toolBarButton;

import javax.swing.table.AbstractTableModel;

/**
 * 
 */
public class ToolBarButtonTableModel extends AbstractTableModel {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private String[] _columnNames;
	/**
	 * 
	 */
	private Object[][] _data;

	/**
	 * 
	 */
	public int getColumnCount() {
		return _columnNames.length;
	}

	/**
	 * 
	 */
	public int getRowCount() {
		return _data.length;
	}

	/**
	 * 
	 */
	public String getColumnName(int col) {
		return _columnNames[col];
	}

	/**
	 * 
	 */
	public Object getValueAt(int row, int col) {
		return _data[row][col];
	}

	/**
	 * 
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
	 * 
	 */
	public void setValues(String[] c, Object[][] d){
    	_columnNames = c;
    	_data = d;
    }
}
