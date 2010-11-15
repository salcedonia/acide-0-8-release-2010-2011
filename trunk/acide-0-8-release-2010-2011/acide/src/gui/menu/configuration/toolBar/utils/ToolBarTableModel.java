package gui.menu.configuration.toolBar.utils;

import javax.swing.table.AbstractTableModel;

/**
 * Table model for the modifiable tool bar of the application
 * 
 * @author
 * <ul>
 * 		<li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 * 		<li><b>Version 0.1-0.6:</b>
 * 			<ul>Diego Cardiel Freire</ul>
 *			<ul>Juan José Ortiz Sánchez</ul>
 * 			<ul>Delfín Rupérez Cañas</ul>
 * 		</li>
 * 		<li><b>Version 0.7:</b>
 * 			<ul>Miguel Martín Lázaro</ul>
 * 		</li>
 * 		<li><b>Version 0.8:</b>
 * 			<ul>Javier Salcedo Gómez</ul>
 * 		</li>
 * </ul>
 * @version 0.8
 * @since 0.1
 * @see AbstractTableModel
 */
public class ToolBarTableModel extends AbstractTableModel {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Column names
	 */
	private String[] _columnNames;
	/**
	 * Table data
	 */
	private Object[][] _data;

	/**
	 * Returns the column count
	 * 
	 * @return the column count
	 */
	public int getColumnCount() {
		return _columnNames.length;
	}

	/**
	 * Returns the row count
	 * 
	 * @return the row count
	 */
	public int getRowCount() {
		return _data.length;
	}

	/**
	 * Returns the table column at the position given as a parameter
	 * 
	 * @param column column to select
	 * @return The table column at the position given as a parameter
	 */
	public String getColumnName(int column) {
		return _columnNames[column];
	}

	/**
	 * Returns the value at the row and column given as parameters
	 * 
	 * @param row row of the table
	 * @param column column of the table
	 * @return the table value at the row and column given as parameters
	 */
	public Object getValueAt(int row, int column) {
		return _data[row][column];
	}

	/**
	 * Returns the column class of a column given as a parameter
	 * 
	 * @param column column to select
	 * @return the column class of a column given as a parameter
	 */
	public Class<?> getColumnClass(int column) {
		return getValueAt(0, column).getClass();
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
	 * Sets the values in a column list given as a parameter
	 * 
	 * @param columnNames column array
	 * @param data data to set
	 */
	public void setValues(String[] columnNames, Object[][] data){
    	_columnNames = columnNames;
    	_data = data;
    }
}
