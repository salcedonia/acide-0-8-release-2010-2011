package es.configuration.lexica;

import javax.swing.table.AbstractTableModel;

/**
 * 
 */
public class MyTableModel extends AbstractTableModel {
    
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	String[] _columnNames;
    /**
     * 
     */
	Object[][] _data;

	/**
	 * 
	 * @param c
	 * @param d
	 */
    public void setValues(String[] c, Object[][] d){
    	_columnNames = c;
    	_data = d;
    }
    
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
}
