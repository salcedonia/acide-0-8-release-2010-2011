package gui.menu.configuration.lexical;

import javax.swing.table.AbstractTableModel;

/**
 * Table model for the tables in the Acide application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class AcideTableModel extends AbstractTableModel {
    
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Column names
	 */
	private String[] _columnNames;
    /**
     * Data
     */
	private Object[][] _data;

	/**
	 * Set the values for the column names and the data.
	 * 
	 * @param columnName Column names.
	 * @param data Data.
	 */
    public void setValues(String[] columnName, Object[][] data){
    	_columnNames = columnName;
    	_data = data;
    }
    
    /*
     * (non-Javadoc)
     * @see javax.swing.table.TableModel#getColumnCount()
     */
    public int getColumnCount() {
        return _columnNames.length;
    }
    
    /*
     * (non-Javadoc)
     * @see javax.swing.table.TableModel#getRowCount()
     */
    public int getRowCount() {
        return _data.length;
    }

    /*
     * (non-Javadoc)
     * @see javax.swing.table.AbstractTableModel#getColumnName(int)
     */
    public String getColumnName(int col) {
        return _columnNames[col];
    }

    /*
     * (non-Javadoc)
     * @see javax.swing.table.TableModel#getValueAt(int, int)
     */
    public Object getValueAt(int row, int col) {
        return _data[row][col];
    }

    /*
     * (non-Javadoc)
     * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
     */
    public Class<?> getColumnClass(int c) {
        return getValueAt(0, c).getClass();
    }
}
