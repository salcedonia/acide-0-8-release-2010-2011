package es.configuracion.lexica;

import javax.swing.table.AbstractTableModel;

public class MyTableModel extends AbstractTableModel {
    String[] columnNames;
    Object[][] data;

    public void setValues(String[] c, Object[][] d){
    	columnNames = c;
    	data = d;
    }
    
    public int getColumnCount() {
        return columnNames.length;
    }
    
    public int getRowCount() {
        return data.length;
    }

    public String getColumnName(int col) {
        return columnNames[col];
    }

    public Object getValueAt(int row, int col) {
        return data[row][col];
    }

    public Class getColumnClass(int c) {
        return getValueAt(0, c).getClass();
    }

}
