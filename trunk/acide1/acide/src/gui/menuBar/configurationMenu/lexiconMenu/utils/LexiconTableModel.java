package gui.menuBar.configurationMenu.lexiconMenu.utils;

import javax.swing.table.AbstractTableModel;

/************************************************************************																
 * Table model for the tables in ACIDE - A Configurable IDE.
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 ***********************************************************************/
public class LexiconTableModel extends AbstractTableModel {
    
	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Column names.
	 */
	private String[] _columnNames;
    /**
     * Data.
     */
	private Object[][] _data;

	/**
	 * Sets the values for the column names and the data.
	 * 
	 * @param columnName column names.
	 * @param data data.
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
