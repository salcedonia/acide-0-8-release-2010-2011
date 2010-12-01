package gui.menuBar.configurationMenu.lexiconMenu.utils;

import javax.swing.table.*; 
import javax.swing.event.TableModelListener; 
import javax.swing.event.TableModelEvent; 

/************************************************************************																
 * Table map of the tables in the lexicon configuration window of ACIDE - A Configurable IDE.
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
public class LexiconTableMap extends AbstractTableModel implements TableModelListener{
	
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Table model.
	 */
    protected TableModel _model; 

    /**
     * Returns the table model.
     * 
     * @return the table model.
     */
    public TableModel  getModel() {
        return _model;
    }

    /**
     * Sets a new value to the table model.
     * 
     * @param model new value to set.
     */
    public void  setModel(TableModel model) {
        _model = model; 
        _model.addTableModelListener(this); 
    }

    /*
     * (non-Javadoc)
     * @see javax.swing.table.TableModel#getValueAt(int, int)
     */
    public Object getValueAt(int aRow, int aColumn) {
        return _model.getValueAt(aRow, aColumn); 
    }
        
    /*
     * (non-Javadoc)
     * @see javax.swing.table.TableModel#getRowCount()
     */
    public int getRowCount() {
        return (_model == null) ? 0 : _model.getRowCount(); 
    }

    /*
     * (non-Javadoc)
     * @see javax.swing.table.TableModel#getColumnCount()
     */
    public int getColumnCount() {
        return (_model == null) ? 0 : _model.getColumnCount(); 
    }
       
    /*
     * (non-Javadoc)
     * @see javax.swing.table.AbstractTableModel#getColumnName(int)
     */
    public String getColumnName(int aColumn) {
        return _model.getColumnName(aColumn); 
    }
    
    /*
     * (non-Javadoc)
     * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
     */
    public Class<?> getColumnClass(int aColumn) {
        return _model.getColumnClass(aColumn); 
    }
        
    /*
     * (non-Javadoc)
     * @see javax.swing.event.TableModelListener#tableChanged(javax.swing.event.TableModelEvent)
     */
    public void tableChanged(TableModelEvent tableModelEvent) {
        fireTableChanged(tableModelEvent);
    }
}
