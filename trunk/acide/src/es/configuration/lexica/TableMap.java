package es.configuration.lexica;

import javax.swing.table.*; 
import javax.swing.event.TableModelListener; 
import javax.swing.event.TableModelEvent; 

/**
 * 
 */
public class TableMap extends AbstractTableModel implements TableModelListener{
	
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
    protected TableModel _model; 

    /**
     * 
     * @return
     */
    public TableModel  getModel() {
        return _model;
    }

    /**
     * 
     * @param model
     */
    public void  setModel(TableModel model) {
        _model = model; 
        _model.addTableModelListener(this); 
    }

    /**
     * 
     */
    public Object getValueAt(int aRow, int aColumn) {
        return _model.getValueAt(aRow, aColumn); 
    }
        
    /**
     * 
     */
    public int getRowCount() {
        return (_model == null) ? 0 : _model.getRowCount(); 
    }

    /**
     * 
     */
    public int getColumnCount() {
        return (_model == null) ? 0 : _model.getColumnCount(); 
    }
       
    /**
     * 
     */
    public String getColumnName(int aColumn) {
        return _model.getColumnName(aColumn); 
    }
    
    /**
     * 
     */
    public Class<?> getColumnClass(int aColumn) {
        return _model.getColumnClass(aColumn); 
    }
        

    public void tableChanged(TableModelEvent e) {
        fireTableChanged(e);
    }
}
