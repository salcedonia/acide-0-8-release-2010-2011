package gui.menu.configuration.lexicon;

import javax.swing.table.*; 
import javax.swing.event.TableModelListener; 
import javax.swing.event.TableModelEvent; 

/**
 * Table map of the tables in the lexicon GUI the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class AcideTableMap extends AbstractTableModel implements TableModelListener{
	
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Table model
	 */
    protected TableModel _model; 

    /**
     * Returns the table model.
     * 
     * @return The table model.
     */
    public TableModel  getModel() {
        return _model;
    }

    /**
     * Set a new value to the table model.
     * 
     * @param model New value to set.
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
    public void tableChanged(TableModelEvent e) {
        fireTableChanged(e);
    }
}
