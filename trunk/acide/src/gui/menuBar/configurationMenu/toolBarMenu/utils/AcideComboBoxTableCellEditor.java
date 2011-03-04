package gui.menuBar.configurationMenu.toolBarMenu.utils;

import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.table.TableCellRenderer;

/**
 * ACIDE - A Configurable IDE combo box table cell editor.
 * 
 * @version 0.8
 * @see JComboBox
 * @see TableCellRenderer
 */
public class AcideComboBoxTableCellEditor extends DefaultCellEditor {
	
	/**
	 * ACIDE - A Configurable IDE combo box table cell editor class serial version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new ACIDE - A Configurable IDE combo box table cell editor.
	 * 
	 * @param items combo box items.
	 */
	public AcideComboBoxTableCellEditor(String[] items) {
		super(new JComboBox(items));
	}
}
