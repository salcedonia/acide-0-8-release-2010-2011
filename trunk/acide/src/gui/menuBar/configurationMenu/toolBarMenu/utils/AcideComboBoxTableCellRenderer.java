package gui.menuBar.configurationMenu.toolBarMenu.utils;

import java.awt.Component;

import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

/**
 * ACIDE - A Configurable IDE combo box table cell renderer.
 * 
 * @version 0.8
 * @see JComboBox
 * @see TableCellRenderer
 */
public class AcideComboBoxTableCellRenderer extends JComboBox implements
		TableCellRenderer {
	/**
	 * ACIDE - A Configurable IDE combo box table cell renderer class serial version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new ACIDE - A Configurable IDE combo box table cell renderer.
	 * 
	 * @param items combo box items.
	 */
	public AcideComboBoxTableCellRenderer(String[] items) {
		super(items);
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
	 */
	public Component getTableCellRendererComponent(JTable table,
			Object value, boolean isSelected, boolean hasFocus, int row,
			int column) {
		if (isSelected) {
			setForeground(table.getSelectionForeground());
			super.setBackground(table.getSelectionBackground());
		} else {
			setForeground(table.getForeground());
			setBackground(table.getBackground());
		}

		// Select the current value
		setSelectedItem(value);

		return this;
	}
}
