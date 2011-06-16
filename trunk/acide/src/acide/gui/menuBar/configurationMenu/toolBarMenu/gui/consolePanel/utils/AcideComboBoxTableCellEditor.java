/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel.utils;

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
