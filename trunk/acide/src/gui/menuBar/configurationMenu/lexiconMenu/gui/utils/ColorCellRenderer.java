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
package gui.menuBar.configurationMenu.lexiconMenu.gui.utils;

import java.awt.Color;
import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;

/**
 * ACIDE - A Configurable IDE color cell renderer. It is used by the tables in
 * the lexicon configuration window.
 * 
 * @version 0.8
 * @see JLabel
 * @see TableCellRenderer
 */
public class ColorCellRenderer extends JLabel implements TableCellRenderer {
	/**
	 * ACIDE - A Configurable IDE color cell renderer class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE color cell renderer unselected border.
	 */
	private Border _unselectedBorder = null;
	/**
	 * ACIDE - A Configurable IDE color cell renderer selected border.
	 */
	private Border _selectedBorder = null;
	/**
	 * ACIDE - A Configurable IDE color cell renderer flag that indicates if the
	 * color renderer is bordered or not.
	 */
	private boolean _isBordered = true;

	/**
	 * Creates a new ACIDE - A Configurable IDE color cell renderer.
	 * 
	 * @param isBordered
	 *            is bordered flag
	 */
	public ColorCellRenderer(boolean isBordered) {

		_isBordered = isBordered;

		// MUST do this for background to show up.
		setOpaque(true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax
	 * .swing.JTable, java.lang.Object, boolean, boolean, int, int)
	 */
	@Override
	public Component getTableCellRendererComponent(JTable table, Object color,
			boolean isSelected, boolean hasFocus, int row, int column) {

		Color newColor = (Color) color;
		setBackground(newColor);
		if (_isBordered) {

			if (isSelected) {
				if (_selectedBorder == null) {
					_selectedBorder = BorderFactory.createMatteBorder(2, 5, 2,
							5, table.getSelectionBackground());
				}
				setBorder(_selectedBorder);
			} else {
				if (_unselectedBorder == null) {
					_unselectedBorder = BorderFactory.createMatteBorder(2, 5,
							2, 5, table.getBackground());
				}
				setBorder(_unselectedBorder);
			}
		}
		return this;
	}
}
