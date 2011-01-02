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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractCellEditor;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JDialog;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;

/************************************************************************
 * ACIDE - A Configurable IDE cell color editor. It is used by the 
 * tables in the panels of the lexicon configuration window.
 * 
 * @version 0.8
 * @see AbstractCellEditor
 * @see ActionListener
 * @see TableCellEditor
 */
public class CellColorEditor extends AbstractCellEditor implements TableCellEditor,
		ActionListener {
	/**
	 * ACIDE - A Configurable IDE cell color editor class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE cell color editor current color.
	 */
	private Color _currentColor;
	/**
	 * ACIDE - A Configurable IDE cell color editor edit button.
	 */
	private JButton _editButton;
	/**
	 * ACIDE - A Configurable IDE cell color editor color chooser.
	 */
	private JColorChooser _colorChooser;
	/**
	 * ACIDE - A Configurable IDE cell color editor dialog.
	 */
	private JDialog _dialog;
	/**
	 * ACIDE - A Configurable IDE cell color editor edit button string.
	 */
	protected static final String EDIT = "edit";

	/**
	 * Creates a new ACIDE - A Configurable IDE cell color editor.
	 */
	public CellColorEditor() {

		_editButton = new JButton();
		_editButton.setActionCommand(EDIT);
		_editButton.addActionListener(this);
		_editButton.setBorderPainted(false);
		_colorChooser = new JColorChooser();
		_dialog = JColorChooser.createDialog(_editButton, "Pick a Color", true,
				_colorChooser, this, null);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		if (EDIT.equals(actionEvent.getActionCommand())) {

			_editButton.setBackground(_currentColor);
			_colorChooser.setColor(_currentColor);
			_dialog.setVisible(true);

			fireEditingStopped();

		} else {
			_currentColor = _colorChooser.getColor();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.CellEditor#getCellEditorValue()
	 */
	@Override
	public Object getCellEditorValue() {
		return _currentColor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.table.TableCellEditor#getTableCellEditorComponent(javax.swing
	 * .JTable, java.lang.Object, boolean, int, int)
	 */
	@Override
	public Component getTableCellEditorComponent(JTable table, Object value,
			boolean isSelected, int row, int column) {
		_currentColor = (Color) value;
		return _editButton;
	}
}
