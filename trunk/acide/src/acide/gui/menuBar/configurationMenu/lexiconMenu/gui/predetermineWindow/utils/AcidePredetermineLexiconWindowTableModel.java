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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.predetermineWindow.utils;

import java.util.ArrayList;

import acide.configuration.workbench.lexiconAssigner.AcideLexiconAssigner;
import acide.configuration.workbench.lexiconAssigner.AcideLexiconAssignerConfigurationList;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.predetermineWindow.AcideDefaultLexiconsWindow;

import javax.swing.table.DefaultTableModel;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE predetermine lexicon window table model.
 * 
 * @version 0.8
 * @see DefaultTableModel
 */
public class AcidePredetermineLexiconWindowTableModel extends DefaultTableModel {

	/**
	 * ACIDE - A Configurable IDE predetermine lexicon window table model class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE predetermine lexicon window table model column
	 * description constant.
	 */
	public static final int COLUMN_DESCRIPTION = 0;
	/**
	 * ACIDE - A Configurable IDE predetermine lexicon window table model column
	 * extensions constant.
	 */
	public static final int COLUMN_EXTENSIONS = 1;
	/**
	 * ACIDE - A Configurable IDE predetermine lexicon window table model column
	 * default lexicon constant.
	 */
	public static final int COLUMN_DEFAULT_LEXICON = 2;
	/**
	 * ACIDE - A Configurable IDE predetermine lexicon window table model item
	 * list.
	 */
	private AcideLexiconAssignerConfigurationList _items;
	/**
	 * ACIDE - A Configurable IDE predetermine lexicon window instance.
	 */
	private AcideDefaultLexiconsWindow _predetermineLexiconWindow;

	/**
	 * Creates a new ACIDE - A Configurable IDE predetermine lexicon
	 * configuration table model.
	 * 
	 * @param predetermineLexiconWindow
	 *            <p>
	 *            ACIDE - A Configurable IDE predetermine lexicon window
	 *            instance.
	 *            </p>
	 *            <p>
	 *            It is needed to tell it that there have been modifications on
	 *            it. Otherwise, when a changed is performed in the model, the
	 *            configuration window will not be able to notice it.
	 *            </p>
	 */
	public AcidePredetermineLexiconWindowTableModel(
			AcideDefaultLexiconsWindow predetermineLexiconWindow) {

		// Stores the predetermine lexicon window instance
		_predetermineLexiconWindow = predetermineLexiconWindow;

		// Creates the item list
		_items = new AcideLexiconAssignerConfigurationList();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return AcideLexiconAssigner.NUMBER_OF_PARAMETERS;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		if (_items != null)
			return _items.getSize();
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int columnIndex) {

		switch (columnIndex) {
		case COLUMN_DESCRIPTION:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s1085");
		case COLUMN_EXTENSIONS:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s1086");
		case COLUMN_DEFAULT_LEXICON:
			return AcideLanguageManager.getInstance().getLabels()
					.getString("s1087");
		default:
			return "Column " + columnIndex;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@Override
	public Object getValueAt(int rowIndex, int columnIndex) {

		AcideLexiconAssigner item = (AcideLexiconAssigner) _items.getFileEditorPanelConfigurationAt(rowIndex);

		switch (columnIndex) {
		case COLUMN_DESCRIPTION:
			return item.getDescription();
		case COLUMN_EXTENSIONS:
			
			String extensions = "";
			
			for(String extension: item.getExtensionList()){
				extensions += extension + ";"; 
			}
			
			return extensions;
		case COLUMN_DEFAULT_LEXICON:
			return item.getLexiconConfiguration();

		default:
			return null;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
	 */
	@Override
	public Class<?> getColumnClass(int column) {
		return getValueAt(0, column).getClass();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
	 */
	@Override
	public boolean isCellEditable(int row, int col) {

		// All the cells are editable
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object,
	 * int, int)
	 */
	@Override
	public void setValueAt(Object value, int rowIndex, int columnIndex) {

		// There are changes on the table in the predetermine lexicon window
		_predetermineLexiconWindow.setAreThereChanges(true);

		// Gets the lexicon assigner from the table row
		AcideLexiconAssigner lexiconAssigner = (AcideLexiconAssigner) _items
				.getFileEditorPanelConfigurationAt(rowIndex);

		switch (columnIndex) {
		case COLUMN_DESCRIPTION:
			
			// Sets its description
			lexiconAssigner.setDescription(value.toString());
			
			// Updates the table model
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_EXTENSIONS:
			
			// Gets the file extensions into a string array
			String[] extensions = value.toString().split(";");
			
			// Creates the parsed extensions
			ArrayList<String> parsedExtensions = new ArrayList<String>();
			
			// Parse the string array into an array list of strings
			for(int index = 0; index < extensions.length; index++)
				parsedExtensions.add(extensions[index]);
			
			// Sets its array list of extensions
			lexiconAssigner.setExtensionList(parsedExtensions);
			
			// Updates the table model
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		case COLUMN_DEFAULT_LEXICON:
		
			// Sets its lexicon configuration
			lexiconAssigner.setLexiconConfiguration(value.toString());
			
			// Updates the table model
			fireTableCellUpdated(rowIndex, columnIndex);
			break;
		default:
			return;
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE predetermine lexicon window table
	 * model item list.
	 * 
	 * @return the ACIDE - A Configurable IDE predetermine lexicon window table
	 *         model item list.
	 */
	public AcideLexiconAssignerConfigurationList getItems() {
		return _items;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE predetermine lexicon
	 * window table model item list.
	 * 
	 * @param items
	 *            new value to set.
	 */
	public void setItems(AcideLexiconAssignerConfigurationList items) {

		// Clears the item list
		_items.reset();

		// Adds all the items to it
		for (int index = 0; index < items.getSize(); index++){
			
			_items.insertFileEditorPanelConfiguration(items
					.getFileEditorPanelConfigurationAt(index));
		
		}
		// Updates the model
		fireTableDataChanged();
	}

	/**
	 * Adds a new item (row) to the model.
	 * 
	 * @param lexiconAssigner
	 *            new item to be added.
	 */
	public void addItem(AcideLexiconAssigner lexiconAssigner) {
		_items.insertFileEditorPanelConfiguration(lexiconAssigner);
	}

	/**
	 * Removes the item from the item list at the position given as a parameter.
	 * 
	 * @param rowIndex
	 *            position to be removed.
	 */
	public void removeItem(int rowIndex) {
		_items.removeFileEditorPanelConfiguration(rowIndex);
	}
}
