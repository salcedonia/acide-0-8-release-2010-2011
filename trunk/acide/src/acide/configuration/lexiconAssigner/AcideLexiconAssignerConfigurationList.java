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
package acide.configuration.lexiconAssigner;

import java.io.Serializable;

import acide.configuration.utils.ObjectList;

/**
 * ACIDE - A Configurable IDE lexicon assigner configuration list.
 * 
 * @version 0.8
 * @see Serializable
 */
public class AcideLexiconAssignerConfigurationList implements Serializable {

	/**
	 * ACIDE - A Configurable IDE lexicon assigner configuration list class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon assigner configuration list object
	 * configuration list.
	 */
	private ObjectList _list;
	/**
	 * ACIDE - A Configurable IDE lexicon assigner configuration list unique
	 * class instance.
	 */
	private static AcideLexiconAssignerConfigurationList _instance;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon assigner configuration
	 * list.
	 */
	public AcideLexiconAssignerConfigurationList() {

		super();
		_list = new ObjectList();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner configuration
	 * list unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner configuration
	 *         list unique class instance.
	 */
	public static AcideLexiconAssignerConfigurationList getInstance() {
		if (_instance == null)
			_instance = new AcideLexiconAssignerConfigurationList();
		return _instance;
	}

	/**
	 * Loads the ACIDE - A Configurable IDE lexicon assigner configuration list.
	 * 
	 * @param lexiconAssignerConfigurationList
	 *            new value to load.
	 */
	public void load(
			AcideLexiconAssignerConfigurationList lexiconAssignerConfigurationList) {
		_instance = lexiconAssignerConfigurationList;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner configuration
	 * from the list at the position given as a parameter.
	 * 
	 * @param index
	 *            index to get.
	 * @return the ACIDE - A Configurable IDE lexicon assigner configuration
	 *         from the list at the position given as a parameter.
	 */
	public AcideLexiconAssigner getFileEditorPanelConfigurationAt(int index) {
		return (AcideLexiconAssigner) _list.getObjectAt(index);
	}

	/**
	 * Insert a new lexicon assigner configuration into the ACIDE - A
	 * Configurable IDE lexicon assigner configuration list.
	 * 
	 * @param lexiconAssigner
	 *            new value to set.
	 */
	public void insertFileEditorPanelConfiguration(
			AcideLexiconAssigner lexiconAssigner) {
		_list.insert(_list.size(), lexiconAssigner);
	}

	/**
	 * Removes a new lexicon assigner configuration into the ACIDE - A
	 * Configurable IDE lexicon assigner configuration list.
	 * 
	 * @param rowIndex
	 *            index to remove.
	 */
	public void removeFileEditorPanelConfiguration(
			int rowIndex) {	
		_list.removeAt(rowIndex);
	}
	
	/**
	 * Returns the ACIDE - A Configurable IDE lexicon assigner configuration
	 * list size.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon assigner configuration
	 *         list size.
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * Resets the ACIDE - A Configurable IDE lexicon assigner configuration
	 * list.
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
