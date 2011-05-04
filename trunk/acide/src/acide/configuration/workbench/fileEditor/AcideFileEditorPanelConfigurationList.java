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
package acide.configuration.workbench.fileEditor;

import java.io.Serializable;

import acide.configuration.utils.ObjectList;

/**
 * ACIDE - A Configurable IDE file editor manager configuration file editor
 * panel configuration list.
 * 
 * @version 0.8
 * @see Serializable
 */
public class AcideFileEditorPanelConfigurationList implements Serializable {

	/**
	 * ACIDE - A Configurable IDE file editor manager configuration file editor
	 * panel configuration list class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration delimiter list object
	 * configuration list.
	 */
	private ObjectList _list;
	/**
	 * ACIDE - A Configurable IDE file editor manager configuration file editor
	 * panel list unique class instance.
	 */
	private static AcideFileEditorPanelConfigurationList _instance;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor manager
	 * configuration file editor panel list.
	 */
	public AcideFileEditorPanelConfigurationList() {

		super();
		_list = new ObjectList();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager configuration
	 * file editor panel list unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager configuration
	 *         file editor panel list unique class instance.
	 */
	public static AcideFileEditorPanelConfigurationList getInstance() {
		if (_instance == null)
			_instance = new AcideFileEditorPanelConfigurationList();
		return _instance;
	}

	/**
	 * Loads the ACIDE - A Configurable IDE file editor manager configuration
	 * file editor panel list.
	 * 
	 * @param fileEditorPanelList
	 *            new value to load.
	 */
	public void load(AcideFileEditorPanelConfigurationList fileEditorPanelList) {
		_instance = fileEditorPanelList;
	}

	/**
	 * Returns the file editor panel configuration at the position given as a
	 * parameter.
	 * 
	 * @param index
	 *            position to get.
	 * @return the file editor panel configuration at the position given as a
	 *         parameter.
	 */
	public AcideFileEditorPanelConfiguration getFileEditorPanelConfigurationAt(
			int index) {
		return (AcideFileEditorPanelConfiguration) _list.getObjectAt(index);
	}

	/**
	 * Insert a new file editor panel configuration.
	 * 
	 * @param fileEditorPanelConfiguration
	 *            new value to set.
	 */
	public void insertFileEditorPanelConfiguration(
			AcideFileEditorPanelConfiguration fileEditorPanelConfiguration) {
		_list.insert(_list.size(), fileEditorPanelConfiguration);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager configuration
	 * file editor panel list size.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager configuration
	 *         file editor panel list size.
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * Resets the ACIDE - A Configurable IDE file editor manager configuration
	 * file list. list.
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
