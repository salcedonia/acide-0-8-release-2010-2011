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
package operations.fileEditor;

import java.io.Serializable;

import es.project.AcideProjectFileType;

/**
 * ACIDE - A Configurable IDE file editor configuration.
 * 
 * Stores the file editor configuration for an opened file in the file editor of
 * the application.
 * 
 * @version 0.8
 */
public class FileEditorPanelConfiguration implements Serializable {

	/**
	 * ACIDE - A Configurable IDE file editor configuration class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE file editor configuration absolute path.
	 */
	private String _path;
	/**
	 * ACIDE - A Configurable IDE file editor configuration file type.
	 */
	private String _type;
	/**
	 * ACIDE - A Configurable IDE file editor configuration caret position.
	 */
	private int _caretPosition;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor configuration.
	 */
	public FileEditorPanelConfiguration() {

		super();
		_path = "";
		_type = "Normal";
		_caretPosition = 0;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration path.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration path.
	 */
	public String getPath() {
		return _path;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration path.
	 * 
	 * @param path
	 *            new value to set.
	 */
	public void setPath(String path) {
		_path = path;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration type.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration type.
	 */
	public AcideProjectFileType getType() {
		
		if(_type.matches("Compilable"))
			return AcideProjectFileType.COMPILABLE;
		if(_type.matches("Main"))
			return AcideProjectFileType.MAIN;
		
		return AcideProjectFileType.NORMAL;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration type.
	 * 
	 * @param type
	 *            new value to set.
	 */
	public void setType(String type) {
		_type = type;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor configuration caret
	 * position.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor configuration caret
	 *         position.
	 */
	public int getCaretPosition() {
		return _caretPosition;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor
	 * configuration caret position.
	 * 
	 * @param _caretPosition
	 *            new value to set.
	 */
	public void setCaretPosition(int caretPosition) {
		_caretPosition = caretPosition;
	}
}
