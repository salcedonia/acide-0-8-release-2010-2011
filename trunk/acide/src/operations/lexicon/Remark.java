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
package operations.lexicon;

import java.awt.Color;
import java.io.Serializable;

/**
 * ACIDE - A Configurable IDE lexicon configuration remark.
 * 
 * @version 0.8
 * @see Serializable
 */
public class Remark implements Serializable {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration remark class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration remark.
	 */
	private static Remark _instance;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration remark content.
	 */
	private String _content;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration remark color.
	 */
	private Color _color;

	/**
	 * Returns the unique ACIDE - A Configurable IDE lexicon configuration
	 * remark class instance.
	 * 
	 * @return the unique ACIDE - A Configurable IDE lexicon configuration
	 *         remark class instance.
	 */
	public static Remark getInstance() {
		if (_instance == null)
			_instance = new Remark();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration remark.
	 */
	public Remark() {
		super();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration remark
	 * content.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration remark
	 *         content.
	 */
	public String getContent() {
		return _content;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * remark content.
	 * 
	 * @param content
	 *            new value to set.
	 */
	public void setContent(String content) {
		_content = content;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration remark color.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration remark color.
	 */
	public Color getColor() {
		return _color;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * remark color.
	 * 
	 * @param color
	 *            new value to set.
	 */
	public void setColor(Color color) {
		_color = color;
	}

	/**
	 * Loads the ACIDE - A Configurable IDE lexicon configuration remark.
	 * 
	 * @param remark
	 *            new value to set.
	 */
	public void load(Remark remark) {
		_instance = remark;
	}

	/**
	 * Resets the ACIDE - A Configurable IDE lexicon configuration remark.
	 */
	public void reset() {
		_content = "";
		_color = Color.BLACK;
	}
}
