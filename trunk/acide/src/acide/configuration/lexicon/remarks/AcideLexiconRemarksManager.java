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
package acide.configuration.lexicon.remarks;

import java.awt.Color;
import java.awt.Font;
import java.io.Serializable;

/**
 * ACIDE - A Configurable IDE lexicon configuration remarks manager.
 * 
 * @version 0.8
 * @see Serializable
 */
public class AcideLexiconRemarksManager implements Serializable {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration remarks manager class
	 * serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration remarks manager symbol.
	 */
	private String _symbol;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration remarks manager is case
	 * sensitive flag.
	 */
	private boolean _isCaseSensitive;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration remarks manager color.
	 */
	private Color _color;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration remarks manager font
	 * style.
	 */
	private int _fontStyle;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration remarks manager.
	 */
	public AcideLexiconRemarksManager() {
		
		super();
		
		// The symbol by default is ""
		_symbol = "";
		
		// Sets the is case sensitive flag as false by default
		_isCaseSensitive = false;
		
		// Sets the color as black by default
		_color = Color.BLACK;
		
		// The font style is plain by default
		_fontStyle = Font.PLAIN;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration remarks
	 * manager symbol.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration remarks
	 *         manager symbol.
	 */
	public String getSymbol() {
		return _symbol;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * remarks manager symbol.
	 * 
	 * @param symbol
	 *            new value to set.
	 */
	public void setSymbol(String symbol) {
		_symbol = symbol;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration remarks
	 * manager color.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration remarks
	 *         manager color.
	 */
	public Color getColor() {
		return _color;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * remarks manager color.
	 * 
	 * @param color
	 *            new value to set.
	 */
	public void setColor(Color color) {
		_color = color;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration remarks
	 * manager font style.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration remarks
	 *         manager font style.
	 */
	public int getFontStyle() {
		return _fontStyle;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * remarks manager font style.
	 * 
	 * @param fontStyle
	 *            new value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_fontStyle = fontStyle;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration remarks
	 * manager case sensitive flag.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration remark case
	 *         sensitive flag.
	 */
	public boolean getIsCaseSensitive() {
		return _isCaseSensitive;
	}

	/**
	 * Sets a new value to ACIDE - A Configurable IDE lexicon configuration
	 * remarks manager is case sensitive flag.
	 * 
	 * @param isCaseSensitive
	 *            new value to set.
	 */
	public void setIsCaseSensitive(boolean isCaseSensitive) {
		_isCaseSensitive = isCaseSensitive;
	}
}
