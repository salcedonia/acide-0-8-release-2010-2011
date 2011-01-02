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

import utils.ObjectList;

/**
 * ACIDE - A Configurable IDE lexicon configuration token type.
 * 
 * @version 0.8
 * @see Serializable
 */
public class TokenType implements Serializable {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type token name.
	 */
	private String _name;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type token color.
	 */
	private Color _color;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type token italic.
	 */
	private boolean _isItalic;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type token bold.
	 */
	private boolean _isBold;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type flag that
	 * indicates if it is case sensitive or not.
	 */
	private boolean _isCaseSensitive;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type object list.
	 */
	private ObjectList _tokenList;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration token
	 * type.
	 */
	public TokenType() {

		super();
		_tokenList = new ObjectList();
		_color = Color.black;
		_isItalic = false;
		_isBold = false;
		_isCaseSensitive = false;
		_name = _color.toString();
		if (_isItalic)
			_name = _name + "Italic";
		if (_isBold)
			_name = _name + "Bold";
		if (_isCaseSensitive)
			_name = _name + "CaseSensitive";
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * token color.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         token color.
	 */
	public Color getColor() {
		return _color;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * token type token color.
	 * 
	 * @param color
	 *            new value to set.
	 */
	public void setColor(Color color) {
		_color = color;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * is italic flag.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         is italic flag.
	 */
	public boolean isItalic() {
		return _isItalic;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * token type is italic flag.
	 * 
	 * @param italic
	 *            new value to set.
	 */
	public void setItalic(boolean italic) {
		_isItalic = italic;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * is bold flag.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         is bold flag.
	 */
	public boolean isBold() {
		return _isBold;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * token type is bold flag.
	 * 
	 * @param bold
	 *            new value to set.
	 */
	public void setBold(boolean bold) {
		_isBold = bold;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * token name.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         token name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets the ACIDE - A Configurable IDE lexicon configuration token type
	 * token name.
	 */
	public void setName() {
		_name = _color.toString();
		if (_isItalic)
			_name = _name + "Italic";
		if (_isBold)
			_name = _name + "Bold";
		if (_isCaseSensitive)
			_name = _name + "CaseSensitive";
	}

	/**
	 * Returns the token at the position given as a parameter.
	 * 
	 * @param index
	 *            position to get.
	 * @return the token at the position given as a parameter.
	 */
	public String getToken(int index) {
		return (String) _tokenList.getObjectAt(index);
	}

	/**
	 * Inserts a new token to the token list.
	 * 
	 * @param token
	 *            new token to insert.
	 */
	public void setToken(String token) {
		_tokenList.insert(_tokenList.size(), token);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * token list size.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         token list size.
	 */
	public int getTokenListSize() {
		return _tokenList.size();
	}

	/**
	 * Removes a token at the position of the list given as a parameter.
	 * 
	 * @param index
	 *            position to remove.
	 */
	public void removeToken(int index) {
		_tokenList.removeAt(index);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * case sensitive flag.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         case sensitive flag.
	 */
	public boolean isCaseSensitive() {
		return _isCaseSensitive;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * token type case sensitive flag.
	 * 
	 * @param caseSensitive
	 *            new value to set.
	 */
	public void setCaseSensitive(boolean caseSensitive) {
		_isCaseSensitive = caseSensitive;
	}
}
