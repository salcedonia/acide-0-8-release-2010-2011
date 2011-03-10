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
package acide.configuration.lexicon.tokens;

import java.awt.Color;
import java.awt.Font;
import java.io.Serializable;

import acide.utils.ObjectList;

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
	 * ACIDE - A Configurable IDE lexicon configuration token type token font
	 * style. 0 -> Plain, 1 -> Italic, 2 -> Bold, 3 -> Bold + Italic.
	 */
	private int _fontStyle;
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
		_fontStyle = Font.PLAIN;
		_isCaseSensitive = false;
		_name = "Color: " + "[R: " + _color.getRed() + ", G: "
				+ _color.getGreen() + ", B: " + _color.getBlue() + "]"
				+ ", Font Style: Plain, Case Sensitive: No";
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

		_name = "Color: " + "[R: " + _color.getRed() + ", G: "
		+ _color.getGreen() + ", B: " + _color.getBlue() + "]";

		switch (_fontStyle) {

		case Font.PLAIN:
			_name = _name + ", Font Style: Plain";
			break;
		case Font.ITALIC:
			_name = _name + ", Font Style: Italic";
			break;
		case Font.BOLD:
			_name = _name + ", Font Style: Bold";
			break;
		case Font.BOLD + Font.ITALIC:
			_name = _name + ", Font Style: Italic and Bold";
			break;
		}
		if (_isCaseSensitive)
			_name = _name + ", Case Sensitive: Yes";
		else
			_name = _name + ", Case Sensitive: No";

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

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * font style.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         font style.
	 */
	public int getFontStyle() {
		return _fontStyle;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * token type font style.
	 * 
	 * @param fontStyle
	 *            new value to set.
	 */
	public void setFontStyle(int fontStyle) {
		_fontStyle = fontStyle;
	}
}
