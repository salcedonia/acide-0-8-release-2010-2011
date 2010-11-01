package operations.lexicon;

import java.awt.Color;
import java.io.Serializable;

import utils.ObjectList;


/**
 * Handles the token type of the lexicon of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class TokenType implements Serializable{

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Token name.
	 */
	private String _name;
	/**
	 * Token color.
	 */
	private Color _color;
	/**
	 * Token italic.
	 */
	private boolean _italic;
	/**
	 * Token bold.
	 */
	private boolean _bold;
	/**
	 * Flag that indicates if it is case sensitive or not.
	 */
	private boolean _caseSensitive;
	/**
	 * Token list.
	 */
	private ObjectList _tokenList;
	
	/**
	 * Constructor of the class.
	 */
	public TokenType() {
		
		super();
		_tokenList = new ObjectList();
		_color = Color.black;
		_italic = false;
		_bold = false;
		_caseSensitive = false;
		_name = _color.toString();
		if (_italic) _name = _name + "Italic";
		if (_bold) _name = _name + "Bold";
		if (_caseSensitive) _name = _name + "CS";
	}

	/**
	 * Returns the token color.
	 * 
	 * @return The token color.
	 */
	public Color getColor() {
		return _color;
	}

	/**
	 * Set a new value to the token color.
	 * 
	 * @param color New value to set.
	 */
	public void setColor(Color color) {
		_color = color;
	}

	/**
	 * Returns the is italic flag.
	 * 
	 * @return The is italic flag.
	 */
	public boolean isItalic() {
		return _italic;
	}

	/**
	 * Set a new value to the is italic flag.
	 * 
	 * @param italic New value to set.
	 */
	public void setItalic(boolean italic) {
		_italic = italic;
	}

	/**
	 * Returns the bold flag.
	 * 
	 * @return The bold flag.
	 */
	public boolean isBold() {
		return _bold;
	}

	/**
	 * Set a new value to the bold flag.
	 * 
	 * @param bold New value to set.
	 */
	public void setBold(boolean bold) {
		_bold = bold;
	}

	/**
	 * Returns the token name.
	 * 
	 * @return The token name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Set the token name.
	 */
	public void setName() {
		_name = _color.toString();
		if (_italic) _name = _name + "Italic";
		if (_bold) _name = _name + "Bold";
		if (_caseSensitive) _name = _name + "CS";
	}

	/**
	 * Returns the token at the position given as a parameter.
	 * 
	 * @param pos Position to get.
	 * 
	 * @return The token at the position given as a parameter.
	 */
	public String getToken(int pos) {
		return (String) _tokenList.getObjectAt(pos);
	}

	/**
	 * Insert a new token to the token list.
	 * 
	 * @param token New token to insert.
	 */
	public void setToken(String token) {
		_tokenList.insert(_tokenList.size(),token);
	}
	
	/**
	 * Return the token list size.
	 * 
	 * @return The token list size.
	 */
	public int getTokenListSize(){
		return _tokenList.size();
	}
	
	/**
	 * Remove a token at the position of the list given as a parameter.
	 * 
	 * @param pos Position to remove.
	 */
	public void removeToken(int pos){
		_tokenList.removeAt(pos);
	}

	/**
	 * Returns the case sensitive flag.
	 * 
	 * @return The case sensitive flag.
	 */
	public boolean isCaseSensitive() {
		return _caseSensitive;
	}

	/**
	 * Set a new value to the case sensitive flag.
	 * 
	 * @param caseSensitive New value to set.
	 */
	public void setCaseSensitive(boolean caseSensitive) {
		_caseSensitive = caseSensitive;
	}
}
