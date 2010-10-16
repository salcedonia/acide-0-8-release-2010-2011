package operations.lexicon;

import java.awt.Color;
import java.io.Serializable;

/**
 * 
 */
public class TokenType implements Serializable{

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private String _name;
	/**
	 * 
	 */
	private Color _color;
	/**
	 * 
	 */
	private boolean _italic;
	/**
	 * 
	 */
	private boolean _bold;
	/**
	 * 
	 */
	private boolean _caseSensitive;
	/**
	 * 
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
	 * 
	 * @return
	 */
	public Color getColor() {
		return _color;
	}

	/**
	 * 
	 * @param color
	 */
	public void setColor(Color color) {
		_color = color;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isItalic() {
		return _italic;
	}

	/**
	 * 
	 * @param italic
	 */
	public void setItalic(boolean italic) {
		_italic = italic;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isBold() {
		return _bold;
	}

	/**
	 * 
	 * @param bold
	 */
	public void setBold(boolean bold) {
		_bold = bold;
	}

	/**
	 * 
	 * @return
	 */
	public String getName() {
		return _name;
	}

	/**
	 * 
	 */
	public void setName() {
		_name = _color.toString();
		if (_italic) _name = _name + "Italic";
		if (_bold) _name = _name + "Bold";
		if (_caseSensitive) _name = _name + "CS";
	}

	/**
	 * 
	 * @param pos
	 * @return
	 */
	public String getToken(int pos) {
		return (String) _tokenList.getObject(pos);
	}

	/**
	 * 
	 * @param token
	 */
	public void setToken(String token) {
		_tokenList.insert(_tokenList.size(),token);
	}
	
	/**
	 * 
	 * @return
	 */
	public int getTokenListSize(){
		return _tokenList.size();
	}
	
	/**
	 * 
	 * @param pos
	 */
	public void removeToken(int pos){
		_tokenList.remove(pos);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCaseSensitive() {
		return _caseSensitive;
	}

	/**
	 * 
	 * @param caseSensitive
	 */
	public void setCaseSensitive(boolean caseSensitive) {
		_caseSensitive = caseSensitive;
	}
}
