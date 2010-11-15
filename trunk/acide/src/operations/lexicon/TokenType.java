package operations.lexicon;

import java.awt.Color;
import java.io.Serializable;

import utils.ObjectList;

/************************************************************************																										
 * Handles the token type of the lexicon of the application
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see Serializable																													
 ***********************************************************************/
public class TokenType implements Serializable{

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Token name
	 */
	private String _name;
	/**
	 * Token color
	 */
	private Color _color;
	/**
	 * Token italic
	 */
	private boolean _italic;
	/**
	 * Token bold
	 */
	private boolean _bold;
	/**
	 * Flag that indicates if it is case sensitive or not
	 */
	private boolean _caseSensitive;
	/**
	 * Token list
	 */
	private ObjectList _tokenList;
	
	/**
	 * Class constructor
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
	 * Returns the token color
	 * 
	 * @return the token color
	 */
	public Color getColor() {
		return _color;
	}

	/**
	 * Sets a new value to the token color
	 * 
	 * @param color new value to set
	 */
	public void setColor(Color color) {
		_color = color;
	}

	/**
	 * Returns the is italic flag
	 * 
	 * @return the is italic flag
	 */
	public boolean isItalic() {
		return _italic;
	}

	/**
	 * Sets a new value to the is italic flag
	 * 
	 * @param italic new value to set
	 */
	public void setItalic(boolean italic) {
		_italic = italic;
	}

	/**
	 * Returns the bold flag
	 * 
	 * @return the bold flag
	 */
	public boolean isBold() {
		return _bold;
	}

	/**
	 * Sets a new value to the bold flag
	 * 
	 * @param bold new value to set
	 */
	public void setBold(boolean bold) {
		_bold = bold;
	}

	/**
	 * Returns the token name
	 * 
	 * @return the token name
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets the token name
	 */
	public void setName() {
		_name = _color.toString();
		if (_italic) _name = _name + "Italic";
		if (_bold) _name = _name + "Bold";
		if (_caseSensitive) _name = _name + "CS";
	}

	/**
	 * Returns the token at the position given as a parameter
	 * 
	 * @param pos position to get
	 * @return the token at the position given as a parameter
	 */
	public String getToken(int pos) {
		return (String) _tokenList.getObjectAt(pos);
	}

	/**
	 * Inserts a new token to the token list
	 * 
	 * @param token new token to insert
	 */
	public void setToken(String token) {
		_tokenList.insert(_tokenList.size(),token);
	}
	
	/**
	 * Returns the token list size
	 * 
	 * @return the token list size
	 */
	public int getTokenListSize(){
		return _tokenList.size();
	}
	
	/**
	 * Removes a token at the position of the list given as a parameter
	 * 
	 * @param pos position to remove
	 */
	public void removeToken(int pos){
		_tokenList.removeAt(pos);
	}

	/**
	 * Returns the case sensitive flag
	 * 
	 * @return the case sensitive flag
	 */
	public boolean isCaseSensitive() {
		return _caseSensitive;
	}

	/**
	 * Sets a new value to the case sensitive flag
	 * 
	 * @param caseSensitive new value to set
	 */
	public void setCaseSensitive(boolean caseSensitive) {
		_caseSensitive = caseSensitive;
	}
}
