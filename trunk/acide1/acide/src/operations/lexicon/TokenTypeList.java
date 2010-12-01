package operations.lexicon;

import java.io.Serializable;

import utils.ObjectList;

/************************************************************************																											
 * Token type list of the lexicon of the application.
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
public class TokenTypeList implements Serializable {

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Object list.
	 */
	private ObjectList _list;
	/**
	 * Class instance.
	 */
	private static TokenTypeList _instance;

	/**
	 * Class constructor.
	 */
	public TokenTypeList() {
		super();
		_list = new ObjectList();
	}

	/**
	 * Returns the unique class instance.
	 * 
	 * @return the unique class instance.
	 */
	public static TokenTypeList getInstance() {
		if (_instance == null)
			_instance = new TokenTypeList();
		return _instance;
	}

	/**
	 * Loads a new token type list.
	 * 
	 * @param list new value to set.
	 */
	public void load(TokenTypeList list) {
		_instance = list;
	}

	/**
	 * Returns a token type from the list in the given position.
	 * 
	 * @param pos position to get.
	 * @return a token type from the list in the given position.
	 */
	public TokenType getTokenType(int pos) {
		return (TokenType) _list.getObjectAt(pos);
	}

	/**
	 * Sets a new token in the list.
	 * 
	 * @param tokenType new value to set.
	 */
	public void setTokenType(TokenType tokenType) {
		_list.insert(_list.size(), tokenType);
	}

	/**
	 * Returns the list size.
	 * 
	 * @return the list size.
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * Inserts a token type into the list.
	 * 
	 * @param tokenType new token type.
	 * @param token token name.
	 */
	public void insertTokenType(TokenType tokenType, String token) {

		boolean found = false;
		boolean found2 = false;
		int pos = 0;
		
		for (int i = 0; i < getSize(); i++) {
			
			String s1 = getTokenType(i).getName();
			String s2 = tokenType.getName();
			if (s1.equals(s2)) {
				found = true;
				pos = i;
			}
			for (int j = 0; j < getTokenType(i).getTokenListSize(); j++) {
				String s3 = token;
				String s4 = getTokenType(i).getToken(j);
				
				if (s3.equals(s4)) {
					found2 = true;
				}
			}
		}

		if (!found) {
			if (!found2) {
				setTokenType(tokenType);
			}
		} else {
			if (!found2) {
				getTokenType(pos).setToken(token);
			}
		}
	}

	/**
	 * Removes a token from the list.
	 * 
	 * @param token token to remove.
	 */
	public void removeTokenAs(String token) {
		
		for (int i = 0; i < getSize(); i++) {
			for (int j = 0; j < getTokenType(i).getTokenListSize(); j++) {
				String s1 = token;
				String s2 = getTokenType(i).getToken(j);
				if (s1.equals(s2)) {
					getTokenType(i).removeToken(j);
				}
			}
			if (getTokenType(i).getTokenListSize() == 0) {
				removeTokenType(i);
			}
		}
	}

	/**
	 * Returns a token from the list.
	 * 
	 * @param token token name.
	 * @return a token from the list.
	 */
	public TokenType getTokenAt(String token) {
		
		for (int i = 0; i < getSize(); i++)
			for (int j = 0; j < getTokenType(i).getTokenListSize(); j++) {
				String s1 = token;
				String s2 = getTokenType(i).getToken(j);
				if (s1.equals(s2))
					return getTokenType(i);			
			}
		
		return null;
	}

	/**
	 * Removes a token type from the list.
	 * 
	 * @param pos position to remove.
	 */
	public void removeTokenType(int pos) {
		_list.removeAt(pos);
	}

	/**
	 * Resets the token type list.
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
