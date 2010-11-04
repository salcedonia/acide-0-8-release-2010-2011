package operations.lexicon;

import utils.ObjectList;

/**
 * Token type list of the lexicon of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class TokenTypeList implements java.io.Serializable {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Object list.
	 */
	private ObjectList _list;
	/**
	 * Instance of the class.
	 */
	private static TokenTypeList _instance;

	/**
	 * Constructor of the class.
	 */
	public TokenTypeList() {
		super();
		_list = new ObjectList();
	}

	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static TokenTypeList getInstance() {
		if (_instance == null)
			_instance = new TokenTypeList();
		return _instance;
	}

	/**
	 * Load a new token type list.
	 * 
	 * @param list New value to set.
	 */
	public void load(TokenTypeList list) {
		_instance = list;
	}

	/**
	 * Returns a token type from the list in the given position.
	 * 
	 * @param pos Position to get.
	 * 
	 * @return A token type from the list in the given position.
	 */
	public TokenType getTokenType(int pos) {
		return (TokenType) _list.getObjectAt(pos);
	}

	/**
	 * Set a new token in the list.
	 * 
	 * @param tokenType New tokenType to set.
	 */
	public void setTokenType(TokenType tokenType) {
		_list.insert(_list.size(), tokenType);
	}

	/**
	 * Returns the list size.
	 * 
	 * @return The list size.
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * Insert a token type into the list.
	 * 
	 * @param tokenType New token type.
	 * 
	 * @param token Token name.
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
	 * Remove a token from the list.
	 * 
	 * @param token Token to remove.
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
	 * @param token Token name.
	 * 
	 * @return A token from the list.
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
	 * Remove a token type from the list.
	 * 
	 * @param pos Position to remove.
	 */
	public void removeTokenType(int pos) {
		_list.removeAt(pos);
	}

	/**
	 * Reset the token type list.
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
