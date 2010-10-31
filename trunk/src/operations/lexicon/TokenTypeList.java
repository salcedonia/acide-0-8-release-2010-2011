package operations.lexicon;

/**
 * 
 */
public class TokenTypeList implements java.io.Serializable {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private ObjectList _list;
	/**
	 * 
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
	 * 
	 * @return
	 */
	public static TokenTypeList getInstance() {
		if (_instance == null)
			_instance = new TokenTypeList();
		return _instance;
	}

	/**
	 * 
	 * @param list
	 */
	public void load(TokenTypeList list) {
		_instance = list;
	}

	/**
	 * 
	 * @param pos
	 * @return
	 */
	public TokenType getTokenType(int pos) {
		return (TokenType) _list.getObject(pos);
	}

	/**
	 * 
	 * @param tokenType
	 */
	public void setTokenType(TokenType tokenType) {
		_list.insert(_list.size(), tokenType);
	}

	/**
	 * 
	 * @return
	 */
	public int getSize() {
		return _list.size();
	}

	/**
	 * 
	 * @param tokenType
	 * @param token
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
	 * 
	 * @param token
	 */
	public void removeToken(String token) {
		
		for (int i = 0; i < getSize(); i++) {
			for (int j = 0; j < getTokenType(i).getTokenListSize(); j++) {
				String s1 = token;
				String s2 = getTokenType(i).getToken(j);
				if (s1.equals(s2)) {
					getTokenType(i).removeToken(j);
				}
			}
			if (getTokenType(i).getTokenListSize() == 0) {
				removeTipoToken(i);
			}
		}
	}

	/**
	 * 
	 * @param token
	 * @return
	 */
	public TokenType getToken(String token) {
		
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
	 * 
	 * @param pos
	 */
	public void removeTipoToken(int pos) {
		_list.remove(pos);
	}

	/**
	 * 
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
