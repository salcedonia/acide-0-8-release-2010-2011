package operations.lexicon;

import java.io.Serializable;

import utils.ObjectList;


/**
 * Handles the delimiter list of the lexicon of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class DelimiterList implements Serializable{

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
	private static DelimiterList _instance;
	
	/**
	 * Constructor of the class.
	 */
	public DelimiterList(){
		
		super();
		_list = new ObjectList();
	}
	
	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static DelimiterList getInstance() {
		if (_instance == null)
			_instance = new DelimiterList();
		return _instance;
	}
	
	/**
	 * Load the delimiter list.
	 * 
	 * @param delimiterList New value to load.
	 */
	public void load(DelimiterList delimiterList){
		_instance = delimiterList;
	}
	
	/**
	 * Returns the delimiter at the position given as a parameter.
	 * 
	 * @param pos Position to get.
	 * 
	 * @return The delimiter at the position given as a parameter.
	 */
	public String getDelimiterAt(int pos) {
		return (String) _list.getObjectAt(pos);
	}

	/**
	 * Insert a new delimiter.
	 * 
	 * @param delimiter New value to set.
	 */
	public void setDelimiter(String delimiter) {
		_list.insert(_list.size(),delimiter);
	}
	
	/**
	 * Returns the list size.
	 * 
	 * @return The list size.
	 */
	public int getSize(){
		return _list.size();
	}
	
	/**
	 * Inserts a new delimiter in the list.
	 * 
	 * @param delimiter New delimiter to insert.
	 */
	public void insertDelimiter(String delimiter){
		
		boolean found = false;
		
		for (int i = 0; i <getSize(); i++){
			String s1 = getDelimiterAt(i);
			
			if (s1.equals(delimiter))
				found = true;
		}
		
		if (!found)
			setDelimiter(delimiter);
	}
	
	/**
	 * Delete a delimiter given as a parameter.
	 * 
	 * @param delimiter Delimiter to delete.
	 */
	public void deleteDelimiter(String delimiter){
		
		for (int i = 0; i <this.getSize(); i++){
			
			String s1 = this.getDelimiterAt(i);
			if (s1.equals(delimiter))
				_list.removeAt(i);
		}
	}

	/**
	 * Reset the list creating a new one.
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
