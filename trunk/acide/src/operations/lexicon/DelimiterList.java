package operations.lexicon;

import java.io.Serializable;

import utils.ObjectList;

/************************************************************************																										
 * Handles the delimiter list of the lexicon of the application
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
public class DelimiterList implements Serializable{

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Object list
	 */
	private ObjectList _list;
	/**
	 * Class instance
	 */
	private static DelimiterList _instance;
	
	/**
	 * Class constructor
	 */
	public DelimiterList(){
		
		super();
		_list = new ObjectList();
	}
	
	/**
	 * Returns the unique class instance
	 * 
	 * @return the unique class instance
	 */
	public static DelimiterList getInstance() {
		if (_instance == null)
			_instance = new DelimiterList();
		return _instance;
	}
	
	/**
	 * Loads the delimiter list
	 * 
	 * @param delimiterList new value to load
	 */
	public void load(DelimiterList delimiterList){
		_instance = delimiterList;
	}
	
	/**
	 * Returns the delimiter at the position given as a parameter
	 * 
	 * @param pos position to get
	 * @return the delimiter at the position given as a parameter
	 */
	public String getDelimiterAt(int pos) {
		return (String) _list.getObjectAt(pos);
	}

	/**
	 * Insert a new delimiter
	 * 
	 * @param delimiter new value to set
	 */
	public void setDelimiter(String delimiter) {
		_list.insert(_list.size(),delimiter);
	}
	
	/**
	 * Returns the list size
	 * 
	 * @return the list size
	 */
	public int getSize(){
		return _list.size();
	}
	
	/**
	 * Inserts a new delimiter in the list
	 * 
	 * @param delimiter new delimiter to insert
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
	 * Deletes a delimiter given as a parameter
	 * 
	 * @param delimiter delimiter to delete
	 */
	public void deleteDelimiter(String delimiter){
		
		for (int i = 0; i <this.getSize(); i++){
			
			String s1 = this.getDelimiterAt(i);
			if (s1.equals(delimiter))
				_list.removeAt(i);
		}
	}

	/**
	 * Resets the list creating a new one
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
