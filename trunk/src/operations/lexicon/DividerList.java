package operations.lexicon;

import java.io.Serializable;

/**
 * 
 */
public class DividerList implements Serializable{

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
	private static DividerList _instance;
	
	/**
	 * Constructor of the class.
	 */
	public DividerList(){
		
		super();
		_list = new ObjectList();
	}
	
	/**
	 * 
	 * @return
	 */
	public static DividerList getInstance() {
		if (_instance == null)
			_instance = new DividerList();
		return _instance;
	}
	
	/**
	 * 
	 * @param dl
	 */
	public void load(DividerList dl){
		_instance = dl;
	}
	
	/**
	 * 
	 * @param pos
	 * @return
	 */
	public String getDivider(int pos) {
		return (String) _list.getObject(pos);
	}

	/**
	 * 
	 * @param divider
	 */
	public void setDivider(String divider) {
		_list.insert(_list.size(),divider);
	}
	
	/**
	 * 
	 * @return
	 */
	public int getSize(){
		return _list.size();
	}
	
	/**
	 * 
	 * @param divider
	 */
	public void insertDivider(String divider){
		boolean encontrado = false;
		@SuppressWarnings("unused")
		int pos ;
		for (int i = 0; i <getSize(); i++){
			String s1 = getDivider(i);
			if (s1.equals(divider)){
				encontrado = true;
				pos = i;
			}
		}
		
		if (!encontrado)
			setDivider(divider);
	}
	
	/**
	 * 
	 * @param divider
	 */
	public void deleteDelimiter(String divider){
		
		for (int i = 0; i <this.getSize(); i++){
			
			String s1 = this.getDivider(i);
			if (s1.equals(divider))
				_list.remove(i);
		}
	}

	/**
	 * 
	 */
	public void reset() {
		_list = new ObjectList();
	}
}
