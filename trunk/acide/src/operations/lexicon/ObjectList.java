package operations.lexicon;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * 
 */
public class ObjectList implements Serializable{

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private ArrayList<Object> _list;
	
	/**
	 * Constructor of the class.
	 */
	public ObjectList() {
		
		super();
		_list = new ArrayList<Object>();
	}
	
	/**
	 * 
	 * @return
	*/
	public boolean isEmpty(){
       return _list.isEmpty();
	}
	
	/**
	 * 
	 * @param pos 
	 * @param obj 
	*/
	public void insert(int pos, Object obj){
		try{
			if(_list.get(pos) != null)
				_list.set(pos,obj);
		}		
		catch(IndexOutOfBoundsException e){
			try{
				_list.add(pos,obj);
			}
			catch(IndexOutOfBoundsException ex){
				
			}
		}
	}
	
	/**
	* 
	* @param pos 
	* @return 
	*/
    public Object getObject(int pos){
    	try{
			return (Object)_list.get(pos);
		}		
		catch(IndexOutOfBoundsException e){return null;}
	}
    
	/**
	* 
	* @param pos 
	*/
	public void remove(int pos){
		try{
		   _list.remove(pos);
		   _list.trimToSize();
		}		
		catch(IndexOutOfBoundsException e){}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public int size(){
		return _list.size();
	}

}
