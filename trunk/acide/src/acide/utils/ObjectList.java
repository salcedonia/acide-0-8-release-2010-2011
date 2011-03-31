package acide.utils;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Serializable object list of the lexicon of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ObjectList implements Serializable {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * List.
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
	 * Returns the is empty flag.
	 * 
	 * @return The is empty flag.
	 */
	public boolean isEmpty() {
		return _list.isEmpty();
	}

	/**
	 * Insert an object in a position given as a parameter.
	 * 
	 * @param pos Position to insert.
	 * @param object Object to insert.
	 */
	public void insert(int pos, Object object) {
		try {
			if (_list.get(pos) != null)
				_list.set(pos, object);
		} catch (IndexOutOfBoundsException e) {
			try {
				_list.add(pos, object);
			} catch (IndexOutOfBoundsException ex) {

			}
		}
	}

	/**
	 * Returns the object at the position given as a parameter.
	 * 
	 * @param pos Position to get.
	 * 
	 * @return The object at the position given as a parameter.
	 */
	public Object getObjectAt(int pos) {
		try {
			return (Object) _list.get(pos);
		} catch (IndexOutOfBoundsException e) {
			return null;
		}
	}

	/**
	 * Removes an element at the position of the list given as a parameter.
	 * 
	 * @param pos Position to remove.
	 */
	public void removeAt(int pos) {
		try {
			_list.remove(pos);
			_list.trimToSize();
		} catch (IndexOutOfBoundsException e) {
		}
	}

	/**
	 * Returns the list size.
	 * 
	 * @return The list size.
	 */
	public int size() {
		return _list.size();
	}

}
