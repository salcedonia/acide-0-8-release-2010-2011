package es.text;

import java.util.StringTokenizer;

import operations.lexicon.ObjectList;

/**
 * 
 */
public class ValidExtensions implements java.io.Serializable{

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
	private static ValidExtensions _instance;
	
	/**
	 * Constructor of the class.
	 */
	public ValidExtensions() {
		super();
		_list = new ObjectList();
	}
	
	/**
	 * 
	 * @param extensions
	 */
	public void carga(ValidExtensions extensions){
		_instance = extensions;
	}

	/**
	 * 
	 * @param pos
	 * @return
	 */
	public Object getExtensionAt(int pos) {
		return _list.getObject(pos);
	}

	/**
	 * 
	 * @param element
	 */
	public void setExtensionAt(Object element) {
		_list.insert(_list.size(),element);
	}
	
	/**
	 * 
	 * @return
	 */
	public static ValidExtensions getInstance() {
		if (_instance == null)
			_instance = new ValidExtensions();
		return _instance;
	}
	
	/**
	 * 
	 * @param extension
	 * @return
	 */
	public boolean isValidExtension(String extension){
		
		for (int i =0; i< _list.size(); i++)
			if (extension.endsWith((String)_list.getObject(i)))
				return true;

		return false;
	}
	
	/**
	 * 
	 * @param string
	 */
	public void tokenizeExtensions(String string){
		
		StringTokenizer tokens = new StringTokenizer(string,",");
		
		for (int i =0; i < tokens.countTokens();i++)
			setExtensionAt(tokens.nextToken());
	}
}
