package main;

/**
 * 
 */
public class AcideControler {

	/**
	 * 
	 */
	private static AcideControler _instance;
	
	/**
	 * 
	 * @return
	 */
	public static AcideControler getInstance(){
		
		if(_instance == null)
			_instance = new AcideControler();
		return _instance;
	}
}
