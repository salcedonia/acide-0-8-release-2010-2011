package operations.lexicon;

import java.awt.Color;
import java.io.Serializable;

/************************************************************************																
 * Handles the comments of the lexicon of the application
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
public class Comments implements Serializable{

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Class instance
	 */
	private static Comments _instance;
	/**
	 * Line comment
	 */
	private String _lineComment;
	/**
	 * Line comment color
	 */
	private Color _lineCommentColor;
	
	/**
	 * Returns the unique class instance
	 * 
	 * @return The unique class instance
	 */
	public static Comments getInstance() {
		if (_instance == null)
			_instance = new Comments();
		return _instance;
	}
	
	/**
	 * Class Constructor
	 */
	public Comments() {
		super();
	}
	
	/**
	 * Returns the line comment
	 * 
	 * @return the line comment
	 */
	public String getLineComment() {
		return _lineComment;
	}

	/**
	 * Sets a new value to the line comment
	 * 
	 * @param lineComment new value to set
	 */
	public void setLineComment(String lineComment) {
		_lineComment = lineComment;
	}

	/**
	 * Returns the line comment color
	 *  
	 * @return the line comment color
	 */
	public Color getLineCommentColor() {
		return _lineCommentColor;
	}

	/**
	 * Sets a new value to the line comment color
	 * 
	 * @param lineCommnetColor new value to set
	 */
	public void setLineCommentColor(Color lineCommnetColor) {
		_lineCommentColor = lineCommnetColor;
	}

	/**
	 * Loads the comment
	 * 
	 * @param comments new value to set
	 */
	public void load(Comments comments){
		_instance = comments;
	}

	/**
	 * Resets the comment
	 */
	public void reset() {
		_lineComment = "";
		_lineCommentColor = Color.BLACK;
	}
}
