package operations.lexicon;

import java.awt.Color;
import java.io.Serializable;

/**
 * Handles the comments of the lexicon of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class Comments implements Serializable{

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Instance of the class.
	 */
	private static Comments _instance;
	/**
	 * Line comment.
	 */
	private String _lineComment;
	/**
	 * Line comment color.
	 */
	private Color _lineCommentColor;
	
	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static Comments getInstance() {
		if (_instance == null)
			_instance = new Comments();
		return _instance;
	}
	
	/**
	 * Constructor of the class.
	 */
	public Comments() {
		super();
	}
	
	/**
	 * Returns the line comment.
	 * 
	 * @return The line comment.
	 */
	public String getLineComment() {
		return _lineComment;
	}

	/**
	 * Set a new value to the line comment.
	 * 
	 * @param lineComment New value to set.
	 */
	public void setLineComment(String lineComment) {
		_lineComment = lineComment;
	}

	/**
	 * Returns the line comment color.
	 *  
	 * @return The line comment color.
	 */
	public Color getLineCommentColor() {
		return _lineCommentColor;
	}

	/**
	 * Set a new value to the line comment color.
	 * 
	 * @param lineCommnetColor New value to set.
	 */
	public void setLineCommentColor(Color lineCommnetColor) {
		_lineCommentColor = lineCommnetColor;
	}

	/**
	 * Load the comment.
	 * 
	 * @param comments New value to set.
	 */
	public void load(Comments comments){
		_instance = comments;
	}

	/**
	 * Reset the comment.
	 */
	public void reset() {
		_lineComment = "";
		_lineCommentColor = Color.BLACK;
	}
}
