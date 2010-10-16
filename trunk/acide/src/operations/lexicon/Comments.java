package operations.lexicon;

import java.awt.Color;
import java.io.Serializable;

/**
 * 
 */
public class Comments implements Serializable{

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static Comments _instance;
	/**
	 * 
	 */
	private String _lineComment;
	/**
	 * 
	 */
	private Color _lineCommentColor;
	
	/**
	 * 
	 * @return
	 */
	public static Comments getInstance() {
		if (_instance == null)
			_instance = new Comments();
		return _instance;
	}
	
	/**
	 * 
	 */
	public Comments() {
		super();
	}
	
	/**
	 * 
	 * @return
	 */
	public String getLineComment() {
		return _lineComment;
	}

	/**
	 * 
	 * @param lineComment
	 */
	public void setLineComment(String lineComment) {
		_lineComment = lineComment;
	}

	/**
	 * 
	 * @return
	 */
	public Color getLineCommentColor() {
		return _lineCommentColor;
	}

	/**
	 * 
	 * @param lineCommnetColor
	 */
	public void setLineCommentColor(Color lineCommnetColor) {
		_lineCommentColor = lineCommnetColor;
	}

	/**
	 * 
	 * @param comments
	 */
	public void load(Comments comments){
		_instance = comments;
	}

	/**
	 * 
	 */
	public void reset() {
		_lineComment = "";
		_lineCommentColor = Color.BLACK;
	}
}
