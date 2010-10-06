package operaciones.lexicas;

import java.awt.Color;

import es.configuracion.lenguajeprog.Lenguaje;

public class Comments implements java.io.Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private static Comments instancia;
	
	private String lineComment;
	
	private Color lineCommentColor;
	
	/**
	 * Crea una unica instancia de Lenguaje
	 * 
	 * @return
	 */
	public static Comments getInstance() {
		if (instancia == null)
			instancia = new Comments();
		return instancia;
	}
	
	
	public Comments() {
		super();
	}
	

	public String getLineComment() {
		return lineComment;
	}


	public void setLineComment(String lineComment) {
		this.lineComment = lineComment;
	}


	public Color getLineCommentColor() {
		return lineCommentColor;
	}


	public void setLineCommentColor(Color lineCommnetColor) {
		this.lineCommentColor = lineCommnetColor;
	}

	public void carga(Comments c){
		instancia = c;
	}


	public void reset() {
		lineComment = "";
		lineCommentColor = Color.BLACK;
	}

}
