package operaciones.lexicas;

import java.awt.Color;

public class TipoToken implements java.io.Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String nombreTipo;
	private Color color;
	private boolean cursiva;
	private boolean negrita;
	private boolean caseSensitive;
	private ListaObjetos tokens;
	
	public TipoToken() {
		super();
		tokens = new ListaObjetos();
		color = Color.black;
		cursiva = false;
		negrita = false;
		caseSensitive = false;
		nombreTipo = color.toString();
		if (cursiva) nombreTipo = nombreTipo + "Cursiva";
		if (negrita) nombreTipo = nombreTipo + "Negrita";
		if (caseSensitive) nombreTipo = nombreTipo + "CS";
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}

	public boolean isCursiva() {
		return cursiva;
	}

	public void setCursiva(boolean cursiva) {
		this.cursiva = cursiva;
	}

	public boolean isNegrita() {
		return negrita;
	}

	public void setNegrita(boolean negrita) {
		this.negrita = negrita;
	}

	public String getNombreTipo() {
		return nombreTipo;
	}

	public void setNombreTipo() {
		nombreTipo = color.toString();
		if (cursiva) nombreTipo = nombreTipo + "Cursiva";
		if (negrita) nombreTipo = nombreTipo + "Negrita";
		if (caseSensitive) nombreTipo = nombreTipo + "CS";
	}

	public String getToken(int pos) {
		return (String) tokens.dameObjeto(pos);
	}

	public void setToken(String token) {
		this.tokens.insertar(tokens.tamanio(),token);
	}
	
	public int getTamanioListaTokens(){
		return tokens.tamanio();
	}
	
	public void eliminarToken(int pos){
		tokens.eliminar(pos);
	}

	public boolean isCaseSensitive() {
		return caseSensitive;
	}

	public void setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}

}
