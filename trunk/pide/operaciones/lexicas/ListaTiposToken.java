package operaciones.lexicas;

import java.awt.Color;

public class ListaTiposToken implements java.io.Serializable{

	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private ListaObjetos tiposToken;
	
	private static ListaTiposToken instancia;
	
	public ListaTiposToken() {
		super();
		tiposToken = new ListaObjetos();
	}
	
	public static ListaTiposToken getInstance() {
		if (instancia == null)
			instancia = new ListaTiposToken();
		return instancia;
	}
	
	public void carga(ListaTiposToken ltt){
		instancia = ltt;
	}
	
	public TipoToken getTipoToken(int pos) {
		return (TipoToken) tiposToken.dameObjeto(pos);
	}

	public void setTipoToken(TipoToken token) {
		this.tiposToken.insertar(tiposToken.tamanio(),token);
	}
	
	public int getTamanio(){
		return tiposToken.tamanio();
	}
	
	public void prueba(){
		TipoToken tt = new TipoToken();
		tt.setToken("diego");
		tt.setColor(Color.blue);
		tiposToken.insertar(tiposToken.tamanio(),tt);
	}
	
	public void insertarTipoToken(TipoToken tt, String Token){
		boolean encontrado = false;
		boolean encontrado2 = false;
		int pos = 0;
		for (int i = 0; i <this.getTamanio(); i++){
			String s1 = this.getTipoToken(i).getNombreTipo();
			String s2 = tt.getNombreTipo();	
			if (s1.equals(s2)){
				encontrado = true;
				pos = i;
			}
			for (int j =0; j<this.getTipoToken(i).getTamanioListaTokens();j++){
				String s3 = Token;
				String s4 = this.getTipoToken(i).getToken(j);
				if (s3.equals(s4)){
					encontrado2 = true;
				}
			}
		}
		
		if (!encontrado){
			if (!encontrado2){
				this.setTipoToken(tt);
			}
		}
		else{
			if (!encontrado2){
				this.getTipoToken(pos).setToken(Token);
			}
		}
	}
	
	public void quitarToken(String Token){
		for (int i = 0; i <this.getTamanio(); i++){
			for (int j =0; j<this.getTipoToken(i).getTamanioListaTokens();j++){
				String s1 = Token;
				String s2 = this.getTipoToken(i).getToken(j);
				if (s1.equals(s2)){
					this.getTipoToken(i).eliminarToken(j);
				}
			}
			if (this.getTipoToken(i).getTamanioListaTokens() == 0){
				this.eliminarTipoToken(i);
			}
		}
	}
	
	public TipoToken devolverToken(String Token){
		for (int i = 0; i <this.getTamanio(); i++){
			for (int j =0; j<this.getTipoToken(i).getTamanioListaTokens();j++){
				String s1 = Token;
				String s2 = this.getTipoToken(i).getToken(j);
				if (s1.equals(s2)){
					return this.getTipoToken(i);
				}
			}
		}
		return null;
	}
	
	public void eliminarTipoToken(int pos){
		tiposToken.eliminar(pos);
	}

	public void reset() {
		tiposToken = new ListaObjetos();
		
	}
}
