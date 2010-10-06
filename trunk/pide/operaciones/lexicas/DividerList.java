package operaciones.lexicas;

import java.awt.Color;

public class DividerList implements java.io.Serializable{

	
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private ListaObjetos Dividers;
	
	private static DividerList instancia;
	
	public DividerList() {
		super();
		Dividers = new ListaObjetos();
	}
	
	public static DividerList getInstance() {
		if (instancia == null)
			instancia = new DividerList();
		return instancia;
	}
	
	public void carga(DividerList dl){
		instancia = dl;
	}
	
	public String getDivider(int pos) {
		return (String) Dividers.dameObjeto(pos);
	}

	public void setDivider(String divider) {
		this.Dividers.insertar(Dividers.tamanio(),divider);
	}
	
	public int getTamanio(){
		return Dividers.tamanio();
	}
	
	
	public void insertDivider(String divider){
		boolean encontrado = false;
		int pos = 0;
		for (int i = 0; i <this.getTamanio(); i++){
			String s1 = this.getDivider(i);
			if (s1.equals(divider)){
				encontrado = true;
				pos = i;
			}
		}
		
		if (!encontrado){
				this.setDivider(divider);
		}
	}
	
	public void deleteDelimiter(String divider){
		for (int i = 0; i <this.getTamanio(); i++){
			String s1 = this.getDivider(i);
			if (s1.equals(divider)){
				Dividers.eliminar(i);
			}
		}
	}

	public void reset() {
		Dividers = new ListaObjetos();
		
	}
}
