package es.texto;

import java.util.StringTokenizer;

import operaciones.lexicas.ListaObjetos;


public class ExtensionesValidas implements java.io.Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ListaObjetos extensiones;
	private static ExtensionesValidas instancia;
	
	public ExtensionesValidas() {
		super();
		extensiones = new ListaObjetos();
	}
	
	public void carga(ExtensionesValidas extensiones){
		instancia = extensiones;
	}

	public Object getExtensionI(int pos) {
		return extensiones.dameObjeto(pos);
	}

	public void setExtensionI(Object elemento) {
		this.extensiones.insertar(this.extensiones.tamanio(),elemento);
	}
	
	public static ExtensionesValidas getInstance() {
		if (instancia == null)
			instancia = new ExtensionesValidas();
		return instancia;
	}
	
	public boolean extensionValida(String extension){
		for (int i =0; i< extensiones.tamanio(); i++){
			if (extension.endsWith((String)extensiones.dameObjeto(i))){
				return true;
			}
		}
		return false;
	}
	
	public void ExtensionesTokenizer(String cadena){
		StringTokenizer tokens=new StringTokenizer(cadena,",");
		for (int i =0;i<tokens.countTokens();i++){
			this.setExtensionI(tokens.nextToken());
		}
	}
}
