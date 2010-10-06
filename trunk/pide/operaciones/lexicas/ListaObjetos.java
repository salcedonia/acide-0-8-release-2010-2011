package operaciones.lexicas;

import java.util.ArrayList;

public class ListaObjetos implements java.io.Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ArrayList lista;
	
	public ListaObjetos() {
		super();
		lista = new ArrayList();
	}
	
	/**
	 * Dice si la lista esta vacia o no 
	 * @return true en el caso afirmativo, false en caso contrario
	*/
	public boolean esVacio(){
       return lista.isEmpty();
	}
	
	/**
	 * Inserta un Objeto en la lista en una posición determinada
	 * (si la posición está ocupada, sobreescribe el Objeto). 
	 * @param pos posicion donde insertar el objeto 
	 * @param obj objeto a insertar
	*/
	public void insertar (int pos, Object obj){
		try{
			if(lista.get(pos)!=null)lista.set(pos,obj);
		}		
		catch(IndexOutOfBoundsException e){
			try{
				lista.add(pos,obj);
			}
			catch(IndexOutOfBoundsException ex){}
		}
	}
	
	/**
	* Accesor del objeto de la posición especificada. Devuelve un Objeto
	* @param pos posición a examinar
	* @return objeto consultado. Si no existe un objeto en la posición especificada, devuelve null.
	*/
    public Object dameObjeto(int pos){
    	try{
			return (Object)lista.get(pos);
		}		
		catch(IndexOutOfBoundsException e){return null;}
	}
    
	/**
	* Elimina el objeto de la posición especificada
	* @param pos posición del objeto a eliminar
	*/
	public void eliminar(int pos){
		try{
		   lista.remove(pos);
		   lista.trimToSize();
		}		
		catch(IndexOutOfBoundsException e){}
	}
	
	
	/**
	 * Devuelve el número de elementos de la lista
	 * @return numero de elementos en la lista
	 */
	public int tamanio(){
		return lista.size();
	}

}
