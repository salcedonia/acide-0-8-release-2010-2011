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
	 * Inserta un Objeto en la lista en una posici�n determinada
	 * (si la posici�n est� ocupada, sobreescribe el Objeto). 
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
	* Accesor del objeto de la posici�n especificada. Devuelve un Objeto
	* @param pos posici�n a examinar
	* @return objeto consultado. Si no existe un objeto en la posici�n especificada, devuelve null.
	*/
    public Object dameObjeto(int pos){
    	try{
			return (Object)lista.get(pos);
		}		
		catch(IndexOutOfBoundsException e){return null;}
	}
    
	/**
	* Elimina el objeto de la posici�n especificada
	* @param pos posici�n del objeto a eliminar
	*/
	public void eliminar(int pos){
		try{
		   lista.remove(pos);
		   lista.trimToSize();
		}		
		catch(IndexOutOfBoundsException e){}
	}
	
	
	/**
	 * Devuelve el n�mero de elementos de la lista
	 * @return numero de elementos en la lista
	 */
	public int tamanio(){
		return lista.size();
	}

}
