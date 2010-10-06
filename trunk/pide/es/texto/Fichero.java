package es.texto;

import gui.Ventana;
import idioma.Idioma;

import javax.swing.*;

import operaciones.log.Log;
import org.apache.log4j.Logger;
import principal.almacenPropiedades;
import java.io.*;
import java.net.URL;
import java.nio.CharBuffer;
import java.util.ArrayList;
import java.util.ResourceBundle;

/**
 * Clase que Lee y Escribe un Fichero de texto
 */
public class Fichero {

	
	
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	private JFileChooser selector;
	public Fichero(){
  	selector=new JFileChooser(); 	
  	}
	/**
	 * Metodo que muestra una ventana para la elección del fichero que se lee
	 * 
	 * @return String con el contenido del fichero
	 */
	public String leer() {
		ResourceBundle labels = Idioma.getInstance().getLabels();
		String nombreFichero =null;
		String fi=null;
		File fic=null;
		
		try {
			fi=almacenPropiedades.getPropiedad("DefaultPath");
			fic=new File(fi);
			selector.setDialogTitle(labels.getString("s9"));
			selector.setCurrentDirectory(fic.getParentFile());
			} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
			
		
		
		int valor = selector.showOpenDialog(null);
	   if (valor == JFileChooser.APPROVE_OPTION) {
		  
		   
			nombreFichero = selector.getSelectedFile().getAbsolutePath();
			// Muestra operacion en el log
			
			logger.info(labels.getString("s300") + nombreFichero);
			 System.out.println(nombreFichero);
			  almacenPropiedades.setPropiedad("DefaultPath",nombreFichero);
	   } else if (valor == JFileChooser.CANCEL_OPTION) {
			selector.cancelSelection();
			// Muestra operacion en el log
			
			logger.info(labels.getString("s302"));
	   }
		return nombreFichero;
		
	}
	/**Metodo que obtiene un PATH
	 * 
	 * @return
	 */
	public String leerRuta(){
		ResourceBundle labels = Idioma.getInstance().getLabels();
		String nombreRuta = " ";
		String fi=null;
		File fic=null;
		try {
			fi=almacenPropiedades.getPropiedad("DefaultPath");
			fic=new File(fi);
			selector.setCurrentDirectory(fic.getParentFile());
			} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
			selector.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	
		int valor = selector.showOpenDialog(null);
	   if (valor == JFileChooser.APPROVE_OPTION)  {
		  	nombreRuta = selector.getSelectedFile().getAbsolutePath();
			// Muestra operacion en el log
			logger.info(labels.getString("s303") + nombreRuta);
		almacenPropiedades.setPropiedad("DefaultPath",nombreRuta);
	   } else if (valor == JFileChooser.CANCEL_OPTION) {
			selector.cancelSelection();
			// Muestra operacion en el log
			logger.info(labels.getString("s304"));
	   }
		return nombreRuta;
		}
	
	public String leer(FiltroFicheros filtro) {
		ResourceBundle labels = Idioma.getInstance().getLabels();
		JFileChooser sel = new JFileChooser();
		String nombreFichero = " ";
		String fi=null;
		File fic=null;
		   try {
				fi=almacenPropiedades.getPropiedad("DefaultPath");
				fic=new File(fi);
				sel.setFileFilter(filtro);
				sel.setCurrentDirectory(fic.getParentFile());
				} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}	
	    
	        int valor = sel.showOpenDialog(null);
	   if (valor == JFileChooser.APPROVE_OPTION) {
		  	nombreFichero = sel.getSelectedFile().getAbsolutePath();
			// Muestra operacion en el log
			logger.info(labels.getString("s305") + nombreFichero);
			almacenPropiedades.setPropiedad("DefaultPath",nombreFichero);
	   } else if (valor == JFileChooser.CANCEL_OPTION) {
			sel.cancelSelection();
			// Muestra operacion en el log
			logger.info(labels.getString("s306"));
		}
		return nombreFichero;
		
	}

	/**
	 * Metodo que muestra una ventana para la elección del fichero que se escribe
	 * 
	 * @return String con el contenido del fichero
	 */
	public String escribir() {
		ResourceBundle labels = Idioma.getInstance().getLabels();
		String nombreFichero = " ";
		String fi=null;
		File fic=null;
		Ventana v = Ventana.getInstance();
		try {
			fi=almacenPropiedades.getPropiedad("DefaultPath");
			fic=new File(fi);
			selector.setCurrentDirectory(fic);
			} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
			boolean a = true;
			int valor = selector.showSaveDialog(selector);
			File f = selector.getSelectedFile();
			if (valor == JFileChooser.APPROVE_OPTION) {
				if(f.exists() && selector.getDialogType() == JFileChooser.SAVE_DIALOG) {
					int res = JOptionPane.showConfirmDialog(null,labels.getString("s954"),labels.getString("s953"),JOptionPane.YES_NO_OPTION);
					if(res== JOptionPane.YES_OPTION){a = true;}
					if(res== JOptionPane.NO_OPTION){
						a = false;
						//cerrar archivo si no se quiere sobreescribir
						if(v.getnuevoMenu().isNPF()){
							v.getCreadorEditor().getPane().remove(v.getCreadorEditor().getEditorSeleccionado());
							v.getCreadorEditor().getPane().validate();
						}
					}  
					
				}else a=true;
			}else if (valor == JFileChooser.CANCEL_OPTION) {
    			selector.cancelSelection();
				// Muestra operacion en el log
				logger.info(labels.getString("s308"));
				a=false;
				//cerrar archivo si se cancela nuevo archivo en proyecto
				if(v.getnuevoMenu().isNPF()){
					v.getCreadorEditor().getPane().remove(v.getCreadorEditor().getEditorSeleccionado());
					v.getCreadorEditor().getPane().validate();
				}
        	}
			if(a){
    				nombreFichero = selector.getSelectedFile().getAbsolutePath();
    				// Muestra operacion en el log
    				logger.info(labels.getString("s307") + nombreFichero);
    				almacenPropiedades.setPropiedad("DefaultPath",nombreFichero);
			}
			
		return nombreFichero;
	}	
	
	/**
	 * Metodo leer: Método público que lee un fichero
	 * 
	 * @param fileName:
	 *            Nombre del fichero que se quiere leer
	 * @return String con el contenido del fichero
	 * @throws IOException 
	 */
	public String cargar(String fileName){
		
		ResourceBundle labels = Idioma.getInstance().getLabels();
		       try {
                 BufferedReader reader;
				reader = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)));
				StringBuffer b=new StringBuffer(""); 
				String cad;

				 while (true){
					cad=reader.readLine();
					if (cad==null)break;
					b.append(cad+ "\n");
					}
					reader.close();
			     System.gc();
					return b.toString();		    	
			     
			     
			} catch (IOException e) {
				e.printStackTrace();
				logger.error(labels.getString("s309") + fileName);
				//JOptionPane.showMessageDialog(null, e.getMessage());
				return null;
			}
		    		    
		              
		
	} // leer
	/**
	 * Metodo que salva un String (contenido del editor de texto) en un fichero
	 * 
	 * @param fichero:
	 *            donde se va a salvar
	 * @param cadena:
	 *            que queremos salvar
	 * @return boolean: Con el resultado de la operacion
	 */
	public boolean salvar(String fichero, String cadena) {
		ResourceBundle labels = Idioma.getInstance().getLabels();
		try { // escritura de datos
			PrintWriter salida = new PrintWriter(new BufferedWriter(
					new FileWriter(fichero)));
			salida.print(cadena);
			salida.close();
			logger.info(labels.getString("s310") + fichero);
			return true;
		}// try
		catch (java.io.IOException e) {
			logger.error(labels.getString("s311") + fichero);
			// System.out.println("Error escribiendo " + fichero);
			return false;
		}// catch
	}

public JFileChooser getFileChooser(){
	return selector;
}
}
