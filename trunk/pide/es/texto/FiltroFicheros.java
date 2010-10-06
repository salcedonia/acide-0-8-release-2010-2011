package es.texto;

import idioma.Idioma;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import javax.swing.filechooser.FileFilter;

public class FiltroFicheros extends FileFilter{
	   // descripción y extensiones aceptadas por el filtro
	   private String description;
	   private List<String> extensions;
	 
	   // constructor a partir de la descripción
	   public FiltroFicheros(String description){
		   ResourceBundle labels = Idioma.getInstance().getLabels();
		   if(description == null){
	         throw new NullPointerException(labels.getString("s312"));
	      }
	      this.description = description;
	      this.extensions = new ArrayList<String>();
	   }
	 
	   // implementación del FileFilter
	   public boolean accept(File file){
	      if(file.isDirectory() || extensions.size()==0) { 
	         return true; 
	      } 
	      String nombreFichero = file.getName().toLowerCase(); 
	      for(String extension : extensions){
	         if(nombreFichero.endsWith(extension)){
	            return true;
	         }
	      }
	      return false;
	   }
	 
	   public String getDescription(){
		  StringBuffer buffer = new StringBuffer(description);
	      buffer.append(" (");
	      for(String extension : extensions){
	         buffer.append(extension).append(" ");
	      }
	      return buffer.append(")").toString();
	   }
	 
	   // métodos de ayuda
	   public void setDescription(String description){
		   ResourceBundle labels = Idioma.getInstance().getLabels();
		   if(description == null){
	         throw new NullPointerException(labels.getString("s313"));
	      }
	      this.description = description;
	   }
	 
	   public void addExtension(String extension){
		   ResourceBundle labels = Idioma.getInstance().getLabels();
		   if(extension == null){
	         throw new NullPointerException(labels.getString("s314"));
	      }
	      extensions.add(extension.toLowerCase());
	   }
	 
	   public void removeExtension(String extension){
	      extensions.remove(extension);
	   }
	 
	   public void clearExtensions(){
	      extensions.clear();
	   }
	 
	   public List<String> getExtensions(){
	      return extensions;
	   }
	}
