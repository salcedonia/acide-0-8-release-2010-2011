package es.configuracion.lenguajeprog;

import idioma.Idioma;

import java.awt.Color;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import operaciones.configuracion.GramaticaConfig;
import operaciones.lexicas.Comments;
import operaciones.lexicas.DividerList;
import operaciones.lexicas.ListaTiposToken;
import principal.almacenPropiedades;

import com.thoughtworks.xstream.XStream;

import es.texto.ExtensionesValidas;

public class Lenguaje {

	private String nombre;
	
	private String pathLenguaje;

	private boolean compiladoInterpretado;

	private ListaTiposToken Listatokens;

	private String pathGramatica;

	private ExtensionesValidas extensiones;

	private static Lenguaje instancia;
	
	private DividerList dividerList;
	
	private Comments comments;

	public Lenguaje() {
		super();
	}

	/**
	 * Crea una unica instancia de Lenguaje
	 * 
	 * @return
	 */
	public static Lenguaje getInstance() {
		if (instancia == null)
			instancia = new Lenguaje();
		return instancia;
	}
	
	
	public void newLanguaje(String pl){
		int index = pl.lastIndexOf("\\");
		String n = pl.substring(index + 1,pl.length());
		if (n.contains(".")){
			index = n.lastIndexOf(".");
			n = n.substring(0,index);
		}
		nombre = n;
		ListaTiposToken.getInstance().reset();
		DividerList.getInstance().reset();
		Comments.getInstance().reset();
		pathLenguaje = pl;
		almacenPropiedades.setPropiedad("pathLenguaje",pl);
		guardar(nombre,false);
	}
	
	public boolean guardar(String nom, boolean CompInt) {
		if ((nombre != null)&&(!nombre.trim().equalsIgnoreCase(""))){
		nombre = nom;
		compiladoInterpretado = CompInt;
		Listatokens = ListaTiposToken.getInstance();
		pathGramatica = GramaticaConfig.ruta;
		extensiones = ExtensionesValidas.getInstance();
		dividerList = DividerList.getInstance();
		comments = Comments.getInstance();
		XStream x = new XStream();
		try {
			FileOutputStream f = new FileOutputStream(pathLenguaje);
			x.toXML(this, f);
			f.close();
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		almacenPropiedades.setPropiedad("pathLenguaje",pathLenguaje);
		}
		return true;
	}
	
	public boolean guardarComo(String nom, boolean CompInt, String path) {
		if ((nombre != null)&&(!nombre.trim().equalsIgnoreCase(""))){
		nombre = nom;
		compiladoInterpretado = CompInt;
		Listatokens = ListaTiposToken.getInstance();
		pathGramatica = GramaticaConfig.ruta;
		extensiones = ExtensionesValidas.getInstance();
		dividerList = DividerList.getInstance();
		comments = Comments.getInstance();
		XStream x = new XStream();
		try {
			FileOutputStream f = new FileOutputStream(path);
			x.toXML(this, f);
			f.close();
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		}
		return true;
	}
	
	public String guardarTemp(String nom, boolean CompInt) {
		File xmlTmp = null;
		if ((nombre != null)&&(!nombre.trim().equalsIgnoreCase(""))){
		nombre = nom;
		compiladoInterpretado = CompInt;
		Listatokens = ListaTiposToken.getInstance();
		pathGramatica = GramaticaConfig.ruta;
		extensiones = ExtensionesValidas.getInstance();
		dividerList = DividerList.getInstance();
		comments = Comments.getInstance();
		XStream x = new XStream();
		
		try {
			xmlTmp = File.createTempFile("TMP",".xml", new File(".//Configuration//Lexical//Temp//"));
			xmlTmp.deleteOnExit();

			FileOutputStream f = new FileOutputStream(xmlTmp);
			
			x.toXML(this, f);
			f.close();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
		}
		return xmlTmp.getAbsolutePath();
	}

	public void cargarTemp(String pathFile) {
		if ((pathFile != null)&&(!pathFile.trim().equalsIgnoreCase(""))){
		
		try {
			XStream x = new XStream();
			FileInputStream f = new FileInputStream(pathFile);
			Lenguaje l =  (Lenguaje) x.fromXML(f);
			String n = l.nombre;
			Boolean ci = l.compiladoInterpretado;
			ListaTiposToken lt = l.Listatokens;
			String pg = l.pathGramatica;
			ExtensionesValidas ex = l.extensiones;
			DividerList dl = l.dividerList;
			Comments c = l.comments;
			f.close();
			
			nombre = n;
			compiladoInterpretado = ci;
			Listatokens = lt;
			pathGramatica = pg;
			extensiones = ex;
			dividerList = dl;
			comments = c;
			pathLenguaje = pathFile;
			
			DividerList.getInstance().carga(dividerList);
			ListaTiposToken.getInstance().carga(Listatokens);
			GramaticaConfig.setRuta(pathGramatica);
			ExtensionesValidas.getInstance().carga(extensiones);
			Comments.getInstance().carga(comments);
			
		} catch (Exception e) {
			e.printStackTrace();
			Idioma i = Idioma.getInstance();
			try {
				i.seleccionIdioma(Integer.parseInt(almacenPropiedades
						.getPropiedad("idioma")));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			ResourceBundle labels = i.getLabels();
			Object[] options = { labels.getString("s445"), labels.getString("s446") };
			JOptionPane.showMessageDialog(null,labels.getString("s526"), labels.getString("s526"),0);
		}
		}
	}
	
	public void cargar(String pathFile) {
		if ((pathFile != null)&&(!pathFile.trim().equalsIgnoreCase(""))){
			//System.out.println("eeeoi"+pathFile);
		try {
			XStream x = new XStream();
			FileInputStream f = new FileInputStream(pathFile);
			Lenguaje l =  (Lenguaje) x.fromXML(f);
			String n = l.nombre;
			Boolean ci = l.compiladoInterpretado;
			ListaTiposToken lt = l.Listatokens;
			String pg = l.pathGramatica;
			ExtensionesValidas ex = l.extensiones;
			DividerList dl = l.dividerList;
			Comments c = l.comments;
			f.close();
			
			nombre = n;
			compiladoInterpretado = ci;
			Listatokens = lt;
			pathGramatica = pg;
			extensiones = ex;
			dividerList = dl;
			comments = c;
			pathLenguaje = pathFile;
			
			DividerList.getInstance().carga(dividerList);
			ListaTiposToken.getInstance().carga(Listatokens);
			GramaticaConfig.setRuta(pathGramatica);
			ExtensionesValidas.getInstance().carga(extensiones);
			Comments.getInstance().carga(comments);
			
			almacenPropiedades.setPropiedad("pathLenguaje",pathFile);
			
		} catch (Exception e) {
			e.printStackTrace();
			/*Idioma i = Idioma.getInstance();
			try {
				i.seleccionIdioma(Integer.parseInt(almacenPropiedades
						.getPropiedad("idioma")));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			ResourceBundle labels = i.getLabels();
			Object[] options = { labels.getString("s445"), labels.getString("s446") };
			JOptionPane.showMessageDialog(null,labels.getString("s526"), labels.getString("s526"),0);
			*/
		}
		}
	}

	/*public boolean isCompiladoInterpretado() {
		return compiladoInterpretado;
	}

	public void setCompiladoInterpretado(boolean compiladoInterpretado) {
		this.compiladoInterpretado = compiladoInterpretado;
	}*/

	public ExtensionesValidas getExtensiones() {
		return extensiones;
	}

	public void setExtensiones(ExtensionesValidas extensiones) {
		this.extensiones = extensiones;
	}

	public ListaTiposToken getListatokens() {
		return Listatokens;
	}

	public void setListatokens(ListaTiposToken listatokens) {
		Listatokens = listatokens;
	}

	public String getNombre() {
		return nombre;
	}

	public void setNombre(String nombre) {
		this.nombre = nombre;
	}

	public String getPathGramatica() {
		return pathGramatica;
	}

	public void setPathGramatica(String pathGramatica) {
		this.pathGramatica = pathGramatica;
	}

	public DividerList getDividerList() {
		return dividerList;
	}

	public void setDividerList(DividerList dividerList) {
		this.dividerList = dividerList;
	}
	
	public String getPathLenguaje(){
		return pathLenguaje;
	}
}
