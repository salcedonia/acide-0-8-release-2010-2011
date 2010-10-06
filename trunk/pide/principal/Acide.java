package principal;

import idioma.Idioma;

import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import operaciones.configuracion.LoadDefaultProject;
import operaciones.fabrica.FactoriaOperaciones;
import operaciones.log.Log;

import org.apache.log4j.Logger;

import es.configuracion.lenguajeprog.Lenguaje;
import es.texto.Fichero;
import gui.StartingWindow;
import gui.Ventana;

/**
 * 
 */
public class Acide {

	public static void main(String[] args) {
		Fichero fi=new Fichero();
		// Show starting window
		System.out.println("23");
		StartingWindow.showStartingWindow();
		// Creamos el logger y lo configuramos
		Log.startLog();
		StartingWindow.setIniciandoLabel("0");
		// Obtenemos el logger
		Logger logger = Log.getLog();
		StartingWindow.setIniciandoLabel("2");
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		StartingWindow.setIniciandoLabel("5");
		ResourceBundle labels = i.getLabels();
		StartingWindow.setIniciandoLabel("7");
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			logger.info(labels.getString("s549"));
		} catch (ClassNotFoundException e) {
			logger.error(labels.getString("s550") + e.getMessage()
					+ labels.getString("s551"));
			e.printStackTrace();
		} catch (InstantiationException e) {
			logger.error(labels.getString("s552") + e.getMessage()
					+ labels.getString("s551"));
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			logger.error(labels.getString("s553") + e.getMessage()
					+ labels.getString("s551"));
			e.printStackTrace();
		} catch (UnsupportedLookAndFeelException e) {
			logger.error(labels.getString("s554") + e.getMessage()
					+ labels.getString("s551"));
			e.printStackTrace();
		}
		StartingWindow.setIniciandoLabel("10");
		logger.info(labels.getString("s555"));
		Lenguaje l = Lenguaje.getInstance();
		
		try {
			l.cargar(almacenPropiedades.getPropiedad("pathLenguaje"));
			Ventana.getInstance().getStatusBar().setMessageLexical(labels.getString("s449") + " " + Lenguaje.getInstance().getNombre());
		} catch (FaltaPropiedadException e) {
			e.printStackTrace();
		}
//		String grammarName = fi.cargar(".\\configuration\\grammars\\currentGrammar\\grammarName.txt");
//		grammarName = grammarName.substring(0,grammarName.length() - 1);
//		Ventana.getInstance().getStatusBar().getMessageGrammar().setText(labels.getString("s248") + " " + grammarName);
		String currentGrammar = null;
		String grammarName = null;
		try {
			currentGrammar = almacenPropiedades.getPropiedad("currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1) index = currentGrammar.lastIndexOf("/");
			grammarName = currentGrammar.substring(index + 1,currentGrammar.length() - 4);
			Ventana.getInstance().getStatusBar().setMessageGrammar(labels.getString("s248") + " " + grammarName);
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null,e.getMessage(),labels.getString("s945"),JOptionPane.ERROR_MESSAGE);
			logger.error(e.getMessage());
		}
		StartingWindow.setIniciandoLabel("12");
		Ventana v = Ventana.getInstance();
		StartingWindow.setIniciandoLabel("15");
		//Obtenemos la ventana y establecemos sus oyentes
		v.getnuevoMenu().estableceOyentesMenu();
		StartingWindow.setIniciandoLabel("17");
		//v.getnuevaSalida().ejecutaCMD();
		StartingWindow.setIniciandoLabel("20");

		//Load Default configuration of acide////
		FactoriaOperaciones facto = FactoriaOperaciones.getInstance();
		//DefaultConfiguration defaultconf = facto.buildDefaultConfiguration();
		//defaultconf.loadDefaultConf();
		//defaultconf.runDefaultConf();
		/////////////////////////////////////

		//Load Default Project//////////////
		LoadDefaultProject defaultPrj = facto.BuildLoadDefaultProject();
		defaultPrj.loadDefault(defaultPrj.preloadDefault());
		
		//v.getSplitPaneV().setDividerLocation(1);
		
		////////////////////////////////////
		
		/*//Load Default Project//////////////
		FactoriaOperaciones facto=FactoriaOperaciones.getInstance();
		LoadDefaultProject defaultPrj=facto.BuildLoadDefaultProject();
		defaultPrj.loadDefault();
		////////////////////////////////////         

		//Load Default configuration of acide////
		DefaultConfiguration defaultconf=facto.buildDefaultConfiguration();
		defaultconf.loadDefaultConf();
		defaultconf.runDefaultConf();
		/////////////////////////////////////*/

	}
}
