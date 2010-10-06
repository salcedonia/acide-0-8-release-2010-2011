package gui.iconos;

import idioma.Idioma;
import javax.swing.*;

import operaciones.configuracion.IconoEditable;
import operaciones.configuracion.ListaIconosEditables;
import operaciones.log.Log;
import operaciones.salida.ProcessThread;
import operaciones.sintacticas.grammar.Prueba;
import org.apache.log4j.Logger;
import principal.almacenPropiedades;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import gui.Ventana;
import gui.salida.Salida;

/**
 * Clase que crea los Iconos
 */
public class Iconos extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private static Logger logger = Log.getLog();

	/**
	 * Atributo que contiene la barra de herramientas fija del programa
	 */
	private static JToolBar barraHerrFija = new JToolBar();

	/**
	 * Añade a la ventana principal la barra de herramientas fija con las
	 * operaciones incluidas por defecto y que estarán siempre disponibles
	 * 
	 * @return La barra de herramientas en un objeto de tipo JToolBar
	 */
	public static JToolBar generaToolBarFija() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		logger.info(labels.getString("s102"));
		JButton nuevo = new JButton(new ImageIcon("iconos/new-acide.png"));
		nuevo.setToolTipText(labels.getString("s103"));
		nuevo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getNuevoFich().doClick();
			}
		});
		JButton abrir = new JButton(new ImageIcon("iconos/open-acide.png"));
		abrir.setToolTipText(labels.getString("s106"));
		abrir.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getAbrirFich().doClick();
			}
		});
		JButton salvar = new JButton(new ImageIcon("iconos/save-acide.png"));
		salvar.setToolTipText(labels.getString("s114"));
		salvar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				//mig
				v.getnuevoMenu().getSave().setEnabled(true);
				v.getnuevoMenu().getSave().doClick();
				//v.getnuevoMenu().getSave().setEnabled(false);
			}
		});
		JButton saveAll = new JButton(
				new ImageIcon("iconos/save-all-acide.png"));
		saveAll.setToolTipText(labels.getString("s229"));
		saveAll.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				//mig
				v.getnuevoMenu().getSaveAll().setEnabled(true);
				v.getnuevoMenu().getSaveAll().doClick();
				//v.getnuevoMenu().getSaveAll().setEnabled(false);
			}
		});
		JButton nuevoproy = new JButton(new ImageIcon(
				"iconos/new-project-acide.png"));
		nuevoproy.setToolTipText(labels.getString("s122"));
		nuevoproy.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getNuevoProyecto().doClick();
			}
		});
		JButton abrirproy = new JButton(new ImageIcon(
				"iconos/open-project-acide.png"));
		abrirproy.setToolTipText(labels.getString("s123"));
		abrirproy.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getAbrirProyecto().doClick();
			}
		});
		JButton guardarproy = new JButton(new ImageIcon(
				"iconos/save-project-acide.png"));
		guardarproy.setToolTipText(labels.getString("s124"));
		guardarproy.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				//mig
				v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
				v.getnuevoMenu().getGuardarProyecto().doClick();
				//v.getnuevoMenu().getGuardarProyecto().setEnabled(false);
			}
		});
		JButton analyzeSintButton = new JButton(labels.getString("s206"));
		analyzeSintButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				/*
				 * boolean recException = false; boolean tokException = false;
				 * boolean noException = true;
				 */
//				Ventana v = Ventana.getInstance();
//				String text = v.getCreadorEditor().dameEditorI(
//						v.getCreadorEditor().getEditorSeleccionado())
//						.getEditor().getText();
//				Prueba.analyze(text);
				//C:\jdk1.5.0_05\jre1.5.0_05\bin\java -jar acide.jar principal.Acide
				try {
					String currentGrammar = almacenPropiedades.getPropiedad("currentGrammar");
					String javaPath = almacenPropiedades.getPropiedad("javaPath");
					//Runtime.getRuntime().exec("\"" + javaPath + "\" -jar \"" + currentGrammar + "\" operaciones.sintacticas.Analyzer");
					ProcessThread p = new ProcessThread();
					Salida s = new Salida(false);
					p.executeCommand("cmd",".","\"" + javaPath + "\" -jar \"" + currentGrammar + "\" operaciones.sintacticas.Analyzer","exit",s);
					System.out.println("\"" + javaPath + "\" -jar \"" + currentGrammar + "\" operaciones.sintacticas.Analyzer");
					JFrame output = new JFrame(labels.getString("s946"));
					output.add(s);
					output.setSize(new Dimension(300,400));
					output.setVisible(true);
				}
				catch (Exception e1) {
					// TODO Pasar esto por archivo de idioma
					JOptionPane.showMessageDialog(null,"Error analyzer","Error",JOptionPane.ERROR_MESSAGE);
				}
			}
		});
		barraHerrFija.removeAll();
		barraHerrFija.add(nuevo);
		barraHerrFija.add(abrir);
		barraHerrFija.add(salvar);
		barraHerrFija.add(saveAll);
		barraHerrFija.add(nuevoproy);
		barraHerrFija.add(abrirproy);
		barraHerrFija.add(guardarproy);
		//barraHerrFija.add(analyzeSintButton);
		logger.info(labels.getString("s125"));
		return barraHerrFija;
	}

	/**
	 * Añade a la ventana principal la barra de herramientas editable por el
	 * usuario con las operaciones que se indiquen para cada uno de los iconos
	 * de la barra
	 */
	public static JToolBar generaToolBarEditable() {
		Idioma id = Idioma.getInstance();
		try {
			id.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = id.getLabels();
		logger.info(labels.getString("s130"));
		barraHerrFija.addSeparator();
		JButton boton;
		for (int i = 0; i < ListaIconosEditables.getTamaño(); i++) {
			final IconoEditable icon = ListaIconosEditables.getIcono(i);
			if (icon.getTieneImagen()) boton = new JButton(new ImageIcon(icon
					.getImagen()));
			else if (!(icon.getNombre().equals(""))) boton = new JButton(icon
					.getNombre());
			else boton = new JButton((new Integer(i + 1)).toString());
			if (!(icon.getTextoAyuda().equals(""))) boton.setToolTipText(icon
					.getTextoAyuda());
			barraHerrFija.add(boton);
			boton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					Ventana.getInstance().getnuevaSalida().ejecutaComando(
							icon.getComando());
				}
			});
		}
		logger.info(labels.getString("s131"));
		return barraHerrFija;
	}

}
