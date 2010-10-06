package gui.parametrizacion;

import gui.Ventana;
import idioma.Idioma;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import operaciones.log.Log;

import org.apache.log4j.Logger;
import principal.almacenPropiedades;

import es.configuracion.lenguajeprog.Lenguaje;
import es.texto.ExtensionesValidas;

/**
 * Clase que implementa el interfaz gráfico para crear una confifuración nueva de un lenguaje o modificar una existente
 */
public class LenguajeGUI extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private JFrame frame;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();
	private String undopath ="";
	
	public LenguajeGUI() {
		ResourceBundle labels = Idioma.getInstance().getLabels();
		logger.info(labels.getString("s351"));
		frame = new JFrame();
		frame.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		frame.setTitle(labels.getString("s352"));
		// Creacion de todos los paneles de la ventana
		JPanel nombrePanel = new JPanel();
		JPanel configPanel = new JPanel();
		JPanel extensionesPanel = new JPanel();
		JPanel tipoPanel = new JPanel();
		JPanel botonesPanel = new JPanel();		
		nombrePanel.setLayout(new GridBagLayout());
		nombrePanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s353")));
		configPanel.setLayout(new GridBagLayout());
		configPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s354")));
		extensionesPanel.setLayout(new GridLayout(0,1));
		extensionesPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s355")));
		tipoPanel.setLayout(new GridLayout(1,0));
		tipoPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s356")));
		botonesPanel.setLayout(new GridBagLayout());		
		// Creacion de todas las componentes de los paneles
		JLabel nombreLabel = new JLabel(labels.getString("s357"),JLabel.CENTER);
		final JTextField nombreTextField = new JTextField();
		nombreTextField.setToolTipText(labels.getString("s358"));
		JButton confLexicaBoton = new JButton(labels.getString("s359"));
		confLexicaBoton.setHorizontalAlignment(JButton.CENTER);
		confLexicaBoton.setToolTipText(labels.getString("s360"));
		JButton confSintacticaBoton = new JButton(labels.getString("s361"));
		confSintacticaBoton.setToolTipText(labels.getString("s362"));
		confSintacticaBoton.setHorizontalAlignment(JButton.CENTER);
		JLabel archivosLabel = new JLabel(labels.getString("s363"), JLabel.CENTER);
		final JTextField archivosTextField = new JTextField();
		archivosTextField.setToolTipText(labels.getString("s364"));
		final JRadioButton compiladoRadio = new JRadioButton(labels.getString("s365"));	
		compiladoRadio.setHorizontalAlignment(JRadioButton.CENTER);
		JRadioButton interpretadoRadio = new JRadioButton(labels.getString("s366"));
		interpretadoRadio.setHorizontalAlignment(JRadioButton.CENTER);
		ButtonGroup grupoRadio = new ButtonGroup();
		grupoRadio.add(compiladoRadio);
		grupoRadio.add(interpretadoRadio);
		JButton aceptarBoton = new JButton(labels.getString("s367"));
		aceptarBoton.setToolTipText(labels.getString("s368"));
		aceptarBoton.setHorizontalAlignment(JButton.CENTER);
		JButton cancelarBoton = new JButton(labels.getString("s369"));
		cancelarBoton.setToolTipText(labels.getString("s370"));
		cancelarBoton.setHorizontalAlignment(JButton.CENTER);
		// Oyentes de los botones
		confLexicaBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					undopath = almacenPropiedades.getPropiedad("pathLenguaje");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				Lenguaje.getInstance().setNombre(nombreTextField.getText());
				new LexicaGUI();
			}
		});
		confSintacticaBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new SintacticaGUI(false);
			}
		});
		aceptarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				
				ResourceBundle labels = Idioma.getInstance().getLabels();
				// leer nombre del lenguaje
				String s = nombreTextField.getText();
				// almacenar extensiones de archivos aceptados
				ExtensionesValidas ev = ExtensionesValidas.getInstance();
				ev.ExtensionesTokenizer(archivosTextField.getText());
				// poner compilado o interpretado
				boolean ci = compiladoRadio.isSelected();
				Lenguaje l = Lenguaje.getInstance();
				l.guardar(s,ci);
				//System.out.println(l.getNombre());
				//OPCION MOSTRAR NUEVO LENGUAJE EN EL PANEL DE PROYECTO//
				Ventana v=Ventana.getInstance();
				if (v.getProyGUI()!= null){
				v.getProyGUI().setNombreConfLexico(l.getNombre());
				v.getProyGUI().setNombreConfLabelLexico(labels.getString("s599") + 
						v.getProyGUI().getNombreConfLexico());
				Ventana.getInstance().getStatusBar().setMessageLexical(labels.getString("s449") + " " + l.getNombre());
				logger.info(labels.getString("s371") + nombreTextField.getText());
				}
				frame.dispose();
				//mig
				//v.getProyecto().setModified(true);
			}
		});
		cancelarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				ResourceBundle labels = Idioma.getInstance().getLabels();
				logger.info(labels.getString("s372"));
				try {				
					Lenguaje.getInstance().cargar(undopath);
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				frame.dispose();
			}
		});
		// Valores para la etiqueta nombreLabel
		c.fill = GridBagConstraints.NONE;
		c.insets = new Insets(5,5,5,5);
		c.gridx = 0;
		c.gridy = 0;
		nombrePanel.add(nombreLabel,c);
		// Valores para el campo de texto nombreTextField
		c.ipadx = 130;
		c.gridx = 1;
		nombrePanel.add(nombreTextField,c);
		// Valores para nombrePanel
		c.fill = GridBagConstraints.BOTH;
		c.ipadx = 0;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(0,0,0,0);
		frame.add(nombrePanel,c);
		// Valores para el boton confLexicaBoton
		c.insets = new Insets(5,5,5,5);
		c.gridx = 0;
		c.gridy = 0;
		configPanel.add(confLexicaBoton,c);
		// Valores para el boton confSintacticaBoton
		c.gridx = 1;
		c.gridy = 0;
		c.insets = new Insets(5,40,5,5);
		configPanel.add(confSintacticaBoton,c);
		// Valores para configPanel
		c.insets = new Insets(0,0,0,0);
		c.gridx = 0;
		c.gridy = 1;
		frame.add(configPanel,c);
		// Valores para la etiqueta archivosLabel
		extensionesPanel.add(archivosLabel);
		// Valores para el campo de texto archivosTextField
		extensionesPanel.add(archivosTextField);
		// Valores para extensionesPanel
		c.gridx = 0;
		c.gridy = 2;
		frame.add(extensionesPanel,c);
		// Valores para el JRadioButton compiladoRadio
		tipoPanel.add(compiladoRadio);
		// Valores para el JRadioButton interpretadoRadio
		tipoPanel.add(interpretadoRadio);
		// Valores para tipoPanel
		c.gridx = 0;
		c.gridy = 3;
		frame.add(tipoPanel,c);
		// Valores para el boton aceptarBoton
		c.insets = new Insets(5,5,5,5);
		c.gridx = 0;
		c.gridy = 0;
		botonesPanel.add(aceptarBoton,c);
		// Valores para el boton cancelarBoton
		c.gridx = 1;
		c.gridy = 0;
		c.insets = new Insets(5,50,5,5);
		botonesPanel.add(cancelarBoton,c);
		// Valores para botonesPanel
		c.insets = new Insets(0,0,0,0);
		c.gridx = 0;
		c.gridy = 4;
		frame.add(botonesPanel,c);
		frame.setVisible(true);
		frame.setResizable(false);
		frame.pack();
		frame.setLocationRelativeTo(null);
		logger.info(labels.getString("s373"));
	}

}
