package gui.parametrizacion;

import es.texto.Fichero;
import gui.Ventana;
import idioma.Idioma;
import java.awt.Checkbox;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import java.util.StringTokenizer;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import operaciones.log.Log;
import org.apache.log4j.Logger;
import principal.almacenPropiedades;

public class ConfigurarConsolaGUI {

	private JFrame frame;
	
	private JPanel panel;
	
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();
	
	public ConfigurarConsolaGUI() {
		
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		
		
		logger.info(labels.getString("s331"));
		frame = new JFrame();
		panel = new JPanel();
		panel.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		frame.setSize(new Dimension(500, 250));
		frame.setTitle(labels.getString("s334"));
		// Creacion de todas las componentes de la ventana
		
		final JLabel pathEjecutable = new JLabel(labels.getString("s337"), JLabel.LEFT);
		final JTextField pathEjecutableField = new JTextField();
		pathEjecutable.setEnabled(false);
		pathEjecutableField.setEnabled(false);
		final Checkbox manualPath = new Checkbox ();		
		JLabel ejecutable = new JLabel(labels.getString("s338"), JLabel.LEFT);
		final JTextField ejecutableField = new JTextField();
		JLabel comandoSalida = new JLabel(labels.getString("s339"), JLabel.LEFT);
		final JTextField comandoSalidaField = new JTextField();
		JLabel echoCommand = new JLabel(labels.getString("s340"), JLabel.LEFT);
		final Checkbox  echoCommandField = new Checkbox ();
		try {
			pathEjecutableField.setText(almacenPropiedades.getPropiedad("pathEjecutable"));
			ejecutableField.setText(almacenPropiedades.getPropiedad("ejecutable"));
			comandoSalidaField.setText(almacenPropiedades.getPropiedad("exitCommand"));
			echoCommandField.setState(Boolean.parseBoolean(almacenPropiedades.getPropiedad("echoCommand")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		JButton examinarBoton = new JButton(labels.getString("s142"));
		examinarBoton.setToolTipText(labels.getString("s301"));
		examinarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f = new Fichero();
				String ruta = f.leer();
				ejecutableField.setText(ruta);
			}
		});
		final JButton examinarBoton2 = new JButton(labels.getString("s142"));
		examinarBoton2.setToolTipText(labels.getString("s301"));
		examinarBoton2.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				//Fichero f = new Fichero();
				//String ruta = f.leer();
				String ruta = "";
				/*int index = ruta.lastIndexOf("\\");
				ruta = ruta.substring(0,index + 1);
				pathEjecutableField.setText(ruta);*/
				JFileChooser fc = new JFileChooser();
				fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				int option = fc.showOpenDialog(null);
				if(option == JFileChooser.APPROVE_OPTION) {
					ruta = fc.getSelectedFile().getAbsolutePath();
				}
				pathEjecutableField.setText(ruta);
				
			}
		});
		examinarBoton2.setEnabled(false);
		JLabel manualPathLabel = new JLabel(labels.getString("s350"), JLabel.LEFT);
		manualPath.addItemListener(new ItemListener(){
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					pathEjecutable.setEnabled(true);
					pathEjecutableField.setEnabled(true);
					examinarBoton2.setEnabled(true);
				} else {
					pathEjecutable.setEnabled(false);
					pathEjecutableField.setEnabled(false);
					examinarBoton2.setEnabled(false);
				}		
			}
		});		
		JButton aplicar = new JButton(labels.getString("s335"));
		aplicar.setVerticalTextPosition(AbstractButton.CENTER);
		aplicar.setHorizontalTextPosition(AbstractButton.LEADING);
		aplicar.setMnemonic(KeyEvent.VK_A);
		aplicar.setToolTipText(labels.getString("s336"));
		// Listener para el boton Aplicar
		aplicar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					Ventana.getInstance().getnuevaSalida().ejecutaComandoSalida();
					if (pathEjecutableField.isEnabled()){
						almacenPropiedades.setPropiedad("pathEjecutable",pathEjecutableField.getText());
						Ventana.getInstance().getProyecto().setShellDir(pathEjecutableField.getText());
					}
					else{
						String pathCalculado = "";
						String saux = ejecutableField.getText();
						StringTokenizer st = new StringTokenizer(saux,"\\"); 
						int limite = st.countTokens();
						for (int i = 0; i < limite -1;i++ ) { 
								pathCalculado = pathCalculado + st.nextToken() + "\\";
						} 
						almacenPropiedades.setPropiedad("pathEjecutable",pathCalculado);
						Ventana.getInstance().getProyecto().setShellDir(pathCalculado);
					}
					almacenPropiedades.setPropiedad("ejecutable",ejecutableField.getText());
					almacenPropiedades.setPropiedad("echoCommand",Boolean.toString(echoCommandField.getState()));
					almacenPropiedades.setPropiedad("exitCommand",comandoSalidaField.getText());
					Ventana.getInstance().getnuevaSalida().resetSalida();
					//mig
					Ventana.getInstance().getProyecto().setShellPath(ejecutableField.getText());
					//System.out.println(ejecutableField.getText());
					Ventana.getInstance().getProyecto().setEchoCommand(Boolean.toString(echoCommandField.getState()));
					Ventana.getInstance().getProyecto().setExitCommand(comandoSalidaField.getText());
					String prj=null;
					try {
						prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					if (!(prj.equals(".//configuration/Default.acidePrj") && Ventana.getInstance().getProyecto().getnombreProy().equals(""))){
						Ventana.getInstance().getProyecto().setModified(true);
					}
					
					if(Ventana.getInstance().getProyGUI() != null) Ventana.getInstance().getProyGUI().setB2(true);
					
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				frame.dispose();
			}
		});
		
		ActionListener l = new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				frame.dispose();
			}
		};

		aplicar.registerKeyboardAction(l, "EscapeKey", 
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 
									   0 , true), 
				JComponent.WHEN_IN_FOCUSED_WINDOW);
		
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(5, 5, 5, 5);
		panel.add(ejecutable, c);
		c.gridx = 1;
		c.ipadx = 100;
		panel.add(ejecutableField, c);
		
		c.gridx = 2;
		c.ipadx = 0;
		panel.add(examinarBoton,c);
		
		c.ipadx = 0;
		c.insets = new Insets(10, 5, 5, 5);
		c.gridx = 0;
		c.gridy = 1;
		panel.add(manualPathLabel, c); 
		c.gridx = 1;
		c.ipadx = 100;
		panel.add(manualPath, c);
		
		
		c.gridx = 0;
		c.gridy = 2;
		panel.add(pathEjecutable, c); 
		c.gridx = 1;
		c.ipadx = 100;
		panel.add(pathEjecutableField, c); 
		
		c.gridx = 2;
		c.ipadx = 0;
		panel.add(examinarBoton2,c);
		
		c.gridx = 0;
		c.gridy = 3;
		panel.add(comandoSalida, c);
		c.gridx = 1;
		c.ipadx = 100;
		panel.add(comandoSalidaField, c);
		
		c.gridx = 0;
		c.gridy = 4;
		panel.add(echoCommand, c);
		c.gridx = 1;
		c.ipadx = 100;
		panel.add(echoCommandField, c);
		
		// Valores para el boton Aplicar
		c.ipadx = 0;
		c.gridx = 0;
		c.gridy = 5;
		panel.add(aplicar, c);
		
		// Se añade el panel al frame y se muestra
		frame.add(panel);
		frame.setVisible(true);
		frame.setResizable(false);
		frame.pack();
		frame.setLocationRelativeTo(null);
	}

}
