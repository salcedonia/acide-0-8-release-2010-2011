package gui.parametrizacion;

import es.texto.Fichero;
import gui.Ventana;
import gui.salida.Salida;
import idioma.Idioma;
import java.awt.Checkbox;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
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
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import operaciones.log.Log;
import operaciones.salida.ProcessThread;

import org.apache.log4j.Logger;
import principal.almacenPropiedades;

public class ComandoExternoGUI {

	private JFrame frame;
	
	private JPanel panel;
	
	private Salida s;
	
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();
	
	public ComandoExternoGUI() {
		
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		
		
		logger.info(labels.getString("s330"));
		frame = new JFrame();
		panel = new JPanel();
		panel.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		//frame.setSize(new Dimension(700, 300));
		frame.setTitle(labels.getString("s342"));
		// Creacion de todas las componentes de la ventana
		
		final JLabel pathEjecutable = new JLabel(labels.getString("s345"), JLabel.CENTER);
		final JTextField pathEjecutableField = new JTextField();
		JLabel ejecutable = new JLabel(labels.getString("s346"), JLabel.CENTER);
		final JTextField ejecutableField = new JTextField();
		JLabel comandoSalida = new JLabel(labels.getString("s347"), JLabel.CENTER);
		final JTextField comandoSalidaField = new JTextField();
		JLabel echoCommand = new JLabel(labels.getString("s348"), JLabel.CENTER);
		final Checkbox  echoCommandField = new Checkbox ();
		pathEjecutable.setEnabled(false);
		pathEjecutableField.setEnabled(false);
		final Checkbox manualPath = new Checkbox ();
		JLabel manualPathLabel = new JLabel(labels.getString("s350"), JLabel.LEFT);
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
				Fichero f = new Fichero();
				String ruta = f.leer();
				int index = ruta.lastIndexOf("\\");
				ruta = ruta.substring(0,index + 1);
				pathEjecutableField.setText(ruta);
			}
		});
		examinarBoton2.setEnabled(false);
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
		
		JLabel command = new JLabel(labels.getString("s349"), JLabel.CENTER);
		final JTextArea  commandField = new JTextArea ();
		commandField.setSize(50,50);
		JScrollPane p = new JScrollPane(commandField);
		p.setPreferredSize(new Dimension(50,50));
		
		try {
			pathEjecutableField.setText(almacenPropiedades.getPropiedad("pathEjecutable"));
			ejecutableField.setText(almacenPropiedades.getPropiedad("ejecutable"));
			comandoSalidaField.setText(almacenPropiedades.getPropiedad("exitCommand"));
			echoCommandField.setState(Boolean.parseBoolean(almacenPropiedades.getPropiedad("echoCommand")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		JButton aplicar = new JButton(labels.getString("s343"));
		aplicar.setVerticalTextPosition(AbstractButton.CENTER);
		aplicar.setHorizontalTextPosition(AbstractButton.LEADING);
		aplicar.setMnemonic(KeyEvent.VK_A);
		aplicar.setToolTipText(labels.getString("s344"));
		// Listener para el boton Aplicar
		aplicar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					String pathEjecutable = "";
					if (pathEjecutableField.isEnabled()){
						pathEjecutable = pathEjecutableField.getText();
					}
					else{
						String pathCalculado = "";
						String saux = ejecutableField.getText();
						StringTokenizer st = new StringTokenizer(saux,"\\"); 
						int limite = st.countTokens();
						for (int i = 0; i < limite -1;i++ ) { 
								pathCalculado = pathCalculado + st.nextToken() + "\\";
						} 
						pathEjecutable = pathCalculado;
					}
					
					
					ProcessThread p = new ProcessThread();
					JFrame resultado = new JFrame();
					s = new Salida(false);
					String com = commandField.getText();
					String c1 = ejecutableField.getText();
					if (Ventana.getInstance().getCreadorEditor().dameNumEditores()>0){
						com = com.replace("$activeFile$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getPath());
						c1 = c1.replace("$activeFile$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getPath());
						com = com.replace("$activeFilePath$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFilePath());
						c1 = c1.replace("$activeFilePath$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFilePath());
						com = com.replace("$activeFileExt$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileExt());
						c1 = c1.replace("$activeFileExt$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileExt());				
						com = com.replace("$activeFileName$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileName());
						c1 = c1.replace("$activeFileName$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileName());				
					}
					//mig
					Ventana v=Ventana.getInstance();
					String prj=null;
					try {
						prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
						//no hay proyecto
						if(Ventana.getInstance().getCreadorEditor().EditorPrincipal()!=null){
						com = com.replace("$mainFile$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getPath());
						c1 = c1.replace("$mainFile$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getPath());
						com = com.replace("$mainFilePath$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFilePath());
						c1 = c1.replace("$mainFilePath$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFilePath());
						com = com.replace("$mainFileExt$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileExt());
						c1 = c1.replace("$mainFileExt$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileExt());
						com = com.replace("$mainFileName$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileName());
						c1 = c1.replace("$mainFileName$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileName());
						}
						else{
							//error no hay main file
						}
					}
					else{//hay proyecto
						int j=-1;
						for (int i=0;i<v.getProyecto().dameNumFich();i++){
							if(v.getProyecto().getfich(i).isMainFile()) j=i;
						}
						if(j!=-1){
						com = com.replace("$mainFile$",v.getProyecto().getfich(j).getPath());
						c1 = c1.replace("$mainFile$",v.getProyecto().getfich(j).getPath());
						com = com.replace("$mainFilePath$",v.getProyecto().getfich(j).getFilePath());
						c1 = c1.replace("$mainFilePath$",v.getProyecto().getfich(j).getFilePath());
						com = com.replace("$mainFileExt$",v.getProyecto().getfich(j).getFileExt());
						c1 = c1.replace("$mainFileExt$",v.getProyecto().getfich(j).getFileExt());
						com = com.replace("$mainFileName$",v.getProyecto().getfich(j).getFileName());
						c1 = c1.replace("$mainFileName$",v.getProyecto().getfich(j).getFileName());
						}
						else{
							//error no hay main file
						}
					}
					
					System.out.println(c1+" "+pathEjecutable+" "+com);
					
					p.executeCommand(c1,pathEjecutable,com,
							comandoSalidaField.getText(),s);
					
					resultado.add(s);
					resultado.setSize(new Dimension(300,400));
					//resultado.pack();
					resultado.setVisible(true);
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
		c.ipadx = 50;
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
		//c.ipadx = 100;
		panel.add(manualPath, c);
		
		
		c.gridx = 0;
		c.gridy = 2;
		panel.add(pathEjecutable, c); 
		c.gridx = 1;
		//c.ipadx = 100;
		panel.add(pathEjecutableField, c); 
		
		c.gridx = 2;
		c.ipadx = 0;
		panel.add(examinarBoton2,c);
		
		c.gridx = 0;
		c.gridy = 3;
		panel.add(comandoSalida, c);
		c.gridx = 1;
		//c.ipadx = 100;
		panel.add(comandoSalidaField, c);
		
		c.gridx = 0;
		c.gridy = 4;
		panel.add(echoCommand, c);
		c.gridx = 1;
		//c.ipadx = 100;
		panel.add(echoCommandField, c);
		
		c.gridx = 0;
		c.gridy = 5;
		panel.add(command, c);
		c.gridx = 1;
		c.gridheight = 2;
		c.ipady = 60;
		panel.add(p, c);
		
		
		// Valores para el boton Aplicar
		c.ipadx = 0;
		c.ipady = 0;
		c.gridx = 0;
		c.gridy = 7;
		c.gridheight = 1;
		panel.add(aplicar, c);
		
		// Se añade el panel al frame y se muestra
		frame.add(panel);
		frame.setResizable(false);
		frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = frame.getSize();
		frame.setLocation((screenSize.width - frameSize.width) / 2,
						  (screenSize.height - frameSize.height) / 2);		
		//frame.setLocationRelativeTo(null);		
		frame.setVisible(true);
		frame.setLocationRelativeTo(null);
	}
	
	public Salida getSalida(){
		return s;
	}

}