package gui.parametrizacion;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ResourceBundle;

import es.texto.ExtFilter;
import es.texto.Fichero;
import gui.Ventana;
import gui.salida.Salida;
import idioma.Idioma;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import operaciones.fabrica.FactoriaGUI;
import operaciones.genericas.Windows;
import operaciones.log.Log;
import operaciones.salida.ProcessThread;

import org.apache.log4j.Logger;

import principal.almacenPropiedades;

/**Clase que implementa el interfaz del menu ejecucion
 * 
 *
 */
public class ExecutionGUI extends JFrame {
	
	private JFrame frame;
	
	/**Panel principal
	 * 
	 */
	private JPanel mainPanel;
	
	private JLabel executionText;
	
	private JTextField executationField;

	private JButton examinePath; 
	
	private JLabel argumentsText;
	
	private JTextField argumentsField;
	
	private JButton runButton;
	
	private JButton cancelButton;
	
	private JPanel buttonPanel;

	private ResourceBundle labels;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	private JTextField executionField;
	
	public ExecutionGUI(){
	   Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		labels = i.getLabels();
	 frame=new JFrame(labels.getString("s639"));
	 frame.setLayout(new GridBagLayout());
	 GridBagConstraints c = new GridBagConstraints();
     c.fill= GridBagConstraints.BOTH;
	 mainPanel= new JPanel();
	 mainPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s640")));
	 mainPanel.setLayout(new GridBagLayout());
	 executionText=new JLabel(labels.getString("s606"));
	 executionField = new JTextField();
	 executionField.setToolTipText(labels.getString("s638"));
	 examinePath=new JButton(labels.getString("s596"));
	 examinePath.setHorizontalAlignment(JButton.CENTER);
	 examinePath.setToolTipText(labels.getString("s641"));
	 examinePath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f =new Fichero();
				 // Selecciona la extension del compilador
				String[] ExtAcide = new String[] { "exe" };
				f.getFileChooser().addChoosableFileFilter(
						new ExtFilter(ExtAcide, "Executable source (*.exe)"));
				String path = f.leer();
				executionField.setText(path);
			}
		});
	 argumentsText= new JLabel(labels.getString("s609"));
	 argumentsField= new JTextField();
	 argumentsField.setToolTipText(labels.getString("s610"));
	 buttonPanel=new JPanel();
	 buttonPanel.setLayout(new GridBagLayout());
	 runButton=new JButton(labels.getString("s154"));
	 runButton.setHorizontalAlignment(JButton.CENTER);
	 runButton.setToolTipText(labels.getString("s154"));
	 
	 runButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v=Ventana.getInstance();
				Process obj; 
				try {
					//mig
					String c1 = executionField.getText();
					String c2 = argumentsField.getText();
					
					if (Ventana.getInstance().getCreadorEditor().dameNumEditores()>0){
						c1 = c1.replace("$activeFile$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getPath());
						c1 = c1.replace("$activeFilePath$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFilePath());
						c1 = c1.replace("$activeFileExt$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileExt());
						c1 = c1.replace("$activeFileName$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileName());

						c2 = c2.replace("$activeFile$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getPath());
						c2 = c2.replace("$activeFilePath$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFilePath());
						c2 = c2.replace("$activeFileExt$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileExt());
						c2 = c2.replace("$activeFileName$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileName());
					}
					
					//mig
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
						c2 = c2.replace("$mainFile$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getPath());
						c1 = c1.replace("$mainFile$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getPath());
						c2 = c2.replace("$mainFilePath$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFilePath());
						c1 = c1.replace("$mainFilePath$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFilePath());
						c2 = c2.replace("$mainFileExt$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileExt());
						c1 = c1.replace("$mainFileExt$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileExt());
						c2 = c2.replace("$mainFileName$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileName());
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
						c2 = c2.replace("$mainFile$",v.getProyecto().getfich(j).getPath());
						c1 = c1.replace("$mainFile$",v.getProyecto().getfich(j).getPath());
						c2 = c2.replace("$mainFilePath$",v.getProyecto().getfich(j).getFilePath());
						c1 = c1.replace("$mainFilePath$",v.getProyecto().getfich(j).getFilePath());
						c2 = c2.replace("$mainFileExt$",v.getProyecto().getfich(j).getFileExt());
						c1 = c1.replace("$mainFileExt$",v.getProyecto().getfich(j).getFileExt());
						c2 = c2.replace("$mainFileName$",v.getProyecto().getfich(j).getFileName());
						c1 = c1.replace("$mainFileName$",v.getProyecto().getfich(j).getFileName());
						}
						else{
							//error no hay main file
						}
					}
					
					//Runtime.getRuntime().exec(executionField.getText()+" "+argumentsField.getText());
					System.out.println(c1+" "+c2);
					Runtime.getRuntime().exec(c1+" "+c2);
					
					//Runtime.getRuntime().exec("\"e:\\MiKTeX 2.7\\miktex\\bin\\latex.exe\" e:\\hlocal\\mig\\mig2.tex"+"\n");
					//Runtime.getRuntime().exec("cmd");
				}
			      catch(IOException ex)
			      {
			        
			        JOptionPane.showMessageDialog(null,ex.getMessage());
			      }
				v.setEnabled(true);
				frame.dispose();
			}
		});
	 
	 cancelButton=new JButton(labels.getString("s162"));
	 cancelButton.setHorizontalAlignment(JButton.CENTER);
	 cancelButton.setToolTipText(labels.getString("s162"));
	 cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v=Ventana.getInstance();
				v.setEnabled(true);
				frame.dispose();
			}
		});
	 runButton.addKeyListener(new Keyboard());
	 cancelButton.addKeyListener(new Keyboard());
	 executionField.addKeyListener(new Keyboard());
	 argumentsField.addKeyListener(new Keyboard());
	 examinePath.addKeyListener(new Keyboard());
	 c.gridx=0;
	 c.gridy=0;
	 mainPanel.add(executionText,c);
	 
	 c.gridx=1;
	 c.ipadx=200;
	 mainPanel.add(executionField,c);
	 
	 c.gridx=2;
	 c.ipadx=0;
	 mainPanel.add(examinePath,c);
	 
	 c.gridx=0;
	 c.gridy=1;
	 mainPanel.add(argumentsText,c);
	 c.gridx=1;
	 c.ipadx=150;
	 mainPanel.add(argumentsField,c);
	 c.ipadx=0;
	 c.gridx=0;
	 c.gridy=0;
	 
	 buttonPanel.add(runButton,c);
	 c.gridx=1;
	 buttonPanel.add(cancelButton,c);
	 c.gridx=0;
	 c.gridy=0;
	 frame.add(mainPanel,c);
	 c.gridx=0;
	 c.gridy=1;
	 frame.add(buttonPanel,c);
	 frame.setResizable(false);
	 frame.pack();
	 frame.setVisible(true);
	 frame.setLocationRelativeTo(null);
	 Windows window=new Windows();
     frame.addWindowListener(window);
     Ventana v=Ventana.getInstance();
     v.setEnabled(false);
	
	}
	class Keyboard extends KeyAdapter {
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
				Ventana v = Ventana.getInstance();
				v.setEnabled(true);
				frame.dispose();
				v.setAlwaysOnTop(true);
				v.setAlwaysOnTop(false);
			}
		}
	}

}

