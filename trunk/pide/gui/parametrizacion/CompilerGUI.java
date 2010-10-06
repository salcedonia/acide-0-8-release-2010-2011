package gui.parametrizacion;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ResourceBundle;

import es.texto.ExtFilter;
import es.texto.Fichero;
import gui.Ventana;
import idioma.Idioma;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import operaciones.genericas.Windows;
import operaciones.log.Log;

import org.apache.log4j.Logger;

import principal.almacenPropiedades;

/** class that implements compiler options
 * 
 *
 */
public class CompilerGUI extends JFrame{

	private JFrame frame;
	
	private JTextField nameExecField;

    private JButton examinePath;
    
	private JTextField argField;
    
    private JLabel nameExecLabel;
    
    private JLabel argLabel;
    
    private JPanel mainPanel1;
    
    private JPanel mainPanel2;
    
    private JPanel buttonPanel;
    
    private JCheckBox checkCompiler;
    
    private JLabel checkLabel;
    
    private JLabel separatorLabel;
    
    private JTextField separatorField;
    
    private JLabel extensionLabel;
    
    private JTextField extensionField;
    
    private JButton ok;
    
    private JButton cancel;
    
    private Logger logger = Log.getLog();
	
	private ResourceBundle labels; 
    
	
    public CompilerGUI(){
    	
    	
    Idioma i = Idioma.getInstance();
      try {
    	i.seleccionIdioma(Integer.parseInt(almacenPropiedades
    		.getPropiedad("idioma")));
    } catch (Exception e) {
    	e.printStackTrace();
    }
    labels = i.getLabels();
    Ventana v=Ventana.getInstance();
    v.setEnabled(false);
    v.getProyecto().setCheckCompiler(false);
    logger.info(labels.getString("s646"));
    frame = new JFrame();
	frame.setLayout(new GridBagLayout());
	frame.setTitle(labels.getString("s647"));
	frame.setLayout(new GridBagLayout());
	GridBagConstraints c = new GridBagConstraints();
    mainPanel1=new JPanel();
    mainPanel1.setBorder(BorderFactory.createTitledBorder(labels.getString("s644")));
	mainPanel1.setLayout(new GridBagLayout());
    mainPanel2=new JPanel();
    mainPanel2.setBorder(BorderFactory.createTitledBorder(labels.getString("s645")));
	mainPanel2.setLayout(new GridBagLayout());
	buttonPanel=new JPanel();
    buttonPanel.setLayout(new GridBagLayout());
    nameExecField=new JTextField();
    nameExecField.setToolTipText(labels.getString("s607"));
    argField=new JTextField();
    argField.setToolTipText(labels.getString("s610"));
    nameExecLabel=new JLabel(labels.getString("s606"));
    argLabel=new JLabel(labels.getString("s609"));
    checkLabel=new JLabel(labels.getString("s650"));
    separatorLabel=new JLabel(labels.getString("s649"));
    extensionLabel=new JLabel(labels.getString("s653"));
    extensionField=new JTextField(7);
    extensionField.setToolTipText(labels.getString("s652"));
    extensionField.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			Ventana v=Ventana.getInstance();
			v.getProyecto().setExtensionFile(extensionField.getText());
			}
			});
    checkCompiler=new JCheckBox();
    checkCompiler.setToolTipText(labels.getString("s650"));
    checkCompiler.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			Ventana v=Ventana.getInstance();
			if (checkCompiler.isSelected()){
				v.getProyecto().setCheckCompiler(true);
			    extensionField.setText(""); 
				extensionField.setEnabled(false);
			    separatorField.setEnabled(true);
			}
			else{ 
				v.getProyecto().setCheckCompiler(false);
			    separatorField.setText("");
			    separatorField.setEnabled(false);
			    extensionField.setEnabled(true);
			}
			}
	});
    separatorField=new JTextField(1);
    separatorField.setToolTipText(labels.getString("s651"));
    separatorField.setEnabled(false);
    separatorField.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		Ventana v=Ventana.getInstance();
		v.getProyecto().setSeparatorFile(separatorField.getText());
		}
		});
    examinePath=new JButton(labels.getString("s596"));
	examinePath.setHorizontalAlignment(JButton.CENTER);
	examinePath.setToolTipText(labels.getString("s641"));
	examinePath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f =new Fichero();
				 // Selecciona la extension del compilador
				String[] ExtAcide = new String[] { "exe" };
				f.getFileChooser().addChoosableFileFilter(
						new ExtFilter(ExtAcide, "Compiler source (*.exe)"));
				String path = f.leer();
				nameExecField.setText(path);
			}
		});
    ok=new JButton(labels.getString("s154"));
    ok.setHorizontalAlignment(JButton.CENTER);
	ok.setToolTipText(labels.getString("s154"));
	 ok.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v=Ventana.getInstance();
		        v.getProyecto().setpathEjecu(nameExecField.getText());		
		        v.getProyecto().setarg(argField.getText());	
				v.getProyecto().setExtensionFile(extensionField.getText());
				v.getProyecto().setSeparatorFile(separatorField.getText());
				v.setEnabled(true);
				if(v.getProyGUI() != null) v.getProyGUI().setB1(true);
				frame.dispose();
			}
		});
	 cancel=new JButton(labels.getString("s162"));
	 cancel.setHorizontalAlignment(JButton.CENTER);
	 cancel.setToolTipText(labels.getString("s162"));
	 cancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v=Ventana.getInstance();
				v.setEnabled(true);
				frame.dispose();
			}
		});
    frame.addKeyListener(new Keyboard());
    c.fill=GridBagConstraints.BOTH;
    c.gridx=0;
	 c.gridy=0;
	 mainPanel1.add(nameExecLabel,c);
	 
	 c.gridx=1;
	 c.ipadx=200;
	 mainPanel1.add(nameExecField,c);
	 
	 c.gridx=2;
	 c.ipadx=0;
	 mainPanel1.add(examinePath,c);
	 
	 c.gridx=0;
	 c.gridy=1;
	 mainPanel1.add(argLabel,c);
	 c.gridx=1;
	 c.ipadx=150;
	 mainPanel1.add(argField,c);
	 c.ipadx=0;
	 c.gridx=0;
	 c.gridy=0;
	 
	 buttonPanel.add(ok,c);
	 c.gridx=1;
	 buttonPanel.add(cancel,c);
	 c.gridx=0;
	 c.gridy=0;
	 frame.add(mainPanel1,c);
	 c.gridx=0;
	 c.gridy=0;
	 mainPanel2.add(checkLabel,c);
	 c.gridx=1;
	 mainPanel2.add(checkCompiler,c);
	 c.gridx=0;
	 c.gridy=1;
	 mainPanel2.add(separatorLabel,c);
	 c.gridx=1;
	 c.gridy=1;
	 mainPanel2.add(separatorField,c);
	 c.gridx=2;
	 c.gridy=1;
     mainPanel2.add(extensionLabel,c);
	 c.gridx=3;
	 c.gridy=1;
	 mainPanel2.add(extensionField,c);
	 c.gridx=0;
	 c.gridy=1;
	 frame.add(mainPanel2,c);
	 c.gridx=0;
	 c.gridy=2;
	 frame.add(buttonPanel,c);
	 frame.setResizable(false);
	 frame.pack();
	 frame.setVisible(true);
	 frame.setLocationRelativeTo(null);
	 Windows window=new Windows();
     frame.addWindowListener(window);
     v.setEnabled(false);
	 //SAVE CONFIGURATION DAFAULT PROJECT
     v.closeDefaultPrj(); 
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
