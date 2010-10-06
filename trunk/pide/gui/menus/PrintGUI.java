package gui.menus;


import gui.Ventana;
import idioma.Idioma;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.print.PageFormat;

import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;

import javax.swing.JPanel;

import operaciones.genericas.PrinterManager;
import operaciones.genericas.Windows;

import principal.almacenPropiedades;



public class PrintGUI {

private JFrame frame;

private JButton print;

private JButton cancel;

private JButton configPage;

private JPanel panelButton;

private  JPanel generalPanel;

private JLabel numPagLabel;

private JCheckBox numPageCheck;

private JLabel date;

private JCheckBox dateCheck;

private PrinterManager pm;
private ResourceBundle labels;


/**
 * Atributo: Instancia PrintGUI
 * 
 */
private static PrintGUI instancia;

// Crea una unica instancia de PrintGUI
public static PrintGUI getInstance() {
	if (instancia == null)
		instancia = new PrintGUI();
	return instancia;
}


public PrintGUI() {
	 Idioma i = Idioma.getInstance();
	try {
		i.seleccionIdioma(Integer.parseInt(almacenPropiedades.getPropiedad("idioma")));
	} catch (Exception e) {
		e.printStackTrace();
	}
	Ventana v=Ventana.getInstance(); 
	 pm=new PrinterManager(v.getCreadorEditor().EditorSeleccionado().getEditor(),false,false);
	 numPageCheck=new JCheckBox();
	 numPageCheck.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if (numPageCheck.isSelected()){
				//System.out.println("seleccionado page");
					pm.setPag(true);
				}
					else 
				pm.setPag(false);
			 
			}
			
	 });
	 
	 
	labels = i.getLabels();  
    frame = new JFrame();
    //frame.setPreferredSize(new Dimension(300,250));
    frame.setTitle(labels.getString("s964"));
    frame.setLayout(new GridBagLayout());
    GridBagConstraints c = new GridBagConstraints();
    panelButton = new JPanel();
    panelButton.setLayout(new GridBagLayout());
    generalPanel = new JPanel();
	generalPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s965")));
	generalPanel.setLayout(new GridBagLayout());      
	numPagLabel = new JLabel(labels.getString("s962"));
	date = new JLabel(labels.getString("s963"));
	dateCheck = new JCheckBox();
	dateCheck.setEnabled(false);
	numPageCheck.setEnabled(false);
	dateCheck.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e) {
			if (dateCheck.isSelected()){
				//System.out.println("seleccionado date");
				pm.setDate(true);
			}
			else 
			pm.setDate(false);
		}	
 });
 
	
	print = new JButton(labels.getString("s624"));
	print.setHorizontalAlignment(JButton.CENTER);
	print.setToolTipText(labels.getString("s624"));
	print.setEnabled(false);
	print.addActionListener(new ActionListener(){
    	public void actionPerformed(ActionEvent e) {
		Ventana v=Ventana.getInstance();   
		pm.imprimir();
		frame.dispose();
		v.setEnabled(true);
		}
		});
	cancel = new JButton(labels.getString("s919"));
	cancel.setHorizontalAlignment(JButton.CENTER);
	cancel.setToolTipText(labels.getString("s919"));
	cancel.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e) {
			Ventana v=Ventana.getInstance();
		    v.setEnabled(true);
			frame.dispose();
			}
		});
	
	configPage = new JButton(labels.getString("s961"));
	configPage.setHorizontalAlignment(JButton.CENTER);
	configPage.setToolTipText(labels.getString("s961"));
	
	configPage.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e) {
		pm.configPage();	
		if (pm.getFormat()!=null){
			print.setEnabled(true);
			dateCheck.setEnabled(true);
			numPageCheck.setEnabled(true);
		}
		//frame.setVisible(true);
		//v.setAlwaysOnTop(true);
		
		}
		});
    c.fill=GridBagConstraints.BOTH;
	c.insets = new Insets(10, 10, 5, 5);
	c.gridx=0;
	c.gridy=0;
	generalPanel.add(configPage,c); 
	c.gridy=1;
	generalPanel.add(numPagLabel,c);
	c.gridx=1;
	generalPanel.add(numPageCheck,c);
	c.gridy=2;
	c.gridx=0;
	generalPanel.add(date,c);
	c.gridx=1;
	generalPanel.add(dateCheck,c);
	c.gridx=0;
	c.gridy=0;
	frame.add(generalPanel,c);
	
	panelButton.add(print, c);
    c.gridx=1;
	panelButton.add(cancel,c);
    c.gridx=0;
	c.gridy=1;
	frame.add(panelButton,c);
    frame.setLocationRelativeTo(null);
    frame.pack();
	frame.setVisible(true);
    frame.setResizable(false);
    configPage.addKeyListener(new Keyboard());
    print.addKeyListener(new Keyboard());
    cancel.addKeyListener(new Keyboard());
    v.setEnabled(false);
    Windows window=new Windows();
	frame.addWindowListener(window);
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


public JButton getConfigPage() {
	return configPage;
}


public void setConfigPage(JButton configPage) {
	this.configPage = configPage;
}


public JButton getPrint() {
	return print;
}


public void setPrint(JButton print) {
	this.print = print;
}


public JFrame getFrame() {
	return frame;
}


public void setFrame(JFrame frame) {
	this.frame = frame;
}	


}



