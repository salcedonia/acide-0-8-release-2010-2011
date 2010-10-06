package gui.parametrizacion;

import idioma.Idioma;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import operaciones.log.Log;

import org.apache.log4j.Logger;

import es.texto.Fichero;

import principal.almacenPropiedades;

public class SetPathsGUI {
	
	private JFrame frame;
	
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();
	
	public SetPathsGUI() {
		// TODO Añadir tooltips a los componentes de la ventana
		// TODO Cuando se deseleccionen los CheckBox dejar el programa sin la ruta
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		logger.info(labels.getString("s914"));
		frame = new JFrame();
		frame.setTitle(labels.getString("s913"));
		frame.setLayout(new GridBagLayout());
		JPanel javaPanel = new JPanel();
		javaPanel.setLayout(new GridBagLayout());
		javaPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s915")));
		JPanel javacPanel = new JPanel();
		javacPanel.setLayout(new GridBagLayout());
		javacPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s916")));
		JPanel jarPanel = new JPanel();
		jarPanel.setLayout(new GridBagLayout());
		jarPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s917")));
		JPanel buttonsPanel = new JPanel();
		buttonsPanel.setLayout(new GridLayout());
		GridBagConstraints c = new GridBagConstraints();
		// Elements of each panel
		String javaPath = null;
		String javacPath = null;
		String jarPath = null;
		try {
			javaPath = almacenPropiedades.getPropiedad("javaPath");
			javacPath = almacenPropiedades.getPropiedad("javacPath");
			jarPath = almacenPropiedades.getPropiedad("jarPath");
		}
		catch (Exception e) {
			logger.error(e.getMessage());
		}
		final String java = javaPath;
		final String javac = javacPath;
		final String jar = jarPath;
		final JTextField javaTField = new JTextField();
		final JTextField javacTField = new JTextField();
		final JTextField jarTField = new JTextField();
		final JCheckBox javaCB = new JCheckBox();		
		final JCheckBox javacCB = new JCheckBox();		
		final JCheckBox jarCB = new JCheckBox();		
		JButton okButton = new JButton(labels.getString("s918"));
		JButton cancelButton = new JButton(labels.getString("s919"));
		final JButton javaBrowse = new JButton(labels.getString("s920"));
		final JButton javacBrowse = new JButton(labels.getString("s921"));
		final JButton jarBrowse = new JButton(labels.getString("s922"));
		// Listeners
		javaCB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (javaCB.isSelected()) {
					if (java.equals("null")) javaTField.setText("");
					else javaTField.setText(java);
					javaTField.setEnabled(true);
					javaBrowse.setEnabled(true);
				}
				else {
					javaTField.setText("");
					javaTField.setEnabled(false);
					javaBrowse.setEnabled(false);
				}
			}
		});
		javacCB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (javacCB.isSelected()) {
					if (javac.equals("null")) javacTField.setText("");
					else javacTField.setText(javac);
					javacTField.setEnabled(true);
					javacBrowse.setEnabled(true);
				}
				else {
					javacTField.setText("");
					javacTField.setEnabled(false);
					javacBrowse.setEnabled(false);
				}
			}
		});
		jarCB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (jarCB.isSelected()) {
					if (jar.equals("null"))	jarTField.setText("");
					else jarTField.setText(jar);
					jarTField.setEnabled(true);
					jarBrowse.setEnabled(true);
				}
				else {
					jarTField.setText("");
					jarTField.setEnabled(false);
					jarBrowse.setEnabled(false);
				}
			}
		});
		javaBrowse.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f = new Fichero();
				String path = f.leer();
				javaTField.setText(path);
			}
		});
		javacBrowse.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f = new Fichero();
				String path = f.leer();
				javacTField.setText(path);
			}
		});
		jarBrowse.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f = new Fichero();
				String path = f.leer();
				jarTField.setText(path);
			}
		});
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String java = javaTField.getText();
				String javac = javacTField.getText();
				String jar = jarTField.getText();
				if (java.equals("")) almacenPropiedades.setPropiedad("javaPath","null");
				else almacenPropiedades.setPropiedad("javaPath",java);
				if (javac.equals("")) almacenPropiedades.setPropiedad("javacPath","null");
				else almacenPropiedades.setPropiedad("javacPath",javac);
				if (jar.equals("")) almacenPropiedades.setPropiedad("jarPath","null");
				else almacenPropiedades.setPropiedad("jarPath",jar);
				logger.info(labels.getString("s925"));
				frame.dispose();
			}
		});
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				frame.dispose();
				logger.info(labels.getString("s924"));
			}
		});
		ActionListener escPressed = new ActionListener()	{
			public void actionPerformed(ActionEvent e) {
				frame.dispose();
			}
		};
		cancelButton.registerKeyboardAction(escPressed,"EscapeKey",KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE,0,true),JComponent.WHEN_IN_FOCUSED_WINDOW);
		// Add components to frame
		c.fill = GridBagConstraints.BOTH;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(5,5,5,5);
		javaPanel.add(javaCB,c);
		c.gridx = 1;
		c.ipadx = 400;
		javaPanel.add(javaTField,c);
		c.ipadx = 0;
		c.gridx = 2;
		javaPanel.add(javaBrowse,c);
		c.gridx = 0;
		javacPanel.add(javacCB,c);
		c.gridx = 1;
		c.ipadx = 400;
		javacPanel.add(javacTField,c);
		c.gridx = 2;
		c.ipadx = 0;
		javacPanel.add(javacBrowse,c);
		c.gridx = 0;
		jarPanel.add(jarCB,c);
		c.gridx = 1;
		c.ipadx = 400;
		jarPanel.add(jarTField,c);
		c.gridx = 2;
		c.ipadx = 0;
		jarPanel.add(jarBrowse,c);
		buttonsPanel.add(okButton);
		buttonsPanel.add(cancelButton);
		c.gridx = 0;
		frame.add(javaPanel,c);
		c.gridy = 1;
		frame.add(javacPanel,c);
		c.gridy = 2;
		frame.add(jarPanel,c);
		c.gridy = 3;
		c.fill = GridBagConstraints.NONE;
		frame.add(buttonsPanel,c);
		frame.pack();
		if (javaPath.equals("null")) {
			javaCB.setSelected(false);
			javaTField.setText("");
			javaTField.setEnabled(false);
			javaBrowse.setEnabled(false);
		}
		else {
			javaCB.setSelected(true);
			javaTField.setText(javaPath);
			javaTField.setEnabled(true);
			javaBrowse.setEnabled(true);
		}
		if (javacPath.equals("null")) {
			javacCB.setSelected(false);
			javacTField.setText("");
			javacTField.setEnabled(false);
			javacBrowse.setEnabled(false);
		}
		else {
			javacCB.setSelected(true);
			javacTField.setText(javacPath);
			javacTField.setEnabled(true);
			javacBrowse.setEnabled(true);
		}
		if (jarPath.equals("null")) {
			jarCB.setSelected(false);
			jarTField.setText("");
			jarTField.setEnabled(false);
			jarBrowse.setEnabled(false);
		}
		else {
			jarCB.setSelected(true);
			jarTField.setText(jarPath);
			jarTField.setEnabled(true);
			jarBrowse.setEnabled(true);
		}
		frame.setLocationRelativeTo(null);
		frame.setResizable(false);
		frame.setVisible(true);
		logger.info(labels.getString("s923"));
	}

}
