package gui.parametrizacion;

import gui.PleaseWaitWindow;
import gui.Ventana;
import gui.salida.Salida;
import idioma.Idioma;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;

import org.apache.log4j.Logger;

import es.bytes.ByteFile;
import es.texto.Fichero;
import es.texto.FiltroFicheros;

import operaciones.log.Log;
import operaciones.salida.ProcessThread;
import operaciones.sintacticas.GrammarGenerator;
import principal.almacenPropiedades;

/**
 * Clase que implementa la ventana que incluye los campos necesarios para
 * realizar la parametrización sintáctica
 */
public class SintacticaGUI extends JFrame {

	private static final long serialVersionUID = 1L;
	
	private static JFrame frame2;
	
	private static boolean changesSaved;
		
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private static Logger logger = Log.getLog();
		
	public SintacticaGUI(boolean modify) {
		changesSaved = true;
		if (modify) modifyGrammarGUI();
		else newGrammarGUI();
	}

	public void newGrammarGUI() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		GridBagConstraints c = new GridBagConstraints();		
		logger.info(labels.getString("s173"));
		frame2 = new JFrame();
		frame2.setLayout(new GridBagLayout());
		frame2.setTitle(labels.getString("s184"));
		JPanel categoriasPanel = new JPanel();
		categoriasPanel.setLayout(new GridBagLayout());
		categoriasPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s175")));
		JPanel reglasPanel = new JPanel();
		reglasPanel.setLayout(new GridBagLayout());
		reglasPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s176")));
		JPanel botones2Panel = new JPanel();
		botones2Panel.setLayout(new GridBagLayout());
		JPanel categoriesButtonsPanel = new JPanel();
		categoriesButtonsPanel.setLayout(new GridBagLayout());
		JPanel rulesButtonsPanel = new JPanel();
		rulesButtonsPanel.setLayout(new GridBagLayout());
		final JTextArea categoriasTextArea = new JTextArea();
		categoriasTextArea.setToolTipText(labels.getString("s179"));
		final JTextArea reglasTextArea = new JTextArea();
		reglasTextArea.setToolTipText(labels.getString("s180"));
		JScrollPane categoriasScroll = new JScrollPane(categoriasTextArea);
		JScrollPane reglasScroll = new JScrollPane(reglasTextArea);
		JButton aceptar2Boton = new JButton(labels.getString("s177"));
		aceptar2Boton.setToolTipText(labels.getString("s181"));
		JButton cancelar2Boton = new JButton(labels.getString("s178"));
		cancelar2Boton.setToolTipText(labels.getString("s182"));
		JButton loadCategoriesButton = new JButton(labels.getString("s192"));
		loadCategoriesButton.setToolTipText(labels.getString("s193"));
		JButton saveCategoriesButton = new JButton(labels.getString("s194"));
		saveCategoriesButton.setToolTipText(labels.getString("s195"));
		JButton loadRulesButton = new JButton(labels.getString("s196"));
		loadRulesButton.setToolTipText(labels.getString("s197"));
		JButton saveRulesButton = new JButton(labels.getString("s198"));
		saveRulesButton.setToolTipText(labels.getString("s199"));
		// Oyentes
		aceptar2Boton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String txt = "header{\npackage operaciones.sintacticas.grammar;\n}\n";
				txt += "class GrammarLexer extends Lexer;\n";
				txt += categoriasTextArea.getText();
				txt += "\nclass GrammarParser extends Parser;\n";
				txt += "options{k=2;}\n";
				txt += reglasTextArea.getText();
				Fichero f = new Fichero();
				boolean salvado = f.salvar("grammar.g",txt);
				salvado = salvado && f.salvar("lexicalCats.txt",categoriasTextArea.getText());
				salvado = salvado && f.salvar("syntaxRules.txt",reglasTextArea.getText());
				if(salvado) logger.info(labels.getString("s185"));
				else logger.info(labels.getString("s186"));
				frame2.dispose();
				// TODO en vez de la ventana de espera cambiar el cursor por un reloj
				PleaseWaitWindow.showPleaseWaitWindow();
				PleaseWaitWindow.refreshPleaseWaitWindow();
				boolean generated = false;
				String newGrammarName = "newGrammar";
				String newGrammarPath = ".//configuration/grammars/newGrammar.jar";
				try {
					generated = GrammarGenerator.generate(newGrammarName);
					String previousGrammar = almacenPropiedades.getPropiedad("currentGrammar");
					if (changesSaved) almacenPropiedades.setPropiedad("previousGrammar",previousGrammar);
					almacenPropiedades.setPropiedad("currentGrammar",newGrammarPath);
					Ventana v = Ventana.getInstance();
					v.getnuevoMenu().getSaveGrammar().setEnabled(false);
					v.validate();
					v.repaint();
					changesSaved = false;
					v.getStatusBar().setMessageGrammar(labels.getString("s248") + " newGrammar (Not saved)");
					frame2.dispose();
					logger.info(labels.getString("s935"));
				}
				catch (Exception e1) {
					JOptionPane.showMessageDialog(null,e1.getMessage(),labels.getString("s930"),JOptionPane.ERROR_MESSAGE);
					logger.error(e1.getMessage());
				}
				PleaseWaitWindow.closePleaseWaitWindow();
				if(generated) logger.info(labels.getString("s208"));
				else logger.error(labels.getString("s209"));
				//mig
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
				
			}
		});
		cancelar2Boton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				frame2.dispose();
				Ventana v = Ventana.getInstance();
				v.setEnabled(true);
				v.setAlwaysOnTop(true);
				v.setAlwaysOnTop(false);
				logger.info(labels.getString("s183"));
			}
		});
		loadRulesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero file = new Fichero();
				String path = file.leer();
				String text = null;
				text = file.cargar(path);
				reglasTextArea.setText(text);
				logger.info(labels.getString("s200"));
			}
		});
		saveRulesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String text = reglasTextArea.getText();
				Fichero f = new Fichero();
				String path = f.escribir();
				boolean saved = f.salvar(path,text);
				if(saved) logger.info(labels.getString("s202") + path);
				else logger.info(labels.getString("s203"));
			}
		});
		loadCategoriesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero file = new Fichero();
				String path = file.leer();
				String text = null;
				text = file.cargar(path);
				categoriasTextArea.setText(text);
				logger.info(labels.getString("s201"));
			}
		});
		saveCategoriesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String text = categoriasTextArea.getText();
				Fichero f = new Fichero();
				String path = f.escribir();
				boolean saved = f.salvar(path,text);
				if(saved) logger.info(labels.getString("s204") + path);
				else logger.info(labels.getString("s205"));
			}
		});
		ActionListener escPressed = new ActionListener()	{
			public void actionPerformed(ActionEvent e) {
				frame2.dispose();
				logger.info(labels.getString("s183"));
			}
		};
		cancelar2Boton.registerKeyboardAction(escPressed,"EscapeKey",KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE,0,true),JComponent.WHEN_IN_FOCUSED_WINDOW);
		// Añadimos los elementos a los paneles
		c.fill = GridBagConstraints.NONE;
		c.gridwidth = 1;
		c.gridheight = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 300;
		c.ipady = 300;
		c.insets = new Insets(5,5,5,5);
		categoriasPanel.add(categoriasScroll,c);
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 0;
		c.ipady = 0;
		c.gridwidth = 1;
		categoriesButtonsPanel.add(loadCategoriesButton,c);
		c.gridx = 1;
		categoriesButtonsPanel.add(saveCategoriesButton,c);
		c.gridy = 1;
		c.gridx = 0;
		categoriasPanel.add(categoriesButtonsPanel,c);
		c.gridwidth = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 400;
		c.ipady = 300;
		reglasPanel.add(reglasScroll,c);
		c.ipadx = 0;
		c.ipady = 0;
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		rulesButtonsPanel.add(loadRulesButton,c);
		c.gridx = 1;
		rulesButtonsPanel.add(saveRulesButton,c);
		c.gridx = 0;
		c.gridy = 1;
		reglasPanel.add(rulesButtonsPanel,c);
		c.fill = GridBagConstraints.BOTH;
		c.gridwidth = 1;
		c.gridheight = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 0;
		c.ipady = 0;
		c.insets = new Insets(5,5,5,5);
		botones2Panel.add(aceptar2Boton,c);
		c.gridx = 1;
		botones2Panel.add(cancelar2Boton,c);
		c.gridx = 0;
		c.ipadx = 0;
		c.gridwidth = 1;
		c.gridy = 1;
		frame2.add(categoriasPanel,c);
		c.gridx = 1;
		frame2.add(reglasPanel,c);
		c.gridwidth = 2;
		c.gridx = 0;
		c.gridy = 2;
		frame2.add(botones2Panel,c);
		frame2.setResizable(false);
		frame2.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = frame2.getSize();
		frame2.setLocation((screenSize.width - frameSize.width) / 2,
						  (screenSize.height - frameSize.height) / 2);
		frame2.setVisible(true);
		logger.info(labels.getString("s174"));
	}
	
	public void modifyGrammarGUI() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		GridBagConstraints c = new GridBagConstraints();		
		logger.info(labels.getString("s173"));
		frame2 = new JFrame();
		frame2.setLayout(new GridBagLayout());
		String currentGrammar = null;
		try {
			currentGrammar = almacenPropiedades.getPropiedad("currentGrammar");
		}
		catch (Exception e2) {
			JOptionPane.showMessageDialog(null,e2.getMessage(),labels.getString("s936"),JOptionPane.ERROR_MESSAGE);
			logger.error(e2.getMessage());
		}
		int index = currentGrammar.lastIndexOf("\\");
		if (index == -1) index = currentGrammar.lastIndexOf("/");
		String grammarName = currentGrammar.substring(index + 1,currentGrammar.length() - 4);
		frame2.setTitle(labels.getString("s230") + " - " + grammarName);
		JPanel categoriasPanel = new JPanel();
		categoriasPanel.setLayout(new GridBagLayout());
		categoriasPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s175")));
		JPanel reglasPanel = new JPanel();
		reglasPanel.setLayout(new GridBagLayout());
		reglasPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s176")));
		JPanel botones2Panel = new JPanel();
		botones2Panel.setLayout(new GridBagLayout());
		JPanel categoriesButtonsPanel = new JPanel();
		categoriesButtonsPanel.setLayout(new GridBagLayout());
		JPanel rulesButtonsPanel = new JPanel();
		rulesButtonsPanel.setLayout(new GridBagLayout());
		final JTextArea categoriasTextArea = new JTextArea();
		categoriasTextArea.setToolTipText(labels.getString("s179"));
		final JTextArea reglasTextArea = new JTextArea();
		reglasTextArea.setToolTipText(labels.getString("s180"));
		JScrollPane categoriasScroll = new JScrollPane(categoriasTextArea);
		JScrollPane reglasScroll = new JScrollPane(reglasTextArea);
		JButton aceptar2Boton = new JButton(labels.getString("s177"));
		aceptar2Boton.setToolTipText(labels.getString("s181"));
		JButton cancelar2Boton = new JButton(labels.getString("s178"));
		cancelar2Boton.setToolTipText(labels.getString("s182"));
		JButton loadCategoriesButton = new JButton(labels.getString("s192"));
		loadCategoriesButton.setToolTipText(labels.getString("s193"));
		JButton saveCategoriesButton = new JButton(labels.getString("s194"));
		saveCategoriesButton.setToolTipText(labels.getString("s195"));
		JButton loadRulesButton = new JButton(labels.getString("s196"));
		loadRulesButton.setToolTipText(labels.getString("s197"));
		JButton saveRulesButton = new JButton(labels.getString("s198"));
		saveRulesButton.setToolTipText(labels.getString("s199"));
		// Oyentes
		aceptar2Boton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String txt = "header{\npackage operaciones.sintacticas.grammar;\n}\n";
				txt += "class GrammarLexer extends Lexer;\n";
				txt += categoriasTextArea.getText();
				txt += "\nclass GrammarParser extends Parser;\n";
				txt += "options{k=2;}\n";
				txt += reglasTextArea.getText();
				Fichero f = new Fichero();
				boolean salvado = f.salvar("grammar.g",txt);
				salvado = salvado && f.salvar("lexicalCats.txt",categoriasTextArea.getText());
				salvado = salvado && f.salvar("syntaxRules.txt",reglasTextArea.getText());
				if(salvado) logger.info(labels.getString("s185"));
				else logger.info(labels.getString("s186"));
				frame2.dispose();
				// TODO en vez de la ventana de espera cambiar el cursor por un reloj
				PleaseWaitWindow.showPleaseWaitWindow();
				PleaseWaitWindow.refreshPleaseWaitWindow();
				boolean generated = false;
				String newGrammarName = "lastModified";
				String newGrammarPath = ".//configuration/grammars/lastModified.jar";
				try {
					generated = GrammarGenerator.generate(newGrammarName);
					String previousGrammar = almacenPropiedades.getPropiedad("currentGrammar");
					if(!previousGrammar.endsWith("lastModified.jar"))
						almacenPropiedades.setPropiedad("previousGrammar",previousGrammar);
					almacenPropiedades.setPropiedad("currentGrammar",newGrammarPath);
					Ventana v = Ventana.getInstance();
					v.getnuevoMenu().getSaveGrammar().setEnabled(true);
					v.validate();
					v.repaint();
					v.getStatusBar().setMessageGrammar(labels.getString("s248") + " lastModified (Not saved)");
					changesSaved = false;
					frame2.dispose();
					logger.info(labels.getString("s935"));
				}
				catch (Exception e1) {
					JOptionPane.showMessageDialog(null,e1.getMessage(),labels.getString("s930"),JOptionPane.ERROR_MESSAGE);
					logger.error(e1.getMessage());
				}
				PleaseWaitWindow.closePleaseWaitWindow();
				if(generated) logger.info(labels.getString("s208"));
				else logger.error(labels.getString("s209"));
			}
		});
		cancelar2Boton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				frame2.dispose();
				Ventana v = Ventana.getInstance();
				v.setEnabled(true);
				v.setAlwaysOnTop(true);
				v.setAlwaysOnTop(false);
				logger.info(labels.getString("s183"));
			}
		});
		loadRulesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero file = new Fichero();
				String path = file.leer();
				String text = null;
				text = file.cargar(path);
				reglasTextArea.setText(text);
				logger.info(labels.getString("s200"));
			}
		});
		saveRulesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String text = reglasTextArea.getText();
				Fichero f = new Fichero();
				String path = f.escribir();
				boolean saved = f.salvar(path,text);
				if(saved) logger.info(labels.getString("s202") + path);
				else logger.info(labels.getString("s203"));
			}
		});
		loadCategoriesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero file = new Fichero();
				String path = file.leer();
				String text = null;
				text = file.cargar(path);
				categoriasTextArea.setText(text);
				logger.info(labels.getString("s201"));
			}
		});
		saveCategoriesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String text = categoriasTextArea.getText();
				Fichero f = new Fichero();
				String path = f.escribir();
				boolean saved = f.salvar(path,text);
				if(saved) logger.info(labels.getString("s204") + path);
				else logger.info(labels.getString("s205"));
			}
		});
		ActionListener escPressed = new ActionListener()	{
			public void actionPerformed(ActionEvent e) {
				frame2.dispose();
				logger.info(labels.getString("s183"));
			}
		};
		cancelar2Boton.registerKeyboardAction(escPressed,"EscapeKey",KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE,0,true),JComponent.WHEN_IN_FOCUSED_WINDOW);
		// Añadimos los elementos a los paneles
		c.fill = GridBagConstraints.NONE;
		c.gridwidth = 1;
		c.gridheight = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 300;
		c.ipady = 300;
		c.insets = new Insets(5,5,5,5);
		categoriasPanel.add(categoriasScroll,c);
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 0;
		c.ipady = 0;
		c.gridwidth = 1;
		categoriesButtonsPanel.add(loadCategoriesButton,c);
		c.gridx = 1;
		categoriesButtonsPanel.add(saveCategoriesButton,c);
		c.gridy = 1;
		c.gridx = 0;
		categoriasPanel.add(categoriesButtonsPanel,c);
		c.gridwidth = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 400;
		c.ipady = 300;
		reglasPanel.add(reglasScroll,c);
		c.ipadx = 0;
		c.ipady = 0;
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		rulesButtonsPanel.add(loadRulesButton,c);
		c.gridx = 1;
		rulesButtonsPanel.add(saveRulesButton,c);
		c.gridx = 0;
		c.gridy = 1;
		reglasPanel.add(rulesButtonsPanel,c);
		c.fill = GridBagConstraints.BOTH;
		c.gridwidth = 1;
		c.gridheight = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 0;
		c.ipady = 0;
		c.insets = new Insets(5,5,5,5);
		botones2Panel.add(aceptar2Boton,c);
		c.gridx = 1;
		botones2Panel.add(cancelar2Boton,c);
		c.gridx = 0;
		c.ipadx = 0;
		c.gridwidth = 1;
		c.gridy = 1;
		frame2.add(categoriasPanel,c);
		c.gridx = 1;
		frame2.add(reglasPanel,c);
		c.gridwidth = 2;
		c.gridx = 0;
		c.gridy = 2;
		frame2.add(botones2Panel,c);
		frame2.setResizable(true);
		frame2.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = frame2.getSize();
		frame2.setLocation((screenSize.width - frameSize.width) / 2,
						  (screenSize.height - frameSize.height) / 2);
		frame2.setVisible(true);
		String jarPath = null;
		String currentPath = null;
		try {
			jarPath = almacenPropiedades.getPropiedad("jarPath");
			currentPath = almacenPropiedades.getPropiedad("currentGrammar");
			int ind = currentPath.lastIndexOf("\\");
			if (ind == -1) ind = currentPath.lastIndexOf("/");
			currentPath = currentPath.substring(0,ind + 1);
			Salida s = new Salida(false);
			ProcessThread p = new ProcessThread();
			p.executeCommand("cmd",currentPath,"\"" + jarPath + "\" xvf " + grammarName + ".jar syntaxRules.txt lexicalCats.txt","exit",s);
			//Runtime.getRuntime().exec("\"" + jarPath + "\" xvf " + grammarName + ".jar syntaxRules.txt lexicalCats.txt");
			Thread.sleep(200);
			Fichero file = new Fichero();
			String txt = file.cargar(currentPath + "lexicalCats.txt");
			categoriasTextArea.setText(txt);
			txt = file.cargar(currentPath + "syntaxRules.txt");
			reglasTextArea.setText(txt);
			logger.info(labels.getString("s174"));
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null,e.getMessage(),labels.getString("s938"),JOptionPane.ERROR_MESSAGE);
			logger.error(e.getMessage());
		}		
	}
	
	public static void loadGrammarGUI() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		JFileChooser chooser = new JFileChooser();
		FiltroFicheros filtro = new FiltroFicheros(labels.getString("s270"));
		filtro.addExtension("jar");
		chooser.setFileFilter(filtro);
		chooser.setCurrentDirectory(new File(".//configuration/grammars/"));
		int option = chooser.showOpenDialog(null);
		if(option == JFileChooser.APPROVE_OPTION) {
			String grammarFile = chooser.getSelectedFile().getAbsolutePath();
			almacenPropiedades.setPropiedad("currentGrammar",grammarFile);
			logger.info(labels.getString("s243") + " " + grammarFile);
			int index = grammarFile.lastIndexOf("\\");
			if (index == -1) index = grammarFile.lastIndexOf("/");
			String grammarName = grammarFile.substring(index + 1,grammarFile.length() - 4);
			Ventana v = Ventana.getInstance();
			v.getStatusBar().setMessageGrammar(labels.getString("s248") + " " + grammarName);
			v.getProyecto().setGrammarConfig(grammarFile);
			v.validate();
			v.repaint();
			changesSaved = true;
			v.getnuevoMenu().getSaveGrammar().setEnabled(false);
			//mig
			String prj=null;
			try {
				prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
				Ventana.getInstance().getProyecto().setModified(true);
			}
			
		}
		else if(option == JFileChooser.CANCEL_OPTION) {
			logger.info(labels.getString("s242"));
		}
	}
	
	public static void saveGrammarGUI() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		try {
			String previous = almacenPropiedades.getPropiedad("previousGrammar");
			String current = almacenPropiedades.getPropiedad("currentGrammar");
			int index = previous.lastIndexOf("\\");
			if (index == -1)  index = previous.lastIndexOf("/");
			String newName = previous.substring(index + 1,previous.length() - 4);
			ByteFile.copy(current,previous);
			almacenPropiedades.setPropiedad("currentGrammar",previous);
			Ventana v = Ventana.getInstance();
			v.getnuevoMenu().getSaveGrammar().setEnabled(false);
			v.getStatusBar().setMessageGrammar(labels.getString("s248") + " " + newName);
			v.getProyecto().setGrammarConfig(previous);
			changesSaved = true;
			logger.info(labels.getString("s940") + ": " + previous);
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null,e.getMessage(),labels.getString("s939"),JOptionPane.ERROR_MESSAGE);
			logger.error(e.getMessage());
		}
	}
	
	public static void saveAsGrammarGUI() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		try {
			String current = almacenPropiedades.getPropiedad("currentGrammar");
			JFileChooser selector = new JFileChooser();
			FiltroFicheros filtro = new FiltroFicheros(labels.getString("s270"));
			filtro.addExtension("jar");
			selector.setFileFilter(filtro);
			selector.setCurrentDirectory(new File(".//configuration/grammars/"));
			String nombreFichero = "";
			int valor = selector.showSaveDialog(selector);
			if (valor == JFileChooser.APPROVE_OPTION) {
				nombreFichero = selector.getSelectedFile().getAbsolutePath();
				if(!nombreFichero.endsWith(".jar")) nombreFichero += ".jar";
				ByteFile.copy(current,nombreFichero);
				almacenPropiedades.setPropiedad("currentGrammar",nombreFichero);
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getSaveGrammar().setEnabled(false);
				int index = nombreFichero.lastIndexOf("\\");
				if (index == -1) index = nombreFichero.lastIndexOf("/");
				String grammarName = nombreFichero.substring(index + 1,nombreFichero.length() - 4);
				v.getStatusBar().setMessageGrammar(labels.getString("s248") + " " + grammarName);
				v.getProyecto().setGrammarConfig(nombreFichero);
				changesSaved = true;
				// Muestra operacion en el log
				logger.info(labels.getString("s941") + ": " + nombreFichero);				
			} 
			else if (valor == JFileChooser.CANCEL_OPTION) {
				selector.cancelSelection();
				// Muestra operacion en el log
				logger.info(labels.getString("s942"));
			}			
		}
		catch (Exception e1) {
			JOptionPane.showMessageDialog(null,e1.getMessage(),labels.getString("s943"),JOptionPane.ERROR_MESSAGE);
			logger.error(e1.getMessage());
		}		
	}

}
