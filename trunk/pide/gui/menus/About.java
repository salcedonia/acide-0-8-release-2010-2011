package gui.menus;

import idioma.Idioma;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import gui.Ventana;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import operaciones.genericas.Windows;
import operaciones.log.Log;
import org.apache.log4j.Logger;
import principal.almacenPropiedades;

/**
 * Clase que muestra la ventana sobre...
 */
public class About {

	private JPanel panel1;

	private JPanel panel2;

	private JFrame frame;

	private JButton aceptar;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	public About() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		final Ventana v = Ventana.getInstance();
		v.setEnabled(false);
		logger.info(labels.getString("s619"));
		frame = new JFrame();
		panel1 = new JPanel();
		panel2 = new JPanel();
		frame.setTitle(labels.getString("s622"));
		panel1.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s625")));
		panel2.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s630")));
		frame.setLayout(new GridBagLayout());
		panel1.setLayout(new GridBagLayout());
		panel2.setLayout(new GridBagLayout());
		aceptar = new JButton(labels.getString("s177"));
		aceptar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				logger.info(labels.getString("s621"));
				frame.dispose();
				v.setAlwaysOnTop(true);
				v.setAlwaysOnTop(false);
				v.setEnabled(true);
			}
		});
		panel1.addKeyListener(new Keyboard());
		panel2.addKeyListener(new Keyboard());
		aceptar.addKeyListener(new Keyboard());
		frame.addWindowListener(new Windows());
		GridBagConstraints c = new GridBagConstraints();
		c.insets = new Insets(5, 5, 5, 5);
		c.fill = GridBagConstraints.BOTH;
		c.gridx = 0;
		c.gridy = 0;
		panel1.add(new JLabel(labels.getString("s626")), c);
		c.gridx = 0;
		c.gridy = 1;
		panel1.add(new JLabel(labels.getString("s627")), c);
		c.gridx = 0;
		c.gridy = 2;
		panel1.add(new JLabel(labels.getString("s628")), c);
		c.gridx = 0;
		c.gridy = 3;
		panel1.add(new JLabel(labels.getString("s629")), c);
		c.gridx = 0;
		c.gridy = 4;
		panel1.add(new JLabel(labels.getString("s966")), c);
		c.insets = new Insets(5, 5, 5, 5);
		c.gridx = 0;
		c.gridy = 0;
		panel2.add(new JLabel(labels.getString("s631")), c);
		c.gridx = 0;
		c.gridy = 1;
		panel2.add(new JLabel(labels.getString("s632")), c);
		c.gridx = 0;
		c.gridy = 2;
		panel2.add(new JLabel(labels.getString("s633")), c);
		c.gridy = 3;
		panel2.add(new JLabel(labels.getString("s634")), c);
		c.gridy = 4;
		panel2.add(new JLabel(labels.getString("s635")), c);
		c.gridx = 0;
		c.gridy = 0;
		frame.add(panel2, c);
		c.gridx = 0;
		c.gridy = 1;
		frame.add(panel1, c);		
		c.gridx = 0;
		c.gridy = 2;
		c.fill = GridBagConstraints.NONE;
		frame.add(aceptar, c);
		frame.setResizable(false);
		frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = frame.getSize();
		frame.setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		frame.setVisible(true);
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