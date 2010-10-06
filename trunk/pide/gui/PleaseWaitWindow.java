package gui;

import idioma.Idioma;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.util.ResourceBundle;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import principal.almacenPropiedades;

public class PleaseWaitWindow {
	
	private static JFrame waitFrame;
	private static ResourceBundle labels;
	private static JLabel waitLabel1;
	private static JLabel waitLabel2;
	private static boolean shown;  

	public static void showPleaseWaitWindow() {
		Idioma i = Idioma.getInstance();

		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		shown = false;
		labels = i.getLabels();
		waitFrame = new JFrame();
		waitFrame.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		waitFrame.setTitle("ACIDE");
		//waitFrame.setSize(400,110);
		JPanel waitPanel = new JPanel();
		waitPanel.setLayout(new GridLayout(2,0));
		waitLabel1 = new JLabel(labels.getString("s216"),JLabel.CENTER);
		waitLabel2 = new JLabel(labels.getString("s931"),JLabel.CENTER); 
		//waitLabel = new JLabel(labels.getString("s171") + "       0%",JLabel.CENTER);
		waitLabel1.setFont(new Font("Arial",Font.BOLD,14));
		waitLabel2.setFont(new Font("Arial",Font.BOLD,14));
		waitPanel.add(waitLabel1);
		waitPanel.add(waitLabel2);
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(7,7,7,7);
		waitFrame.add(waitPanel,c);
		//System.out.println(waitLabel1.getText());
		waitFrame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = waitFrame.getSize();
		waitFrame.setLocation((screenSize.width - frameSize.width) / 2,
						  (screenSize.height - frameSize.height) / 2);
		//waitFrame.setLocationRelativeTo(null);
		//waitFrame.pack();
		//waitLabel.setText(labels.getString("s216"));
		waitFrame.setResizable(false);
		waitFrame.setAlwaysOnTop(true);
		waitFrame.setVisible(true);
		shown = true;
	}
	
	public static void closePleaseWaitWindow() {
		waitFrame.dispose();
		shown = false;
	}
	
	public static void refreshPleaseWaitWindow() {
		waitLabel1.setText(labels.getString("s216"));
		waitLabel2.setText(labels.getString("s931"));
		waitFrame.validate();
	}

	public static boolean isShown() {
		return shown;
	}
	
	/*public static void setIniciandoLabel(String percent) {
		waitFrame.setText(labels.getString("s171") + "       " + percent + "%");
	}*/

}
