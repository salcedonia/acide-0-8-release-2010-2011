package gui;

import idioma.Idioma;

import java.awt.BorderLayout;
import java.awt.Font;
import java.util.ResourceBundle;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import principal.almacenPropiedades;

public class StartingWindow {
	
	private static JFrame iniciandoFrame;
	private static JLabel iniciandoLabel;
	private static ResourceBundle labels;

	public static void showStartingWindow() {
		Idioma i = Idioma.getInstance();

		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		labels = i.getLabels();
		iniciandoFrame = new JFrame();
		iniciandoFrame.setLayout(new BorderLayout());
		iniciandoFrame.setTitle("ACIDE 0.7");
		iniciandoFrame.setSize(200,70);
		JPanel iniciandoPanel = new JPanel();
		iniciandoPanel.setLayout(new BorderLayout());
		iniciandoLabel = new JLabel(labels.getString("s171") + "       0%",JLabel.CENTER);
		iniciandoLabel.setFont(new Font("Arial",Font.BOLD,14));
		iniciandoPanel.add(iniciandoLabel,BorderLayout.CENTER);
		iniciandoFrame.add(iniciandoPanel,BorderLayout.CENTER);
		iniciandoFrame.setLocationRelativeTo(null);
		//iniciandoFrame.pack();
		iniciandoFrame.setResizable(false);
		iniciandoFrame.setVisible(true);
	}
	
	public static void closeStartingWindow() {
		iniciandoFrame.dispose();
	}
	
	public static void setIniciandoLabel(String percent) {
		iniciandoLabel.setText(labels.getString("s171") + "       " + percent + "%");
	}

}
