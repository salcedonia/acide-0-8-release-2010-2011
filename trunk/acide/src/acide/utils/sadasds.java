package acide.utils;

import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;

public class sadasds {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		JTextPane text = new JTextPane();
		text.setPreferredSize(new Dimension(200,200));
		JFrame frame = new JFrame();
		JPanel panel = new JPanel();
		panel.add(new JScrollPane(text));
		frame.add(panel);
		
		frame.pack();
		frame.setLocationRelativeTo(null);
		frame.setVisible(true);
	}

}
