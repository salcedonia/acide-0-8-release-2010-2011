package gui.splashScreen;


import java.awt.BorderLayout;
import java.awt.Font;
import java.util.ResourceBundle;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

import properties.PropertiesManager;

import language.Language;

/**
 * 
 */
public class SplashScreen{
	
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private static final String IMAGE = "./resources/images/splashScreen.png";
	/**
	 * 
	 */
	private static JLabel _image;
	/**
	 * 
	 */
	private static JLabel _label;
	/**
	 * 
	 */
	private static ResourceBundle _labels;
	/**
	 * 
	 */
	private static JPanel _panel;
	/**
	 * 
	 */
	private static JFrame _frame;
	/**
	 * 
	 */
	private static JProgressBar _progressBar;
	
	/**
	 * 
	 */
	public static void showStartingWindow() {
		
		Language language = Language.getInstance();

		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		_labels = language.getLabels();
		
		_frame = new JFrame();
		_frame.setLayout(new BorderLayout());
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setTitle("ACIDE 0.8");
		_frame.setSize(600,450);
		
		_panel = new JPanel();
		_panel.setLayout(new BorderLayout());
		
		_progressBar = new JProgressBar();
		_progressBar.setStringPainted(true);
		
		_label = new JLabel(_labels.getString("s171"),JLabel.LEFT);
		_label.setFont(new Font("Arial",Font.BOLD,12));
		
		_image = new JLabel(new ImageIcon(IMAGE));
		
		_panel.add(_image,BorderLayout.NORTH);
		_panel.add(_label,BorderLayout.CENTER);
		_panel.add(_progressBar,BorderLayout.SOUTH);
		_frame.add(_panel,BorderLayout.CENTER);
		_frame.setLocationRelativeTo(null);
		_frame.setResizable(false);
		_frame.setVisible(true);
	}
	
	/**
	 * 
	 */
	public static void closeStartingWindow() {
		_frame.dispose();
	}
	
	/**
	 * 
	 * @param percent
	 */
	public static void setProgressBar(int value) {
		_progressBar.setValue(value);
	}
}
