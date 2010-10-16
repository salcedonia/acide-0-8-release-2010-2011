package gui.pleaseWaitWindow;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import properties.PropertiesManager;

import language.Language;

/**
 * 
 */
public class PleaseWaitWindow {
	
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private static JFrame _frame;
	/**
	 * 
	 */
	private static ResourceBundle _labels;
	/**
	 * 
	 */
	private static JLabel _label1;
	/**
	 * 
	 */
	private static JLabel _label2;
	/**
	 * 
	 */
	private static boolean _isShown;  

	/**
	 * 
	 */
	public static void showPleaseWaitWindow() {
		
		Language language = Language.getInstance();

		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		_isShown = false;
		_labels = language.getLabels();
		
		_frame = new JFrame();
		_frame.setLayout(new GridBagLayout());
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setTitle("ACIDE 0.8");
		
		JPanel waitPanel = new JPanel();
		waitPanel.setLayout(new GridLayout(2,0));
		_label1 = new JLabel(_labels.getString("s216"),JLabel.CENTER);
		_label2 = new JLabel(_labels.getString("s931"),JLabel.CENTER); 
		_label1.setFont(new Font("Arial",Font.BOLD,14));
		_label2.setFont(new Font("Arial",Font.BOLD,14));
		waitPanel.add(_label1);
		waitPanel.add(_label2);
		
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(7,7,7,7);
		_frame.add(waitPanel,constraints);
		_frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = _frame.getSize();
		_frame.setLocation((screenSize.width - frameSize.width) / 2,
						  (screenSize.height - frameSize.height) / 2);
		_frame.setResizable(false);
		_frame.setAlwaysOnTop(true);
		_frame.setVisible(true);
		_isShown = true;
	}
	
	/**
	 * 
	 */
	public static void closePleaseWaitWindow() {
		_frame.dispose();
		_isShown = false;
	}
	
	/**
	 * 
	 */
	public static void refreshPleaseWaitWindow() {
		_label1.setText(_labels.getString("s216"));
		_label2.setText(_labels.getString("s931"));
		_frame.validate();
	}

	/**
	 * 
	 * @return
	 */
	public static boolean isShown() {
		return _isShown;
	}
}
