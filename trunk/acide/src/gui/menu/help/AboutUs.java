package gui.menu.help;

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
import gui.MainWindow;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import language.Language;
import operations.listeners.AcideWindowListener;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * 
 */
public class AboutUs {

	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private JPanel _infoPanel;
	/**
	 * 
	 */
	private JPanel _developersPanel;
	/**
	 * 
	 */
	private JFrame _frame;
	/**
	 * 
	 */
	private JButton _btnAccept;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public AboutUs() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
		final ResourceBundle labels = language.getLabels();
		final MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(false);
		_logger.info(labels.getString("s619"));
		
		_frame = new JFrame();
		_frame.setTitle(labels.getString("s622"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());
		
		_infoPanel = new JPanel();
		_infoPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s625")));
		_infoPanel.setLayout(new GridBagLayout());
		
		_developersPanel = new JPanel();
		_developersPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s630")));
		_developersPanel.setLayout(new GridBagLayout());
		
		_btnAccept = new JButton(labels.getString("s177"));
		_btnAccept.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				_logger.info(labels.getString("s621"));
				_frame.dispose();
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
				mainWindow.setEnabled(true);
			}
		});
		
		_infoPanel.addKeyListener(new Keyboard());
		_developersPanel.addKeyListener(new Keyboard());
		_btnAccept.addKeyListener(new Keyboard());
		_frame.addWindowListener(new AcideWindowListener());
		
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_infoPanel.add(new JLabel(labels.getString("s626")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_infoPanel.add(new JLabel(labels.getString("s627")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		_infoPanel.add(new JLabel(labels.getString("s628")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 3;
		_infoPanel.add(new JLabel(labels.getString("s629")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 4;
		_infoPanel.add(new JLabel(labels.getString("s966")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 5;
		_infoPanel.add(new JLabel(labels.getString("s967")), constraints);
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_developersPanel.add(new JLabel(labels.getString("s631")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_developersPanel.add(new JLabel(labels.getString("s632")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		_developersPanel.add(new JLabel(labels.getString("s633")), constraints);
		constraints.gridy = 3;
		_developersPanel.add(new JLabel(labels.getString("s634")), constraints);
		constraints.gridy = 4;
		_developersPanel.add(new JLabel(labels.getString("s635")), constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_developersPanel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_infoPanel, constraints);		
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.fill = GridBagConstraints.NONE;
		_frame.add(_btnAccept, constraints);
		_frame.setResizable(false);
		_frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = _frame.getSize();
		_frame.setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		_frame.setVisible(true);
	}

	/**
	 * 
	 */
	class Keyboard extends KeyAdapter {
		
		/**
		 * 
		 */
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
			}
		}
	}
}