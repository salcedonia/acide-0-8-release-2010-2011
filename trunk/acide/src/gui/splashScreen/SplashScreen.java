package gui.splashScreen;

import java.awt.*;
import javax.swing.*;

/**
 * Splash Screen of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class SplashScreen extends JWindow {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file of the splash screen.
	 */
	private static final String IMAGE = "./resources/images/splashScreen.png";
	/**
	 * Label to show the image of the splash screen.
	 */
	private static JLabel _image;
	/**
	 * Progress bar of the splash screen
	 */
	private static JProgressBar _progressBar;

	/**
	 * Shows the splash screen window.
	 */
	public void showSplashScreenWindow() {

		// PANEL
		JPanel content = (JPanel) getContentPane();
		
		// SET THE WINDOW'S BOUNDS, CENTERING THE WINDOW
		int width = 575;
		int height = 397;
		Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
		int x = (screen.width - width) / 2;
		int y = (screen.height - height) / 2;
		setBounds(x, y, width, height);

		// PROGRESS BAR
		_progressBar = new JProgressBar();
		_progressBar.setStringPainted(true);

		_image = new JLabel(new ImageIcon(IMAGE));

		// ADD THE COMPONENTS WITH THE LAYOUT
		content.add(_image, BorderLayout.NORTH);
		content.add(_progressBar, BorderLayout.CENTER);
		content.setBorder(BorderFactory.createRaisedBevelBorder());
		
		// DISPLAY IT
		setVisible(true);
	}

	/**
	 * Close the splash screen window.
	 */
	public void closeSplashScreenWindow() {
		setVisible(false);
	}

	/**
	 * Set a new value to the progress bar given as a parameter.
	 * 
	 * @param percent
	 *            New value to set.
	 */
	public static void setProgressBar(int value) {
		_progressBar.setValue(value);
	}
}
