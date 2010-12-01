package gui.splashScreen;

import java.awt.*;
import javax.swing.*;

/************************************************************************																
 * Splash screen of ACIDE - A Configurable IDE										
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see JWindow																													
 ***********************************************************************/
public class AcideSplashScreen extends JWindow {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file of the splash screen
	 */
	private static final String IMAGE = "./resources/images/splashScreen.png";
	/**
	 * Label to show the image of the splash screen
	 */
	private static JLabel _image;
	/**
	 * Progress bar of the splash screen
	 */
	private static JProgressBar _progressBar;

	/**
	 * Shows the splash screen window
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
	 * Closes the splash screen window
	 */
	public void closeSplashScreenWindow() {
		setVisible(false);
	}

	/**
	 * Sets a new value to the progress bar given as a parameter
	 * 
	 * @param percent
	 *            new value to set
	 */
	public static void setProgressBar(int value) {
		_progressBar.setValue(value);
	}
}
