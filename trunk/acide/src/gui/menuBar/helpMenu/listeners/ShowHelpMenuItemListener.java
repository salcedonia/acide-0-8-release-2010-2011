package gui.menuBar.helpMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Show help menu item listener.											
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
 * @see ActionListener																													
 ***********************************************************************/
public class ShowHelpMenuItemListener implements ActionListener {

	/**
	 * Help URL of the help file in Spanish.
	 */
	private final static String SPANISH_HELP_URL = "resources/help/ayuda.txt";
	/**
	 * Help URL of the help file in English.
	 */
	private final static String ENGLISH_HELP_URL = "resources/help/help.txt";
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		String path = "";
		
		try {

			if (MainWindow.getInstance().getProjectConfiguration()
					.getLanguage().equals("spanish"))
				// SPANISH USER GUIDE
				path = SPANISH_HELP_URL;
			else
				// ENGLISH USER'S GUIDE
				path = ENGLISH_HELP_URL;
			
			Desktop.getDesktop().open(new File(path));	
				
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			
			// Gets the language
			AcideLanguage language = AcideLanguage.getInstance();

			try {
				language.getLanguage(ResourceManager.getInstance().getProperty("language"));
			} catch (Exception exception2) {
				
				// Updates the log
				AcideLog.getLog().error(exception2.getMessage());
				exception2.printStackTrace();
			}

			// Gets the labels
			final ResourceBundle labels = language.getLabels();
			
			// HELP GUIDE NOT FOUND
			JOptionPane.showMessageDialog(null, labels.getString("s969") + path, labels.getString("s945"), JOptionPane.ERROR_MESSAGE);
		}
		// GUIFactory.getInstance().buildHelp();
		/*
		 * try { Desktop.getDesktop().browse(new URI(HELP_URL)); } catch
		 * (IOException e) { e.printStackTrace(); } catch
		 * (URISyntaxException e) { e.printStackTrace(); }
		 */
	}
}
