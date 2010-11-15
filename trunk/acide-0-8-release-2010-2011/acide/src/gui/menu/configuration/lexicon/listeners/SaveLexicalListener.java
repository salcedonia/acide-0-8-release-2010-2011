package gui.menu.configuration.lexicon.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.Language;
import operations.log.Log;

import properties.PropertiesManager;
import es.configuration.lexicon.LexiconConfiguration;

/************************************************************************																
 * Save lexicon menu item listener											
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
public class SaveLexicalListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		LexiconConfiguration programmingLanguage = LexiconConfiguration
				.getInstance();

		// Gets the current lexicon path
		String path = programmingLanguage.getName();

		// If it is ok
		if (!path.equals(" ")) {

			// Gets the lexicon name
			int index = path.lastIndexOf("\\");
			if (index == -1)
				index = path.lastIndexOf("/");
			String languageName = path.substring(index + 1, path.length());

			if (languageName.contains(".")) {
				index = languageName.lastIndexOf(".");
				languageName = languageName.substring(0, index);
			}

			// Saves it
			boolean result = programmingLanguage.save(languageName, false);

			// If it could save it
			if (result)
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);
			else
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
		} else {
			
			// Updates the log
			Log.getLog().info(labels.getString("s92"));
		}
	}
}
