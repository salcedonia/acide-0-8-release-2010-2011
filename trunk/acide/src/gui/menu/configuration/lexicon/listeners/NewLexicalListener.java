package gui.menu.configuration.lexicon.listeners;

import es.configuration.lexicon.LexiconConfiguration;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import operations.log.Log;

import language.Language;
import properties.PropertiesManager;

/************************************************************************																
 * New lexicon menu item listener											
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
public class NewLexicalListener implements ActionListener {

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

		String languagePath = "";
		String languageName = "";
		languageName = JOptionPane.showInputDialog(null, labels
				.getString("s453"), labels.getString("s454"),
				JOptionPane.INFORMATION_MESSAGE);

		// If it is ok
		if (!languageName.equals("")) {

			if(languageName.contains(".xml"))
				languagePath = "./configuration/lexical/" + languageName;
			else
				languagePath = "./configuration/lexical/" + languageName + ".xml";

			// RESET ALL THE OPENED FILES IN THE EDITOR WITH THE NEW LEXICAL
			// CONFIGURATION
			LexiconConfiguration.getInstance().newLexical(languagePath);
			int numEditors = MainWindow.getInstance().getEditorManager()
					.getNumEditors();
			for (int j = 0; j < numEditors; j++)
				MainWindow.getInstance().getEditorManager().getEditorAt(j)
						.resetDocument();

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().getLexiconMessage()
					.setText(
							labels.getString("s449")
									+ " "
									+ LexiconConfiguration.getInstance()
											.getName());
			
			// Updates the project configuration
			MainWindow.getInstance().getProjectConfiguration()
					.setLexicalConfiguration(
							LexiconConfiguration.getInstance().getName());

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
				
				// The project has been modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);
			}
		} else {

			// Missed name
			JOptionPane.showMessageDialog(null, labels.getString("s976"),
					labels.getString("s972"), JOptionPane.WARNING_MESSAGE);
		}
	}
}