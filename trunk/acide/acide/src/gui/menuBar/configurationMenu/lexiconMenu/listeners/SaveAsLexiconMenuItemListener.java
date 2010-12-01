package gui.menuBar.configurationMenu.lexiconMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import language.AcideLanguage;
import operations.log.AcideLog;

import resources.ResourceManager;
import es.configuration.lexicon.LexiconConfiguration;
import es.text.TextFileFilter;

/************************************************************************																
 * Save as lexicon menu item listener.
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
public class SaveAsLexiconMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels TO DISPLAY
		ResourceBundle labels = language.getLabels();

		JFileChooser fileChooser = new JFileChooser(labels.getString("s126"));
		TextFileFilter filter = new TextFileFilter(labels.getString("s287"));
		filter.addExtension("xml");
		fileChooser.setFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/lexical"));

		int chosenOption = fileChooser.showSaveDialog(null);
		String filePath = " ";

		if (chosenOption == JFileChooser.APPROVE_OPTION)
			filePath = fileChooser.getSelectedFile().getAbsolutePath();

		// If the path is ok
		if (!filePath.equals(" ")) {

			// Gets the lexicon name
			int index = filePath.lastIndexOf("\\");
			if (index == -1)
				index = filePath.lastIndexOf("/");
			String fileName = filePath.substring(index + 1, filePath.length());
			
			if (fileName.contains(".")) {
				index = fileName.lastIndexOf(".");
				fileName = fileName.substring(0, index);
			}

			// Save lexicon as
			LexiconConfiguration programmingLanguage = LexiconConfiguration
					.getInstance();
			boolean result = programmingLanguage.saveAs(fileName, false,
					filePath);

			// If it could save it
			if (result) {
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);

			} else {
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
			}
		} else
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s92"));
	}
}
