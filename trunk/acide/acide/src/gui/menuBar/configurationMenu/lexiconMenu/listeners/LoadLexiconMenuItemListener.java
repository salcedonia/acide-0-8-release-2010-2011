package gui.menuBar.configurationMenu.lexiconMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;

import language.AcideLanguage;
import es.configuration.lexicon.LexiconConfiguration;
import es.text.TextFileFilter;
import gui.mainWindow.MainWindow;

/************************************************************************																
 * Load lexicon menu item listener.
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
public class LoadLexiconMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		ResourceBundle labels = AcideLanguage.getInstance().getLabels();
		TextFileFilter filter = new TextFileFilter(labels.getString("s327"));
		filter.addExtension(".xml");

		JFileChooser fileChooser = new JFileChooser();
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.addChoosableFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/lexical/"));

		int chosenOption = fileChooser.showOpenDialog(fileChooser);

		String filePath = " ";

		if (chosenOption == JFileChooser.APPROVE_OPTION)
			filePath = fileChooser.getSelectedFile().getAbsolutePath();

		// If the path is ok
		if (!filePath.equals(" ")) {

			// Loads the lexicon configuration
			LexiconConfiguration.getInstance().load(
					fileChooser.getSelectedFile().getAbsolutePath());

			// RESETS ALL THE OPENED FILES IN THE EDITOR WITH THE NEW LEXICAL
			// CONFIGURATION
			int numEditors = MainWindow.getInstance().getFileEditorManager()
					.getNumFileEditorPanels();
			for (int i = 0; i < numEditors; i++)
				MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(i)
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
							LexiconConfiguration.getInstance().getPath());
			
			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
				
				// The project has been modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);
			}
		}
	}
}
