package gui.menu.file.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.Language;
import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Close all files item listener											
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
public class CloseAllFilesListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
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
		final ResourceBundle labels = language.getLabels();

		int numEditors = MainWindow.getInstance().getEditorManager().getNumEditors();
		MainWindow.getInstance().getEditorManager().setSelectedEditorAt(numEditors - 1);

		// Checks the opened editors
		for (int i = numEditors - 1; i >= 0; i--) {

			MainWindow.getInstance().getEditorManager().setSelectedEditorAt(i);

			// It is a modified editor
			if (MainWindow.getInstance().getEditorManager().isRedButton()) {
				
				// Ask the user for saving the file
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s643"));

				// If yes
				if (chosenOption == JOptionPane.OK_OPTION) {

					// If it is the new file
					if (MainWindow.getInstance().getEditorManager().getSelectedEditor()
							.getAbsolutePath().equals(labels.getString("s79"))) {

						IOFactory ioFactory = IOFactory.getInstance();
						TextFile textFile = ioFactory.buildFile();
						String f = " ";
						f = textFile.write();
						if (f.equals(" ")) {
							
							// Updates the log
							Log.getLog().info(labels.getString("s92"));
						} else {

							boolean result = textFile.save(f, MainWindow.getInstance()
									.getEditorManager().getSelectedEditor()
									.getText());

							// If it could save it
							if (result) {
								
								// Updates the log
								Log.getLog().info(labels.getString("s93") + f
										+ labels.getString("s94"));
								
								// Sets green button
								MainWindow.getInstance().getEditorManager()
										.setGreenButton();
								
								// Sets the path
								MainWindow.getInstance().getEditorManager()
										.getEditorAt(i).setAbsolutePath(f);
								
								// Sets the tool tip text
								MainWindow.getInstance().getEditorManager()
										.getEditorAt(i).setToolTipText(f);
								
								// Gets the name
								int index = f.lastIndexOf("\\");
								if (index == -1)
									index = f.lastIndexOf("/");										
								String file = f
										.substring(index + 1, f.length());
								MainWindow.getInstance().getEditorManager()
										.getEditorAt(i).setName(file);
								
								// Creates the file
								File explorerFile = new File(MainWindow.getInstance()
										.getEditorManager()
										.getSelectedEditor().getAbsolutePath());
								MainWindow
										.getInstance()
										.getEditorManager()
										.getSelectedEditor()
										.setLastChange(
												explorerFile.lastModified());
								MainWindow.getInstance().getEditorManager()
										.getSelectedEditor().setLastSize(
												explorerFile.length());

							} else {
								
								// Updates the log
								Log.getLog().info(labels.getString("s95") + f);
							}
						}

					} else {
						MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
						MainWindow.getInstance().getMenu().getFile().getSaveFile().doClick();
					}
					
					// Not the default configuration
					if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
						
						// The project has been modified
						MainWindow.getInstance().getProjectConfiguration().setIsModified(
								true);

					// Sets the editors to closed in the project configuration
					for (int pos = 0; pos < MainWindow.getInstance()
							.getProjectConfiguration().getFileListSize(); pos++) {
						
						if (MainWindow.getInstance().getProjectConfiguration().getFileAt(
								pos).getPath().equals(
										MainWindow.getInstance().getEditorManager()
										.getEditorAt(i).getAbsolutePath())) {
							MainWindow.getInstance().getProjectConfiguration().getFileAt(
									pos).setIsOpened(false);
						}
					}
				} else {
					if (chosenOption == JOptionPane.CANCEL_OPTION)
						return;
				}
			}
			
			// Sets the editors to closed in the project configuration
			for (int pos = 0; pos < MainWindow.getInstance().getProjectConfiguration()
					.getFileListSize(); pos++) {
				if (MainWindow.getInstance().getProjectConfiguration().getFileAt(pos)
						.getPath().equals(
								MainWindow.getInstance().getEditorManager()
										.getEditorAt(i).getAbsolutePath())) {
					MainWindow.getInstance().getProjectConfiguration().getFileAt(pos)
							.setIsOpened(false);
				}
			}

		}
		
		for (int i = 0; i < numEditors; i++) {
			MainWindow.getInstance().getEditorManager().setSelectedEditorAt(0);
			MainWindow.getInstance().getEditorManager().getPane().remove(0);
			MainWindow.getInstance().getEditorManager().getPane().validate();
		}

		// Disables the EDIT and FILE menu
		MainWindow.getInstance().getMenu().getFile().disableMenu();
		MainWindow.getInstance().getMenu().disableEditMenu();
	}
}
