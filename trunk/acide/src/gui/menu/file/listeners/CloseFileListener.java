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
 * Close file menu item listener											
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
public class CloseFileListener implements ActionListener {

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

		int selectedEditorIndex = MainWindow.getInstance().getEditorManager()
				.getSelectedEditorIndex();

		// If the editor has been modified
		if (MainWindow.getInstance().getEditorManager().isRedButton()) {
			
			// Do you want to save the file?
			int chosenOption = JOptionPane.showConfirmDialog(null, labels
					.getString("s643"));

			// If ok
			if (chosenOption == JOptionPane.OK_OPTION) {

				// If it is the NEW FILE
				if (MainWindow.getInstance().getEditorManager()
						.getSelectedEditor().getAbsolutePath().equals(
								labels.getString("s79"))) {

					TextFile textFile = IOFactory.getInstance().buildFile();
					String filePath = " ";
					filePath = textFile.write();
					
					if (!filePath.equals(" ")) {
						
						// Saves it
						boolean result = textFile.save(filePath, MainWindow
								.getInstance().getEditorManager()
								.getSelectedEditor().getText());

						// If it could save it
						if (result) {
							
							// Updates the log
							Log.getLog().info(labels.getString("s93") + filePath
									+ labels.getString("s94"));
							
							// Sets the green button
							MainWindow.getInstance().getEditorManager()
									.setGreenButton();
							
							// Sets the path
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(selectedEditorIndex).setAbsolutePath(filePath);
							
							// Sets the tool tip text
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(selectedEditorIndex).setToolTipText(filePath);
							
							// Gets the file name
							int index = filePath.lastIndexOf("\\");
							if(index == -1)
								index = filePath.lastIndexOf("/");
							String file = filePath.substring(index + 1, filePath.length());
							
							// Sets the title
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(selectedEditorIndex).setName(file);
							
							// Saves the original file
							File explorerFile = new File(MainWindow
									.getInstance().getEditorManager()
									.getSelectedEditor().getAbsolutePath());
							MainWindow.getInstance().getEditorManager()
									.getSelectedEditor().setLastChange(
											explorerFile.lastModified());
							MainWindow.getInstance().getEditorManager()
									.getSelectedEditor().setLastSize(
											explorerFile.length());
							
							// Updates the status bar
							MainWindow.getInstance().getStatusBar()
									.setMessage("");
						}
						else
							// Updates the log
							Log.getLog().info(labels.getString("s92"));
					} 
					else
						
						// Updates the log
						Log.getLog().info(labels.getString("s95") + filePath);
				}		
				else {
					
					// Saves the file
					MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
					MainWindow.getInstance().getMenu().getFile().getSaveFile().doClick();
					MainWindow.getInstance().getStatusBar().setMessage("");
				}

				// Updates the file state in the project configuration
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getFileListSize(); i++) {
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance()
											.getEditorManager()
											.getEditorAt(selectedEditorIndex).getAbsolutePath())) {
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsOpened(false);
					}
				}
							
				// Not default project
				if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
					
					// The project is modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				
				// Removes the tab
				MainWindow.getInstance().getEditorManager().getPane()
						.remove(selectedEditorIndex);

			} else 
				if (chosenOption == JOptionPane.NO_OPTION) {
					
					// Removes the tab
					MainWindow.getInstance().getEditorManager().getPane()
						.remove(selectedEditorIndex);
					
					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setMessage("");
			}
		} else {

			// Updates the file state in the project configuration
			for (int i = 0; i < MainWindow.getInstance()
					.getProjectConfiguration().getFileListSize(); i++) {
				
				if (MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(i).getPath().equals(
								MainWindow.getInstance().getEditorManager()
										.getEditorAt(selectedEditorIndex).getAbsolutePath())) {
					
					// Is not opened
					MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).setIsOpened(false);
				}
			}
			
			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);
			
			// Removes the tab
			MainWindow.getInstance().getEditorManager().getPane().remove(
					selectedEditorIndex);
			
			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setMessage("");
		}
		
		// No more opened tabs
		if (MainWindow.getInstance().getEditorManager().getPane()
				.getTabCount() == 0) {
			
			// Disables the FILE menu
			MainWindow.getInstance().getMenu().getFile().disableMenu();
			
			// Disables the EDIT menu
			MainWindow.getInstance().getMenu().disableEditMenu();
		}
	}
}

