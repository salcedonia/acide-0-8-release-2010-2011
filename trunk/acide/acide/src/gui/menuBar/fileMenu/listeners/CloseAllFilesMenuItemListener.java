package gui.menuBar.fileMenu.listeners;

import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguage;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Close all files item listener.											
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
public class CloseAllFilesMenuItemListener implements ActionListener {

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
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		int numEditors = MainWindow.getInstance().getFileEditorManager().getNumFileEditorPanels();
		MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(numEditors - 1);

		// Checks the opened editors
		for (int i = numEditors - 1; i >= 0; i--) {

			MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(i);

			// It is a modified editor
			if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {
				
				// Ask the user for saving the file
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s643"));

				// If yes
				if (chosenOption == JOptionPane.OK_OPTION) {

					// If it is the new file
					if (MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
							.getAbsolutePath().equals(labels.getString("s79"))) {

						AcideIOFactory ioFactory = AcideIOFactory.getInstance();
						TextFile textFile = ioFactory.buildFile();
						String f = " ";
						f = textFile.write();
						if (f.equals(" ")) {
							
							// Updates the log
							AcideLog.getLog().info(labels.getString("s92"));
						} else {

							boolean result = textFile.save(f, MainWindow.getInstance()
									.getFileEditorManager().getSelectedFileEditorPanel()
									.getTextEditionAreaContent());

							// If it could save it
							if (result) {
								
								// Updates the log
								AcideLog.getLog().info(labels.getString("s93") + f
										+ labels.getString("s94"));
								
								// Sets green button
								MainWindow.getInstance().getFileEditorManager()
										.setGreenButton();
								
								// Sets the path
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).setAbsolutePath(f);
								
								// Sets the tool tip text
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).setToolTipText(f);
								
								// Gets the name
								int index = f.lastIndexOf("\\");
								if (index == -1)
									index = f.lastIndexOf("/");										
								String file = f
										.substring(index + 1, f.length());
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).setName(file);
								
								// Creates the file
								File explorerFile = new File(MainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel().getAbsolutePath());
								MainWindow
										.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.setLastChange(
												explorerFile.lastModified());
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel().setLastSize(
												explorerFile.length());

							} else {
								
								// Updates the log
								AcideLog.getLog().info(labels.getString("s95") + f);
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
										MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).getAbsolutePath())) {
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
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).getAbsolutePath())) {
					MainWindow.getInstance().getProjectConfiguration().getFileAt(pos)
							.setIsOpened(false);
				}
			}

		}
		
		for (int i = 0; i < numEditors; i++) {
			MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane().remove(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane().validate();
		}

		// Disables the EDIT and FILE menu
		MainWindow.getInstance().getMenu().getFile().disableMenu();
		MainWindow.getInstance().getMenu().disableEditMenu();
	}
}
