package gui.menuBar.configurationMenu.toolBarMenu.listeners;

import es.configuration.toolBar.shellComandToolBar.ShellCommandList;
import es.text.TextFileFilter;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.toolBarMenu.gui.ToolBarConfigurationWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Load tool bar listener.											
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
public class LoadToolBarMenuItemListener implements ActionListener{

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
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
		
		JFileChooser fileChooser = new JFileChooser();
		TextFileFilter filter = new TextFileFilter(labels
				.getString("s904"));
		filter.addExtension("TBcfg");
		fileChooser.setFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/toolbar/"));
		
		int chosenOption = fileChooser.showOpenDialog(null);
		if (chosenOption == JFileChooser.APPROVE_OPTION) {
			
			String toolBarFile = fileChooser.getSelectedFile().getAbsolutePath();
			
			try {
				ShellCommandList.loadList(toolBarFile);
				ShellCommandList.loadAuxList(toolBarFile);
				MainWindow.getInstance().buildToolBar();
				
				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", toolBarFile);
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();
				MainWindow.getInstance().getMenu().getConfiguration().getToolBar().getSaveToolBar().setEnabled(false);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s905") + toolBarFile);
				
				ToolBarConfigurationWindow.setAreChangesSaved(true);
								
				// Not default project
				if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
					
					// The project has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);

			} catch (Exception exception) {
				
				// Error message
				JOptionPane.showMessageDialog(null,
						labels.getString("s906") + toolBarFile,
						labels.getString("s907"),
						JOptionPane.ERROR_MESSAGE);
				
				// Updates the log
				AcideLog.getLog().error(labels.getString("s906") + toolBarFile);
			}
		} else 
			// Cancel option
			if (chosenOption == JFileChooser.CANCEL_OPTION) {
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s908"));
		}
	}
}
