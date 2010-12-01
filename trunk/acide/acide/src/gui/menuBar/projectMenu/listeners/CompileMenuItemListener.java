package gui.menuBar.projectMenu.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.JOptionPane;

import operations.log.AcideLog;

/************************************************************************																
 * Compile menu item listener.											
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
public class CompileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Closes the default project
		MainWindow.getInstance().closeDefaultProject();

		try {
			
			// Is it possible to compile
			if (MainWindow.getInstance().getProjectConfiguration().isCheckCompiler()) {

				String fileToCompile = "";

				for (int i = 0; i < MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList(); i++) {
					
					// IS COMPILABLE?
					if (MainWindow.getInstance().getProjectConfiguration().getFileAt(i)
							.isCompilableFile()){
					
						fileToCompile = fileToCompile
								+ "\""
								+ MainWindow.getInstance().getProjectConfiguration()
										.getFileAt(i).getPath()
								+ "\""
								+ MainWindow.getInstance().getProjectConfiguration()
										.getSeparatorFile();
					}
				}
				
				if (fileToCompile.length() > 0) {
					fileToCompile = fileToCompile.substring(0,
							fileToCompile.length() - 1);
					
					// If the compiler path has been defined
					if (MainWindow.getInstance().getProjectConfiguration()
							.getCompilerPath() != null)
						
						// EXECUTES THE COMPILATION
						Runtime.getRuntime().exec(MainWindow.getInstance().getProjectConfiguration()
								.getCompilerPath()
								+ " "
								+ MainWindow.getInstance().getProjectConfiguration()
										.getCompilerArguments()
								+ " "
								+ fileToCompile);
				}
			} else {
				
				String extension = MainWindow.getInstance().getProjectConfiguration()
						.getFileExtension();
				
				for (int i = 0; i < MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList(); i++) {
					
					ExplorerFile file = MainWindow.getInstance()
							.getProjectConfiguration().getFileAt(i);
					
					// Is not a directory
					if (!file.isDirectory()) {
						
						// Gets the extension
						String name = file.getPath();
						String ext = name.substring(
								name.lastIndexOf(".") + 1, name.length());
						
						if (ext.equals(extension)) {
							
							if (MainWindow.getInstance().getProjectConfiguration()
									.getCompilerPath() != null) {
								
								// EXECUTES THE COMPILATION
								Runtime.getRuntime().exec(MainWindow.getInstance()
										.getProjectConfiguration()
										.getCompilerPath()
										+ " "
										+ MainWindow.getInstance()
												.getProjectConfiguration()
												.getCompilerArguments()
										+ " \"" + name + "\"");
							}
						}
					}
				}
			}
		} catch (IOException exception) {
			
			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage());
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}

