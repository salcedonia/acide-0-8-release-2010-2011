package gui.toolBarPanel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToolBar;

import es.configuration.toolBar.shellComandToolBar.ShellCommand;
import es.configuration.toolBar.shellComandToolBar.ShellCommandList;
import gui.mainWindow.MainWindow;

import operations.log.AcideLog;

import resources.ResourceManager;

import language.AcideLanguage;

/************************************************************************																
 * Shell command tool bar of ACIDE - A Configurable IDE. Its buttons 
 * executes shell commands in the ACIDE - A Configurable IDE shell.										
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
 * @see JToolBar																													
 ***********************************************************************/
public class ShellCommandToolBar extends JToolBar {

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * Creates a new shell command tool bar. Builds the tool bar with
	 * the tool bar project configuration.
	 * 
	 * @return The editable toolBar.
	 */
	public ShellCommandToolBar() {
		
		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		}
		catch (Exception exception) {
			
			// Updates the message
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s130"));
		
		// Removes all the buttons
		removeAll();
		
		// Adds all the buttons
		JButton button;
		for (int command = 0; command < ShellCommandList.getSize(); command++) {
			
			final ShellCommand newCommand = ShellCommandList.getCommandAt(command);
			if (newCommand.getHasIcon()) button = new JButton(new ImageIcon(newCommand
					.getIcon()));
			else if (!(newCommand.getName().equals(""))) button = new JButton(newCommand
					.getName());
			else button = new JButton((new Integer(command + 1)).toString());
			if (!(newCommand.getHintText().equals(""))) button.setToolTipText(newCommand
					.getHintText());
			
			// Adds the button to the tool bar
			add(button);
			
			// LISTENER
			button.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				@Override
				public void actionPerformed(ActionEvent actionEvent) {
					
					// ACTION
					MainWindow.getInstance().getOutput().executeCommand(
							newCommand.getAction());
					
					// Sets the caret position at the end of the text of the output
					MainWindow.getInstance()
							.getOutput().getTextComponent().setCaretPosition(MainWindow.getInstance()
							.getOutput().getTextComponent().getDocument().getLength());
					
					// Sets the focus in the OUTPUT
					MainWindow.getInstance()
					.getOutput().getTextComponent().requestFocusInWindow();
				}
			});
		}
		// Updates the log
		AcideLog.getLog().info(labels.getString("s131"));
	}
}
