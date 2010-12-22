package gui.toolBarPanel.shellCommandToolBar;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import es.configuration.toolBar.shellComandToolBar.ShellCommand;
import es.configuration.toolBar.shellComandToolBar.ShellCommandList;
import gui.mainWindow.MainWindow;

import operations.log.AcideLog;

import resources.ResourceManager;

import language.AcideLanguage;

/************************************************************************
 * Shell command tool bar of ACIDE - A Configurable IDE. Its buttons executes
 * shell commands in the ACIDE - A Configurable IDE shell.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
 * @see ArrayList
 ***********************************************************************/
public class ShellCommandToolBar extends ArrayList<JButton> {

	/**
	 * Shell command tool bar class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Shell command tool bar unique class instance.
	 */
	private static ShellCommandToolBar _instance;

	/**
	 * Creates a new shell command tool bar.
	 */
	public ShellCommandToolBar() {
		super();
	}

	/**
	 * Returns the shell command tool bar unique class instance.
	 * 
	 * @return the shell command tool bar unique class instance.
	 */
	public static ShellCommandToolBar getInstance() {

		if (_instance == null)
			_instance = new ShellCommandToolBar();
		return _instance;
	}

	/**
	 * Builds the tool bar with the tool bar project configuration.
	 * 
	 * @return the modifiable toolBar.
	 */
	public ShellCommandToolBar build() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the message
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s130"));

		// Removes all the buttons
		clear();

		// Adds all the buttons
		JButton button;
		for (int command = 0; command < ShellCommandList.getSize(); command++) {

			final ShellCommand newCommand = ShellCommandList
					.getShellCommandAt(command);
			if (newCommand.getHasIcon())
				button = new JButton(new ImageIcon(newCommand.getIcon()));
			else if (!(newCommand.getName().equals("")))
				button = new JButton(newCommand.getName());
			else
				button = new JButton((new Integer(command + 1)).toString());
			if (!(newCommand.getHintText().equals("")))
				button.setToolTipText(newCommand.getHintText());

			// Adds the button to the tool bar
			add(button);

			// LISTENER
			button.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see
				 * java.awt.event.ActionListener#actionPerformed(java.awt.event
				 * .ActionEvent)
				 */
				@Override
				public void actionPerformed(ActionEvent actionEvent) {

					JFileChooser fileChooser = new JFileChooser();
					int returnValue = 0;
					
					// ACTION
					switch (newCommand.getParameterType()) {

					case NONE:
						MainWindow.getInstance().getOutputPanel()
								.executeCommand(newCommand.getAction(), "");
						break;
					case TEXT:
						
						// Ask to the user for the text
						String text = JOptionPane.showInputDialog(null, labels.getString("s1009"));
						
						MainWindow.getInstance().getOutputPanel()
						.executeCommand(newCommand.getAction(), text);
						break;
					case FILE:
						
						// Ask to the user for the file
						fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
						returnValue = fileChooser.showOpenDialog(null);

				        if (returnValue == JFileChooser.APPROVE_OPTION) {
				            
				            MainWindow.getInstance().getOutputPanel()
							.executeCommand(newCommand.getAction(), fileChooser.getSelectedFile().getAbsolutePath());
				        }

						break;
					case DIRECTORY:
						
						// Ask to the user for the directory
						fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
						returnValue = fileChooser.showOpenDialog(null);

				        if (returnValue == JFileChooser.APPROVE_OPTION) {
				            
				            MainWindow.getInstance().getOutputPanel()
							.executeCommand(newCommand.getAction(), fileChooser.getSelectedFile().getAbsolutePath());
				        }
				        
						break;
					}

					// Sets the caret position at the end of the text of the
					// output
					MainWindow
							.getInstance()
							.getOutputPanel()
							.getTextComponent()
							.setCaretPosition(
									MainWindow.getInstance().getOutputPanel()
											.getTextComponent().getDocument()
											.getLength());

					// Sets the focus in the OUTPUT
					MainWindow.getInstance().getOutputPanel()
							.getTextComponent().requestFocusInWindow();
				}
			});
		}
		// Updates the log
		AcideLog.getLog().info(labels.getString("s131"));

		return this;
	}
}
