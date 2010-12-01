package gui.toolBarPanel;

import java.awt.BorderLayout;

import javax.swing.*;

/************************************************************************																
 * Tool bar panel of ACIDE - A Configurable IDE.										
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
 * @see JPanel																													
 ***********************************************************************/
public class AcideToolBarPanel extends JPanel {

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Static tool bar of the tool bar panel.
	 */
	private StaticToolBar _staticToolBar;
	/**
	 * Shell command tool bar of the tool bar panel.
	 */
	private ShellCommandToolBar _shellCommandToolBar;
	/**
	 * Application command tool bar of the tool bar panel.
	 */
	private ApplicationCommandToolBar _applicationCommandToolBar;
	
	/**
	 * Creates a new ACIDE tool bar panel with the different tool bar types. 
	 */
	public AcideToolBarPanel(){
		
		// Creates the different tool bars
		_staticToolBar = new StaticToolBar();
		_shellCommandToolBar = new ShellCommandToolBar();
		_applicationCommandToolBar = new ApplicationCommandToolBar();
		
		setLayout(new BorderLayout());
		
		// Adds the components to the panel
		add(_staticToolBar, BorderLayout.WEST);
		add(_shellCommandToolBar, BorderLayout.CENTER);
		//add(_applicationCommandToolBar);
	}

	/**
	 * Return the static tool bar.
	 * 
	 * @return the static tool bar.
	 */
	public StaticToolBar getStaticToolBar() {
		return _staticToolBar;
	}

	/**
	 * Sets a new value to the static tool bar.
	 * 
	 * @param staticToolBar new value to set.
	 */
	public void setStaticToolBar(StaticToolBar staticToolBar) {
		_staticToolBar = staticToolBar;
	}

	/**
	 * Returns the shell command tool bar.
	 * 
	 * @return the shell command tool bar.
	 */
	public ShellCommandToolBar getShellCommandToolBar() {
		return _shellCommandToolBar;
	}

	/**
	 * Sets a new value to the shell command tool bar.
	 * 
	 * @param shellCommandToolBar new value to set.
	 */
	public void setShellCommandToolBar(ShellCommandToolBar shellCommandToolBar) {
		_shellCommandToolBar = shellCommandToolBar;
	}

	/**
	 * Returns the application tool bar.
	 * 
	 * @return the application command tool bar.
	 */
	public ApplicationCommandToolBar getApplicationCommandToolBar() {
		return _applicationCommandToolBar;
	}

	/**
	 * Sets a new value to the application command tool bar.
	 * 
	 * @param applicationCommandToolBar new value to set.
	 */
	public void setApplicationCommandToolBar(
			ApplicationCommandToolBar applicationCommandToolBar) {
		_applicationCommandToolBar = applicationCommandToolBar;
	}
}
