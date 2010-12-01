package gui.fileEditor.fileEditorManager.utils.logic.closeButton;

import gui.fileEditor.fileEditorManager.utils.logic.closeButton.listeners.CloseButtonActionListener;

import java.awt.Insets;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.plaf.UIResource;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Implements used UIResource when the close button is added to the
 * TabbedPane.
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
 * @see UIResource																													
 ***********************************************************************/
public class CloseButton extends JButton implements UIResource {

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Red icon for the closing button.
	 */
	private static final String RED_ICON = "./resources/icons/editor/closeModified.png";
	/**
	 * Green icon for the closing button.
	 */
	private static final String GREEN_ICON = "./resources/icons/editor/closeNotModified.png";
	/**
	 * Path of the displayed icon.
	 */
	private String _selectedIcon = "";

	/**
	 * Creates a new close button.
	 * 
	 * @param index
	 *            editor index.
	 */
	public CloseButton(int index) {

		super(new CloseButtonActionListener(index));
		
		// Sets the green icon
		_selectedIcon = GREEN_ICON;
		setIcon(new ImageIcon(_selectedIcon));
		
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
		
		// Sets the tool tip text
		setToolTipText(labels.getString("s316"));
		
		setMargin(new Insets(0, 0, 0, 0));
		validate();
	}

	/**
	 * Sets the color of the button to red.
	 */
	public void setRedCloseButton() {
		_selectedIcon = RED_ICON;
		setIcon(new ImageIcon(RED_ICON));
	}

	/**
	 * Sets the color of the button to green.
	 */
	public void setGreenCloseButton() {
		_selectedIcon = GREEN_ICON;
		setIcon(new ImageIcon(GREEN_ICON));
	}

	/**
	 * Returns true if the button is red and false in other case.
	 * 
	 * @return true if the button is red and false in other case.
	 */
	public boolean isRedButton() {
		return _selectedIcon.matches(RED_ICON);
	}
}
