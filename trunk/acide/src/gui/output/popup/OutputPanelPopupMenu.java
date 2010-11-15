package gui.output.popup;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import operations.factory.GUIFactory;
import operations.log.Log;

import language.Language;
import properties.PropertiesManager;

/************************************************************************																
 * Output panel popup menu of ACIDE - A Configurable IDE											
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
 * @see JPopupMenu	
 * @see JMenuItem																													
 ***********************************************************************/
public class OutputPanelPopupMenu extends JPopupMenu {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the copy menu item
	 */
	private final static String COPY = "./resources/icons/menu/edit/copy.png";
	/**
	 * Image file for the paste menu item
	 */
	private final static String PASTE = "./resources/icons/menu/edit/paste.png";
	/**
	 * image file for the cut menu item
	 */
	private final static String CUT = "./resources/icons/menu/edit/cut.png";
	/**
	 * Copy menu item
	 */
	private JMenuItem _copy;
	/**
	 * Cut menu item
	 */
	private JMenuItem _cut;
	/**
	 * Paste menu item
	 */
	private JMenuItem _paste;
	/**
	 * Output visualization settings menu item
	 */
	private JMenuItem _visualizationSettings;
	/**
	 * Reset menu item
	 */
	private JMenuItem _reset;
	/**
	 * Control+C menu item
	 */
	private JMenuItem _controlC;

	/**
	 * Class constructor
	 */
	public OutputPanelPopupMenu() {

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

		// OUTPUT EDITOR
		_visualizationSettings = new JMenuItem(labels.getString("s986"));
		_visualizationSettings.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				GUIFactory.getInstance().buildOutputVisualizationOptionsWindow();
			}
		});
		add(_visualizationSettings);
		addSeparator();

		// COPY
		_copy = new JMenuItem(labels.getString("s187"), new ImageIcon(COPY));
		_copy.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getOutput().getTextComponent().copy();
			}
		});
		add(_copy);

		// CUT
		_cut = new JMenuItem(labels.getString("s188"), new ImageIcon(CUT));
		_cut.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				if (MainWindow.getInstance().getOutput().getTextComponent()
						.getSelectionStart() >= MainWindow.getInstance()
						.getOutput().getPromptCaretPosition())
					MainWindow.getInstance().getOutput().getTextComponent()
							.cut();
			}
		});
		add(_cut);

		// PASTE
		_paste = new JMenuItem(labels.getString("s189"), new ImageIcon(PASTE));
		_paste.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				if (MainWindow.getInstance().getOutput().getTextComponent()
						.getSelectionStart() >= MainWindow.getInstance()
						.getOutput().getPromptCaretPosition())
					MainWindow.getInstance().getOutput().getTextComponent()
							.paste();
			}
		});
		add(_paste);
		addSeparator();

		// CONTROL + C
		_controlC = new JMenuItem("Ctrl-C");
		_controlC.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// SEND THE CTRL-C TO THE OUTPUT WRITER
				//MainWindow.getInstance().getOutput()
					//	.sendCommandToOutput(Character.toString((char) 3));
			}
		});
		add(_controlC);

		// RESET
		_reset = new JMenuItem(labels.getString("s987"));
		_reset.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getOutput().resetOutput();
			}
		});
		add(_reset);
	}

	/**
	 * Returns the copy menu item
	 * 
	 * @return the copy menu item
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Returns the copy menu item
	 * 
	 * @return the copy menu item
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Returns the paste menu item
	 * 
	 * @return the paste menu item
	 */
	public JMenuItem getPaste() {
		return _paste;
	}
}
