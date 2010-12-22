package gui.outputPanel.popup;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import operations.factory.AcideGUIFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

import language.AcideLanguage;

/************************************************************************																
 * Output panel popup menu of ACIDE - A Configurable IDE.										
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
public class AcideOutputPanelPopupMenu extends JPopupMenu {

	/**
	 * Acide output panel popup menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the copy menu item.
	 */
	private final static ImageIcon COPY_IMAGE = new ImageIcon("./resources/icons/menu/edit/copy.png");
	/**
	 * Image file for the paste menu item.
	 */
	private final static ImageIcon PASTE_IMAGE = new ImageIcon("./resources/icons/menu/edit/paste.png");
	/**
	 * image file for the cut menu item.
	 */
	private final static ImageIcon CUT_IMAGE = new ImageIcon("./resources/icons/menu/edit/cut.png");
	/**
	 * Shell display options menu item image icon.
	 */
	private final static ImageIcon SHELL_DISPLAY_OPTIONS_IMAGE = new ImageIcon("./resources/icons/menu/configuration/output/displaySettings.png");
	/**
	 * Copy menu item.
	 */
	private JMenuItem _copy;
	/**
	 * Cut menu item.
	 */
	private JMenuItem _cut;
	/**
	 * Paste menu item.
	 */
	private JMenuItem _paste;
	/**
	 * Shell display options menu item.
	 */
	private JMenuItem _shellDisplayOptions;
	/**
	 * Reset menu item.
	 */
	private JMenuItem _reset;
	/**
	 * Control+C menu item.
	 */
	private JMenuItem _controlC;
	/**
	 * Clear console buffer menu item.
	 */
	private JMenuItem _clearConsoleBuffer;

	/**
	 * Class constructor
	 */
	public AcideOutputPanelPopupMenu() {

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

		// OUTPUT EDITOR
		_shellDisplayOptions = new JMenuItem(labels.getString("s986"), SHELL_DISPLAY_OPTIONS_IMAGE);
		_shellDisplayOptions.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				AcideGUIFactory.getInstance().buildOutputVisualizationOptionsWindow();
			}
		});
		add(_shellDisplayOptions);
		addSeparator();

		// COPY
		_copy = new JMenuItem(labels.getString("s187"), COPY_IMAGE);
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
				MainWindow.getInstance().getOutputPanel().getTextComponent().copy();
			}
		});
		add(_copy);

		// CUT
		_cut = new JMenuItem(labels.getString("s188"), CUT_IMAGE);
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

				if (MainWindow.getInstance().getOutputPanel().getTextComponent()
						.getSelectionStart() >= MainWindow.getInstance()
						.getOutputPanel().getPromptCaretPosition())
					MainWindow.getInstance().getOutputPanel().getTextComponent()
							.cut();
			}
		});
		add(_cut);

		// PASTE
		_paste = new JMenuItem(labels.getString("s189"), PASTE_IMAGE);
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

				if (MainWindow.getInstance().getOutputPanel().getTextComponent()
						.getSelectionStart() >= MainWindow.getInstance()
						.getOutputPanel().getPromptCaretPosition())
					MainWindow.getInstance().getOutputPanel().getTextComponent()
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
				MainWindow.getInstance().getOutputPanel().resetOutput();
			}
		});
		add(_reset);
		
		// CLEAR CONSOLE BUFFER
		_clearConsoleBuffer = new JMenuItem(labels.getString("s999"));
		_clearConsoleBuffer.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getOutputPanel().clearOutputBuffer();
			}
		});
		add(_clearConsoleBuffer);
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
