package gui.menu.view;

import gui.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import operations.configuration.MenuConfiguration;
import operations.factory.GUIFactory;

import language.Language;

import properties.PropertiesManager;

/**
 * 
 */
public class ViewMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private final static String SHOW_LOG = "./resources/icons/menu/view/showLog.png";
	/**
	 * 
	 */
	private final static String SHOW_EXPLORER = "./resources/icons/menu/view/showExplorer.png";
	/**
	 * 
	 */
	private final static String SHOW_SHELL_WINDOWS = "./resources/icons/menu/view/showShellWindows.png";
	/**
	 * 
	 */
	private JMenuItem _showLog;
	/**
	 * 
	 */
	private JCheckBoxMenuItem _cbShowBrowser;
	/**
	 * 
	 */
	private JCheckBoxMenuItem _cbShowShellWindow;
	/**
	 * 
	 */
	private int _shellSize;

	/**
	 * Constructor of the class.
	 */
	public ViewMenu(){
		
		// MENU ITEM
		_showLog = new JMenuItem(new ImageIcon(SHOW_LOG));
		_cbShowBrowser = new JCheckBoxMenuItem(new ImageIcon(SHOW_EXPLORER));
		_cbShowBrowser.setSelected(true);		
		_cbShowShellWindow = new JCheckBoxMenuItem(new ImageIcon(SHOW_SHELL_WINDOWS));
		_cbShowShellWindow.setSelected(true);

		setLanguageLabels();
	}

	/**
	 * 
	 */
	public void setLanguageLabels() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		final ResourceBundle labels = language.getLabels();

		// SHOW LOG
		_showLog.setText(labels.getString("s28"));
		_showLog.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		
		// SHOW BROWSER
		_cbShowBrowser.setText(labels.getString("s221"));
		
		// SHOW SHELL WINDOW
		_cbShowShellWindow.setText(labels.getString("s223"));
	}
	
	/**
	 * 
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (MenuConfiguration.getShowLog())
			add(_showLog);
		if (MenuConfiguration.getShowBrowser())
			add(_cbShowBrowser);
		if (MenuConfiguration.getShowShellWindow())
			add(_cbShowShellWindow);
	}
	
	/**
	 * 
	 */
	public void setListeners(){
		
		// SHOW LOG 
		_showLog.addActionListener(new ShowLogListener());
		
		// SHOW BROWSER
		_cbShowBrowser.addActionListener(new ShowBrowserListener());
		
		// SHOW SHELL WINDOWS
		_cbShowShellWindow.addActionListener(new ShowShellWindowListener());
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getShowLog() {
		return _showLog;
	}

	/**
	 * 
	 * @param mostrarLog
	 */
	public void setShowLog(JMenuItem mostrarLog) {
		_showLog = mostrarLog;
	}
	
	/**
	 * 
	 * @return
	 */
	public JCheckBoxMenuItem getShowBrowser() {
		return _cbShowBrowser;
	}

	/**
	 * 
	 * @param showBrowserCBox
	 */
	public void setShowBrowserCBox(JCheckBoxMenuItem showBrowserCBox) {
		_cbShowBrowser = showBrowserCBox;
	}

	/**
	 * 
	 * @return
	 */
	public JCheckBoxMenuItem getShowShellWindow() {
		return _cbShowShellWindow;
	}

	/**
	 * 
	 * @param showShellWindowCBox
	 */
	public void setShowShellWindowCBox(JCheckBoxMenuItem showShellWindowCBox) {
		_cbShowShellWindow = showShellWindowCBox;
	}

	/**
	 * 
	 */
	class ShowLogListener implements ActionListener {
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			GUIFactory.getInstance().buildLogTab();
		}
	}

	/**
	 * 
	 */
	class ShowShellWindowListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			MainWindow mainWindow = MainWindow.getInstance();
			
			if (_cbShowShellWindow.isSelected()) {
				mainWindow.getSplitPaneHorizontal().setDividerLocation(_shellSize);
				mainWindow.getSplitPaneHorizontal().getBottomComponent()
						.setVisible(true);
			} else {
				_shellSize = mainWindow.getSplitPaneHorizontal().getDividerLocation();
				mainWindow.getSplitPaneHorizontal().setDividerLocation(0);
				mainWindow.getSplitPaneHorizontal().getBottomComponent()
						.setVisible(false);
			}
			String prj = null;
			try {
				prj = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
					.getProjectConfiguration().getName().equals(""))) {
				mainWindow.getProjectConfiguration().setIsModified(true);
			}
		}
	}

	/**
	 * 
	 */
	class ShowBrowserListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			MainWindow mainWindow = MainWindow.getInstance();
			if (_cbShowBrowser.isSelected()) {
				mainWindow.getExplorer().showExplorer();
			} else
				mainWindow.getExplorer().disposeExplorer();
			String prj = null;
			try {
				prj = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
					.getProjectConfiguration().getName().equals(""))) {
				mainWindow.getProjectConfiguration().setIsModified(true);
			}
		}
	}
}