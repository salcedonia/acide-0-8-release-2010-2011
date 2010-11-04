package gui.menu.view;

import es.configuration.menu.MenuConfiguration;
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

import operations.factory.GUIFactory;

import language.Language;

import properties.PropertiesManager;

/**
 * View menu of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
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
	 * @param showLog
	 */
	public void setShowLog(JMenuItem showLog) {
		_showLog = showLog;
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
	 * Show log listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
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
	 * Show shell window listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ShowShellWindowListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			if (_cbShowShellWindow.isSelected()) {
				MainWindow.getInstance().getSplitPaneHorizontal().setDividerLocation(_shellSize);
				MainWindow.getInstance().getSplitPaneHorizontal().getBottomComponent()
						.setVisible(true);
			} else {
				_shellSize = MainWindow.getInstance().getSplitPaneHorizontal().getDividerLocation();
				MainWindow.getInstance().getSplitPaneHorizontal().setDividerLocation(0);
				MainWindow.getInstance().getSplitPaneHorizontal().getBottomComponent()
						.setVisible(false);
			}
			
			// NOT DEFAULT PROJECT
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
				
				// THE PROJECT HAS BEEN MODIFIED
				MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
		}
	}

	/**
	 * Show browser listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ShowBrowserListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			if (_cbShowBrowser.isSelected())
				MainWindow.getInstance().getExplorer().showExplorer();
			else
				MainWindow.getInstance().getExplorer().disposeExplorer();

			// NOT DEFAULT PROJECT
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
				
				// THE PROJECT HAS BEEN MODIFIED
				MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
		}
	}
}