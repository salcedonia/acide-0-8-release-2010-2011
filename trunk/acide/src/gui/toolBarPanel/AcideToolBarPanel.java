package gui.toolBarPanel;

import gui.toolBarPanel.shellCommandToolBar.ShellCommandToolBar;
import gui.toolBarPanel.staticToolBar.StaticToolBar;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Tool bar panel of ACIDE - A Configurable IDE.
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
 * @see JPanel
 ***********************************************************************/
public class AcideToolBarPanel extends JPanel {

	/**
	 * Acide tool bar panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Main window image icon.
	 */
	private static final ImageIcon LEFT_IMAGE_ICON = new ImageIcon(
			"./resources/icons/toolBar/left.png");
	/**
	 * Main window image icon.
	 */
	private static final ImageIcon RIGHT_IMAGE_ICON = new ImageIcon(
			"./resources/icons/toolBar/right.png");
	/**
	 * Command tool bar.
	 */
	private JToolBar _toolBar;
	/**
	 * Tool bar scroll pane which contains the tool bar itself.
	 */
	private JScrollPane _toolBarScrollPane;
	/**
	 * Button left panel. It is used to encapsulate the button into it
	 * so the programmer can define the preferred size of the button.
	 * Otherwise because of the BorderLayout of the main panel displays
	 * the button without it.
	 */
	private JPanel _buttonLeftPanel;
	/**
	 * Button right panel. It is used to encapsulate the button into it
	 * so the programmer can define the preferred size of the button.
	 * Otherwise because of the BorderLayout of the main panel displays
	 * the button without it.
	 */
	private JPanel _buttonRightPanel;
	/**
	 * Scroll left button.
	 */
	private JButton _scrollLeftButton;
	/**
	 * Scroll right button.
	 */
	private JButton _scrollRightButton;
	/**
	 * Point component X used for calculating the place where the scroll bar
	 * has to be settled down. 
	 */
	private int _pointX;
	/**
	 * Total width of the toolbar menu.
	 */
	private int _totalWidth;
	/**
	 * Point where the scroll bar has to be settled down.
	 */
	private Point point;
	/**
	 * Increment used for the displacement.
	 */
	private final int INCREMENT = 20;

	/**
	 * Creates a new Acide tool bar panel.
	 */
	public AcideToolBarPanel() {

		super(new BorderLayout());

		point = new Point(0, 0);

		// TOOL BAR
		_toolBar = new JToolBar();
		_toolBar.setFloatable(false);
	}

	/**
	 * Creates the components.
	 */
	private void initComponents() {

		// SCROLL LEFT BUTTON
		_scrollLeftButton = new JButton(LEFT_IMAGE_ICON);
		_scrollLeftButton.setPreferredSize(new Dimension(30,32));
		_scrollLeftButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// If is is below the threshold 
				if (_pointX - INCREMENT >= 0)
					
					// Decrements normally
					_pointX -= INCREMENT;
				else
					// Then takes the minimum value
					_pointX = 0;

				// Updates the point
				point.x = _pointX;
				
				// Moves to the calculated point
				_toolBarScrollPane.getViewport().setViewPosition(point);
				
				// Tells to the scroll pane to move there
				_toolBarScrollPane.validate();
			}
		});
		_buttonLeftPanel = new JPanel();
		_buttonLeftPanel.add(_scrollLeftButton);
		add(_buttonLeftPanel, BorderLayout.LINE_START);

		// SCROLL RIGHT BUTTON
		_scrollRightButton = new JButton(RIGHT_IMAGE_ICON);
		_scrollRightButton.setPreferredSize(new Dimension(30,32));
		_scrollRightButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// If it is below the bounds
				if(_pointX + INCREMENT < _toolBarScrollPane.getVisibleRect().width)	
					// Increments normally
					_pointX += INCREMENT;
					
				// Updates the point
				point.x = _pointX;
				
				// Moves to the calculated point
				_toolBarScrollPane.getViewport().setViewPosition(point);
				
				// Tells to the scroll pane to move there
				_toolBarScrollPane.validate();
			}
		});
		_buttonRightPanel = new JPanel();
		_buttonRightPanel.add(_scrollRightButton);
		add(_buttonRightPanel, BorderLayout.LINE_END);
		

		// TOOL BAR SCROLL PANE
		_toolBarScrollPane = new JScrollPane();
		_toolBarScrollPane
				.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
		_toolBarScrollPane
				.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		_toolBarScrollPane.setViewportView(_toolBar);
		add(_toolBarScrollPane, BorderLayout.CENTER);
	}

	/**
	 * Builds the Acide tool bar panel.
	 * 
	 * @return the Acide tool bar panel.
	 */
	public void buildAcideToolBarPanel() {

		// Builds the static tool bar
		addStaticToolBar(StaticToolBar.getInstance().build());

		// Builds the modifiable tool bar
		addShellCommandToolBar(ShellCommandToolBar.getInstance().build());

		// Creates the components
		initComponents();

		// Add the component listener so it can resizes when it changes
		_toolBarScrollPane.addComponentListener(new ComponentAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ComponentListener#componentResized(java.awt.event
			 * .ComponentEvent)
			 */
			@Override
			public void componentResized(ComponentEvent componentEvent) {

				_totalWidth = _toolBar.getBounds().width;
				if (getBounds().width < _totalWidth) {
					// added left/right buttons for side scrolling
					_scrollLeftButton.setVisible(true);
					_scrollRightButton.setVisible(true);
				} else {
					_scrollLeftButton.setVisible(false);
					_scrollRightButton.setVisible(false);
					_pointX = 0;
				}
				validate();
			}
		});
	}

	/**
	 * Adds a shell command given as a parameter to the tool bar.
	 * 
	 * @param shellCommandToolBar
	 *            new shell command to add.
	 */
	public void addShellCommandToolBar(ShellCommandToolBar shellCommandToolBar) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s130"));
		_toolBar.addSeparator();

		// Adds the static buttons
		for (JButton staticButton : shellCommandToolBar)
			_toolBar.add(staticButton);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s125"));
	}

	/**
	 * Adds the static tool bar command given as a parameter to the tool bar.
	 * 
	 * @param staticToolBar
	 *            static tool bar command to add.
	 */
	public void addStaticToolBar(StaticToolBar staticToolBar) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Removes the previous components
		_toolBar.removeAll();

		// Adds the static buttons
		for (JButton staticButton : staticToolBar)
			_toolBar.add(staticButton);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s125"));
	}
}
