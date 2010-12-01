package gui.fileEditor.fileEditorManager.utils.logic.testPlaf;

import gui.fileEditor.fileEditorManager.utils.logic.closeButton.CloseButton;

import java.awt.Container;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.plaf.basic.BasicTabbedPaneUI;
import javax.swing.plaf.basic.BasicTabbedPaneUI.TabbedPaneLayout;

/************************************************************************																
 * Handles the opened editors in the editor builder.											
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
 * @see BasicTabbedPaneUI																													
 ***********************************************************************/
public class TestPlaf extends BasicTabbedPaneUI {

	/**
	 * TestPlafLayout for the tabbedPane.
	 */
	private TestPlafLayout _testPlafLayout;
	/**
	 * Array for close buttons of the tabs in the editor.
	 */
	private static ArrayList<CloseButton> _closeButtons = new ArrayList<CloseButton>();
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#createLayoutManager()
	 */
	@Override
	protected LayoutManager createLayoutManager() {

		_testPlafLayout = new TestPlafLayout();
		return _testPlafLayout;
	}

	/**
	 * Returns the close button from the list at the position given as a
	 * parameter.
	 * 
	 * @param position
	 *            position of the close button.
	 * 
	 * @return the close button from the list at the position given as a
	 *         parameter.
	 */
	public CloseButton getCloseButtonAt(int position) {
		return _testPlafLayout.getCloseButtonAt(position);
	}

	/**
	 * Returns the close button list.
	 * 
	 * @return the close button list.
	 */
	public ArrayList<CloseButton> getCloseButtons() {
		return _testPlafLayout.getCloseButtons();
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#getTabInsets(int, int)
	 */
	@Override
	protected Insets getTabInsets(int tabPlacement, int tabIndex) {

		Insets defaultInsets = (Insets) super.getTabInsets(tabPlacement,
				tabIndex).clone();

		defaultInsets.right += 40;
		defaultInsets.top += 4;
		defaultInsets.bottom += 4;

		return defaultInsets;
	}

	/************************************************************************																
	 * Handles the tab layout.
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
	 * @see TabbedPaneLayout																													
	 ***********************************************************************/
	class TestPlafLayout extends TabbedPaneLayout {

		/**
		 * Returns the close button from the list at the position given as a
		 * parameter.
		 * 
		 * @param position
		 *            Position of the close button in the list.
		 * 
		 * @return the close button from the list at the position given as a
		 *         parameter.
		 */
		public CloseButton getCloseButtonAt(int position) {
			return (CloseButton) _closeButtons.get(position);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.LayoutManager#layoutContainer(java.awt.Container)
		 */
		@Override
		public void layoutContainer(Container parent) {

			super.layoutContainer(parent);

			while (tabPane.getTabCount() > _closeButtons.size())
				_closeButtons.add(new CloseButton(_closeButtons.size()));

			Rectangle rectangle = new Rectangle();
			int i;

			for (i = 0; i < tabPane.getTabCount(); i++) {

				rectangle = getTabBounds(i, rectangle);
				JButton closeButton = (JButton) _closeButtons.get(i);
				closeButton.setLocation(rectangle.x + rectangle.width - 20,
						rectangle.y + 5);
				closeButton.setSize(15, 15);
				tabPane.add(closeButton);
			}

			for (; i < _closeButtons.size(); i++) {
				tabPane.remove((JButton) _closeButtons.get(i));
				_closeButtons.remove(i);
			}
		}

		/**
		 * Returns the close button list.
		 * 
		 * @return The close button list.
		 */
		public ArrayList<CloseButton> getCloseButtons() {
			return _closeButtons;
		}
	}
}
