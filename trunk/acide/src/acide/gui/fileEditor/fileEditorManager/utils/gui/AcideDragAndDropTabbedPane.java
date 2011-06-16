/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package acide.gui.fileEditor.fileEditorManager.utils.gui;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE drag and drop tabbed pane.
 * 
 * @version 0.8
 * @see JTabbedPane
 */
public class AcideDragAndDropTabbedPane extends JTabbedPane {

	/**
	 * ACIDE - A Configurable IDE drag and drop tabbed pane class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE drag and drop tabbed pane line width.
	 */
	private static final int LINE_WIDTH = 3;
	/**
	 * ACIDE - A Configurable IDE drag and drop tabbed pane name.
	 */
	private static final String NAME = "test";
	/**
	 * ACIDE - A Configurable IDE drag and drop tabbed pane ghost glass pane.
	 */
	private final GhostGlassPane _ghostGlassPane = new GhostGlassPane();
	/**
	 * ACIDE - A Configurable IDE drag and drop tabbed pane rectangle pane.
	 */
	private final Rectangle2D _lineRectangle = new Rectangle2D.Double();
	/**
	 * ACIDE - A Configurable IDE drag and drop tabbed pane line color.
	 */
	private final Color _lineColor = new Color(0, 100, 255);
	/**
	 * ACIDE - A Configurable IDE drag and drop tabbed pane drag tab index.
	 */
	private int _dragTabIndex = -1;
	/**
	 * ACIDE - A Configurable IDE drag and drop tabbed pane icon.
	 */
	private ImageIcon _icon;
	/**
	 * Indicates if it has ghost glass panel or not.
	 */
	private boolean _hasGhost = true;

	/**
	 * Creates a new ACIDE - A Configurable IDE drag and drop tabbed pane.
	 */
	public AcideDragAndDropTabbedPane() {

		super();

		// Creates a new drag source listener
		final DragSourceListener dragSourceListener = new DragSourceListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.dnd.DragSourceListener#dragEnter(java.awt.dnd.
			 * DragSourceDragEvent)
			 */
			@Override
			public void dragEnter(DragSourceDragEvent dragSourceDragEvent) {
				dragSourceDragEvent.getDragSourceContext().setCursor(
						DragSource.DefaultMoveDrop);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.dnd.DragSourceListener#dragExit(java.awt.dnd.DragSourceEvent
			 * )
			 */
			@Override
			public void dragExit(DragSourceEvent dragSourceEvent) {
				dragSourceEvent.getDragSourceContext().setCursor(
						DragSource.DefaultMoveNoDrop);
				_lineRectangle.setRect(0, 0, 0, 0);
				_ghostGlassPane.setLocation(new Point(-1000, -1000));
				_ghostGlassPane.repaint();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.dnd.DragSourceListener#dragOver(java.awt.dnd.
			 * DragSourceDragEvent)
			 */
			@Override
			public void dragOver(DragSourceDragEvent dragSourceDragEvent) {

				// This method returns a Point indicating the cursor location in
				// screen coordinates at the moment
				Point tabPt = dragSourceDragEvent.getLocation();
				SwingUtilities.convertPointFromScreen(tabPt,
						AcideDragAndDropTabbedPane.this);
				Point glassPt = dragSourceDragEvent.getLocation();
				SwingUtilities.convertPointFromScreen(glassPt, _ghostGlassPane);
				int targetIdx = getTargetTabIndex(glassPt);
				if (getTabAreaBound().contains(tabPt) && targetIdx >= 0
						&& targetIdx != _dragTabIndex
						&& targetIdx != _dragTabIndex + 1) {
					dragSourceDragEvent.getDragSourceContext().setCursor(
							DragSource.DefaultMoveDrop);
				} else {
					dragSourceDragEvent.getDragSourceContext().setCursor(
							DragSource.DefaultMoveNoDrop);
				}
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.dnd.DragSourceListener#dragDropEnd(java.awt.dnd.
			 * DragSourceDropEvent)
			 */
			public void dragDropEnd(DragSourceDropEvent dragSourceDropEvent) {
				_lineRectangle.setRect(0, 0, 0, 0);
				_dragTabIndex = -1;
				if (hasGhost()) {
					_ghostGlassPane.setVisible(false);
					_ghostGlassPane.setImage(null);
				}
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.dnd.DragSourceListener#dropActionChanged(java.awt.dnd
			 * .DragSourceDragEvent)
			 */
			@Override
			public void dropActionChanged(
					DragSourceDragEvent dragSourceDragEvent) {
			}
		};

		// Creates a new transferable item
		final Transferable t = new Transferable() {

			/**
			 * Transferable flavour.
			 */
			private final DataFlavor FLAVOR = new DataFlavor(
					DataFlavor.javaJVMLocalObjectMimeType, NAME);

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.datatransfer.Transferable#getTransferData(java.awt.
			 * datatransfer.DataFlavor)
			 */
			@Override
			public Object getTransferData(DataFlavor dataFlavor) {
				return AcideDragAndDropTabbedPane.this;
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.datatransfer.Transferable#getTransferDataFlavors()
			 */
			@Override
			public DataFlavor[] getTransferDataFlavors() {
				DataFlavor[] f = new DataFlavor[1];
				f[0] = this.FLAVOR;
				return f;
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.datatransfer.Transferable#isDataFlavorSupported(java
			 * .awt.datatransfer.DataFlavor)
			 */
			@Override
			public boolean isDataFlavorSupported(DataFlavor flavor) {
				return flavor.getHumanPresentableName().equals(NAME);
			}
		};

		// Creates a new drag gesture listener
		final DragGestureListener dragGestureListener = new DragGestureListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.dnd.DragGestureListener#dragGestureRecognized(java.awt
			 * .dnd.DragGestureEvent)
			 */
			@Override
			public void dragGestureRecognized(DragGestureEvent dragGestureEvent) {

				Point tabPoint = dragGestureEvent.getDragOrigin();
				_dragTabIndex = indexAtLocation(tabPoint.x, tabPoint.y);

				if (_dragTabIndex < 0)
					return;

				initGlassPane(dragGestureEvent.getComponent(),
						dragGestureEvent.getDragOrigin());
				try {
					dragGestureEvent.startDrag(DragSource.DefaultMoveDrop, t,
							dragSourceListener);
				} catch (InvalidDnDOperationException exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
			}
		};

		// Creates a new drop target
		new DropTarget(_ghostGlassPane, DnDConstants.ACTION_COPY_OR_MOVE,
				new AcideDropTargetListener(), true);

		// Creates a new drag source
		new DragSource().createDefaultDragGestureRecognizer(this,
				DnDConstants.ACTION_COPY_OR_MOVE, dragGestureListener);
	}

	/**
	 * ACIDE - A Configurable IDE drop target listener.
	 * 
	 * @version 0.8
	 * @see DropTargetListener
	 */
	class AcideDropTargetListener implements DropTargetListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.dnd.DropTargetListener#dragEnter(java.awt.dnd.
		 * DropTargetDragEvent)
		 */
		@Override
		public void dragEnter(DropTargetDragEvent dropTargetDragEvent) {

			if (isDragAcceptable(dropTargetDragEvent))
				dropTargetDragEvent.acceptDrag(dropTargetDragEvent
						.getDropAction());
			else
				dropTargetDragEvent.rejectDrag();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.dnd.DropTargetListener#dragExit(java.awt.dnd.DropTargetEvent
		 * )
		 */
		@Override
		public void dragExit(DropTargetEvent dropTargetEvent) {
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.dnd.DropTargetListener#dropActionChanged(java.awt.dnd.
		 * DropTargetDragEvent)
		 */
		@Override
		public void dropActionChanged(DropTargetDragEvent dropTargetEvent) {
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.dnd.DropTargetListener#dragOver(java.awt.dnd.DropTargetDragEvent
		 * )
		 */
		@Override
		public void dragOver(final DropTargetDragEvent dropTargetEvent) {

			if (getTabPlacement() == JTabbedPane.TOP
					|| getTabPlacement() == JTabbedPane.BOTTOM) {
				initTargetLeftRightLine(getTargetTabIndex(dropTargetEvent
						.getLocation()));
			} else {
				initTargetTopBottomLine(getTargetTabIndex(dropTargetEvent
						.getLocation()));
			}
			repaint();
			if (hasGhost()) {
				_ghostGlassPane.setLocation(dropTargetEvent.getLocation());
				_ghostGlassPane.repaint();
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.dnd.DropTargetListener#drop(java.awt.dnd.DropTargetDropEvent
		 * )
		 */
		@Override
		public void drop(DropTargetDropEvent dropTargetEvent) {

			if (isDropAcceptable(dropTargetEvent)) {
				convertTab(_dragTabIndex,
						getTargetTabIndex(dropTargetEvent.getLocation()));
				dropTargetEvent.dropComplete(true);
			} else {
				dropTargetEvent.dropComplete(false);
			}
			repaint();
		}

		/**
		 * 
		 * @param dropTargetEvent
		 * @return
		 */
		public boolean isDragAcceptable(DropTargetDragEvent dropTargetEvent) {
			Transferable t = dropTargetEvent.getTransferable();
			if (t == null)
				return false;
			DataFlavor[] f = dropTargetEvent.getCurrentDataFlavors();
			if (t.isDataFlavorSupported(f[0]) && _dragTabIndex >= 0) {
				return true;
			}
			return false;
		}

		/**
		 * 
		 * @param dropTargetEvent
		 * @return
		 */
		public boolean isDropAcceptable(DropTargetDropEvent dropTargetEvent) {
			Transferable t = dropTargetEvent.getTransferable();
			if (t == null)
				return false;
			DataFlavor[] f = t.getTransferDataFlavors();
			if (t.isDataFlavorSupported(f[0]) && _dragTabIndex >= 0) {
				return true;
			}
			return false;
		}
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE icon.
	 * 
	 * @param icon
	 *            new value to set.
	 */
	public void setIcon(ImageIcon icon) {
		_icon = icon;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE has ghost flag.
	 * 
	 * @param hasGhost
	 *            new value to set.
	 */
	public void setPaintGhost(boolean hasGhost) {
		_hasGhost = hasGhost;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE has ghost flag.
	 * 
	 * @return the ACIDE - A Configurable IDE has ghost flag.
	 */
	public boolean hasGhost() {
		return _hasGhost;
	}

	/**
	 * Returns the target tab index.
	 * 
	 * @param ghostGlassPoint
	 *            ghost glass point.
	 * 
	 * @return the target tab index.
	 */
	private int getTargetTabIndex(Point ghostGlassPoint) {

		// Gets the tab point
		Point tabPoint = SwingUtilities.convertPoint(_ghostGlassPane,
				ghostGlassPoint, AcideDragAndDropTabbedPane.this);

		// Checks if the tab placement is top or bottom
		boolean isTopOrBottom = getTabPlacement() == JTabbedPane.TOP
				|| getTabPlacement() == JTabbedPane.BOTTOM;

		for (int index = 0; index < getTabCount(); index++) {

			// Gets the rectangle of the tab
			Rectangle rectangle = getBoundsAt(index);

			if (isTopOrBottom)
				rectangle.setRect(rectangle.x - rectangle.width / 2,
						rectangle.y, rectangle.width, rectangle.height);
			else
				rectangle.setRect(rectangle.x, rectangle.y - rectangle.height
						/ 2, rectangle.width, rectangle.height);

			if (rectangle.contains(tabPoint))
				return index;
		}

		// Gets the rectangle from the last tab
		Rectangle rectangle = getBoundsAt(getTabCount() - 1);

		if (isTopOrBottom)
			rectangle.setRect(rectangle.x + rectangle.width / 2, rectangle.y,
					rectangle.width, rectangle.height);
		else
			rectangle.setRect(rectangle.x, rectangle.y + rectangle.height / 2,
					rectangle.width, rectangle.height);

		return rectangle.contains(tabPoint) ? getTabCount() : -1;
	}

	/**
	 * Converts the dragged tab.
	 * 
	 * @param previousIndex
	 *            previous tab index.
	 * @param nextIndex
	 *            next tab index.
	 */
	private void convertTab(int previousIndex, int nextIndex) {

		// Checks the bounds
		if (nextIndex < 0 || previousIndex == nextIndex) {
			return;
		}

		// Gets the component
		Component component = getComponentAt(previousIndex);

		// Gets the previous title
		String previousTitle = getTitleAt(previousIndex);

		// If the next index is the last one
		if (nextIndex == getTabCount()) {
			
			// Removes the previous index
			remove(previousIndex);
			
			// Adds the tab
			addTab(previousTitle, _icon, component);
			
			// Sets the selected index
			setSelectedIndex(getTabCount() - 1);
		} else if (previousIndex > nextIndex) {
			
			// Removes the previous index
			remove(previousIndex);
			
			// Inserts the tab
			insertTab(previousTitle, _icon, component, null, nextIndex);
			
			// Sets the selected index
			setSelectedIndex(nextIndex);
		} else {
			
			// Removes the previous index
			remove(previousIndex);
			
			// Inserts the tab
			insertTab(previousTitle, _icon, component, null, nextIndex - 1);
			
			// Sets the selected index
			setSelectedIndex(nextIndex - 1);
		}
		
		// Repaints the component
		component.repaint();
	}

	/**
	 * 
	 * @param next
	 */
	private void initTargetLeftRightLine(int next) {
		if (next < 0 || _dragTabIndex == next || next - _dragTabIndex == 1) {
			_lineRectangle.setRect(0, 0, 0, 0);
		} else if (next == getTabCount()) {
			Rectangle rect = getBoundsAt(getTabCount() - 1);
			_lineRectangle.setRect(rect.x + rect.width - LINE_WIDTH / 2,
					rect.y, LINE_WIDTH, rect.height);
		} else if (next == 0) {
			Rectangle rect = getBoundsAt(0);
			_lineRectangle.setRect(-LINE_WIDTH / 2, rect.y, LINE_WIDTH,
					rect.height);
		} else {
			Rectangle rect = getBoundsAt(next - 1);
			_lineRectangle.setRect(rect.x + rect.width - LINE_WIDTH / 2,
					rect.y, LINE_WIDTH, rect.height);
		}
	}

	/**
	 * Initializes the target top bottom line.
	 * 
	 * @param nextIndex new tab index.
	 */
	private void initTargetTopBottomLine(int nextIndex) {
		if (nextIndex < 0 || _dragTabIndex == nextIndex || nextIndex - _dragTabIndex == 1) {
			_lineRectangle.setRect(0, 0, 0, 0);
		} else if (nextIndex == getTabCount()) {
			Rectangle rectangle = getBoundsAt(getTabCount() - 1);
			_lineRectangle.setRect(rectangle.x, rectangle.y + rectangle.height - LINE_WIDTH
					/ 2, rectangle.width, LINE_WIDTH);
		} else if (nextIndex == 0) {
			Rectangle rectangle = getBoundsAt(0);
			_lineRectangle.setRect(rectangle.x, -LINE_WIDTH / 2, rectangle.width,
					LINE_WIDTH);
		} else {
			Rectangle rectangle = getBoundsAt(nextIndex - 1);
			_lineRectangle.setRect(rectangle.x, rectangle.y + rectangle.height - LINE_WIDTH
					/ 2, rectangle.width, LINE_WIDTH);
		}
	}

	/**
	 * Initializes the glass pane.
	 * 
	 * @param component tab component.
	 * @param tabPoint tab point.
	 */
	private void initGlassPane(Component component, Point tabPoint) {

		// Sets the glass pane
		getRootPane().setGlassPane(_ghostGlassPane);
		
		if (hasGhost()) {
			
			// Gets the bounds
			Rectangle rectangle = getBoundsAt(_dragTabIndex);
			BufferedImage image = new BufferedImage(component.getWidth(),
					component.getHeight(), BufferedImage.TYPE_INT_ARGB);
			Graphics g = image.getGraphics();
			component.paint(g);
			image = image.getSubimage(rectangle.x, rectangle.y, rectangle.width, rectangle.height);
			image.getSubimage(28, 4, 18, 18);
			_ghostGlassPane.setImage(image);
		}
		
		// Gets the glass panel point
		Point glassPanelPoint = SwingUtilities.convertPoint(component, tabPoint, _ghostGlassPane);
		
		// Sets the ghost glass panel point
		_ghostGlassPane.setLocation(glassPanelPoint);
		
		// Sets the ghost glass pane visible
		_ghostGlassPane.setVisible(true);
	}

	/**
	 * Returns the tab area bound.
	 * 
	 * @return the tab area bound.
	 */
	private Rectangle getTabAreaBound() {
		
		// Gets the last tab bounds
		Rectangle lastTab = getUI().getTabBounds(this, getTabCount() - 1);
		
		// Parses it to a rectangle
		return new Rectangle(0, 0, getWidth(), lastTab.y + lastTab.height);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		if (_dragTabIndex >= 0) {
			Graphics2D g2 = (Graphics2D) g;
			g2.setPaint(_lineColor);
			g2.fill(_lineRectangle);
			// Rectangle rect = getBoundsAt(_dragTabIndex);
			// image = image.getSubimage(rect.x, rect.y, rect.width,
			// rect.height);
			// g2.drawImage(image,rect.x,rect.y, null);
		}
	}
}

/**
 * ACIDE - A Configurable IDE ghost glass panel.
 * 
 * @version 0.8
 * @see JPanel
 */
class GhostGlassPane extends JPanel {

	/**
	 * ACIDE - A Configurable IDE ghost glass panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE ghost glass panel panel composite.
	 */
	private final AlphaComposite _composite;
	/**
	 * ACIDE - A Configurable IDE ghost glass panel location.
	 */
	private Point _location = new Point(0, 0);
	/**
	 * ACIDE - A Configurable IDE ghost glass panel buffered image dragging
	 * ghost.
	 */
	private BufferedImage _draggingGhost = null;

	/**
	 * Creates a new ACIDE - A Configurable IDE ghost glass panel.
	 */
	public GhostGlassPane() {
		setOpaque(false);
		_composite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f);
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE ghost glass panel image.
	 * 
	 * @param draggingGhost new value to set.
	 */
	public void setImage(BufferedImage draggingGhost) {
		_draggingGhost = draggingGhost;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE ghost glass panel location.
	 * 
	 * @param location
	 */
	public void setLocation(Point location) {
		_location = location;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	@Override
	public void paintComponent(Graphics g) {
		if (_draggingGhost == null)
			return;
		Graphics2D g2 = (Graphics2D) g;
		g2.setComposite(_composite);
		double xx = _location.getX() - (_draggingGhost.getWidth(this) / 2d);
		double yy = _location.getY() - (_draggingGhost.getHeight(this) / 2d);
		g2.drawImage(_draggingGhost, (int) xx, (int) yy, null);
	}
}