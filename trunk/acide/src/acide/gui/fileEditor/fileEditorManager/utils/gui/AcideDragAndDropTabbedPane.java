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
 * Drag and drop tabbed pane for the editor pane of ACIDE - A Configurable IDE.
 * 
 * @version 0.8
 * @see JTabbedPane
 */
public class AcideDragAndDropTabbedPane extends JTabbedPane {

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Line width.
	 */
	private static final int LINE_WIDTH = 3;
	/**
	 * Name.
	 */
	private static final String NAME = "test";
	/**
	 * Ghost glass pane.
	 */
	private final GhostGlassPane _ghostGlassPane = new GhostGlassPane();
	/**
	 * Rectangle pane.
	 */
	private final Rectangle2D _lineRectangle = new Rectangle2D.Double();
	/**
	 * Line color.
	 */
	private final Color _lineColor = new Color(0, 100, 255);
	/**
	 * Drag tab index.
	 */
	private int _dragTabIndex = -1;
	/**
	 * Icon.
	 */
	private ImageIcon _icon;

	/**
	 * Class constructor
	 */
	public AcideDragAndDropTabbedPane() {

		super();

		final DragSourceListener dsl = new DragSourceListener() {
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
				_ghostGlassPane.setPoint(new Point(-1000, -1000));
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
			public void dragDropEnd(DragSourceDropEvent e) {
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

		final Transferable t = new Transferable() {

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

		final DragGestureListener dgl = new DragGestureListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.dnd.DragGestureListener#dragGestureRecognized(java.awt
			 * .dnd.DragGestureEvent)
			 */
			@Override
			public void dragGestureRecognized(DragGestureEvent dragGestureEvent) {

				Point tabPt = dragGestureEvent.getDragOrigin();
				_dragTabIndex = indexAtLocation(tabPt.x, tabPt.y);

				if (_dragTabIndex < 0)
					return;

				initGlassPane(dragGestureEvent.getComponent(),
						dragGestureEvent.getDragOrigin());
				try {
					dragGestureEvent.startDrag(DragSource.DefaultMoveDrop, t,
							dsl);
				} catch (InvalidDnDOperationException exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
			}
		};

		new DropTarget(_ghostGlassPane, DnDConstants.ACTION_COPY_OR_MOVE,
				new CDropTargetListener(), true);
		new DragSource().createDefaultDragGestureRecognizer(this,
				DnDConstants.ACTION_COPY_OR_MOVE, dgl);
	}

	/**
	 * 
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class CDropTargetListener implements DropTargetListener {

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
				_ghostGlassPane.setPoint(dropTargetEvent.getLocation());
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
	 * Indicates if hast ghost glass panel or not
	 */
	private boolean hasGhost = true;

	/**
	 * 
	 * @param flag
	 */
	public void setPaintGhost(boolean flag) {
		hasGhost = flag;
	}

	/**
	 * 
	 * @return
	 */
	public boolean hasGhost() {
		return hasGhost;
	}

	/**
	 * 
	 * @param glassPt
	 * @return
	 */
	private int getTargetTabIndex(Point glassPt) {
		Point tabPt = SwingUtilities.convertPoint(_ghostGlassPane, glassPt,
				AcideDragAndDropTabbedPane.this);
		boolean isTB = getTabPlacement() == JTabbedPane.TOP
				|| getTabPlacement() == JTabbedPane.BOTTOM;
		for (int i = 0; i < getTabCount(); i++) {
			Rectangle r = getBoundsAt(i);
			if (isTB)
				r.setRect(r.x - r.width / 2, r.y, r.width, r.height);
			else
				r.setRect(r.x, r.y - r.height / 2, r.width, r.height);
			if (r.contains(tabPt))
				return i;
		}
		Rectangle r = getBoundsAt(getTabCount() - 1);
		if (isTB)
			r.setRect(r.x + r.width / 2, r.y, r.width, r.height);
		else
			r.setRect(r.x, r.y + r.height / 2, r.width, r.height);
		return r.contains(tabPt) ? getTabCount() : -1;
	}

	/**
	 * 
	 * @param prev
	 * @param next
	 */
	private void convertTab(int prev, int next) {
		if (next < 0 || prev == next) {
			return;
		}
		Component cmp = getComponentAt(prev);
		String str = getTitleAt(prev);
		if (next == getTabCount()) {
			remove(prev);
			addTab(str, _icon, cmp);
			setSelectedIndex(getTabCount() - 1);
		} else if (prev > next) {
			// System.out.println(" >: press="+prev+" next="+next);
			remove(prev);
			insertTab(str, _icon, cmp, null, next);
			setSelectedIndex(next);
		} else {
			remove(prev);
			insertTab(str, _icon, cmp, null, next - 1);
			setSelectedIndex(next - 1);
		}
		cmp.repaint();
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
	 * 
	 * @param next
	 */
	private void initTargetTopBottomLine(int next) {
		if (next < 0 || _dragTabIndex == next || next - _dragTabIndex == 1) {
			_lineRectangle.setRect(0, 0, 0, 0);
		} else if (next == getTabCount()) {
			Rectangle rect = getBoundsAt(getTabCount() - 1);
			_lineRectangle.setRect(rect.x, rect.y + rect.height - LINE_WIDTH
					/ 2, rect.width, LINE_WIDTH);
		} else if (next == 0) {
			Rectangle rect = getBoundsAt(0);
			_lineRectangle.setRect(rect.x, -LINE_WIDTH / 2, rect.width,
					LINE_WIDTH);
		} else {
			Rectangle rect = getBoundsAt(next - 1);
			_lineRectangle.setRect(rect.x, rect.y + rect.height - LINE_WIDTH
					/ 2, rect.width, LINE_WIDTH);
		}
	}

	/**
	 * 
	 * @param c
	 * @param tabPt
	 */
	private void initGlassPane(Component c, Point tabPt) {

		getRootPane().setGlassPane(_ghostGlassPane);
		if (hasGhost()) {
			Rectangle rect = getBoundsAt(_dragTabIndex);
			BufferedImage image = new BufferedImage(c.getWidth(),
					c.getHeight(), BufferedImage.TYPE_INT_ARGB);
			Graphics g = image.getGraphics();
			c.paint(g);
			image = image.getSubimage(rect.x, rect.y, rect.width, rect.height);
			image.getSubimage(28, 4, 18, 18);
			_ghostGlassPane.setImage(image);
		}
		Point glassPt = SwingUtilities.convertPoint(c, tabPt, _ghostGlassPane);
		_ghostGlassPane.setPoint(glassPt);
		_ghostGlassPane.setVisible(true);
	}

	/**
	 * 
	 * @return
	 */
	private Rectangle getTabAreaBound() {
		Rectangle lastTab = getUI().getTabBounds(this, getTabCount() - 1);
		return new Rectangle(0, 0, getWidth(), lastTab.y + lastTab.height);
	}

	/**
	 * 
	 */
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

	/**
	 * 
	 * @param i
	 */
	public void setIcon(ImageIcon i) {
		_icon = i;
	}
}

/**
 * 
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class GhostGlassPane extends JPanel {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Panel composite
	 */
	private final AlphaComposite _composite;
	/**
	 * Location
	 */
	private Point _location = new Point(0, 0);
	/**
	 * Buffered image dragging ghost
	 */
	private BufferedImage _draggingGhost = null;

	/**
	 * Class constructor
	 */
	public GhostGlassPane() {
		setOpaque(false);
		_composite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f);
	}

	/**
	 * 
	 * @param draggingGhost
	 */
	public void setImage(BufferedImage draggingGhost) {
		_draggingGhost = draggingGhost;
	}

	/**
	 * 
	 * @param location
	 */
	public void setPoint(Point location) {
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