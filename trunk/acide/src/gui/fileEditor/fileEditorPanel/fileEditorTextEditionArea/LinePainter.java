package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import operations.log.AcideLog;

/************************************************************************
 * Track the movement of the Caret by painting a background line at the current
 * caret position.
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
 * @see JComponent
 ***********************************************************************/
public class LinePainter extends JComponent implements
		Highlighter.HighlightPainter, CaretListener, MouseListener,
		MouseMotionListener {

	/**
	 * Line painter class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Line painter text component.
	 */
	private JTextComponent _textComponent;
	/**
	 * Line painter color.
	 */
	private Color _color;
	/**
	 * Line painter last view.
	 */
	private Rectangle _lastView;

	/**
	 * The line color will be calculated automatically by attempting to make the
	 * current selection lighter by a factor of 1.2.
	 * 
	 * @param textComponent
	 *            text component that requires background line painting.
	 */
	public LinePainter(JTextComponent textComponent) {
		this(textComponent, null);
		setLighter(textComponent.getSelectionColor());
	}

	/**
	 * Manually control the line color.
	 * 
	 * @param textComponent
	 *            text component that requires background line painting.
	 * @param color
	 *            the color of the background line.
	 */
	public LinePainter(JTextComponent textComponent, Color color) {

		_textComponent = textComponent;
		setColor(color);

		// Add listeners so we know when to change highlighting

		textComponent.addCaretListener(this);
		textComponent.addMouseListener(this);
		textComponent.addMouseMotionListener(this);

		// Turn highlighting on by adding a dummy highlight

		try {
			textComponent.getHighlighter().addHighlight(0, 0, this);
		} catch (BadLocationException badLocationException) {
			
			// Updates the log
			AcideLog.getLog().error(badLocationException.getMessage());
			badLocationException.printStackTrace();
		}
	}

	/**
	 * You can reset the line color at any time.
	 * 
	 * @param color
	 *            the color of the background line.
	 */
	public void setColor(Color color) {
		_color = color;
	}

	/**
	 * Calculate the line color by making the selection color lighter.
	 * 
	 * @return the color of the background line.
	 */
	public void setLighter(Color color) {
		setColor(new Color(235, 235, 255));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.text.Highlighter.HighlightPainter#paint(java.awt.Graphics,
	 * int, int, java.awt.Shape, javax.swing.text.JTextComponent)
	 */
	@Override
	public void paint(Graphics g, int p0, int p1, Shape bounds, JTextComponent c) {
		try {

			Rectangle r = c.modelToView(c.getCaretPosition());
			g.setColor(_color);
			g.fillRect(0, r.y, c.getWidth(), r.height);

			if (_lastView == null)
				_lastView = r;
			
		} catch (BadLocationException badLocationException) {

			// Updates the log
			AcideLog.getLog().error(badLocationException.getMessage());
			badLocationException.printStackTrace();
		}
	}

	/**
	 * Caret position has changed, remove the highlight.
	 */
	private void resetHighlight() {

		// Uses invokeLater to make sure updates to the Document are completed,
		// otherwise Undo processing causes the modelToView method to loop.
		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				try {

					int offset = _textComponent.getCaretPosition();
					Rectangle currentView = _textComponent.modelToView(offset);

					// Removes the highlighting from the previously highlighted
					// line

					if (_lastView != null)
						if (_lastView.y != currentView.y) {
							_textComponent
									.repaint(0, _lastView.y,
											_textComponent.getWidth(),
											_lastView.height);
							_lastView = currentView;
						}
				} catch (BadLocationException badLocationException) {

					// Updates the log
					AcideLog.getLog().error(badLocationException.getMessage());
					badLocationException.printStackTrace();
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent)
	 */
	@Override
	public void caretUpdate(CaretEvent caretEvent) {
		resetHighlight();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		resetHighlight();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
	 */
	public void mouseEntered(MouseEvent mouseEvent) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseExited(MouseEvent mouseEvent) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseMotionListener#mouseDragged(java.awt.event.MouseEvent
	 * )
	 */
	@Override
	public void mouseDragged(MouseEvent mouseEvent) {
		resetHighlight();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseMotionListener#mouseMoved(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseMoved(MouseEvent mouseEvent) {
	}
}
