package gui.outputPanel.listeners;

import gui.mainWindow.MainWindow;
import gui.outputPanel.AcideOutputPanel;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.text.BadLocationException;

import operations.log.AcideLog;

/************************************************************************																
 * Output panel keyboard listener.										
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
 * @see KeyListener																													
 ***********************************************************************/
public class AcideOutputPanelKeyboardListener implements KeyListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyTyped(KeyEvent keyEvent) {

		AcideOutputPanel outputPanel = MainWindow.getInstance().getOutput();
		
		if (outputPanel.getTextComponent() != null) {

			if (outputPanel.getTextComponent().getCaretPosition() < outputPanel.getPromptCaretPosition())
				keyEvent.consume();
			else
				outputPanel
						.setSelectionSize(outputPanel.getSelectionSize() + 1);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		AcideOutputPanel outputPanel = MainWindow.getInstance().getOutput();
		
		if (outputPanel.getTextComponent() != null) {

			// If the caret is in the limit with the prompt
			// and the key is left, up or down
			if ((outputPanel.getTextComponent().getCaretPosition() == outputPanel.getPromptCaretPosition())
					&& ((keyEvent.getKeyCode() == KeyEvent.VK_LEFT)
							|| (keyEvent.getKeyCode() == 8) || keyEvent
							.getKeyCode() == KeyEvent.VK_UP))
				// Consumes the key
				keyEvent.consume();

			// If the caret is behind the prompt
			if (outputPanel.getTextComponent().getSelectionStart() < outputPanel.getPromptCaretPosition())

				// Consumes the key
				keyEvent.consume();

			// Gets the command
			String command = (String) outputPanel.getTextComponent().getText().subSequence(
					outputPanel.getPromptCaretPosition(), outputPanel.getDefaultStyledDocument().getLength());

			switch (keyEvent.getKeyCode()) {

			case KeyEvent.VK_ENTER:

				if (keyEvent.getKeyChar() == '\n') {

					if (outputPanel.getProcessThread().getWriter() != null)
						outputPanel.sendCommandToOutput(command);
				}
				break;

			case KeyEvent.VK_UP:

				keyEvent.consume();

				if (outputPanel.getProcessThread().getWriter() != null) {

					// If there are commands in the historic
					if (outputPanel.getHistoricCurrentIndex() > -1) {

						if (outputPanel.getHistoricCurrentIndex() == 0)
							outputPanel.setHistoricCurrentIndex(outputPanel.getHistoricMaximumIndex() - 1);
						else
							outputPanel.setHistoricCurrentIndex(outputPanel.getHistoricCurrentIndex() - 1);

						try {

							outputPanel.getDefaultStyledDocument().remove(outputPanel.getPromptCaretPosition(),
									command.length());
							
							if (outputPanel.getHistoricCurrentIndex() > -1)
								outputPanel.getDefaultStyledDocument().insertString(outputPanel.getPromptCaretPosition(),
										outputPanel.getHistoric().get(outputPanel.getHistoricCurrentIndex()), null);
						} catch (BadLocationException exception) {
							
							// Updates the log
							AcideLog.getLog().error(exception.getMessage());
							exception.printStackTrace();
						}
					}
				}
				break;

			case KeyEvent.VK_DOWN:

				if (outputPanel.getProcessThread().getWriter() != null) {
					
					// If there are commands in the historic
					if (outputPanel.getHistoricCurrentIndex() > -1) {

						if (outputPanel.getHistoricCurrentIndex() >= outputPanel.getHistoricMaximumIndex() - 1)
							outputPanel.setHistoricCurrentIndex(0);
						else
							outputPanel.setHistoricCurrentIndex(outputPanel.getHistoricCurrentIndex() + 1);

						// Replaces the command by the previous one
						try {
							outputPanel.getDefaultStyledDocument().remove(outputPanel.getPromptCaretPosition(),
									command.length());
							outputPanel.getDefaultStyledDocument().insertString(outputPanel.getPromptCaretPosition(),
									outputPanel.getHistoric().get(outputPanel.getHistoricCurrentIndex()), null);
						} catch (BadLocationException exception) {
							
							// Updates the log
							AcideLog.getLog().error(exception.getMessage());
							exception.printStackTrace();
						}
					}
				}
				break;

			case KeyEvent.VK_ESCAPE:

				if (outputPanel.getProcessThread().getWriter() != null) {
					
					// Removes the text selection
					try {
						outputPanel.getDefaultStyledDocument().remove(outputPanel.getPromptCaretPosition(), command.length());
					} catch (BadLocationException exception) {
						
						// Updates the log
						AcideLog.getLog().error(exception.getMessage());
						exception.printStackTrace();
					}
				}
				break;

			case KeyEvent.VK_HOME:

				// Consumes the key
				keyEvent.consume();

				if (outputPanel.getProcessThread().getWriter() != null) {
					// Sets the caret after the prompt
					outputPanel.getTextComponent().setCaretPosition(outputPanel.getPromptCaretPosition());
				}
				break;

			case KeyEvent.VK_END:

				// Consumes the key
				keyEvent.consume();

				if (outputPanel.getProcessThread().getWriter() != null) {
					// Sets the caret at the end of the command
					outputPanel.getTextComponent().setCaretPosition(outputPanel.getPromptCaretPosition()
							+ command.length());
				}
				break;

			case KeyEvent.VK_C:

				// CTRL + C --> COPY
				if (keyEvent.isControlDown())
					outputPanel.getTextComponent().copy();
				break;
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyReleased(KeyEvent keyEvent) {

		AcideOutputPanel outputPanel = MainWindow.getInstance().getOutput();
		
		if (outputPanel.getTextComponent() != null)
			
			// If the caret is the not editable zone
			if (outputPanel.getTextComponent().getCaretPosition() < outputPanel.getPromptCaretPosition())

				// Consumes the key
				keyEvent.consume();
	}
}
