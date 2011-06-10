package acide.utils;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

public class TextComponentIndent extends JFrame {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public TextComponentIndent() {
		JTextPane textComponent = new JTextPane();
		textComponent.setText("\tone\ntwo\nthree\n\tfour");
		JScrollPane scrollPane = new JScrollPane(textComponent);
		getContentPane().add(scrollPane);
		ActionMap am = textComponent.getActionMap();
		am.put(DefaultEditorKit.insertBreakAction, new IndentBreakAction());
	}

	public static class IndentBreakAction extends TextAction {
		/**
		 * Creates this object with the appropriate identifier.
		 */
		public IndentBreakAction() {
			super(DefaultEditorKit.insertBreakAction);
		}

		/**
		 * The operation to perform when this action is triggered.
		 * 
		 * @param e
		 *            the action event
		 */
		public void actionPerformed(ActionEvent e) {
			JTextComponent target = getTextComponent(e);
			if (target == null)
				return;
			if ((!target.isEditable()) || (!target.isEnabled())) {
				UIManager.getLookAndFeel().provideErrorFeedback(target);
				return;
			}
			try {
				// Determine which line we are on
				Document doc = target.getDocument();
				Element rootElement = doc.getDefaultRootElement();
				int selectionStart = target.getSelectionStart();
				int line = rootElement.getElementIndex(selectionStart);
				// Get the text for this line
				int start = rootElement.getElement(line).getStartOffset();
				int end = rootElement.getElement(line).getEndOffset();
				int length = end - start;
				String text = doc.getText(start, length);
				int offset = 0;
				// Get the number of white spaces characters at the start of the
				// line
				for (offset = 0; offset < length; offset++) {
					char c = text.charAt(offset);
					if (c != ' ' && c != '\t')
						break;
				}
				// When splitting the text include white space at start of line
				// else do default processing
				if (selectionStart - start > offset)
					target.replaceSelection("\n" + text.substring(0, offset));
				else
					target.replaceSelection("\n");
			} catch (BadLocationException ble) {
			}
		}
	}

	public static void main(String[] args) {
		TextComponentIndent frame = new TextComponentIndent();
		frame.setDefaultCloseOperation(EXIT_ON_CLOSE);
		frame.setSize(new Dimension(300, 100));
		frame.setLocationRelativeTo(null);
		frame.setVisible(true);
	}
}
