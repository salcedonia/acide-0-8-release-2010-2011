package utils;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.event.*;
import java.awt.Color;
import java.awt.BorderLayout;
import java.util.Stack;

public class BraceMatcher extends JTextPane implements Runnable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static Color[] matchColor = { Color.blue, Color.magenta, Color.green };
	public static Color badColor = Color.red;

	private AttributeSet[] matchAttrSet;
	private AttributeSet badAttrSet;

	public BraceMatcher() {
		// create an array of AttributeSets from the array of Colors
		StyleContext sc = StyleContext.getDefaultStyleContext();
		badAttrSet = sc.addAttribute(SimpleAttributeSet.EMPTY,
				StyleConstants.Foreground, badColor);
		matchAttrSet = new AttributeSet[matchColor.length];
		for (int j = 0; j < matchColor.length; j++)
			matchAttrSet[j] = sc.addAttribute(SimpleAttributeSet.EMPTY,
					StyleConstants.Foreground, matchColor[j]);
	}

	// match and color the parens/brackets/braces
	public void run() {
		StyledDocument doc = getStyledDocument();
		String text = "";
		int len = doc.getLength();
		try {
			text = doc.getText(0, len);
		} catch (BadLocationException ble) {
		}
		Stack<String> stack = new Stack<String>();
		for (int j = 0; j < text.length(); j += 1) {
			char ch = text.charAt(j);
			if (ch == '(' || ch == '[' || ch == '{') {
				int depth = stack.size();
				stack.push("" + ch + j); // push a String containg the char and
											// the offset
				AttributeSet aset = matchAttrSet[depth % matchAttrSet.length];
				doc.setCharacterAttributes(j, 1, aset, false);
			}
			if (ch == ')' || ch == ']' || ch == '}') {
				String peek = stack.empty() ? "." : (String) stack.peek();
				if (matches(peek.charAt(0), ch)) { // does it match?
					stack.pop();
					int depth = stack.size();
					AttributeSet aset = matchAttrSet[depth
							% matchAttrSet.length];
					doc.setCharacterAttributes(j, 1, aset, false);
				} else { // mismatch
					doc.setCharacterAttributes(j, 1, badAttrSet, false);
				}
			}
		}

		while (!stack.empty()) { // anything left in the stack is a mismatch
			String pop = (String) stack.pop();
			int offset = Integer.parseInt(pop.substring(1));
			doc.setCharacterAttributes(offset, 1, badAttrSet, false);
		}
	}

	// unset the foreground color (if any) whenever the user enters text
	// (if not for this, text entered after a paren would catch the paren's
	// color)
	public void replaceSelection(String content) {
		getInputAttributes().removeAttribute(StyleConstants.Foreground);
		super.replaceSelection(content);
	}

	// return true if 'left' and 'right' are matching parenthesis/brackets/braces
	public static boolean matches(char left, char right) {
		if (left == '(')
			return (right == ')');
		if (left == '[')
			return (right == ']');
		if (left == '{')
			return (right == '}');
		return false;
	}

	public static void main(String[] args) {
		JFrame frame = new JFrame("ParenMatcher");

		final BraceMatcher matcher = new BraceMatcher();
		matcher.setText("int fact(int n) {\n" + "  if (n <= 1) return 1;\n"
				+ "  return(n * fact(n-1));\n" + "}\n");
		frame.getContentPane().add(new JScrollPane(matcher),
				BorderLayout.CENTER);

		JButton matchButton = new JButton("match parens");
		matchButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ae) {
				matcher.run();
			}
		});
		frame.getContentPane().add(matchButton, BorderLayout.SOUTH);

		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setSize(200, 150);
		frame.setVisible(true);
	}
}
