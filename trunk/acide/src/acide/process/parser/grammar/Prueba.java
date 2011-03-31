package acide.process.parser.grammar;

import java.io.StringReader;

import javax.swing.JOptionPane;

import antlr.RecognitionException;
import antlr.TokenStreamException;

public class Prueba {
	
	//private static String code;
	
	/**
	 * 
	 * @param code
	 * @throws TokenStreamException 
	 * @throws RecognitionException 
	 */
	public static void analyze(String code) /*throws RecognitionException, TokenStreamException*/ {
	//public static void main(String[] args) {
		//ExprLexer lexer = new ExprLexer(new StringReader(code));
		GrammarLexer lexer = new GrammarLexer(new StringReader(code));
        //ExprParser parser = new ExprParser(lexer);
		GrammarParser parser = new GrammarParser(lexer);
        //boolean noException = true;
        //boolean recException = false;
        //boolean tokException = false;
        try {
			parser.expr();
		}
		catch (RecognitionException e) {
			//e.printStackTrace();
			//System.out.println("Capturada RecognitionException");
			//JOptionPane.showConfirmDialog(null,"RecognitionException");
			JOptionPane.showMessageDialog(null,"RecognitionException","RecognitionException",JOptionPane.ERROR_MESSAGE);
			System.out.println("RecognitionException: " + e);
			//noException = false;
			//recException = true;
		}
		catch (TokenStreamException e) {
			//e.printStackTrace();
			//System.out.println("Capturada TokenStreamException");
			//JOptionPane.showConfirmDialog(null,"TokenStreamException");
			//JOptionPane.showMessageDialog(null,"TokenStreamException","TokenStreamException",JOptionPane.ERROR_MESSAGE);
			JOptionPane.showMessageDialog(null,e,"TokenStreamException",JOptionPane.ERROR_MESSAGE);
			System.out.println("TokenStreamException: " + e);
			//noException = false;
			//tokException = true;
		}
        /*catch(Exception e) {
        	noException = false;
        	JOptionPane.showMessageDialog(null,"Exception","Exception",JOptionPane.ERROR_MESSAGE);
        }*/
		//if(noException) JOptionPane.showMessageDialog(null,"Correcto","Correct",JOptionPane.INFORMATION_MESSAGE);
		//if(recException) JOptionPane.showMessageDialog(null,"RecognitionException","RecognitionException",JOptionPane.ERROR_MESSAGE);
		//if(tokException) JOptionPane.showMessageDialog(null,"TokenStreamException","TokenStreamException",JOptionPane.ERROR_MESSAGE);
	}

}
