package acide.process.parser.grammar;

import antlr.RecognitionException;
import antlr.TokenStreamException;

public class PruebaMicrocalculadora {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		ExprLexer lexer = new ExprLexer(System.in);
        ExprParser parser = new ExprParser(lexer);
        try {
			parser.expr();
		}
		catch (RecognitionException e) {
			//e.printStackTrace();
			System.out.println("Capturada RecognitionException");
		}
		catch (TokenStreamException e) {
			//e.printStackTrace();
			System.out.println("Capturada TokenStreamException");
		}
	}

}
