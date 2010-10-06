package antlr;

/* ANTLR Translator Generator
 * Project led by Terence Parr at http://www.cs.usfca.edu
 * Software rights: http://www.antlr.org/license.html
 *
 * $Id: ANTLRException.java,v 1.1 2007/04/18 16:34:40 juanjortiz Exp $
 */

public class ANTLRException extends Exception {

    public ANTLRException() {
        super();
    }

    public ANTLRException(String s) {
        super(s);
    }

	public ANTLRException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public ANTLRException(Throwable cause) {
		super(cause);
	}
}
