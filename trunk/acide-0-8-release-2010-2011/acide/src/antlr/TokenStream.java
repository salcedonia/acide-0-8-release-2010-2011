package antlr;

/* ANTLR Translator Generator
 * Project led by Terence Parr at http://www.cs.usfca.edu
 * Software rights: http://www.antlr.org/license.html
 *
 * $Id: TokenStream.java,v 1.1 2007/04/18 16:34:40 juanjortiz Exp $
 */

public interface TokenStream {
    public Token nextToken() throws TokenStreamException;
}
