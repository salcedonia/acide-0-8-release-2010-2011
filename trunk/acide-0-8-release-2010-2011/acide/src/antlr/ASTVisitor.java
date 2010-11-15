package antlr;

/* ANTLR Translator Generator
 * Project led by Terence Parr at http://www.cs.usfca.edu
 * Software rights: http://www.antlr.org/license.html
 *
 * $Id: ASTVisitor.java,v 1.1 2007/04/18 16:34:41 juanjortiz Exp $
 */

import antlr.collections.AST;

public interface ASTVisitor {
    public void visit(AST node);
}
