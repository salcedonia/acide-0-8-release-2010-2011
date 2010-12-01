package antlr.collections.impl;

/* ANTLR Translator Generator
 * Project led by Terence Parr at http://www.cs.usfca.edu
 * Software rights: http://www.antlr.org/license.html
 *
 * $Id: LLCell.java,v 1.1 2007/04/18 16:34:41 juanjortiz Exp $
 */

/**A linked list cell, which contains a ref to the object and next cell.
 * The data,next members are public to this class, but not outside the
 * collections.impl package.
 *
 * @author Terence Parr
 * <a href=http://www.MageLang.com>MageLang Institute</a>
 */
class LLCell {
    Object data;
    LLCell next;


    public LLCell(Object o) {
        data = o;
    }
}
