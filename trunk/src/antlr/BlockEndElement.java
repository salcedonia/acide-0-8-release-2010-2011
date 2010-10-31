package antlr;

/* ANTLR Translator Generator
 * Project led by Terence Parr at http://www.cs.usfca.edu
 * Software rights: http://www.antlr.org/license.html
 *
 * $Id: BlockEndElement.java,v 1.1 2007/04/18 16:34:40 juanjortiz Exp $
 */

/**All alternative blocks are "terminated" by BlockEndElements unless
 * they are rule blocks (in which case they use RuleEndElement).
 */
class BlockEndElement extends AlternativeElement {
    protected boolean[] lock;	// for analysis; used to avoid infinite loops
    protected AlternativeBlock block;// ending blocks know what block they terminate


    public BlockEndElement(Grammar g) {
        super(g);
        lock = new boolean[g.maxk + 1];
    }

    public Lookahead look(int k) {
        return grammar.theLLkAnalyzer.look(k, this);
    }

    public String toString() {
        //return " [BlkEnd]";
        return "";
    }
}
