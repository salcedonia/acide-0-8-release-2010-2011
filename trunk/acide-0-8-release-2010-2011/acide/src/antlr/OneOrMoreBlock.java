package antlr;

/* ANTLR Translator Generator
 * Project led by Terence Parr at http://www.cs.usfca.edu
 * Software rights: http://www.antlr.org/license.html
 *
 * $Id: OneOrMoreBlock.java,v 1.1 2007/04/18 16:34:41 juanjortiz Exp $
 */

class OneOrMoreBlock extends BlockWithImpliedExitPath {

    public OneOrMoreBlock(Grammar g) {
        super(g);
    }

    public OneOrMoreBlock(Grammar g, Token start) {
        super(g, start);
    }

    public void generate() {
        grammar.generator.gen(this);
    }

    public Lookahead look(int k) {
        return grammar.theLLkAnalyzer.look(k, this);
    }

    public String toString() {
        return super.toString() + "+";
    }
}
