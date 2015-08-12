import org.bytedeco.javacpp.*;
import org.bytedeco.javacpp.annotation.*;

import it.gonzo.assessor.WordNetAssessor;
import it.gonzo.similarity.utils.SimilarityConstants;

import edu.stanford.nlp.classify.Classifier;
import edu.stanford.nlp.classify.ColumnDataClassifier;
import edu.stanford.nlp.classify.LinearClassifier;
import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.objectbank.ObjectBank;
import edu.stanford.nlp.util.ErasureUtils;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import java.util.ArrayList;
import java.util.List;


@Platform
public class Main {
    public static class Anagrind extends FunctionPointer {
      private static final String VERB = "verb";
      private static final String ANAGRIND = "1";
      private static final String SPACE = " ";

        public @Name("anagrind") boolean call(String words) {

          if (multipleWords(words)) {
            words = getSynonym(words);
          }

          try {
            getWordSimilarityMeasures(words);
          }
          catch (IOException e) {
            e.printStackTrace();
            return false;
          }

          try {
            return isAnagrind();
          }
          catch (ClassNotFoundException e1) {
            e1.printStackTrace();
            return false;
          }
          catch (IOException e2) {
            e2.printStackTrace();
            return false;
          }
        }

        private static boolean multipleWords(String words) {
            if (words.contains(SPACE))
              return true;
            return false;
        }

        private static String getSynonym(String words)
        // How to determine whether word has same meaning e.g. to Call On. Synonym = turn.
        // call on is not an anagrind, but turn is...
        // This could generate false positives.
          // SynonymFinder sf = new SynonymFinder;
          // sf.getSynonym(words);
          return words;
        }

        private static void getWordSimilarityMeasures(String word) throws IOException {
          WordNetAssessor wn = new WordNetAssessor();
          FileWriter fw = new FileWriter("output.tsv");

          String comparer = null;
          StringBuilder output = new StringBuilder();

          Stemmer s = new Stemmer();
          List<String> stems = new ArrayList<String>();

          output.append("1" + "\t" + word + "\t");
          stems.addAll(s.getStems(word, VERB));
          if (stems.size() == 0) {
            return;
          }


          FileReader fr2 = new FileReader("verbComparerPOP.txt");
          BufferedReader comparerReader = new BufferedReader(fr2);

          while((comparer = comparerReader.readLine()) != null) {
            output.append(wn.getWordNetVerbSimilarityByIC(stems.get(0), comparer,
            SimilarityConstants.FAITH_MEASURE,
            SimilarityConstants.INTRINSIC_IC));
            output.append("\t");
          }

          comparerReader.close();
          fr2.close();
          stems.clear();
          output.append("\n");

          fw.write(output.toString());
          fw.close();
        }

        private static boolean isAnagrind() throws IOException, ClassNotFoundException {
          PrintStream err = System.err;
          System.setErr(new PrintStream(new OutputStream() {
            public void write(int b) {
            }
          }));
          FileInputStream fis = new FileInputStream("anagrind.ser");
          ObjectInputStream ois = new ObjectInputStream(fis);
          LinearClassifier<String,String> lc = ErasureUtils.uncheckedCast(ois.readObject());
	        ois.close();
	        ColumnDataClassifier cdc2 = new ColumnDataClassifier("Anagrinds.prop");
          System.setErr(err);
          for (String line : ObjectBank.getLineIterator("output.tsv")) {
		          Datum<String,String> d2 = cdc2.makeDatumFromLine(line);
		          if (lc.classOf(d2).charAt(0) == '1') {
                return true;
              }
	        }
          return false;
        }
    }
}
