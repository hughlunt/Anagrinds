import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.List;

import edu.mit.jwi.Dictionary;
import edu.mit.jwi.IDictionary;
import edu.mit.jwi.item.POS;
import edu.mit.jwi.morph.WordnetStemmer;

public class Stemmer {
	private final String PATH = "/usr/local/WordNet-3.0" + File.separator + "dict";
	private static final String VERB = "verb";
	private static final String ADJECTIVE = "adjective";
	private static final String ADVERB = "adverb";

	private WordnetStemmer stemmer;
	private URL url;
	private IDictionary dict;


	public Stemmer() throws IOException {
		this.url = new URL ("file", null, PATH);

		this.dict = new Dictionary (url);
		dict.open();

		this.stemmer = new WordnetStemmer (dict);
	}


	public List<String> getStems(String word, String pos) {
		switch (pos) {
		case VERB :
			return stemmer.findStems(word, POS.VERB);
		case ADJECTIVE :
			return stemmer.findStems(word, POS.ADJECTIVE);
		case ADVERB :
			return stemmer.findStems(word, POS.ADVERB);
		default :
			return Collections.emptyList();
		}
	}

}
